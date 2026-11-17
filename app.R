# app.R
#
# Interactive multi-page PDF template builder and batch parser.
#
# Implements:
#   - Multi-page PDF template creation
#   - JSON template import/export
#   - Native embedded-PDF-text extraction
#   - Sample preview
#   - Batch parsing from uploaded PDFs or a selected directory
#   - One output row per PDF and one column per field
#
# OCR is not included in this version.

# =============================================================================
# Package setup
# =============================================================================

required_packages <- c(
  "shiny",
  "shinyFiles",
  "pdftools",
  "png",
  "jsonlite",
  "DT"
)

missing_packages <- setdiff(
  required_packages,
  rownames(installed.packages())
)

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "The following packages are required but not installed: ",
      paste(missing_packages, collapse = ", "),
      "\n\nInstall them with:\n",
      "install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

invisible(lapply(
  required_packages,
  library,
  character.only = TRUE
))

# =============================================================================
# Constants and defaults
# =============================================================================

TEMPLATE_SCHEMA_VERSION <- "1.0.0"
BUILDER_RENDER_DPI <- 150L

default_parser_settings <- function() {
  list(
    x_pad_pct = 0.005,
    y_pad_pct = 0.005,
    capture_mode = "center",
    min_word_overlap_pct = 1,
    line_tolerance = 3
  )
}

empty_fields <- function() {
  data.frame(
    field_id = character(),
    name = character(),
    page = integer(),
    xmin_norm = numeric(),
    ymin_norm = numeric(),
    xmax_norm = numeric(),
    ymax_norm = numeric(),
    stringsAsFactors = FALSE
  )
}

empty_parse_result <- function() {
  data.frame(
    field_id = character(),
    field_name = character(),
    page = integer(),
    raw_text = character(),
    extraction_method = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# General utility functions
# =============================================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

scalar_value <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }

  x[[1]]
}

safe_character <- function(x, default = "") {
  value <- scalar_value(x, default)

  if (is.null(value) || is.na(value)) {
    return(default)
  }

  as.character(value)
}

safe_numeric <- function(x, default = 0) {
  value <- suppressWarnings(as.numeric(scalar_value(x, default)))

  if (length(value) == 0 || is.na(value) || !is.finite(value)) {
    return(default)
  }

  value
}

safe_integer <- function(x, default = 0L) {
  value <- suppressWarnings(as.integer(scalar_value(x, default)))

  if (length(value) == 0 || is.na(value)) {
    return(default)
  }

  value
}

collapse_messages <- function(x) {
  x <- unique(trimws(as.character(x)))
  x <- x[!is.na(x) & nzchar(x)]

  paste(x, collapse = "; ")
}

make_field_id <- function() {
  paste0(
    "field_",
    format(Sys.time(), "%Y%m%d%H%M%OS6"),
    "_",
    sprintf("%06d", sample.int(999999, 1))
  )
}

safe_filename_stem <- function(x, default = "pdf_template") {
  x <- trimws(x %||% "")

  if (!nzchar(x)) {
    x <- default
  }

  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)

  if (!nzchar(x)) {
    default
  } else {
    x
  }
}

# =============================================================================
# Template schema and JSON functions
# =============================================================================

normalize_fields <- function(fields) {
  if (is.null(fields) || length(fields) == 0) {
    return(empty_fields())
  }

  if (is.data.frame(fields)) {
    fields_df <- fields
  } else if (
    is.list(fields) &&
      length(fields) > 0 &&
      all(vapply(fields, is.list, logical(1)))
  ) {
    fields_df <- do.call(
      rbind,
      lapply(fields, function(field) {
        as.data.frame(field, stringsAsFactors = FALSE)
      })
    )
  } else {
    fields_df <- tryCatch(
      as.data.frame(fields, stringsAsFactors = FALSE),
      error = function(e) {
        stop(
          "Template fields could not be converted to a data frame: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  required_columns <- c(
    "field_id",
    "name",
    "page",
    "xmin_norm",
    "ymin_norm",
    "xmax_norm",
    "ymax_norm"
  )

  for (column_name in required_columns) {
    if (!column_name %in% names(fields_df)) {
      fields_df[[column_name]] <- NA
    }
  }

  fields_df <- fields_df[, required_columns, drop = FALSE]

  fields_df$field_id <- as.character(fields_df$field_id)
  fields_df$name <- as.character(fields_df$name)
  fields_df$page <- suppressWarnings(as.integer(fields_df$page))

  coordinate_columns <- c(
    "xmin_norm",
    "ymin_norm",
    "xmax_norm",
    "ymax_norm"
  )

  for (column_name in coordinate_columns) {
    fields_df[[column_name]] <- suppressWarnings(
      as.numeric(fields_df[[column_name]])
    )
  }

  missing_ids <- is.na(fields_df$field_id) |
    !nzchar(trimws(fields_df$field_id))

  if (any(missing_ids)) {
    fields_df$field_id[missing_ids] <- vapply(
      seq_len(sum(missing_ids)),
      function(i) make_field_id(),
      character(1)
    )
  }

  rownames(fields_df) <- NULL
  fields_df
}

normalize_parser_settings <- function(settings) {
  defaults <- default_parser_settings()
  settings <- settings %||% list()

  capture_mode <- safe_character(
    settings$capture_mode,
    defaults$capture_mode
  )

  if (!capture_mode %in% c("center", "overlap")) {
    capture_mode <- defaults$capture_mode
  }

  list(
    x_pad_pct = safe_numeric(
      settings$x_pad_pct,
      defaults$x_pad_pct
    ),
    y_pad_pct = safe_numeric(
      settings$y_pad_pct,
      defaults$y_pad_pct
    ),
    capture_mode = capture_mode,
    min_word_overlap_pct = safe_numeric(
      settings$min_word_overlap_pct,
      defaults$min_word_overlap_pct
    ),
    line_tolerance = safe_numeric(
      settings$line_tolerance,
      defaults$line_tolerance
    )
  )
}

normalize_template <- function(template) {
  if (!is.list(template)) {
    stop("The template must be a JSON object.", call. = FALSE)
  }

  metadata <- template$template %||% list()

  normalized <- list(
    schema_version = safe_character(
      template$schema_version,
      TEMPLATE_SCHEMA_VERSION
    ),
    template = list(
      name = safe_character(metadata$name, "pdf_template"),
      created_at = safe_character(
        metadata$created_at,
        format(Sys.time(), tz = "UTC", usetz = TRUE)
      ),
      source_pdf_name = safe_character(
        metadata$source_pdf_name,
        ""
      ),
      expected_pages = safe_integer(
        metadata$expected_pages,
        0L
      ),
      render_dpi = safe_integer(
        metadata$render_dpi,
        BUILDER_RENDER_DPI
      )
    ),
    settings = normalize_parser_settings(template$settings),
    fields = normalize_fields(template$fields)
  )

  normalized
}

create_template_object <- function(
  name,
  source_pdf_name,
  expected_pages,
  fields,
  settings,
  created_at = NULL
) {
  normalize_template(list(
    schema_version = TEMPLATE_SCHEMA_VERSION,
    template = list(
      name = name,
      created_at = created_at %||%
        format(Sys.time(), tz = "UTC", usetz = TRUE),
      source_pdf_name = source_pdf_name,
      expected_pages = expected_pages,
      render_dpi = BUILDER_RENDER_DPI
    ),
    settings = settings,
    fields = fields
  ))
}

validate_template <- function(template) {
  problems <- character()

  normalized <- tryCatch(
    normalize_template(template),
    error = function(e) e
  )

  if (inherits(normalized, "error")) {
    return(paste("Template normalization failed:", normalized$message))
  }

  if (
    !identical(
      normalized$schema_version,
      TEMPLATE_SCHEMA_VERSION
    )
  ) {
    problems <- c(
      problems,
      paste0(
        "Unsupported schema version '",
        normalized$schema_version,
        "'. This application supports version '",
        TEMPLATE_SCHEMA_VERSION,
        "'."
      )
    )
  }

  if (!nzchar(trimws(normalized$template$name))) {
    problems <- c(problems, "The template name is empty.")
  }

  settings <- normalized$settings

  if (
    !is.finite(settings$x_pad_pct) ||
      settings$x_pad_pct < 0 ||
      settings$x_pad_pct > 1
  ) {
    problems <- c(
      problems,
      "Horizontal padding must be between 0 and 1."
    )
  }

  if (
    !is.finite(settings$y_pad_pct) ||
      settings$y_pad_pct < 0 ||
      settings$y_pad_pct > 1
  ) {
    problems <- c(
      problems,
      "Vertical padding must be between 0 and 1."
    )
  }

  if (!settings$capture_mode %in% c("center", "overlap")) {
    problems <- c(
      problems,
      "Capture mode must be 'center' or 'overlap'."
    )
  }

  if (
    !is.finite(settings$min_word_overlap_pct) ||
      settings$min_word_overlap_pct < 0 ||
      settings$min_word_overlap_pct > 100
  ) {
    problems <- c(
      problems,
      "Minimum word overlap must be between 0 and 100."
    )
  }

  if (
    !is.finite(settings$line_tolerance) ||
      settings$line_tolerance <= 0
  ) {
    problems <- c(
      problems,
      "Line tolerance must be greater than zero."
    )
  }

  fields <- normalized$fields

  if (nrow(fields) == 0) {
    problems <- c(
      problems,
      "The template does not contain any fields."
    )

    return(unique(problems))
  }

  blank_names <- is.na(fields$name) |
    !nzchar(trimws(fields$name))

  if (any(blank_names)) {
    problems <- c(
      problems,
      paste0(
        "Fields with blank names: ",
        paste(which(blank_names), collapse = ", "),
        "."
      )
    )
  }

  duplicated_names <- unique(
    fields$name[duplicated(fields$name)]
  )

  duplicated_names <- duplicated_names[
    !is.na(duplicated_names)
  ]

  if (length(duplicated_names) > 0) {
    problems <- c(
      problems,
      paste0(
        "Field names must be unique. Duplicates: ",
        paste(duplicated_names, collapse = ", "),
        "."
      )
    )
  }

  duplicated_ids <- unique(
    fields$field_id[duplicated(fields$field_id)]
  )

  duplicated_ids <- duplicated_ids[
    !is.na(duplicated_ids)
  ]

  if (length(duplicated_ids) > 0) {
    problems <- c(
      problems,
      "Field IDs must be unique."
    )
  }

  invalid_pages <- is.na(fields$page) | fields$page < 1

  if (any(invalid_pages)) {
    problems <- c(
      problems,
      paste0(
        "Fields with invalid page numbers: ",
        paste(which(invalid_pages), collapse = ", "),
        "."
      )
    )
  }

  coordinate_columns <- c(
    "xmin_norm",
    "ymin_norm",
    "xmax_norm",
    "ymax_norm"
  )

  for (column_name in coordinate_columns) {
    values <- fields[[column_name]]

    invalid <- is.na(values) |
      !is.finite(values) |
      values < 0 |
      values > 1

    if (any(invalid)) {
      problems <- c(
        problems,
        paste0(
          "Column '",
          column_name,
          "' contains values outside 0 to 1."
        )
      )
    }
  }

  invalid_x <- is.na(fields$xmin_norm) |
    is.na(fields$xmax_norm) |
    fields$xmin_norm >= fields$xmax_norm

  if (any(invalid_x)) {
    problems <- c(
      problems,
      "Every field must have xmin_norm less than xmax_norm."
    )
  }

  invalid_y <- is.na(fields$ymin_norm) |
    is.na(fields$ymax_norm) |
    fields$ymin_norm >= fields$ymax_norm

  if (any(invalid_y)) {
    problems <- c(
      problems,
      "Every field must have ymin_norm less than ymax_norm."
    )
  }

  unique(problems)
}

write_template_json <- function(template, file) {
  problems <- validate_template(template)

  if (length(problems) > 0) {
    stop(
      paste(
        "Template validation failed:",
        paste(problems, collapse = "\n- "),
        sep = "\n- "
      ),
      call. = FALSE
    )
  }

  template <- normalize_template(template)

  jsonlite::write_json(
    template,
    path = file,
    pretty = TRUE,
    auto_unbox = TRUE,
    dataframe = "rows",
    na = "null",
    null = "null",
    digits = 15
  )
}

read_template_json <- function(file) {
  raw_template <- jsonlite::read_json(
    file,
    simplifyVector = TRUE
  )

  template <- normalize_template(raw_template)
  problems <- validate_template(template)

  if (length(problems) > 0) {
    stop(
      paste(
        "The imported template is invalid:",
        paste(problems, collapse = "\n- "),
        sep = "\n- "
      ),
      call. = FALSE
    )
  }

  template
}

# =============================================================================
# PDF rendering functions
# =============================================================================

render_pdf_page <- function(
  pdf_file,
  page,
  dpi = BUILDER_RENDER_DPI
) {
  output_file <- tempfile(
    pattern = paste0("pdf_page_", page, "_"),
    fileext = ".png"
  )

  on.exit(unlink(output_file, force = TRUE), add = TRUE)

  rendered_files <- pdftools::pdf_convert(
    pdf = pdf_file,
    format = "png",
    pages = as.integer(page),
    dpi = as.integer(dpi),
    filenames = output_file,
    verbose = FALSE
  )

  if (
    length(rendered_files) == 0 ||
      !file.exists(rendered_files[[1]])
  ) {
    stop(
      paste("PDF page", page, "could not be rendered."),
      call. = FALSE
    )
  }

  image <- png::readPNG(rendered_files[[1]])
  image_dimensions <- dim(image)

  if (
    is.null(image_dimensions) ||
      length(image_dimensions) < 2
  ) {
    stop(
      paste("Rendered page", page, "has invalid image dimensions."),
      call. = FALSE
    )
  }

  page_height <- as.numeric(image_dimensions[[1]])
  page_width <- as.numeric(image_dimensions[[2]])

  if (
    length(page_width) != 1 ||
      length(page_height) != 1 ||
      !is.finite(page_width) ||
      !is.finite(page_height) ||
      page_width <= 0 ||
      page_height <= 0
  ) {
    stop(
      paste(
        "Rendered page has invalid dimensions:",
        page_width,
        "x",
        page_height
      ),
      call. = FALSE
    )
  }

  list(
    image = image,
    width = page_width,
    height = page_height,
    page = as.integer(page),
    dpi = as.integer(dpi)
  )
}

# =============================================================================
# Native PDF extraction engine
# =============================================================================

prepare_native_page_words <- function(words) {
  if (is.null(words) || nrow(words) == 0) {
    return(data.frame())
  }

  words <- as.data.frame(
    words,
    stringsAsFactors = FALSE
  )

  required_columns <- c(
    "x",
    "y",
    "width",
    "height",
    "text"
  )

  if (!all(required_columns %in% names(words))) {
    return(data.frame())
  }

  words <- words[
    !is.na(words$text) & nzchar(words$text),
    ,
    drop = FALSE
  ]

  if (nrow(words) == 0) {
    return(data.frame())
  }

  words$word_xmin <- words$x
  words$word_ymin <- words$y
  words$word_xmax <- words$x + words$width
  words$word_ymax <- words$y + words$height
  words$word_xcenter <- words$x + words$width / 2
  words$word_ycenter <- words$y + words$height / 2

  words
}

calculate_field_bounds <- function(
  field,
  page_width,
  page_height,
  settings
) {
  list(
    xmin = max(
      0,
      field$xmin_norm[[1]] * page_width - settings$x_pad_pct * page_width
    ),
    xmax = min(
      page_width,
      field$xmax_norm[[1]] * page_width + settings$x_pad_pct * page_width
    ),
    ymin = max(
      0,
      field$ymin_norm[[1]] * page_height - settings$y_pad_pct * page_height
    ),
    ymax = min(
      page_height,
      field$ymax_norm[[1]] * page_height + settings$y_pad_pct * page_height
    )
  )
}

select_words_for_field <- function(
  words,
  bounds,
  settings
) {
  if (nrow(words) == 0) {
    return(words)
  }

  if (identical(settings$capture_mode, "center")) {
    keep <- words$word_xcenter >= bounds$xmin &
      words$word_xcenter <= bounds$xmax &
      words$word_ycenter >= bounds$ymin &
      words$word_ycenter <= bounds$ymax
  } else {
    overlap_width <- pmax(
      0,
      pmin(words$word_xmax, bounds$xmax) -
        pmax(words$word_xmin, bounds$xmin)
    )

    overlap_height <- pmax(
      0,
      pmin(words$word_ymax, bounds$ymax) -
        pmax(words$word_ymin, bounds$ymin)
    )

    overlap_area <- overlap_width * overlap_height
    word_area <- pmax(words$width * words$height, 1e-9)
    overlap_fraction <- overlap_area / word_area

    keep <- overlap_fraction >= settings$min_word_overlap_pct / 100
  }

  words[keep, , drop = FALSE]
}

collapse_selected_words <- function(
  selected_words,
  line_tolerance
) {
  if (nrow(selected_words) == 0) {
    return("")
  }

  line_tolerance <- max(line_tolerance, 1e-9)

  line_key <- round(
    selected_words$y / line_tolerance
  )

  selected_words <- selected_words[
    order(line_key, selected_words$x),
    ,
    drop = FALSE
  ]

  paste(selected_words$text, collapse = " ")
}

extract_native_field <- function(
  field,
  words,
  page_width,
  page_height,
  settings
) {
  bounds <- calculate_field_bounds(
    field = field,
    page_width = page_width,
    page_height = page_height,
    settings = settings
  )

  selected_words <- select_words_for_field(
    words = words,
    bounds = bounds,
    settings = settings
  )

  collapse_selected_words(
    selected_words = selected_words,
    line_tolerance = settings$line_tolerance
  )
}

parse_pdf_fields <- function(
  pdf_file,
  template
) {
  template <- normalize_template(template)
  problems <- validate_template(template)

  if (length(problems) > 0) {
    stop(
      paste(
        "Template validation failed:",
        paste(problems, collapse = "\n- "),
        sep = "\n- "
      ),
      call. = FALSE
    )
  }

  if (!file.exists(pdf_file)) {
    stop(
      paste("PDF file does not exist:", pdf_file),
      call. = FALSE
    )
  }

  fields <- template$fields
  settings <- template$settings

  pdf_information <- pdftools::pdf_info(pdf_file)
  actual_pages <- safe_integer(pdf_information$pages, 0L)

  if (actual_pages < 1) {
    stop(
      "The PDF does not contain any readable pages.",
      call. = FALSE
    )
  }

  page_sizes <- pdftools::pdf_pagesize(pdf_file)
  all_pdf_words <- pdftools::pdf_data(pdf_file)

  prepared_pages <- vector(
    mode = "list",
    length = actual_pages
  )

  needed_pages <- sort(unique(fields$page))
  available_needed_pages <- needed_pages[
    needed_pages >= 1 &
      needed_pages <= actual_pages
  ]

  for (page_number in available_needed_pages) {
    prepared_pages[[page_number]] <-
      prepare_native_page_words(
        all_pdf_words[[page_number]]
      )
  }

  results <- vector(
    mode = "list",
    length = nrow(fields)
  )

  for (i in seq_len(nrow(fields))) {
    field <- fields[i, , drop = FALSE]
    page_number <- field$page[[1]]

    if (
      is.na(page_number) ||
        page_number < 1 ||
        page_number > actual_pages
    ) {
      results[[i]] <- data.frame(
        field_id = field$field_id[[1]],
        field_name = field$name[[1]],
        page = page_number,
        raw_text = "",
        extraction_method = "native",
        status = "warning",
        message = paste0(
          "Template field requires page ",
          page_number,
          ", but the PDF contains ",
          actual_pages,
          " page(s)."
        ),
        stringsAsFactors = FALSE
      )

      next
    }

    page_width <- page_sizes$width[[page_number]]
    page_height <- page_sizes$height[[page_number]]
    words <- prepared_pages[[page_number]]

    if (is.null(words) || nrow(words) == 0) {
      results[[i]] <- data.frame(
        field_id = field$field_id[[1]],
        field_name = field$name[[1]],
        page = page_number,
        raw_text = "",
        extraction_method = "native",
        status = "warning",
        message = paste0(
          "Page ",
          page_number,
          " does not contain embedded PDF text."
        ),
        stringsAsFactors = FALSE
      )

      next
    }

    extracted_text <- extract_native_field(
      field = field,
      words = words,
      page_width = page_width,
      page_height = page_height,
      settings = settings
    )

    results[[i]] <- data.frame(
      field_id = field$field_id[[1]],
      field_name = field$name[[1]],
      page = page_number,
      raw_text = extracted_text,
      extraction_method = "native",
      status = "ok",
      message = "",
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}

parse_pdf_to_row <- function(
  pdf_file,
  template,
  source_name = basename(pdf_file)
) {
  template <- normalize_template(template)

  field_results <- parse_pdf_fields(
    pdf_file = pdf_file,
    template = template
  )

  status_levels <- field_results$status

  overall_status <- if (any(status_levels == "error")) {
    "error"
  } else if (any(status_levels == "warning")) {
    "warning"
  } else {
    "ok"
  }

  overall_message <- collapse_messages(
    field_results$message
  )

  field_values <- as.list(field_results$raw_text)
  names(field_values) <- field_results$field_name

  wide_fields <- as.data.frame(
    field_values,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  cbind(
    data.frame(
      source_file = source_name,
      parse_status = overall_status,
      parse_message = overall_message,
      stringsAsFactors = FALSE
    ),
    wide_fields,
    stringsAsFactors = FALSE
  )
}

create_failed_parse_row <- function(
  source_name,
  template,
  error_message
) {
  template <- normalize_template(template)

  field_values <- as.list(
    rep(NA_character_, nrow(template$fields))
  )

  names(field_values) <- template$fields$name

  wide_fields <- as.data.frame(
    field_values,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  cbind(
    data.frame(
      source_file = source_name,
      parse_status = "error",
      parse_message = error_message,
      stringsAsFactors = FALSE
    ),
    wide_fields,
    stringsAsFactors = FALSE
  )
}

parse_pdf_batch <- function(
  file_manifest,
  template,
  progress_callback = NULL
) {
  template <- normalize_template(template)
  problems <- validate_template(template)

  if (length(problems) > 0) {
    stop(
      paste(
        "Template validation failed:",
        paste(problems, collapse = "\n- "),
        sep = "\n- "
      ),
      call. = FALSE
    )
  }

  required_manifest_columns <- c(
    "path",
    "source_file"
  )

  if (
    !all(
      required_manifest_columns %in% names(file_manifest)
    )
  ) {
    stop(
      "The file manifest must contain path and source_file columns.",
      call. = FALSE
    )
  }

  if (nrow(file_manifest) == 0) {
    stop(
      "No PDF files were provided.",
      call. = FALSE
    )
  }

  output_rows <- vector(
    mode = "list",
    length = nrow(file_manifest)
  )

  for (i in seq_len(nrow(file_manifest))) {
    pdf_path <- file_manifest$path[[i]]
    source_name <- file_manifest$source_file[[i]]

    if (is.function(progress_callback)) {
      progress_callback(
        index = i,
        total = nrow(file_manifest),
        source_file = source_name
      )
    }

    output_rows[[i]] <- tryCatch(
      parse_pdf_to_row(
        pdf_file = pdf_path,
        template = template,
        source_name = source_name
      ),
      error = function(e) {
        create_failed_parse_row(
          source_name = source_name,
          template = template,
          error_message = conditionMessage(e)
        )
      }
    )
  }

  result <- do.call(rbind, output_rows)
  rownames(result) <- NULL
  result
}

# =============================================================================
# User interface
# =============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      .page-navigation {
        display: flex;
        align-items: end;
        gap: 10px;
        flex-wrap: wrap;
        margin-bottom: 15px;
      }

      .page-navigation .form-group {
        margin-bottom: 0;
        min-width: 130px;
      }

      .status-box {
        background: #f5f5f5;
        border-left: 4px solid #2f65a7;
        padding: 12px;
        margin-bottom: 15px;
        white-space: pre-wrap;
      }
      
      .template-status-summary {
        display: flex;
        align-items: center;
        flex-wrap: wrap;
        gap: 6px 16px;
        padding: 8px 12px;
        margin: 8px 0 14px 0;
        background-color: #f7f7f7;
        border: 1px solid #d6d6d6;
        border-left: 4px solid #2f65a7;
        border-radius: 4px;
        font-size: 13px;
        line-height: 1.3;
    }

      .template-status-title {
        font-weight: 700;
        color: #2f65a7;
      }
    
      .template-status-item {
        white-space: nowrap;
      }
    
      .template-status-valid {
        color: #146b2e;
        font-weight: 700;
      }
    
      .template-status-invalid {
        color: #b00020;
        font-weight: 700;
        cursor: help;
      }


      .btn-full-width {
        width: 100%;
        margin-bottom: 8px;
      }

      .validation-error {
        color: #b00020;
        font-weight: 600;
      }

      .validation-success {
        color: #146b2e;
        font-weight: 600;
      }
      
      .pdf-render-container {
        background-color: #e9ecef;
        border: 1px solid #d0d4d8;
        border-radius: 4px;
        padding: 18px;
        overflow: auto;
      }

      /* Make the rendered plot resemble a physical page. */
      .pdf-render-container .shiny-plot-output {
        background-color: white;
        border: 1px solid #c8c8c8;
        box-shadow: 0 2px 7px rgba(0, 0, 0, 0.16);
        margin: 0 auto;
      }

      /* Prevent small inline-image spacing beneath the generated plot. */
      .pdf-render-container .shiny-plot-output img {
        display: block;
}
    "
    ))
  ),

  titlePanel("Interactive Multi-Page PDF Parser"),

  tabsetPanel(
    id = "main_tabs",

    # -------------------------------------------------------------------------
    # Template builder
    # -------------------------------------------------------------------------

    tabPanel(
      title = "Build Template",

      sidebarLayout(
        sidebarPanel(
          fileInput(
            "builder_pdf",
            "Choose a representative PDF",
            accept = ".pdf"
          ),

          textInput(
            "template_name",
            "Template name",
            value = "pdf_template"
          ),

          helpText(
            paste(
              "Upload a representative form, navigate to a page,",
              "and drag a rectangle around each field."
            )
          ),

          hr(),

          textInput(
            "field_name",
            "Field name"
          ),

          actionButton(
            "add_field",
            "Add selected field",
            class = "btn-primary btn-full-width"
          ),

          actionButton(
            "delete_selected_field",
            "Delete selected field",
            class = "btn-full-width"
          ),

          actionButton(
            "clear_current_page",
            "Clear fields on current page",
            class = "btn-full-width"
          ),

          actionButton(
            "clear_all_fields",
            "Clear all fields",
            class = "btn-full-width"
          ),

          hr(),

          h4("Parser settings"),

          sliderInput(
            "x_pad_pct",
            "Horizontal padding",
            min = 0,
            max = 0.05,
            value = 0.005,
            step = 0.001
          ),

          sliderInput(
            "y_pad_pct",
            "Vertical padding",
            min = 0,
            max = 0.05,
            value = 0.005,
            step = 0.001
          ),

          selectInput(
            "capture_mode",
            "Capture rule",
            choices = c(
              "Word center must be inside field" = "center",
              "Word must overlap field" = "overlap"
            ),
            selected = "center"
          ),

          sliderInput(
            "min_word_overlap_pct",
            "Minimum overlap percentage",
            min = 0,
            max = 100,
            value = 1,
            step = 1
          ),

          sliderInput(
            "line_tolerance",
            "Line sorting tolerance",
            min = 1,
            max = 20,
            value = 3,
            step = 1
          ),

          hr(),

          actionButton(
            "preview_sample",
            "Preview sample parsing",
            class = "btn-primary btn-full-width"
          ),

          downloadButton(
            "download_template",
            "Export template JSON",
            class = "btn-full-width"
          ),

          width = 3
        ),

        mainPanel(
          div(
            class = "page-navigation",

            actionButton(
              "previous_page",
              "Previous page"
            ),

            selectInput(
              "builder_page",
              "Current page",
              choices = character()
            ),

            actionButton(
              "next_page",
              "Next page"
            )
          ),

          div(
            class = "pdf-render-container",

            plotOutput(
              "page_plot",
              height = "850px",
              brush = brushOpts(
                id = "page_brush",
                direction = "xy",
                resetOnNew = FALSE
              )
            )
          ),

          uiOutput("builder_status"),

          hr(),

          h4("Saved fields"),

          helpText(
            paste(
              "Select a row before clicking",
              "'Delete selected field'."
            )
          ),

          DT::DTOutput("fields_table"),

          hr(),

          h4("Representative PDF preview"),

          DT::DTOutput("sample_preview_table"),

          width = 9
        )
      )
    ),

    # -------------------------------------------------------------------------
    # Batch parser
    # -------------------------------------------------------------------------

    tabPanel(
      title = "Parse PDFs",

      sidebarLayout(
        sidebarPanel(
          h4("1. Select a template"),

          radioButtons(
            "parser_template_source",
            "Template source",
            choices = c(
              "Use current builder template" = "current",
              "Load a JSON template" = "imported"
            ),
            selected = "current"
          ),

          conditionalPanel(
            condition = "input.parser_template_source == 'imported'",

            fileInput(
              "imported_template_file",
              "Choose a template JSON file",
              accept = c(".json", "application/json")
            )
          ),

          hr(),

          h4("2. Select PDF documents"),

          radioButtons(
            "batch_input_mode",
            "PDF input method",
            choices = c(
              "Upload one or more PDFs" = "upload",
              "Select a folder" = "folder"
            ),
            selected = "upload"
          ),

          conditionalPanel(
            condition = "input.batch_input_mode == 'upload'",

            fileInput(
              "batch_pdf_files",
              "Choose PDF documents",
              accept = ".pdf",
              multiple = TRUE
            )
          ),

          conditionalPanel(
            condition = "input.batch_input_mode == 'folder'",

            shinyFiles::shinyDirButton(
              "batch_pdf_directory",
              "Choose PDF folder",
              "Select a directory containing PDFs"
            ),

            checkboxInput(
              "recursive_folder_search",
              "Include subfolders",
              value = FALSE
            ),

            helpText(
              paste(
                "Folder selection accesses the filesystem visible",
                "to the R process. In a remotely hosted app, this is",
                "the server filesystem, not the user's computer."
              )
            ),

            verbatimTextOutput(
              "selected_directory_text",
              placeholder = TRUE
            )
          ),

          hr(),

          actionButton(
            "run_batch_parse",
            "Parse selected PDFs",
            class = "btn-primary btn-full-width"
          ),

          downloadButton(
            "download_batch_csv",
            "Save results to CSV",
            class = "btn-full-width"
          ),

          width = 3
        ),

        mainPanel(
          h4("Selected template"),

          verbatimTextOutput(
            "parser_template_summary",
            placeholder = TRUE
          ),

          hr(),

          h4("Parsing status"),

          verbatimTextOutput(
            "batch_status",
            placeholder = TRUE
          ),

          hr(),

          h4("First five parsed rows"),

          DT::DTOutput("batch_preview_table"),

          hr(),

          h4("Documents with warnings or errors"),

          DT::DTOutput("batch_issues_table"),

          width = 9
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {
  # ---------------------------------------------------------------------------
  # Shared application state
  # ---------------------------------------------------------------------------

  builder_fields <- reactiveVal(empty_fields())
  builder_source_path <- reactiveVal(NULL)
  builder_source_name <- reactiveVal("")
  builder_page_count <- reactiveVal(0L)
  builder_created_at <- reactiveVal(
    format(Sys.time(), tz = "UTC", usetz = TRUE)
  )

  current_page_number <- reactiveVal(1L)
  current_page_render <- reactiveVal(NULL)

  sample_preview <- reactiveVal(empty_parse_result())

  imported_template <- reactiveVal(NULL)
  imported_template_error <- reactiveVal("")

  batch_result <- reactiveVal(NULL)
  batch_status_message <- reactiveVal(
    "No batch parse has been run."
  )

  selected_directory <- reactiveVal(NULL)

  # ---------------------------------------------------------------------------
  # Current builder settings and template
  # ---------------------------------------------------------------------------

  current_builder_settings <- reactive({
    normalize_parser_settings(list(
      x_pad_pct = input$x_pad_pct,
      y_pad_pct = input$y_pad_pct,
      capture_mode = input$capture_mode,
      min_word_overlap_pct = input$min_word_overlap_pct,
      line_tolerance = input$line_tolerance
    ))
  })

  current_builder_template <- reactive({
    create_template_object(
      name = input$template_name %||% "pdf_template",
      source_pdf_name = builder_source_name(),
      expected_pages = builder_page_count(),
      fields = builder_fields(),
      settings = current_builder_settings(),
      created_at = builder_created_at()
    )
  })

  # ---------------------------------------------------------------------------
  # Render a builder page
  # ---------------------------------------------------------------------------

  render_builder_page <- function(page_number) {
    pdf_path <- builder_source_path()

    if (is.null(pdf_path) || !file.exists(pdf_path)) {
      current_page_render(NULL)
      return(invisible(NULL))
    }

    page_count <- builder_page_count()

    if (
      page_number < 1 ||
        page_number > page_count
    ) {
      current_page_render(NULL)
      return(invisible(NULL))
    }

    tryCatch(
      {
        rendered <- render_pdf_page(
          pdf_file = pdf_path,
          page = page_number,
          dpi = BUILDER_RENDER_DPI
        )

        current_page_render(rendered)
        session$resetBrush("page_brush")
      },
      error = function(e) {
        current_page_render(NULL)

        showNotification(
          paste(
            "Could not render page",
            page_number,
            ":",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  }

  # ---------------------------------------------------------------------------
  # Representative PDF upload
  # ---------------------------------------------------------------------------

  observeEvent(input$builder_pdf, {
    req(input$builder_pdf)

    tryCatch(
      {
        pdf_path <- input$builder_pdf$datapath
        pdf_information <- pdftools::pdf_info(pdf_path)
        page_count <- safe_integer(
          pdf_information$pages,
          0L
        )

        if (page_count < 1) {
          stop("The uploaded PDF contains no readable pages.")
        }

        builder_source_path(pdf_path)
        builder_source_name(input$builder_pdf$name)
        builder_page_count(page_count)
        builder_created_at(
          format(Sys.time(), tz = "UTC", usetz = TRUE)
        )

        builder_fields(empty_fields())
        sample_preview(empty_parse_result())
        current_page_number(1L)

        updateSelectInput(
          session,
          "builder_page",
          choices = seq_len(page_count),
          selected = 1L
        )

        render_builder_page(1L)

        showNotification(
          paste(
            "Loaded",
            input$builder_pdf$name,
            "with",
            page_count,
            "page(s)."
          ),
          type = "message"
        )
      },
      error = function(e) {
        builder_source_path(NULL)
        builder_source_name("")
        builder_page_count(0L)
        builder_fields(empty_fields())
        current_page_render(NULL)
        sample_preview(empty_parse_result())

        updateSelectInput(
          session,
          "builder_page",
          choices = character()
        )

        showNotification(
          paste(
            "Could not load PDF:",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })

  # ---------------------------------------------------------------------------
  # Page navigation
  # ---------------------------------------------------------------------------

  observeEvent(input$builder_page, {
    req(input$builder_page)

    page_number <- suppressWarnings(
      as.integer(input$builder_page)
    )

    if (
      is.na(page_number) ||
        page_number < 1 ||
        page_number > builder_page_count()
    ) {
      return()
    }

    if (!identical(current_page_number(), page_number)) {
      current_page_number(page_number)
    }

    render_builder_page(page_number)
  })

  observeEvent(input$previous_page, {
    req(builder_page_count() > 0)

    new_page <- max(
      1L,
      current_page_number() - 1L
    )

    updateSelectInput(
      session,
      "builder_page",
      selected = new_page
    )
  })

  observeEvent(input$next_page, {
    req(builder_page_count() > 0)

    new_page <- min(
      builder_page_count(),
      current_page_number() + 1L
    )

    updateSelectInput(
      session,
      "builder_page",
      selected = new_page
    )
  })

  # ---------------------------------------------------------------------------
  # Builder page plot
  # ---------------------------------------------------------------------------

  output$page_plot <- renderPlot({
    # Display an instructional placeholder before a PDF is uploaded.
    if (
      is.null(input$builder_pdf) ||
        is.null(builder_source_path())
    ) {
      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()

      graphics::text(
        x = 0.5,
        y = 0.55,
        labels = "Upload a PDF to begin constructing a template",
        cex = 1.3,
        font = 2,
        col = "#4a4a4a"
      )

      graphics::text(
        x = 0.5,
        y = 0.47,
        labels = "The selected PDF page will appear here.",
        cex = 1,
        col = "#777777"
      )

      return(invisible(NULL))
    }

    rendered <- current_page_render()

    # The PDF has been uploaded, but its page may still be rendering.
    if (
      is.null(rendered) ||
        is.null(rendered$image)
    ) {
      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()

      graphics::text(
        x = 0.5,
        y = 0.5,
        labels = "Rendering PDF page...",
        cex = 1.2,
        col = "#4a4a4a"
      )

      return(invisible(NULL))
    }

    image <- rendered$image
    image_dimensions <- dim(image)

    # Handle an invalid rendered image without sending invalid values to plot().
    if (
      is.null(image_dimensions) ||
        length(image_dimensions) < 2
    ) {
      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()

      graphics::text(
        x = 0.5,
        y = 0.5,
        labels = "The current PDF page could not be displayed.",
        cex = 1.2,
        col = "#b00020"
      )

      return(invisible(NULL))
    }

    height <- as.numeric(image_dimensions[[1]])
    width <- as.numeric(image_dimensions[[2]])

    if (
      length(width) != 1 ||
        length(height) != 1 ||
        !is.finite(width) ||
        !is.finite(height) ||
        width <= 0 ||
        height <= 0
    ) {
      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()

      graphics::text(
        x = 0.5,
        y = 0.5,
        labels = "The rendered PDF page has invalid dimensions.",
        cex = 1.2,
        col = "#b00020"
      )

      return(invisible(NULL))
    }

    current_page <- current_page_number()

    graphics::par(mar = c(0, 0, 0, 0))

    graphics::plot.new()

    graphics::plot.window(
      xlim = c(0, width),
      ylim = c(height, 0),
      xaxs = "i",
      yaxs = "i",
      asp = 1
    )

    graphics::rasterImage(
      image,
      xleft = 0,
      ybottom = height,
      xright = width,
      ytop = 0,
      interpolate = TRUE
    )

    saved_fields <- builder_fields()

    page_fields <- saved_fields[
      !is.na(saved_fields$page) &
        saved_fields$page == current_page,
      ,
      drop = FALSE
    ]

    if (nrow(page_fields) > 0) {
      for (i in seq_len(nrow(page_fields))) {
        xmin <- page_fields$xmin_norm[[i]] * width
        ymin <- page_fields$ymin_norm[[i]] * height
        xmax <- page_fields$xmax_norm[[i]] * width
        ymax <- page_fields$ymax_norm[[i]] * height

        coordinates <- c(xmin, ymin, xmax, ymax)

        if (
          length(coordinates) == 4 &&
            all(is.finite(coordinates))
        ) {
          graphics::rect(
            xmin,
            ymin,
            xmax,
            ymax,
            border = "#b00020",
            lwd = 2
          )

          graphics::text(
            x = (xmin + xmax) / 2,
            y = (ymin + ymax) / 2,
            labels = page_fields$name[[i]],
            col = "#b00020",
            cex = 0.85,
            font = 2
          )
        }
      }
    }

    current_brush <- input$page_brush

    if (!is.null(current_brush)) {
      brush_coordinates <- c(
        current_brush$xmin,
        current_brush$ymin,
        current_brush$xmax,
        current_brush$ymax
      )

      if (
        length(brush_coordinates) == 4 &&
          all(is.finite(brush_coordinates))
      ) {
        graphics::rect(
          current_brush$xmin,
          current_brush$ymin,
          current_brush$xmax,
          current_brush$ymax,
          border = "#0066cc",
          lwd = 2,
          lty = 2
        )
      }
    }
  })

  # ---------------------------------------------------------------------------
  # Add a field
  # ---------------------------------------------------------------------------

  observeEvent(input$add_field, {
    rendered <- current_page_render()
    brush <- input$page_brush
    field_name <- trimws(input$field_name %||% "")

    if (is.null(rendered)) {
      showNotification(
        "Upload a PDF before adding fields.",
        type = "warning"
      )
      return()
    }

    if (is.null(brush)) {
      showNotification(
        "Drag a rectangle on the PDF first.",
        type = "warning"
      )
      return()
    }

    if (!nzchar(field_name)) {
      showNotification(
        "Enter a field name.",
        type = "warning"
      )
      return()
    }

    current_fields <- builder_fields()

    if (field_name %in% current_fields$name) {
      showNotification(
        paste(
          "Field names must be unique. A field named",
          shQuote(field_name),
          "already exists."
        ),
        type = "warning",
        duration = NULL
      )
      return()
    }

    width <- rendered$width
    height <- rendered$height

    xmin <- max(
      0,
      min(brush$xmin, brush$xmax)
    )

    xmax <- min(
      width,
      max(brush$xmin, brush$xmax)
    )

    ymin <- max(
      0,
      min(brush$ymin, brush$ymax)
    )

    ymax <- min(
      height,
      max(brush$ymin, brush$ymax)
    )

    if (
      (xmax - xmin) < 2 ||
        (ymax - ymin) < 2
    ) {
      showNotification(
        "The selected rectangle is too small.",
        type = "warning"
      )
      return()
    }

    new_field <- data.frame(
      field_id = make_field_id(),
      name = field_name,
      page = current_page_number(),
      xmin_norm = xmin / width,
      ymin_norm = ymin / height,
      xmax_norm = xmax / width,
      ymax_norm = ymax / height,
      stringsAsFactors = FALSE
    )

    builder_fields(
      rbind(current_fields, new_field)
    )

    sample_preview(empty_parse_result())

    updateTextInput(
      session,
      "field_name",
      value = ""
    )

    session$resetBrush("page_brush")
  })

  # ---------------------------------------------------------------------------
  # Delete selected field
  # ---------------------------------------------------------------------------

  observeEvent(input$delete_selected_field, {
    selected_row <- input$fields_table_rows_selected
    current_fields <- builder_fields()

    if (
      is.null(selected_row) ||
        length(selected_row) == 0
    ) {
      showNotification(
        "Select a field row to delete.",
        type = "warning"
      )
      return()
    }

    if (
      selected_row < 1 ||
        selected_row > nrow(current_fields)
    ) {
      showNotification(
        "The selected field could not be found.",
        type = "warning"
      )
      return()
    }

    deleted_name <- current_fields$name[[selected_row]]
    deleted_page <- current_fields$page[[selected_row]]

    updated_fields <- current_fields[
      -selected_row,
      ,
      drop = FALSE
    ]

    builder_fields(updated_fields)
    sample_preview(empty_parse_result())

    updateTextInput(
      session,
      "field_name",
      value = deleted_name
    )

    if (deleted_page == current_page_number()) {
      session$resetBrush("page_brush")
    }

    showNotification(
      paste("Deleted field:", deleted_name),
      type = "message"
    )
  })

  # ---------------------------------------------------------------------------
  # Clear fields
  # ---------------------------------------------------------------------------

  observeEvent(input$clear_current_page, {
    current_fields <- builder_fields()

    if (nrow(current_fields) == 0) {
      showNotification(
        "There are no fields to clear.",
        type = "warning"
      )
      return()
    }

    current_page <- current_page_number()

    updated_fields <- current_fields[
      current_fields$page != current_page,
      ,
      drop = FALSE
    ]

    removed_count <- nrow(current_fields) -
      nrow(updated_fields)

    builder_fields(updated_fields)
    sample_preview(empty_parse_result())
    session$resetBrush("page_brush")

    showNotification(
      paste(
        "Removed",
        removed_count,
        "field(s) from page",
        current_page,
        "."
      ),
      type = "message"
    )
  })

  observeEvent(input$clear_all_fields, {
    builder_fields(empty_fields())
    sample_preview(empty_parse_result())
    session$resetBrush("page_brush")

    showNotification(
      "All fields were removed.",
      type = "message"
    )
  })

  # ---------------------------------------------------------------------------
  # Builder status
  # ---------------------------------------------------------------------------

  output$builder_status <- renderUI({
    if (is.null(builder_source_path())) {
      return(
        div(
          class = "template-status-summary",
          span(
            class = "template-status-title",
            "Template Status Summary"
          ),
          span(
            class = "template-status-item",
            "Upload a PDF to begin constructing a template."
          )
        )
      )
    }

    fields <- builder_fields()
    current_page <- current_page_number()
    problems <- validate_template(current_builder_template())

    fields_on_current_page <- sum(
      !is.na(fields$page) &
        fields$page == current_page
    )

    if (length(problems) == 0) {
      validation_status <- span(
        class = "template-status-valid",
        "Ready to export"
      )
    } else {
      # Show the complete validation details when the user hovers over
      # the compact status message.
      problem_tooltip <- paste(
        problems,
        collapse = "\n"
      )

      validation_status <- span(
        class = "template-status-invalid",
        title = problem_tooltip,
        paste0(
          "Not ready (",
          length(problems),
          if (length(problems) == 1) {
            " issue)"
          } else {
            " issues)"
          }
        )
      )
    }

    div(
      class = "template-status-summary",

      span(
        class = "template-status-title",
        "Template Status Summary"
      ),

      span(
        class = "template-status-item",
        tags$b("PDF: "),
        builder_source_name()
      ),

      span(
        class = "template-status-item",
        tags$b("Pages: "),
        builder_page_count()
      ),

      span(
        class = "template-status-item",
        tags$b("Current page: "),
        current_page
      ),

      span(
        class = "template-status-item",
        tags$b("Saved fields: "),
        nrow(fields)
      ),

      span(
        class = "template-status-item",
        tags$b("On this page: "),
        fields_on_current_page
      ),

      span(
        class = "template-status-item",
        tags$b("Status: "),
        validation_status
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Fields table
  # ---------------------------------------------------------------------------

  output$fields_table <- DT::renderDT({
    fields <- builder_fields()

    if (nrow(fields) == 0) {
      display <- data.frame(
        Message = "No fields have been defined.",
        stringsAsFactors = FALSE
      )

      return(
        DT::datatable(
          display,
          rownames = FALSE,
          options = list(
            dom = "t",
            ordering = FALSE
          ),
          selection = "none"
        )
      )
    }

    display <- fields[, c(
      "name",
      "page",
      "xmin_norm",
      "ymin_norm",
      "xmax_norm",
      "ymax_norm"
    )]

    names(display) <- c(
      "Field name",
      "Page",
      "X minimum",
      "Y minimum",
      "X maximum",
      "Y maximum"
    )

    DT::datatable(
      display,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    ) |>
      DT::formatRound(
        columns = c(
          "X minimum",
          "Y minimum",
          "X maximum",
          "Y maximum"
        ),
        digits = 4
      )
  })

  # ---------------------------------------------------------------------------
  # Representative PDF preview
  # ---------------------------------------------------------------------------

  observeEvent(input$preview_sample, {
    pdf_path <- builder_source_path()

    if (is.null(pdf_path)) {
      showNotification(
        "Upload a representative PDF first.",
        type = "warning"
      )
      return()
    }

    template <- current_builder_template()
    problems <- validate_template(template)

    if (length(problems) > 0) {
      showNotification(
        paste(
          "The template cannot be previewed:",
          paste(problems, collapse = " ")
        ),
        type = "error",
        duration = NULL
      )
      return()
    }

    tryCatch(
      {
        result <- parse_pdf_fields(
          pdf_file = pdf_path,
          template = template
        )

        sample_preview(result)

        showNotification(
          "Representative PDF preview completed.",
          type = "message"
        )
      },
      error = function(e) {
        sample_preview(empty_parse_result())

        showNotification(
          paste(
            "Preview failed:",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })

  output$sample_preview_table <- DT::renderDT({
    preview <- sample_preview()

    if (nrow(preview) == 0) {
      display <- data.frame(
        Message = paste(
          "No preview is available.",
          "Click 'Preview sample parsing'."
        ),
        stringsAsFactors = FALSE
      )

      return(
        DT::datatable(
          display,
          rownames = FALSE,
          options = list(
            dom = "t",
            ordering = FALSE
          )
        )
      )
    }

    display <- preview[, c(
      "page",
      "field_name",
      "raw_text",
      "extraction_method",
      "status",
      "message"
    )]

    names(display) <- c(
      "Page",
      "Field",
      "Extracted text",
      "Method",
      "Status",
      "Message"
    )

    DT::datatable(
      display,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Template JSON export
  # ---------------------------------------------------------------------------

  output$download_template <- downloadHandler(
    filename = function() {
      paste0(
        safe_filename_stem(input$template_name),
        ".json"
      )
    },

    content = function(file) {
      template <- current_builder_template()
      problems <- validate_template(template)

      if (length(problems) > 0) {
        stop(
          paste(
            "Template export failed:",
            paste(problems, collapse = " ")
          ),
          call. = FALSE
        )
      }

      write_template_json(
        template = template,
        file = file
      )
    },

    contentType = "application/json"
  )

  # ---------------------------------------------------------------------------
  # Imported JSON template
  # ---------------------------------------------------------------------------

  observeEvent(input$imported_template_file, {
    req(input$imported_template_file)

    tryCatch(
      {
        template <- read_template_json(
          input$imported_template_file$datapath
        )

        imported_template(template)
        imported_template_error("")

        showNotification(
          paste(
            "Loaded template:",
            template$template$name
          ),
          type = "message"
        )
      },
      error = function(e) {
        imported_template(NULL)
        imported_template_error(
          conditionMessage(e)
        )

        showNotification(
          paste(
            "Template import failed:",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })

  # ---------------------------------------------------------------------------
  # Template selected for batch parsing
  # ---------------------------------------------------------------------------

  selected_parser_template <- reactive({
    if (
      identical(
        input$parser_template_source,
        "imported"
      )
    ) {
      imported_template()
    } else {
      current_builder_template()
    }
  })

  output$parser_template_summary <- renderText({
    source <- input$parser_template_source

    if (identical(source, "imported")) {
      if (nzchar(imported_template_error())) {
        return(
          paste(
            "Imported template error:",
            imported_template_error()
          )
        )
      }

      template <- imported_template()

      if (is.null(template)) {
        return("No JSON template has been loaded.")
      }

      source_label <- "Imported JSON template"
    } else {
      template <- current_builder_template()
      source_label <- "Current builder template"
    }

    problems <- validate_template(template)
    fields <- normalize_fields(template$fields)

    required_pages <- if (nrow(fields) > 0) {
      max(fields$page, na.rm = TRUE)
    } else {
      0
    }

    paste0(
      "Source: ",
      source_label,
      "\n",
      "Template name: ",
      template$template$name,
      "\n",
      "Schema version: ",
      template$schema_version,
      "\n",
      "Fields: ",
      nrow(fields),
      "\n",
      "Highest required page: ",
      required_pages,
      "\n",
      "Validation: ",
      if (length(problems) == 0) {
        "Valid"
      } else {
        paste(problems, collapse = " ")
      }
    )
  })

  # ---------------------------------------------------------------------------
  # Directory selection
  # ---------------------------------------------------------------------------

  volumes <- shinyFiles::getVolumes()()

  shinyFiles::shinyDirChoose(
    input = input,
    id = "batch_pdf_directory",
    session = session,
    roots = volumes
  )

  observeEvent(input$batch_pdf_directory, {
    parsed_directory <- shinyFiles::parseDirPath(
      roots = volumes,
      selection = input$batch_pdf_directory
    )

    if (
      is.null(parsed_directory) ||
        nrow(parsed_directory) == 0
    ) {
      selected_directory(NULL)
      return()
    }

    selected_directory(
      as.character(parsed_directory$datapath[[1]])
    )
  })

  output$selected_directory_text <- renderText({
    directory <- selected_directory()

    if (is.null(directory)) {
      "No directory selected."
    } else {
      paste("Selected directory:", directory)
    }
  })

  # ---------------------------------------------------------------------------
  # Build PDF file manifest
  # ---------------------------------------------------------------------------

  build_batch_file_manifest <- reactive({
    input_mode <- input$batch_input_mode

    if (identical(input_mode, "upload")) {
      uploaded_files <- input$batch_pdf_files

      if (
        is.null(uploaded_files) ||
          nrow(uploaded_files) == 0
      ) {
        return(data.frame(
          path = character(),
          source_file = character(),
          stringsAsFactors = FALSE
        ))
      }

      return(data.frame(
        path = uploaded_files$datapath,
        source_file = uploaded_files$name,
        stringsAsFactors = FALSE
      ))
    }

    directory <- selected_directory()

    if (
      is.null(directory) ||
        !dir.exists(directory)
    ) {
      return(data.frame(
        path = character(),
        source_file = character(),
        stringsAsFactors = FALSE
      ))
    }

    pdf_paths <- list.files(
      path = directory,
      pattern = "\\.pdf$",
      full.names = TRUE,
      recursive = isTRUE(
        input$recursive_folder_search
      ),
      ignore.case = TRUE
    )

    data.frame(
      path = pdf_paths,
      source_file = basename(pdf_paths),
      stringsAsFactors = FALSE
    )
  })

  # ---------------------------------------------------------------------------
  # Run batch parser
  # ---------------------------------------------------------------------------

  observeEvent(input$run_batch_parse, {
    template <- selected_parser_template()

    if (is.null(template)) {
      showNotification(
        "Select or import a template first.",
        type = "warning"
      )
      return()
    }

    problems <- validate_template(template)

    if (length(problems) > 0) {
      showNotification(
        paste(
          "The selected template is invalid:",
          paste(problems, collapse = " ")
        ),
        type = "error",
        duration = NULL
      )
      return()
    }

    file_manifest <- build_batch_file_manifest()

    if (nrow(file_manifest) == 0) {
      showNotification(
        "No PDF files were selected.",
        type = "warning"
      )
      return()
    }

    batch_result(NULL)

    batch_status_message(
      paste(
        "Parsing",
        nrow(file_manifest),
        "PDF document(s)..."
      )
    )

    tryCatch(
      {
        result <- withProgress(
          message = "Parsing PDF documents",
          value = 0,
          {
            parse_pdf_batch(
              file_manifest = file_manifest,
              template = template,
              progress_callback = function(
                index,
                total,
                source_file
              ) {
                progress_value <- index / total

                setProgress(
                  value = progress_value,
                  detail = paste(
                    index,
                    "of",
                    total,
                    "-",
                    source_file
                  )
                )
              }
            )
          }
        )

        batch_result(result)

        ok_count <- sum(
          result$parse_status == "ok",
          na.rm = TRUE
        )

        warning_count <- sum(
          result$parse_status == "warning",
          na.rm = TRUE
        )

        error_count <- sum(
          result$parse_status == "error",
          na.rm = TRUE
        )

        batch_status_message(
          paste0(
            "Parsing complete.\n",
            "Documents parsed: ",
            nrow(result),
            "\n",
            "Successful: ",
            ok_count,
            "\n",
            "Warnings: ",
            warning_count,
            "\n",
            "Errors: ",
            error_count
          )
        )

        showNotification(
          paste(
            "Parsing complete for",
            nrow(result),
            "document(s)."
          ),
          type = if (error_count > 0) {
            "warning"
          } else {
            "message"
          }
        )
      },
      error = function(e) {
        batch_result(NULL)

        batch_status_message(
          paste(
            "Batch parsing failed:",
            conditionMessage(e)
          )
        )

        showNotification(
          paste(
            "Batch parsing failed:",
            conditionMessage(e)
          ),
          type = "error",
          duration = NULL
        )
      }
    )
  })

  # ---------------------------------------------------------------------------
  # Batch output
  # ---------------------------------------------------------------------------

  output$batch_status <- renderText({
    batch_status_message()
  })

  output$batch_preview_table <- DT::renderDT({
    result <- batch_result()

    if (is.null(result) || nrow(result) == 0) {
      display <- data.frame(
        Message = "No batch parsing result is available.",
        stringsAsFactors = FALSE
      )

      return(
        DT::datatable(
          display,
          rownames = FALSE,
          options = list(
            dom = "t",
            ordering = FALSE
          )
        )
      )
    }

    preview <- utils::head(result, 5)

    DT::datatable(
      preview,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      )
    )
  })

  output$batch_issues_table <- DT::renderDT({
    result <- batch_result()

    if (is.null(result) || nrow(result) == 0) {
      display <- data.frame(
        Message = "No parsing result is available.",
        stringsAsFactors = FALSE
      )

      return(
        DT::datatable(
          display,
          rownames = FALSE,
          options = list(
            dom = "t",
            ordering = FALSE
          )
        )
      )
    }

    issues <- result[
      result$parse_status != "ok",
      c(
        "source_file",
        "parse_status",
        "parse_message"
      ),
      drop = FALSE
    ]

    if (nrow(issues) == 0) {
      issues <- data.frame(
        Message = "No warnings or errors were reported.",
        stringsAsFactors = FALSE
      )
    }

    DT::datatable(
      issues,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Batch CSV download
  # ---------------------------------------------------------------------------

  output$download_batch_csv <- downloadHandler(
    filename = function() {
      paste0(
        "parsed_pdf_results_",
        Sys.Date(),
        ".csv"
      )
    },

    content = function(file) {
      result <- batch_result()

      if (is.null(result) || nrow(result) == 0) {
        stop(
          "No parsed result is available to download.",
          call. = FALSE
        )
      }

      utils::write.csv(
        result,
        file = file,
        row.names = FALSE,
        na = ""
      )
    },

    contentType = "text/csv"
  )
}

shinyApp(ui, server, options = list(launch.browser = TRUE))

# Resulting JSON formatTemplates exported by the application will resemble:{
#   "schema_version": "1.0.0",
#   "template": {
#     "name": "death_certificate",
#     "created_at": "2026-03-07 12:00:00 UTC",
#     "source_pdf_name": "sample.pdf",
#     "expected_pages": 3,
#     "render_dpi": 150
#   },
#   "settings": {
#     "x_pad_pct": 0.005,
#     "y_pad_pct": 0.005,
#     "capture_mode": "center",
#     "min_word_overlap_pct": 1,
#     "line_tolerance": 3
#   },
#   "fields": [
#     {
#       "field_id": "field_20260307120000_123456",
#       "name": "patient_name",
#       "page": 1,
#       "xmin_norm": 0.12,
#       "ymin_norm": 0.18,
#       "xmax_norm": 0.48,
#       "ymax_norm": 0.23
#     },
#     {
#       "field_id": "field_20260307120001_654321",
#       "name": "cause_of_death",
#       "page": 2,
#       "xmin_norm": 0.1,
#       "ymin_norm": 0.41,
#       "xmax_norm": 0.88,
#       "ymax_norm": 0.6
#     }
#   ]
# }
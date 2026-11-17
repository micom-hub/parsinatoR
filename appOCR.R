required_packages <- c(
  "shiny", "shinyFiles", "pdftools", "png", "jsonlite", "DT",
  "magick", "tesseract", "bslib"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      "\nInstall them with install.packages()."
    ),
    call. = FALSE
  )
}

invisible(lapply(
  c("shiny", "shinyFiles", "pdftools", "png", "DT"),
  library,
  character.only = TRUE
))

options(shiny.launch.browser = TRUE)

ocr_script <- file.path("R", "ocr.R")

if (!file.exists(ocr_script)) {
  stop("The required file R/ocr.R was not found.", call. = FALSE)
}

source(ocr_script, local = FALSE)

TEMPLATE_SCHEMA_VERSION <- "1.1.0"
SUPPORTED_TEMPLATE_VERSIONS <- c("1.0.0", "1.1.0")
BUILDER_RENDER_DPI <- 150L

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

safe_scalar <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0) return(default)
  value <- x[[1]]
  if (is.null(value) || is.na(value)) default else value
}

safe_character <- function(x, default = "") {
  as.character(safe_scalar(x, default))
}

safe_numeric <- function(x, default = 0) {
  value <- suppressWarnings(as.numeric(safe_scalar(x, default)))
  if (length(value) != 1 || is.na(value) || !is.finite(value)) default else value
}

safe_integer <- function(x, default = 0L) {
  value <- suppressWarnings(as.integer(safe_scalar(x, default)))
  if (length(value) != 1 || is.na(value)) default else value
}

collapse_messages <- function(x) {
  x <- trimws(as.character(x))
  x <- unique(x[!is.na(x) & nzchar(x)])
  paste(x, collapse = "; ")
}

safe_filename_stem <- function(x, default = "pdf_template") {
  x <- trimws(x %||% "")
  if (!nzchar(x)) x <- default
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (nzchar(x)) x else default
}

make_field_id <- function() {
  paste0(
    "field_",
    format(Sys.time(), "%Y%m%d%H%M%OS6"),
    "_",
    sprintf("%06d", sample.int(999999, 1))
  )
}

help_label <- function(label, help_text) {
  tags$span(
    label,
    tags$span(
      class = "help-tip",
      tabindex = "0",
      role = "button",
      `aria-label` = paste("Help for", label),
      `data-toggle` = "tooltip",
      `data-placement` = "right",
      title = help_text,
      "?"
    )
  )
}

empty_manifest <- function() {
  data.frame(
    path = character(),
    source_file = character(),
    stringsAsFactors = FALSE
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
    extraction_mode = character(),
    content_type = character(),
    preprocessing_profile = character(),
    ocr_language = character(),
    minimum_confidence = numeric(),
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
    recognition_confidence = numeric(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

default_parser_settings <- function() {
  list(
    x_pad_pct = 0.005,
    y_pad_pct = 0.005,
    capture_mode = "center",
    min_word_overlap_pct = 1,
    line_tolerance = 3,
    ocr = ocr_default_settings()
  )
}

fill_character_default <- function(x, default) {
  x <- as.character(x)
  missing <- is.na(x) | !nzchar(trimws(x))
  x[missing] <- default
  x
}

normalize_fields <- function(fields) {
  if (is.null(fields) || length(fields) == 0) return(empty_fields())
  
  if (is.data.frame(fields)) {
    fields_df <- fields
  } else if (
    is.list(fields) &&
    length(fields) > 0 &&
    all(vapply(fields, is.list, logical(1)))
  ) {
    rows <- lapply(fields, function(field) {
      as.data.frame(field, stringsAsFactors = FALSE, optional = TRUE)
    })
    
    all_names <- unique(unlist(lapply(rows, names)))
    
    rows <- lapply(rows, function(row) {
      missing <- setdiff(all_names, names(row))
      for (column in missing) row[[column]] <- NA
      row[, all_names, drop = FALSE]
    })
    
    fields_df <- do.call(rbind, rows)
  } else {
    fields_df <- as.data.frame(
      fields,
      stringsAsFactors = FALSE,
      optional = TRUE
    )
  }
  
  required_columns <- names(empty_fields())
  
  for (column in required_columns) {
    if (!column %in% names(fields_df)) fields_df[[column]] <- NA
  }
  
  fields_df <- fields_df[, required_columns, drop = FALSE]
  fields_df$field_id <- as.character(fields_df$field_id)
  fields_df$name <- as.character(fields_df$name)
  fields_df$page <- suppressWarnings(as.integer(fields_df$page))
  
  for (column in c("xmin_norm", "ymin_norm", "xmax_norm", "ymax_norm")) {
    fields_df[[column]] <- suppressWarnings(as.numeric(fields_df[[column]]))
  }
  
  fields_df$extraction_mode <- fill_character_default(
    fields_df$extraction_mode,
    "inherit"
  )
  
  fields_df$content_type <- fill_character_default(
    fields_df$content_type,
    "single_line"
  )
  
  fields_df$preprocessing_profile <- fill_character_default(
    fields_df$preprocessing_profile,
    "inherit"
  )
  
  fields_df$ocr_language <- as.character(fields_df$ocr_language)
  fields_df$ocr_language[is.na(fields_df$ocr_language)] <- ""
  
  fields_df$minimum_confidence <- suppressWarnings(
    as.numeric(fields_df$minimum_confidence)
  )
  
  fields_df$minimum_confidence[
    is.na(fields_df$minimum_confidence)
  ] <- 50
  
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

normalize_parser_settings <- function(settings = NULL) {
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
    x_pad_pct = safe_numeric(settings$x_pad_pct, defaults$x_pad_pct),
    y_pad_pct = safe_numeric(settings$y_pad_pct, defaults$y_pad_pct),
    capture_mode = capture_mode,
    min_word_overlap_pct = safe_numeric(
      settings$min_word_overlap_pct,
      defaults$min_word_overlap_pct
    ),
    line_tolerance = safe_numeric(
      settings$line_tolerance,
      defaults$line_tolerance
    ),
    ocr = normalize_ocr_settings(settings$ocr)
  )
}

migrate_template <- function(template) {
  if (!is.list(template)) {
    stop("The template must be a JSON object.", call. = FALSE)
  }
  
  version <- safe_character(template$schema_version, "1.0.0")
  
  if (!version %in% SUPPORTED_TEMPLATE_VERSIONS) {
    stop(
      paste("Unsupported template schema version:", version),
      call. = FALSE
    )
  }
  
  template$settings <- template$settings %||% list()
  template$fields <- normalize_fields(template$fields)
  template$settings$ocr <- normalize_ocr_settings(template$settings$ocr)
  template$schema_version <- TEMPLATE_SCHEMA_VERSION
  template
}

normalize_template <- function(template) {
  template <- migrate_template(template)
  metadata <- template$template %||% list()
  
  list(
    schema_version = TEMPLATE_SCHEMA_VERSION,
    template = list(
      name = safe_character(metadata$name, "pdf_template"),
      created_at = safe_character(
        metadata$created_at,
        format(Sys.time(), tz = "UTC", usetz = TRUE)
      ),
      source_pdf_name = safe_character(metadata$source_pdf_name, ""),
      expected_pages = safe_integer(metadata$expected_pages, 0L),
      render_dpi = safe_integer(
        metadata$render_dpi,
        BUILDER_RENDER_DPI
      )
    ),
    settings = normalize_parser_settings(template$settings),
    fields = normalize_fields(template$fields)
  )
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
    return(paste("Template normalization failed:", conditionMessage(normalized)))
  }
  
  settings <- normalized$settings
  ocr_settings <- settings$ocr
  fields <- normalized$fields
  
  if (!nzchar(trimws(normalized$template$name))) {
    problems <- c(problems, "Template name is empty.")
  }
  
  if (
    !is.finite(settings$x_pad_pct) ||
    settings$x_pad_pct < 0 ||
    settings$x_pad_pct > 1
  ) {
    problems <- c(
      problems,
      "Horizontal native-text padding must be between 0 and 1."
    )
  }
  
  if (
    !is.finite(settings$y_pad_pct) ||
    settings$y_pad_pct < 0 ||
    settings$y_pad_pct > 1
  ) {
    problems <- c(
      problems,
      "Vertical native-text padding must be between 0 and 1."
    )
  }
  
  if (!settings$capture_mode %in% c("center", "overlap")) {
    problems <- c(problems, "Capture mode is invalid.")
  }
  
  if (
    settings$min_word_overlap_pct < 0 ||
    settings$min_word_overlap_pct > 100
  ) {
    problems <- c(
      problems,
      "Minimum word overlap must be between 0 and 100."
    )
  }
  
  if (settings$line_tolerance <= 0) {
    problems <- c(problems, "Line tolerance must be greater than zero.")
  }
  
  valid_default_modes <- c(
    "native", "printed_ocr", "handwriting", "auto", "hybrid"
  )
  
  if (
    !ocr_settings$default_extraction_mode %in%
    valid_default_modes
  ) {
    problems <- c(problems, "The default recognition mode is invalid.")
  }
  
  if (ocr_settings$dpi < 72) {
    problems <- c(problems, "Recognition DPI must be at least 72.")
  }
  
  if (
    ocr_settings$minimum_confidence < 0 ||
    ocr_settings$minimum_confidence > 100
  ) {
    problems <- c(
      problems,
      "Default confidence must be between 0 and 100."
    )
  }
  
  if (nrow(fields) == 0) {
    return(unique(c(
      problems,
      "The template does not contain any fields."
    )))
  }
  
  if (any(is.na(fields$name) | !nzchar(trimws(fields$name)))) {
    problems <- c(problems, "One or more fields have blank names.")
  }
  
  duplicate_names <- unique(fields$name[duplicated(fields$name)])
  
  if (length(duplicate_names) > 0) {
    problems <- c(
      problems,
      paste(
        "Field names must be unique. Duplicates:",
        paste(duplicate_names, collapse = ", ")
      )
    )
  }
  
  if (any(duplicated(fields$field_id))) {
    problems <- c(problems, "Field IDs must be unique.")
  }
  
  if (any(is.na(fields$page) | fields$page < 1)) {
    problems <- c(
      problems,
      "Every field must have a positive page number."
    )
  }
  
  for (column in c(
    "xmin_norm", "ymin_norm", "xmax_norm", "ymax_norm"
  )) {
    values <- fields[[column]]
    
    if (any(
      is.na(values) |
      !is.finite(values) |
      values < 0 |
      values > 1
    )) {
      problems <- c(
        problems,
        paste(column, "must contain values between 0 and 1.")
      )
    }
  }
  
  if (any(fields$xmin_norm >= fields$xmax_norm, na.rm = TRUE)) {
    problems <- c(
      problems,
      "Every field must have xmin_norm less than xmax_norm."
    )
  }
  
  if (any(fields$ymin_norm >= fields$ymax_norm, na.rm = TRUE)) {
    problems <- c(
      problems,
      "Every field must have ymin_norm less than ymax_norm."
    )
  }
  
  valid_field_modes <- c(
    "inherit", "native", "printed_ocr", "handwriting", "auto", "hybrid"
  )
  
  if (any(!fields$extraction_mode %in% valid_field_modes)) {
    problems <- c(
      problems,
      "One or more fields have an invalid extraction mode."
    )
  }
  
  valid_content_types <- c(
    "single_character", "single_word", "single_line", "multiline",
    "sparse_text", "numeric", "date", "identifier"
  )
  
  if (any(!fields$content_type %in% valid_content_types)) {
    problems <- c(
      problems,
      "One or more fields have an invalid content type."
    )
  }
  
  if (any(
    fields$minimum_confidence < 0 |
    fields$minimum_confidence > 100,
    na.rm = TRUE
  )) {
    problems <- c(
      problems,
      "Field confidence thresholds must be between 0 and 100."
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
  
  jsonlite::write_json(
    normalize_template(template),
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
  template <- jsonlite::read_json(file, simplifyVector = TRUE)
  template <- normalize_template(template)
  problems <- validate_template(template)
  
  if (length(problems) > 0) {
    stop(
      paste(
        "Imported template is invalid:",
        paste(problems, collapse = "\n- "),
        sep = "\n- "
      ),
      call. = FALSE
    )
  }
  
  template
}

render_builder_pdf_page <- function(
    pdf_file,
    page,
    dpi = BUILDER_RENDER_DPI
) {
  output_file <- tempfile(
    pattern = paste0("builder_page_", page, "_"),
    fileext = ".png"
  )
  
  on.exit(unlink(output_file, force = TRUE), add = TRUE)
  
  rendered <- pdftools::pdf_convert(
    pdf = pdf_file,
    format = "png",
    pages = as.integer(page),
    dpi = as.integer(dpi),
    filenames = output_file,
    verbose = FALSE
  )
  
  if (
    length(rendered) == 0 ||
    !file.exists(rendered[[1]])
  ) {
    stop(
      paste("PDF page", page, "could not be rendered."),
      call. = FALSE
    )
  }
  
  image <- png::readPNG(rendered[[1]])
  dimensions <- dim(image)
  
  if (is.null(dimensions) || length(dimensions) < 2) {
    stop("Rendered page has invalid dimensions.", call. = FALSE)
  }
  
  height <- as.numeric(dimensions[[1]])
  width <- as.numeric(dimensions[[2]])
  
  if (
    !is.finite(width) ||
    !is.finite(height) ||
    width <= 0 ||
    height <= 0
  ) {
    stop("Rendered page has invalid dimensions.", call. = FALSE)
  }
  
  list(
    image = image,
    width = width,
    height = height,
    page = as.integer(page),
    dpi = as.integer(dpi)
  )
}

prepare_native_page_words <- function(words) {
  if (is.null(words) || nrow(words) == 0) return(data.frame())
  
  words <- as.data.frame(words, stringsAsFactors = FALSE)
  required <- c("x", "y", "width", "height", "text")
  
  if (!all(required %in% names(words))) return(data.frame())
  
  words <- words[
    !is.na(words$text) & nzchar(words$text),
    ,
    drop = FALSE
  ]
  
  if (nrow(words) == 0) return(data.frame())
  
  words$word_xmin <- words$x
  words$word_ymin <- words$y
  words$word_xmax <- words$x + words$width
  words$word_ymax <- words$y + words$height
  words$word_xcenter <- words$x + words$width / 2
  words$word_ycenter <- words$y + words$height / 2
  words
}

calculate_native_field_bounds <- function(
    field,
    page_width,
    page_height,
    settings
) {
  list(
    xmin = max(
      0,
      field$xmin_norm[[1]] * page_width -
        settings$x_pad_pct * page_width
    ),
    xmax = min(
      page_width,
      field$xmax_norm[[1]] * page_width +
        settings$x_pad_pct * page_width
    ),
    ymin = max(
      0,
      field$ymin_norm[[1]] * page_height -
        settings$y_pad_pct * page_height
    ),
    ymax = min(
      page_height,
      field$ymax_norm[[1]] * page_height +
        settings$y_pad_pct * page_height
    )
  )
}

extract_native_field <- function(
    field,
    words,
    page_width,
    page_height,
    settings
) {
  if (is.null(words) || nrow(words) == 0) return("")
  
  bounds <- calculate_native_field_bounds(
    field,
    page_width,
    page_height,
    settings
  )
  
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
    
    keep <- overlap_area / word_area >=
      settings$min_word_overlap_pct / 100
  }
  
  selected <- words[keep, , drop = FALSE]
  
  if (nrow(selected) == 0) return("")
  
  line_key <- round(
    selected$y / max(settings$line_tolerance, 1e-9)
  )
  
  selected <- selected[
    order(line_key, selected$x),
    ,
    drop = FALSE
  ]
  
  normalize_recognized_text(
    paste(selected$text, collapse = " ")
  )
}

parse_pdf_fields <- function(pdf_file, template) {
  template <- normalize_template(template)
  problems <- validate_template(template)
  
  if (length(problems) > 0) {
    stop(
      paste(
        "Template validation failed:",
        paste(problems, collapse = "; ")
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
  ocr_settings <- normalize_ocr_settings(settings$ocr)
  pdf_information <- pdftools::pdf_info(pdf_file)
  actual_pages <- safe_integer(pdf_information$pages, 0L)
  
  if (actual_pages < 1) {
    stop("PDF contains no readable pages.", call. = FALSE)
  }
  
  page_sizes <- pdftools::pdf_pagesize(pdf_file)
  
  all_pdf_words <- tryCatch(
    pdftools::pdf_data(pdf_file),
    error = function(e) vector("list", actual_pages)
  )
  
  prepared_pages <- vector("list", actual_pages)
  
  available_pages <- sort(unique(fields$page))
  available_pages <- available_pages[
    available_pages >= 1 &
      available_pages <= actual_pages
  ]
  
  for (page_number in available_pages) {
    page_words <- if (page_number <= length(all_pdf_words)) {
      all_pdf_words[[page_number]]
    } else {
      NULL
    }
    
    prepared_pages[[page_number]] <-
      prepare_native_page_words(page_words)
  }
  
  page_cache <- create_ocr_page_cache()
  engine_cache <- create_ocr_engine_cache()
  
  on.exit({
    clear_ocr_cache(page_cache)
    clear_ocr_cache(engine_cache)
  }, add = TRUE)
  
  environment_cache <- new.env(parent = emptyenv())
  
  run_recognition <- function(
    field,
    page_number,
    recognition_type
  ) {
    if (!isTRUE(ocr_settings$enabled)) {
      return(list(
        raw_text = "",
        confidence = NA_real_,
        status = "error",
        message = "Character recognition is disabled.",
        extraction_method = recognition_type
      ))
    }
    
    field_settings <- resolve_field_recognition_settings(
      field,
      ocr_settings
    )
    
    language <- field_settings$language
    environment_key <- gsub("[^A-Za-z0-9_]+", "_", language)
    
    if (!exists(
      environment_key,
      envir = environment_cache,
      inherits = FALSE
    )) {
      assign(
        environment_key,
        check_ocr_environment(language),
        envir = environment_cache
      )
    }
    
    environment_status <- get(
      environment_key,
      envir = environment_cache,
      inherits = FALSE
    )
    
    if (!isTRUE(environment_status$ready)) {
      return(list(
        raw_text = "",
        confidence = NA_real_,
        status = "error",
        message = paste(
          environment_status$problems,
          collapse = "; "
        ),
        extraction_method = recognition_type
      ))
    }
    
    page_image <- tryCatch(
      get_cached_recognition_page(
        pdf_file = pdf_file,
        page = page_number,
        dpi = ocr_settings$dpi,
        page_cache = page_cache
      ),
      error = function(e) e
    )
    
    if (inherits(page_image, "error")) {
      return(list(
        raw_text = "",
        confidence = NA_real_,
        status = "error",
        message = conditionMessage(page_image),
        extraction_method = recognition_type
      ))
    }
    
    tryCatch(
      extract_field_with_recognition(
        field = field,
        page_image = page_image,
        ocr_settings = ocr_settings,
        engine_cache = engine_cache,
        recognition_type = recognition_type
      ),
      error = function(e) {
        list(
          raw_text = "",
          confidence = NA_real_,
          status = "error",
          message = conditionMessage(e),
          extraction_method = recognition_type
        )
      }
    )
  }
  
  results <- vector("list", nrow(fields))
  
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
        extraction_method = "none",
        recognition_confidence = NA_real_,
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
    
    field_settings <- resolve_field_recognition_settings(
      field,
      ocr_settings
    )
    
    mode <- field_settings$extraction_mode
    words <- prepared_pages[[page_number]]
    
    native_text <- extract_native_field(
      field = field,
      words = words,
      page_width = page_sizes$width[[page_number]],
      page_height = page_sizes$height[[page_number]],
      settings = settings
    )
    
    native_usable <- is_usable_recognized_text(
      native_text,
      ocr_settings$auto_fallback_min_characters
    )
    
    final_result <- NULL
    
    if (identical(mode, "native")) {
      no_page_text <- is.null(words) || nrow(words) == 0
      
      final_result <- list(
        raw_text = native_text,
        confidence = NA_real_,
        status = if (no_page_text) "warning" else "ok",
        message = if (no_page_text) {
          paste0(
            "Page ",
            page_number,
            " does not contain embedded PDF text."
          )
        } else {
          ""
        },
        extraction_method = "native"
      )
    }
    
    if (identical(mode, "printed_ocr")) {
      final_result <- run_recognition(
        field,
        page_number,
        "printed"
      )
    }
    
    if (identical(mode, "handwriting")) {
      final_result <- run_recognition(
        field,
        page_number,
        "handwriting"
      )
    }
    
    if (identical(mode, "auto")) {
      if (native_usable) {
        final_result <- list(
          raw_text = native_text,
          confidence = NA_real_,
          status = "ok",
          message = "",
          extraction_method = "native"
        )
      } else {
        final_result <- run_recognition(
          field,
          page_number,
          "printed"
        )
        final_result$extraction_method <-
          "printed_ocr_fallback"
      }
    }
    
    if (identical(mode, "hybrid")) {
      handwriting_result <- run_recognition(
        field,
        page_number,
        "handwriting"
      )
      
      handwriting_usable <- is_usable_recognized_text(
        handwriting_result$raw_text,
        1L
      )
      
      if (native_usable && !handwriting_usable) {
        final_result <- list(
          raw_text = native_text,
          confidence = NA_real_,
          status = "ok",
          message = "",
          extraction_method = "native_hybrid"
        )
      } else if (!native_usable && handwriting_usable) {
        final_result <- handwriting_result
        final_result$extraction_method <-
          "handwriting_hybrid"
      } else if (
        native_usable &&
        handwriting_usable &&
        identical(
          tolower(trimws(native_text)),
          tolower(trimws(handwriting_result$raw_text))
        )
      ) {
        final_result <- handwriting_result
        final_result$extraction_method <-
          "native_handwriting_agreement"
      } else if (native_usable && handwriting_usable) {
        final_result <- list(
          raw_text = native_text,
          confidence = handwriting_result$confidence,
          status = "warning",
          message = paste0(
            "Native extraction and handwriting recognition ",
            "returned different values. Native: '",
            native_text,
            "'. Handwriting: '",
            handwriting_result$raw_text,
            "'."
          ),
          extraction_method = "hybrid_conflict"
        )
      } else {
        final_result <- handwriting_result
        final_result$status <- "warning"
        final_result$message <- paste(
          "Neither native extraction nor handwriting",
          "recognition returned usable text."
        )
        final_result$extraction_method <- "hybrid_empty"
      }
    }
    
    if (is.null(final_result)) {
      final_result <- list(
        raw_text = "",
        confidence = NA_real_,
        status = "error",
        message = paste("Unsupported extraction mode:", mode),
        extraction_method = "none"
      )
    }
    
    results[[i]] <- data.frame(
      field_id = field$field_id[[1]],
      field_name = field$name[[1]],
      page = page_number,
      raw_text = normalize_recognized_text(final_result$raw_text),
      extraction_method = final_result$extraction_method,
      recognition_confidence = final_result$confidence,
      status = final_result$status,
      message = final_result$message,
      stringsAsFactors = FALSE
    )
  }
  
  result <- do.call(rbind, results)
  rownames(result) <- NULL
  result
}

create_failed_parse_row <- function(
    source_name,
    template,
    error_message
) {
  fields <- normalize_template(template)$fields
  values <- as.list(rep(NA_character_, nrow(fields)))
  names(values) <- fields$name
  
  cbind(
    data.frame(
      source_file = source_name,
      parse_status = "error",
      parse_message = error_message,
      stringsAsFactors = FALSE
    ),
    as.data.frame(
      values,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  )
}

create_failed_quality_rows <- function(
    source_name,
    template,
    error_message
) {
  fields <- normalize_template(template)$fields
  
  data.frame(
    source_file = source_name,
    field_id = fields$field_id,
    field_name = fields$name,
    page = fields$page,
    raw_text = NA_character_,
    extraction_method = "none",
    recognition_confidence = NA_real_,
    status = "error",
    message = error_message,
    stringsAsFactors = FALSE
  )
}

parse_pdf_batch_with_quality <- function(
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
        paste(problems, collapse = "; ")
      ),
      call. = FALSE
    )
  }
  
  if (!all(c("path", "source_file") %in% names(file_manifest))) {
    stop(
      "File manifest must contain path and source_file columns.",
      call. = FALSE
    )
  }
  
  if (nrow(file_manifest) == 0) {
    stop("No PDF files were provided.", call. = FALSE)
  }
  
  wide_rows <- vector("list", nrow(file_manifest))
  quality_rows <- vector("list", nrow(file_manifest))
  
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
    
    parsed <- tryCatch(
      parse_pdf_fields(pdf_path, template),
      error = function(e) e
    )
    
    if (inherits(parsed, "error")) {
      error_message <- conditionMessage(parsed)
      
      wide_rows[[i]] <- create_failed_parse_row(
        source_name,
        template,
        error_message
      )
      
      quality_rows[[i]] <- create_failed_quality_rows(
        source_name,
        template,
        error_message
      )
      
      next
    }
    
    document_status <- if (any(parsed$status == "error")) {
      "error"
    } else if (any(parsed$status == "warning")) {
      "warning"
    } else {
      "ok"
    }
    
    field_values <- as.list(parsed$raw_text)
    names(field_values) <- parsed$field_name
    
    wide_rows[[i]] <- cbind(
      data.frame(
        source_file = source_name,
        parse_status = document_status,
        parse_message = collapse_messages(parsed$message),
        stringsAsFactors = FALSE
      ),
      as.data.frame(
        field_values,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
    
    parsed$source_file <- source_name
    
    quality_rows[[i]] <- parsed[, c(
      "source_file",
      "field_id",
      "field_name",
      "page",
      "raw_text",
      "extraction_method",
      "recognition_confidence",
      "status",
      "message"
    )]
  }
  
  wide <- do.call(rbind, wide_rows)
  quality <- do.call(rbind, quality_rows)
  rownames(wide) <- NULL
  rownames(quality) <- NULL
  
  list(data = wide, quality = quality)
}

app_theme <- bslib::bs_theme(
  version = 3,
  bg = "#F7F7F7",
  fg = "#00274C",
  primary = "#00274C",
  secondary = "#5A5A5A",
  success = "#2D6A4F",
  info = "#2F65A7",
  warning = "#FFCB05",
  danger = "#C94242",
  base_font = bslib::font_collection(
    "-apple-system",
    "BlinkMacSystemFont",
    "Segoe UI",
    "Arial",
    "sans-serif"
  ),
  heading_font = bslib::font_collection(
    "Arial",
    "Helvetica",
    "sans-serif"
  )
)

ui <- fluidPage(
  theme = app_theme,
  
  tags$head(
    tags$link(
      rel = "icon",
      type = "image/png",
      href = "logo.png"
    ),
    
    tags$meta(
      name = "theme-color",
      content = "#00274C"
    ),
    
    tags$script(HTML("
      $(function () {
        $('body').tooltip({
          selector: '.help-tip',
          container: 'body',
          trigger: 'hover focus'
        });
      });
    ")),
    
    tags$style(HTML("
      :root {
        --um-blue: #00274C;
        --um-maize: #FFCB05;
        --um-light-blue: #2F65A7;
        --um-pale-blue: #EAF2F8;
        --um-page: #F4F5F6;
        --um-border: #D6D9DC;
        --um-text: #1F2D3D;
        --um-muted: #5A6673;
        --um-success: #2D6A4F;
        --um-danger: #B42318;
      }

      html,
      body {
        min-height: 100%;
      }

      body {
        margin: 0;
        color: var(--um-text);
        background:
          linear-gradient(
            180deg,
            var(--um-blue) 0,
            var(--um-blue) 116px,
            var(--um-page) 116px,
            var(--um-page) 100%
          );
        font-family:
          -apple-system,
          BlinkMacSystemFont,
          'Segoe UI',
          Arial,
          sans-serif;
      }

      .container-fluid {
        max-width: 1800px;
        margin: 0 auto;
        padding: 0 24px 36px 24px;
      }

      .um-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        min-height: 92px;
        margin: 0 -24px;
        padding: 12px 28px;
        color: white;
        background: var(--um-blue);
      }

      .um-brand {
        display: flex;
        align-items: center;
        min-width: 0;
      }

      .um-logo-container {
        display: flex;
        align-items: center;
        justify-content: center;
        width: 76px;
        height: 64px;
        margin-right: 18px;
        padding: 6px;
        background: white;
        border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.22);
      }

      .um-logo {
        display: block;
        max-width: 100%;
        max-height: 100%;
        object-fit: contain;
      }

      .um-title-group {
        min-width: 0;
      }

      .um-eyebrow {
        margin-bottom: 2px;
        color: var(--um-maize);
        font-size: 13px;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
      }

      .um-app-title {
        margin: 0;
        color: white;
        font-size: 28px;
        font-weight: 700;
        line-height: 1.15;
      }

      .um-app-subtitle {
        margin-top: 5px;
        color: #D8E4EF;
        font-size: 14px;
      }

      .nav-tabs {
        margin: 0 -24px 22px -24px;
        padding: 0 28px;
        background: white;
        border-bottom: 4px solid var(--um-maize);
        box-shadow: 0 3px 10px rgba(0, 39, 76, 0.12);
      }

      .nav-tabs > li {
        margin-bottom: 0;
      }

      .nav-tabs > li > a {
        margin: 0;
        padding: 15px 22px 13px 22px;
        color: var(--um-blue);
        background: transparent;
        border: 0;
        border-radius: 0;
        font-weight: 700;
      }

      .nav-tabs > li > a:hover,
      .nav-tabs > li > a:focus {
        color: var(--um-blue);
        background: #FFF4BF;
        border: 0;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: white;
        background: var(--um-blue);
        border: 0;
        box-shadow: inset 0 -4px 0 var(--um-maize);
      }

      .tab-content {
        padding-top: 0;
      }

      .well {
        padding: 20px;
        background: white;
        border: 1px solid var(--um-border);
        border-top: 6px solid var(--um-blue);
        border-radius: 8px;
        box-shadow: 0 4px 14px rgba(0, 39, 76, 0.09);
      }

      .main-panel-card {
        min-height: 200px;
        padding: 20px;
        background: white;
        border: 1px solid var(--um-border);
        border-radius: 8px;
        box-shadow: 0 4px 14px rgba(0, 39, 76, 0.08);
      }

      h1,
      h2,
      h3,
      h4,
      h5,
      h6 {
        color: var(--um-blue);
        font-family: Arial, Helvetica, sans-serif;
        font-weight: 700;
      }

      h4 {
        padding-bottom: 7px;
        border-bottom: 2px solid var(--um-maize);
      }

      hr {
        border-top-color: #D9DEE3;
      }

      label,
      .control-label {
        color: var(--um-blue);
        font-weight: 650;
      }

      .form-control {
        min-height: 38px;
        color: var(--um-text);
        background-color: white;
        border: 1px solid #9CA9B5;
        border-radius: 4px;
        box-shadow: none;
      }

      .form-control:focus {
        border-color: var(--um-light-blue);
        box-shadow: 0 0 0 3px rgba(47, 101, 167, 0.18);
      }

      .selectize-input {
        min-height: 38px;
        padding: 9px 10px;
        border-color: #9CA9B5;
        border-radius: 4px;
        box-shadow: none;
      }

      .selectize-input.focus {
        border-color: var(--um-light-blue);
        box-shadow: 0 0 0 3px rgba(47, 101, 167, 0.18);
      }

      .btn {
        min-height: 38px;
        border-radius: 4px;
        font-weight: 700;
        transition:
          background-color 120ms ease,
          border-color 120ms ease,
          box-shadow 120ms ease,
          transform 120ms ease;
      }

      .btn:hover {
        transform: translateY(-1px);
      }

      .btn:focus {
        outline: 3px solid rgba(255, 203, 5, 0.65);
        outline-offset: 2px;
      }

      .btn-primary {
        color: white;
        background: var(--um-blue);
        border-color: var(--um-blue);
      }

      .btn-primary:hover,
      .btn-primary:focus,
      .btn-primary:active {
        color: var(--um-blue);
        background: var(--um-maize);
        border-color: #D6AB00;
      }

      .btn-default {
        color: var(--um-blue);
        background: white;
        border-color: var(--um-blue);
      }

      .btn-default:hover,
      .btn-default:focus,
      .btn-default:active {
        color: var(--um-blue);
        background: #FFF4BF;
        border-color: var(--um-blue);
      }

      .shiny-download-link {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-height: 38px;
        padding: 8px 12px;
        color: var(--um-blue);
        background: var(--um-maize);
        border: 1px solid #D6AB00;
        border-radius: 4px;
        font-weight: 700;
        text-decoration: none;
      }

      .shiny-download-link:hover,
      .shiny-download-link:focus {
        color: white;
        background: var(--um-blue);
        border-color: var(--um-blue);
        text-decoration: none;
      }

      .btn-full-width,
      .shiny-download-link.btn-full-width {
        width: 100%;
        margin-bottom: 8px;
      }

      .page-navigation {
        display: flex;
        align-items: end;
        gap: 10px;
        flex-wrap: wrap;
        margin-bottom: 12px;
        padding: 12px;
        background: var(--um-pale-blue);
        border: 1px solid #C8D8E8;
        border-left: 5px solid var(--um-blue);
        border-radius: 6px;
      }

      .page-navigation .form-group {
        min-width: 150px;
        margin-bottom: 0;
      }

      .help-tip {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 18px;
        height: 18px;
        margin-left: 5px;
        color: var(--um-blue);
        background: var(--um-maize);
        border: 1px solid #D6AB00;
        border-radius: 50%;
        font-size: 11px;
        font-weight: 800;
        line-height: 1;
        vertical-align: middle;
        cursor: help;
      }

      .help-tip:hover,
      .help-tip:focus {
        color: white;
        background: var(--um-blue);
        border-color: var(--um-blue);
        outline: 3px solid rgba(255, 203, 5, 0.5);
      }

      .settings-group {
        margin-bottom: 14px;
      }
      
      .add-field-button {
        width: 100%;
        min-height: 46px;
        margin: 4px 0 18px 0;
        color: var(--um-maize);
        background: var(--um-blue);
        border: 2px solid var(--um-blue);
        border-radius: 5px;
        font-size: 15px;
        font-weight: 800;
        letter-spacing: 0.01em;
        box-shadow: 0 3px 8px rgba(0, 39, 76, 0.18);
      }

      .add-field-button:hover,
      .add-field-button:focus,
      .add-field-button:active {
        color: var(--um-blue) !important;
        background: #FFF4BF !important;
        border-color: var(--um-blue) !important;
      }
      
      .modify-fields-section {
        margin-top: 4px;
        margin-bottom: 16px;
        padding: 14px;
        background: #F8FAFC;
        border: 1px solid #C9D2DB;
        border-left: 5px solid var(--um-blue);
        border-radius: 6px;
      }
      
      .modify-fields-title {
        margin: 0 0 12px 0;
        padding-bottom: 7px;
        color: var(--um-blue);
        border-bottom: 3px solid var(--um-maize);
        font-size: 17px;
        font-weight: 700;
      }
      
      .field-action-button {
        width: 100%;
        margin-bottom: 8px;
      }
      
      .field-action-danger {
        color: #8A1C13;
        background: white;
        border-color: #B42318;
      }
      
      .field-action-danger:hover,
      .field-action-danger:focus,
      .field-action-danger:active {
        color: white !important;
        background: #B42318 !important;
        border-color: #8A1C13 !important;
      }
      
      .template-output-section {
        margin-top: 18px;
        padding-top: 16px;
        border-top: 2px solid var(--um-maize);
      }

      .settings-group-title {
        margin-top: 0;
        margin-bottom: 12px;
        padding-bottom: 8px;
        color: var(--um-blue);
        border-bottom: 3px solid var(--um-maize);
        font-size: 18px;
        font-weight: 700;
      }

      .advanced-settings {
        margin: 12px 0 18px 0;
        overflow: hidden;
        background: #F8FAFC;
        border: 1px solid #C9D2DB;
        border-radius: 6px;
      }

      .advanced-settings > summary {
        padding: 11px 13px;
        color: var(--um-blue);
        background: var(--um-pale-blue);
        border-left: 5px solid var(--um-maize);
        font-weight: 700;
        cursor: pointer;
        user-select: none;
      }

      .advanced-settings > summary:hover,
      .advanced-settings > summary:focus {
        background: #DCEAF5;
      }

      .advanced-settings[open] > summary {
        border-bottom: 1px solid #C9D2DB;
      }

      .advanced-settings-content {
        padding: 14px;
      }

      .advanced-subheading {
        margin-top: 6px;
        padding-bottom: 5px;
        color: var(--um-blue);
        border-bottom: 1px solid #B9C6D2;
        font-size: 15px;
        font-weight: 700;
      }

      .pdf-render-container {
        padding: 22px;
        overflow: auto;
        background:
          linear-gradient(
            135deg,
            #E6E9EC 25%,
            #EDF0F2 25%,
            #EDF0F2 50%,
            #E6E9EC 50%,
            #E6E9EC 75%,
            #EDF0F2 75%,
            #EDF0F2 100%
          );
        background-size: 24px 24px;
        border: 1px solid #C7CDD2;
        border-radius: 6px;
      }

      .pdf-render-container .shiny-plot-output {
        margin: 0 auto;
        background: white;
        border: 1px solid #AAB2B9;
        box-shadow: 0 5px 16px rgba(0, 39, 76, 0.2);
      }

      .pdf-render-container .shiny-plot-output img {
        display: block;
      }

      .template-status-summary {
        display: flex;
        align-items: center;
        flex-wrap: wrap;
        gap: 7px 18px;
        padding: 10px 14px;
        margin: 10px 0 18px 0;
        color: var(--um-text);
        background: white;
        border: 1px solid var(--um-border);
        border-left: 6px solid var(--um-maize);
        border-radius: 5px;
        box-shadow: 0 2px 7px rgba(0, 39, 76, 0.07);
        font-size: 13px;
        line-height: 1.35;
      }

      .template-status-title {
        color: var(--um-blue);
        font-weight: 800;
      }

      .template-status-item {
        white-space: nowrap;
      }

      .template-status-valid {
        color: var(--um-success);
        font-weight: 800;
      }

      .template-status-invalid {
        color: var(--um-danger);
        font-weight: 800;
        cursor: help;
      }

      .ocr-ready {
        margin-top: 8px;
        padding: 8px 10px;
        color: #174A35;
        background: #E7F4EC;
        border-left: 4px solid var(--um-success);
        border-radius: 3px;
        font-size: 12px;
      }

      .ocr-not-ready {
        margin-top: 8px;
        padding: 8px 10px;
        color: #7A1A14;
        background: #FDECEA;
        border-left: 4px solid var(--um-danger);
        border-radius: 3px;
        font-size: 12px;
      }

      .section-note,
      .help-block {
        color: var(--um-muted);
        font-size: 12px;
      }

      .tooltip-inner {
        max-width: 360px;
        padding: 10px 12px;
        color: white;
        background: var(--um-blue);
        text-align: left;
      }

      .tooltip.top .tooltip-arrow {
        border-top-color: var(--um-blue);
      }

      .tooltip.right .tooltip-arrow {
        border-right-color: var(--um-blue);
      }

      .tooltip.bottom .tooltip-arrow {
        border-bottom-color: var(--um-blue);
      }

      .tooltip.left .tooltip-arrow {
        border-left-color: var(--um-blue);
      }

      .dataTables_wrapper {
        padding: 5px 0;
        color: var(--um-text);
      }

      table.dataTable {
        border-collapse: collapse !important;
        border: 1px solid var(--um-border);
      }

      table.dataTable thead th {
        color: white;
        background: var(--um-blue);
        border-bottom: 3px solid var(--um-maize) !important;
      }

      table.dataTable tbody tr:nth-child(even) {
        background: #F4F7FA;
      }

      table.dataTable tbody tr:hover {
        background: #FFF4BF;
      }

      table.dataTable tbody tr.selected,
      table.dataTable tbody tr.selected > .sorting_1 {
        color: var(--um-blue) !important;
        background: #FFE36E !important;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        color: white !important;
        background: var(--um-blue) !important;
        border-color: var(--um-blue) !important;
      }

      .progress {
        height: 22px;
        background: #DDE3E8;
        border-radius: 4px;
      }

      .progress-bar {
        color: var(--um-blue);
        background: var(--um-maize);
        font-weight: 700;
      }

      .shiny-notification {
        color: var(--um-text);
        background: white;
        border: 1px solid var(--um-border);
        border-left: 6px solid var(--um-blue);
        box-shadow: 0 4px 12px rgba(0, 39, 76, 0.16);
      }

      .shiny-notification-message {
        border-left-color: var(--um-success);
      }

      .shiny-notification-warning {
        border-left-color: var(--um-maize);
      }

      .shiny-notification-error {
        border-left-color: var(--um-danger);
      }

      pre {
        color: var(--um-text);
        background: #F5F7F9;
        border-color: var(--um-border);
      }

      input[type='checkbox'],
      input[type='radio'] {
        accent-color: var(--um-blue);
      }

      .irs-bar,
      .irs-bar-edge {
        background: var(--um-blue);
        border-color: var(--um-blue);
      }

      .irs-single,
      .irs-from,
      .irs-to {
        color: var(--um-blue);
        background: var(--um-maize);
      }

      .irs-single:after,
      .irs-from:after,
      .irs-to:after {
        border-top-color: var(--um-maize);
      }

      .irs-slider {
        background: var(--um-blue);
        border-color: white;
        box-shadow: 0 0 0 2px var(--um-blue);
      }

      @media (max-width: 767px) {
        body {
          background:
            linear-gradient(
              180deg,
              var(--um-blue) 0,
              var(--um-blue) 150px,
              var(--um-page) 150px,
              var(--um-page) 100%
            );
        }

        .container-fluid {
          padding-left: 12px;
          padding-right: 12px;
        }

        .um-header {
          margin-left: -12px;
          margin-right: -12px;
          padding: 12px 16px;
        }

        .um-logo-container {
          width: 58px;
          height: 52px;
          margin-right: 12px;
        }

        .um-app-title {
          font-size: 21px;
        }

        .um-app-subtitle {
          display: none;
        }

        .nav-tabs {
          margin-left: -12px;
          margin-right: -12px;
          padding: 0 12px;
        }

        .nav-tabs > li > a {
          padding: 12px 14px;
        }

        .well,
        .main-panel-card {
          padding: 14px;
        }
      }
    "))
  ),
  
  div(
    class = "um-header",
    
    div(
      class = "um-brand",
      
      div(
        class = "um-logo-container",
        
        tags$img(
          src = "logo.png",
          class = "um-logo",
          alt = "University of Michigan logo"
        )
      ),
      
      div(
        class = "um-title-group",
        
        div(
          class = "um-eyebrow",
          "Michigan Public Health Integrated Center for Outbreak Analytics and Modeling"
        ),
        
        tags$h1(
          class = "um-app-title",
          "Interactive PDF Parser"
        ),
        
        div(
          class = "um-app-subtitle",
          "Build reusable templates and extract structured data from PDF forms"
        )
      )
    )
),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel(
      "Build Template",
      
      sidebarLayout(
        sidebarPanel(
          div(
            class = "settings-group",
            
            h4(
              class = "settings-group-title",
              "Template"
            ),
            
            fileInput(
              "builder_pdf",
              help_label(
                "Representative PDF",
                paste(
                  "Upload a blank or completed example of the form.",
                  "You will draw extraction fields on this document.",
                  "Use the same form layout that will be parsed later."
                )
              ),
              accept = ".pdf"
            ),
            
            textInput(
              "template_name",
              help_label(
                "Template name",
                paste(
                  "Names the reusable parser template and its exported JSON file.",
                  "Use a short descriptive name, such as intake_form_v1."
                )
              ),
              value = "pdf_template"
            )
          ),
          
          hr(),
          
          div(
            class = "settings-group",
            
            h4(
              class = "settings-group-title",
              "Add Field"
            ),
            
            textInput(
              "field_name",
              help_label(
                "Field name",
                paste(
                  "Becomes the output CSV column name.",
                  "Names must be unique, such as patient_name or visit_date."
                )
              )
            ),
            
            selectInput(
              "field_extraction_mode",
              help_label(
                "Extraction method",
                paste(
                  "Use template default applies the template-wide method.",
                  "Native is best for computer-filled PDFs with embedded text.",
                  "Printed OCR reads scanned or flattened printed text.",
                  "Handwriting is experimental and is intended for neat handwritten entries.",
                  "Auto tries native text and then printed OCR.",
                  "Hybrid compares native text with handwriting recognition."
                )
              ),
              choices = c(
                "Use template default" = "inherit",
                "Native embedded PDF text" = "native",
                "Printed OCR" = "printed_ocr",
                "Handwriting recognition — experimental" =
                  "handwriting",
                "Auto: native, then printed OCR" = "auto",
                "Hybrid: native and handwriting" = "hybrid"
              ),
              selected = "inherit"
            ),
            
            selectInput(
              "field_content_type",
              help_label(
                "Content type",
                paste(
                  "Describes the expected field layout.",
                  "Single line suits most names and short answers.",
                  "Single word or character is useful for compact fields.",
                  "Multiple lines suits notes.",
                  "Number, date, and identifier restrict recognition to likely characters."
                )
              ),
              choices = c(
                "Single line" = "single_line",
                "Single word" = "single_word",
                "Single character" = "single_character",
                "Multiple lines" = "multiline",
                "Sparse text" = "sparse_text",
                "Number" = "numeric",
                "Date" = "date",
                "Identifier" = "identifier"
              ),
              selected = "single_line"
            ),
            
            selectInput(
              "default_extraction_mode",
              help_label(
                "Template default method",
                paste(
                  "Used by every field set to Use template default.",
                  "Auto is recommended for mixed computer-filled and scanned forms.",
                  "Native is fastest for PDFs with embedded text.",
                  "Printed OCR is useful for scanned printed forms.",
                  "Handwriting and Hybrid are experimental."
                )
              ),
              choices = c(
                "Auto: native, then printed OCR" = "auto",
                "Native embedded PDF text" = "native",
                "Printed OCR" = "printed_ocr",
                "Handwriting recognition — experimental" =
                  "handwriting",
                "Hybrid native/handwriting" = "hybrid"
              ),
              selected = "auto"
            )
          ),
          
          tags$details(
            class = "advanced-settings",
            
            tags$summary("Advanced settings"),
            
            div(
              class = "advanced-settings-content",
              
              h5(
                class = "advanced-subheading",
                "Field overrides"
              ),
              
              selectInput(
                "field_preprocessing_profile",
                help_label(
                  "Field image preprocessing",
                  paste(
                    "Controls image cleanup before recognition.",
                    "Use template default applies the template-wide profile.",
                    "Standard suits most scans.",
                    "High contrast helps dark text.",
                    "Faint text helps light scans.",
                    "Handwriting applies conservative cleanup for pen entries.",
                    "None preserves the original crop."
                  )
                ),
                choices = c(
                  "Use template default" = "inherit",
                  "None" = "none",
                  "Standard" = "standard",
                  "High contrast" = "high_contrast",
                  "Faint text" = "faint_text",
                  "Handwriting" = "handwriting"
                ),
                selected = "inherit"
              ),
              
              textInput(
                "field_ocr_language",
                help_label(
                  "Field language override",
                  paste(
                    "Overrides the template OCR language for this field.",
                    "Leave blank to use the template language.",
                    "Use an installed Tesseract code such as eng or spa."
                  )
                ),
                value = "",
                placeholder = "Use template language"
              ),
              
              sliderInput(
                "field_minimum_confidence",
                help_label(
                  "Field confidence threshold",
                  paste(
                    "Recognition below this score is flagged for review.",
                    "Lower values produce fewer warnings.",
                    "Higher values are stricter.",
                    "Confidence is a review aid rather than a guarantee of accuracy."
                  )
                ),
                min = 0,
                max = 100,
                value = 50,
                step = 1
              ),
              
              hr(),
              
              h5(
                class = "advanced-subheading",
                "Native PDF text"
              ),
              
              sliderInput(
                "x_pad_pct",
                help_label(
                  "Horizontal padding",
                  paste(
                    "Expands each native-text field left and right.",
                    "Increase it when edge characters are missed.",
                    "Decrease it when neighboring text is captured."
                  )
                ),
                min = 0,
                max = 0.05,
                value = 0.005,
                step = 0.001
              ),
              
              sliderInput(
                "y_pad_pct",
                help_label(
                  "Vertical padding",
                  paste(
                    "Expands each native-text field above and below.",
                    "Increase it when text near an edge is missed.",
                    "Decrease it when adjacent lines are captured."
                  )
                ),
                min = 0,
                max = 0.05,
                value = 0.005,
                step = 0.001
              ),
              
              selectInput(
                "capture_mode",
                help_label(
                  "Native text capture rule",
                  paste(
                    "Center captures a word when its center lies inside the field",
                    "and is recommended for most forms.",
                    "Overlap captures partly intersecting words and is useful",
                    "when fields cannot tightly surround an answer."
                  )
                ),
                choices = c(
                  "Word center inside field" = "center",
                  "Word overlaps field" = "overlap"
                ),
                selected = "center"
              ),
              
              sliderInput(
                "min_word_overlap_pct",
                help_label(
                  "Minimum word overlap",
                  paste(
                    "Used only with the overlap capture rule.",
                    "Low values capture words that barely touch the field.",
                    "Higher values reduce accidental neighboring text."
                  )
                ),
                min = 0,
                max = 100,
                value = 1,
                step = 1
              ),
              
              sliderInput(
                "line_tolerance",
                help_label(
                  "Line sorting tolerance",
                  paste(
                    "Groups native PDF words with nearby vertical positions.",
                    "Increase it if words on one line appear out of order.",
                    "Decrease it if separate lines are incorrectly combined."
                  )
                ),
                min = 1,
                max = 20,
                value = 3,
                step = 1
              ),
              
              hr(),
              
              h5(
                class = "advanced-subheading",
                "OCR and handwriting"
              ),
              
              checkboxInput(
                "ocr_enabled",
                help_label(
                  "Enable image recognition",
                  paste(
                    "Allows printed OCR and experimental handwriting recognition.",
                    "Disable it when only embedded PDF text should be used",
                    "or when OCR is unavailable on the system."
                  )
                ),
                value = TRUE
              ),
              
              textInput(
                "ocr_language",
                help_label(
                  "Recognition language",
                  paste(
                    "Selects the installed Tesseract language model.",
                    "Use eng for English or another installed language code.",
                    "The matching language pack must be installed."
                  )
                ),
                value = "eng"
              ),
              
              selectInput(
                "ocr_preprocessing_profile",
                help_label(
                  "Default image preprocessing",
                  paste(
                    "Standard is recommended for most scans.",
                    "High contrast can help dark printed text.",
                    "Faint text can help light scans.",
                    "Handwriting uses conservative cleanup for pen entries.",
                    "None is useful when preprocessing damages the text."
                  )
                ),
                choices = c(
                  "None" = "none",
                  "Standard" = "standard",
                  "High contrast" = "high_contrast",
                  "Faint text" = "faint_text",
                  "Handwriting" = "handwriting"
                ),
                selected = "standard"
              ),
              
              selectInput(
                "ocr_dpi",
                help_label(
                  "Recognition resolution",
                  paste(
                    "Controls the image resolution used for recognition.",
                    "300 DPI is recommended.",
                    "200 DPI is faster for clear, large text.",
                    "400 DPI may help small or faint writing but uses more memory."
                  )
                ),
                choices = c(
                  "200 DPI" = 200,
                  "300 DPI" = 300,
                  "400 DPI" = 400
                ),
                selected = 300
              ),
              
              sliderInput(
                "ocr_minimum_confidence",
                help_label(
                  "Default confidence threshold",
                  paste(
                    "Recognition below this score is flagged for review.",
                    "Individual fields can override this value.",
                    "A value around 50 is a practical starting point."
                  )
                ),
                min = 0,
                max = 100,
                value = 50,
                step = 1
              ),
              
              selectInput(
                "ocr_upscale_pct",
                help_label(
                  "Recognition image scaling",
                  paste(
                    "Enlarges field crops before recognition.",
                    "200% is recommended for many small form fields.",
                    "Higher scaling may help small writing but increases processing time.",
                    "Original size is useful for high-resolution scans."
                  )
                ),
                choices = c(
                  "Original size" = 100,
                  "150%" = 150,
                  "200%" = 200,
                  "300%" = 300
                ),
                selected = 200
              ),
              
              uiOutput("ocr_environment_status"),
              
              p(
                class = "section-note",
                paste(
                  "Handwriting mode is experimental and is best suited",
                  "to neat block writing, dates, numbers, and identifiers."
                )
              )
            )
          ),
          
          actionButton(
            "add_field",
            "Add Field",
            class = "add-field-button"
          ),
          
          div(
            class = "modify-fields-section",
            
            h4(
              class = "modify-fields-title",
              "Modify Fields - Select a Saved Field"
            ),
            
            actionButton(
              "load_selected_field",
              "Load selected field settings",
              class = "field-action-button"
            ),
            
            actionButton(
              "update_selected_field",
              "Update selected field settings",
              class = "field-action-button"
            ),
            
            actionButton(
              "delete_selected_field",
              "Delete selected field",
              class = "field-action-button field-action-danger"
            ),
            
            actionButton(
              "clear_current_page",
              "Clear fields on current page",
              class = "field-action-button field-action-danger"
            ),
            
            actionButton(
              "clear_all_fields",
              "Clear all fields",
              class = "field-action-button field-action-danger"
            )
          ),
          
          div(
            class = "template-output-section",
            
            actionButton(
              "preview_sample",
              "Preview sample parsing",
              class = "btn-primary btn-full-width"
            ),
            
            downloadButton(
              "download_template",
              "Export template JSON",
              class = "btn-full-width"
            )
          ),
          
          width = 3
        ),
 
        
        mainPanel(
          div(
            class = "main-panel-card",
            
            div(
              class = "page-navigation",
              
              actionButton(
                "previous_page",
                "Previous page"
              ),
              
              selectInput(
                "builder_page",
                help_label(
                  "Current page",
                  paste(
                    "Selects the template PDF page being displayed.",
                    "New fields are assigned to the currently displayed page."
                  )
                ),
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
            
            h4("Saved fields"),
            
            helpText(
              paste(
                "Select a row to load, update, or delete its settings.",
                "Updating settings does not change the field coordinates."
              )
            ),
            
            DT::DTOutput("fields_table"),
            
            hr(),
            
            h4("Representative PDF preview"),
            
            DT::DTOutput("sample_preview_table")
          ),
          
          width = 9
        )
      )
    ),
    
    tabPanel(
      "Parse PDFs",
      
      sidebarLayout(
        sidebarPanel(
          h4("Template"),
          
          radioButtons(
            "parser_template_source",
            help_label(
              "Template source",
              paste(
                "Use the template currently being built or import a saved JSON template.",
                "Import is useful when reusing a template in a later session."
              )
            ),
            choices = c(
              "Use current builder template" = "current",
              "Load a JSON template" = "imported"
            ),
            selected = "current"
          ),
          
          conditionalPanel(
            condition =
              "input.parser_template_source == 'imported'",
            
            fileInput(
              "imported_template_file",
              help_label(
                "Template JSON file",
                paste(
                  "Loads a previously exported parser template.",
                  "The file contains field locations and parser settings."
                )
              ),
              accept = c(".json", "application/json")
            )
          ),
          
          hr(),
          
          h4("PDF documents"),
          
          radioButtons(
            "batch_input_mode",
            help_label(
              "PDF input method",
              paste(
                "Upload PDFs works locally and on hosted apps.",
                "Folder selection is convenient for local use",
                "but accesses only the filesystem visible to the R process."
              )
            ),
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
              help_label(
                "PDF documents",
                paste(
                  "Select one or more PDFs that share the template layout.",
                  "Each document becomes one row in the parsed output."
                )
              ),
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
              help_label(
                "Include subfolders",
                paste(
                  "When enabled, PDFs inside nested folders are also parsed.",
                  "Leave disabled when only the selected folder should be used."
                )
              ),
              value = FALSE
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
            "Save parsed data CSV",
            class = "btn-full-width"
          ),
          
          downloadButton(
            "download_quality_csv",
            "Save quality-control CSV",
            class = "btn-full-width"
          ),
          
          width = 3
        ),
        
        mainPanel(
          div(
            class = "main-panel-card",
            
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
            
            hr(),
            
            h4("Field-level recognition quality"),
            
            DT::DTOutput("quality_preview_table")
          ),
          
          width = 9
        )
      )
    )
  )
)

server <- function(input, output, session) {
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
  batch_quality_control <- reactiveVal(NULL)
  batch_status_message <- reactiveVal(
    "No batch parse has been run."
  )
  selected_directory <- reactiveVal(NULL)
  
  current_builder_settings <- reactive({
    normalize_parser_settings(list(
      x_pad_pct = input$x_pad_pct,
      y_pad_pct = input$y_pad_pct,
      capture_mode = input$capture_mode,
      min_word_overlap_pct =
        input$min_word_overlap_pct,
      line_tolerance = input$line_tolerance,
      ocr = list(
        enabled = isTRUE(input$ocr_enabled),
        language = input$ocr_language,
        dpi = as.integer(input$ocr_dpi),
        default_extraction_mode =
          input$default_extraction_mode,
        preprocessing_profile =
          input$ocr_preprocessing_profile,
        printed_psm = 7L,
        handwriting_psm = 7L,
        minimum_confidence =
          input$ocr_minimum_confidence,
        auto_fallback_min_characters = 1L,
        x_pad_pct = input$x_pad_pct,
        y_pad_pct = input$y_pad_pct,
        upscale_pct = as.integer(input$ocr_upscale_pct)
      )
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
  
  output$ocr_environment_status <- renderUI({
    language <- input$ocr_language %||% "eng"
    status <- check_ocr_environment(language)
    
    if (isTRUE(status$ready)) {
      return(
        div(
          class = "ocr-ready",
          paste("OCR environment is ready. Language:", language)
        )
      )
    }
    
    div(
      class = "ocr-not-ready",
      tags$b("OCR is not ready:"),
      tags$ul(lapply(status$problems, tags$li))
    )
  })
  
  render_current_builder_page <- function(page_number) {
    pdf_path <- builder_source_path()
    
    if (is.null(pdf_path) || !file.exists(pdf_path)) {
      current_page_render(NULL)
      return(invisible(NULL))
    }
    
    if (
      page_number < 1 ||
      page_number > builder_page_count()
    ) {
      current_page_render(NULL)
      return(invisible(NULL))
    }
    
    tryCatch(
      {
        current_page_render(
          render_builder_pdf_page(
            pdf_file = pdf_path,
            page = page_number,
            dpi = BUILDER_RENDER_DPI
          )
        )
        
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
  
  observeEvent(input$builder_pdf, {
    req(input$builder_pdf)
    
    tryCatch(
      {
        pdf_path <- input$builder_pdf$datapath
        information <- pdftools::pdf_info(pdf_path)
        pages <- safe_integer(information$pages, 0L)
        
        if (pages < 1) {
          stop("The uploaded PDF contains no readable pages.")
        }
        
        builder_source_path(pdf_path)
        builder_source_name(input$builder_pdf$name)
        builder_page_count(pages)
        builder_created_at(
          format(Sys.time(), tz = "UTC", usetz = TRUE)
        )
        builder_fields(empty_fields())
        sample_preview(empty_parse_result())
        current_page_number(1L)
        
        updateSelectInput(
          session,
          "builder_page",
          choices = seq_len(pages),
          selected = 1L
        )
        
        render_current_builder_page(1L)
        
        showNotification(
          paste(
            "Loaded",
            input$builder_pdf$name,
            "with",
            pages,
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
  
  observeEvent(input$builder_page, {
    page_number <- suppressWarnings(as.integer(input$builder_page))
    
    if (
      is.na(page_number) ||
      page_number < 1 ||
      page_number > builder_page_count()
    ) {
      return()
    }
    
    current_page_number(page_number)
    render_current_builder_page(page_number)
  })
  
  observeEvent(input$previous_page, {
    req(builder_page_count() > 0)
    
    updateSelectInput(
      session,
      "builder_page",
      selected = max(1L, current_page_number() - 1L)
    )
  })
  
  observeEvent(input$next_page, {
    req(builder_page_count() > 0)
    
    updateSelectInput(
      session,
      "builder_page",
      selected = min(
        builder_page_count(),
        current_page_number() + 1L
      )
    )
  })
  
  output$page_plot <- renderPlot({
    draw_message <- function(
    main_text,
    secondary_text = NULL,
    color = "#00274C"
    ) {
      graphics::par(mar = c(0, 0, 0, 0))
      graphics::plot.new()
      
      graphics::text(
        0.5,
        0.55,
        labels = main_text,
        cex = 1.25,
        font = 2,
        col = color
      )
      
      if (!is.null(secondary_text)) {
        graphics::text(
          0.5,
          0.47,
          labels = secondary_text,
          cex = 1,
          col = "#5A6673"
        )
      }
    }
    
    if (
      is.null(input$builder_pdf) ||
      is.null(builder_source_path())
    ) {
      draw_message(
        "Upload a PDF to begin constructing a template",
        "The selected PDF page will appear here."
      )
      return(invisible(NULL))
    }
    
    rendered <- current_page_render()
    
    if (is.null(rendered) || is.null(rendered$image)) {
      draw_message("Rendering PDF page...")
      return(invisible(NULL))
    }
    
    image <- rendered$image
    dimensions <- dim(image)
    
    if (is.null(dimensions) || length(dimensions) < 2) {
      draw_message(
        "The current PDF page could not be displayed.",
        color = "#B42318"
      )
      return(invisible(NULL))
    }
    
    height <- as.numeric(dimensions[[1]])
    width <- as.numeric(dimensions[[2]])
    
    if (
      !is.finite(width) ||
      !is.finite(height) ||
      width <= 0 ||
      height <= 0
    ) {
      draw_message(
        "The rendered PDF page has invalid dimensions.",
        color = "#B42318"
      )
      return(invisible(NULL))
    }
    
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
    
    fields <- builder_fields()
    
    page_fields <- fields[
      !is.na(fields$page) &
        fields$page == current_page_number(),
      ,
      drop = FALSE
    ]
    
    if (nrow(page_fields) > 0) {
      for (i in seq_len(nrow(page_fields))) {
        coordinates <- c(
          page_fields$xmin_norm[[i]] * width,
          page_fields$ymin_norm[[i]] * height,
          page_fields$xmax_norm[[i]] * width,
          page_fields$ymax_norm[[i]] * height
        )
        
        if (!all(is.finite(coordinates))) next
        
        graphics::rect(
          coordinates[[1]],
          coordinates[[2]],
          coordinates[[3]],
          coordinates[[4]],
          border = "#C94242",
          lwd = 2
        )
        
        graphics::text(
          x = mean(coordinates[c(1, 3)]),
          y = mean(coordinates[c(2, 4)]),
          labels = page_fields$name[[i]],
          col = "#C94242",
          cex = 0.85,
          font = 2
        )
      }
    }
    
    brush <- input$page_brush
    
    if (!is.null(brush)) {
      brush_coordinates <- c(
        brush$xmin,
        brush$ymin,
        brush$xmax,
        brush$ymax
      )
      
      if (all(is.finite(brush_coordinates))) {
        graphics::rect(
          brush$xmin,
          brush$ymin,
          brush$xmax,
          brush$ymax,
          border = "#00274C",
          lwd = 2,
          lty = 2
        )
      }
    }
  })
  
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
      showNotification("Enter a field name.", type = "warning")
      return()
    }
    
    current_fields <- builder_fields()
    
    if (field_name %in% current_fields$name) {
      showNotification(
        "Field names must be unique.",
        type = "warning"
      )
      return()
    }
    
    width <- rendered$width
    height <- rendered$height
    xmin <- max(0, min(brush$xmin, brush$xmax))
    xmax <- min(width, max(brush$xmin, brush$xmax))
    ymin <- max(0, min(brush$ymin, brush$ymax))
    ymax <- min(height, max(brush$ymin, brush$ymax))
    
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
      extraction_mode = input$field_extraction_mode,
      content_type = input$field_content_type,
      preprocessing_profile =
        input$field_preprocessing_profile,
      ocr_language = trimws(
        input$field_ocr_language %||% ""
      ),
      minimum_confidence =
        input$field_minimum_confidence,
      stringsAsFactors = FALSE
    )
    
    builder_fields(rbind(current_fields, new_field))
    sample_preview(empty_parse_result())
    updateTextInput(session, "field_name", value = "")
    session$resetBrush("page_brush")
  })
  
  observeEvent(input$load_selected_field, {
    selected <- input$fields_table_rows_selected
    fields <- builder_fields()
    
    if (
      is.null(selected) ||
      length(selected) != 1 ||
      selected < 1 ||
      selected > nrow(fields)
    ) {
      showNotification(
        "Select one field row first.",
        type = "warning"
      )
      return()
    }
    
    field <- fields[selected, , drop = FALSE]
    
    updateTextInput(
      session,
      "field_name",
      value = field$name[[1]]
    )
    
    updateSelectInput(
      session,
      "field_extraction_mode",
      selected = field$extraction_mode[[1]]
    )
    
    updateSelectInput(
      session,
      "field_content_type",
      selected = field$content_type[[1]]
    )
    
    updateSelectInput(
      session,
      "field_preprocessing_profile",
      selected = field$preprocessing_profile[[1]]
    )
    
    updateTextInput(
      session,
      "field_ocr_language",
      value = field$ocr_language[[1]]
    )
    
    updateSliderInput(
      session,
      "field_minimum_confidence",
      value = field$minimum_confidence[[1]]
    )
    
    updateSelectInput(
      session,
      "builder_page",
      selected = field$page[[1]]
    )
  })
  
  observeEvent(input$update_selected_field, {
    selected <- input$fields_table_rows_selected
    fields <- builder_fields()
    
    if (
      is.null(selected) ||
      length(selected) != 1 ||
      selected < 1 ||
      selected > nrow(fields)
    ) {
      showNotification(
        "Select one field row first.",
        type = "warning"
      )
      return()
    }
    
    new_name <- trimws(input$field_name %||% "")
    
    if (!nzchar(new_name)) {
      showNotification("Enter a field name.", type = "warning")
      return()
    }
    
    duplicate <- fields$name == new_name
    duplicate[[selected]] <- FALSE
    
    if (any(duplicate)) {
      showNotification(
        "Field names must be unique.",
        type = "warning"
      )
      return()
    }
    
    fields$name[[selected]] <- new_name
    fields$extraction_mode[[selected]] <-
      input$field_extraction_mode
    fields$content_type[[selected]] <-
      input$field_content_type
    fields$preprocessing_profile[[selected]] <-
      input$field_preprocessing_profile
    fields$ocr_language[[selected]] <-
      trimws(input$field_ocr_language %||% "")
    fields$minimum_confidence[[selected]] <-
      input$field_minimum_confidence
    
    builder_fields(fields)
    sample_preview(empty_parse_result())
    
    showNotification(
      paste("Updated field:", new_name),
      type = "message"
    )
  })
  
  observeEvent(input$delete_selected_field, {
    selected <- input$fields_table_rows_selected
    fields <- builder_fields()
    
    if (
      is.null(selected) ||
      length(selected) != 1 ||
      selected < 1 ||
      selected > nrow(fields)
    ) {
      showNotification(
        "Select one field row first.",
        type = "warning"
      )
      return()
    }
    
    deleted_name <- fields$name[[selected]]
    fields <- fields[-selected, , drop = FALSE]
    builder_fields(fields)
    sample_preview(empty_parse_result())
    session$resetBrush("page_brush")
    
    showNotification(
      paste("Deleted field:", deleted_name),
      type = "message"
    )
  })
  
  observeEvent(input$clear_current_page, {
    fields <- builder_fields()
    if (nrow(fields) == 0) return()
    
    current_page <- current_page_number()
    removed <- sum(fields$page == current_page, na.rm = TRUE)
    
    fields <- fields[
      is.na(fields$page) |
        fields$page != current_page,
      ,
      drop = FALSE
    ]
    
    builder_fields(fields)
    sample_preview(empty_parse_result())
    session$resetBrush("page_brush")
    
    showNotification(
      paste("Removed", removed, "field(s)."),
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
            "Upload a PDF to begin constructing a template."
          )
        )
      )
    }
    
    fields <- builder_fields()
    page <- current_page_number()
    problems <- validate_template(current_builder_template())
    
    status <- if (length(problems) == 0) {
      span(
        class = "template-status-valid",
        "Ready to export"
      )
    } else {
      span(
        class = "template-status-invalid",
        title = paste(problems, collapse = "\n"),
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
        page
      ),
      
      span(
        class = "template-status-item",
        tags$b("Saved fields: "),
        nrow(fields)
      ),
      
      span(
        class = "template-status-item",
        tags$b("On this page: "),
        sum(fields$page == page, na.rm = TRUE)
      ),
      
      span(
        class = "template-status-item",
        tags$b("Status: "),
        status
      )
    )
  })
  
  output$fields_table <- DT::renderDT({
    fields <- builder_fields()
    
    if (nrow(fields) == 0) {
      return(
        DT::datatable(
          data.frame(
            Message = "No fields have been defined."
          ),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t", ordering = FALSE)
        )
      )
    }
    
    display <- fields[, c(
      "name",
      "page",
      "extraction_mode",
      "content_type",
      "preprocessing_profile",
      "minimum_confidence",
      "xmin_norm",
      "ymin_norm",
      "xmax_norm",
      "ymax_norm"
    )]
    
    names(display) <- c(
      "Field name",
      "Page",
      "Extraction",
      "Content type",
      "Preprocessing",
      "Min. confidence",
      "X minimum",
      "Y minimum",
      "X maximum",
      "Y maximum"
    )
    
    table <- DT::datatable(
      display,
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
    DT::formatRound(
      table,
      columns = c(
        "Min. confidence",
        "X minimum",
        "Y minimum",
        "X maximum",
        "Y maximum"
      ),
      digits = 3
    )
  })
  
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
          "Template cannot be previewed:",
          paste(problems, collapse = " ")
        ),
        type = "error",
        duration = NULL
      )
      return()
    }
    
    tryCatch(
      {
        withProgress(
          message = "Parsing representative PDF",
          value = 0.2,
          {
            result <- parse_pdf_fields(pdf_path, template)
            setProgress(value = 1)
            sample_preview(result)
          }
        )
        
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
      return(
        DT::datatable(
          data.frame(
            Message = paste(
              "No preview is available.",
              "Click 'Preview sample parsing'."
            )
          ),
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE)
        )
      )
    }
    
    display <- preview[, c(
      "page",
      "field_name",
      "raw_text",
      "extraction_method",
      "recognition_confidence",
      "status",
      "message"
    )]
    
    names(display) <- c(
      "Page",
      "Field",
      "Extracted text",
      "Method",
      "Confidence",
      "Status",
      "Message"
    )
    
    table <- DT::datatable(
      display,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
    DT::formatRound(
      table,
      columns = "Confidence",
      digits = 1
    )
  })
  
  output$download_template <- downloadHandler(
    filename = function() {
      paste0(
        safe_filename_stem(input$template_name),
        ".json"
      )
    },
    
    content = function(file) {
      write_template_json(
        current_builder_template(),
        file
      )
    },
    
    contentType = "application/json"
  )
  
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
        imported_template_error(conditionMessage(e))
        
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
    if (
      identical(
        input$parser_template_source,
        "imported"
      )
    ) {
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
    
    highest_page <- if (nrow(fields) > 0) {
      max(fields$page, na.rm = TRUE)
    } else {
      0
    }
    
    paste0(
      "Source: ", source_label, "\n",
      "Template name: ", template$template$name, "\n",
      "Schema version: ", template$schema_version, "\n",
      "Fields: ", nrow(fields), "\n",
      "Highest required page: ", highest_page, "\n",
      "Validation: ",
      if (length(problems) == 0) {
        "Valid"
      } else {
        paste(problems, collapse = " ")
      }
    )
  })
  
  volumes <- shinyFiles::getVolumes()()
  
  shinyFiles::shinyDirChoose(
    input = input,
    id = "batch_pdf_directory",
    session = session,
    roots = volumes
  )
  
  observeEvent(input$batch_pdf_directory, {
    parsed <- shinyFiles::parseDirPath(
      roots = volumes,
      selection = input$batch_pdf_directory
    )
    
    if (is.null(parsed) || length(parsed) == 0) {
      selected_directory(NULL)
      return()
    }
    
    path <- if (is.data.frame(parsed)) {
      if (nrow(parsed) == 0) {
        NULL
      } else {
        as.character(parsed$datapath[[1]])
      }
    } else {
      as.character(parsed[[1]])
    }
    
    selected_directory(path)
  })
  
  output$selected_directory_text <- renderText({
    directory <- selected_directory()
    
    if (is.null(directory)) {
      "No directory selected."
    } else {
      paste("Selected directory:", directory)
    }
  })
  
  build_batch_file_manifest <- reactive({
    if (identical(input$batch_input_mode, "upload")) {
      uploaded <- input$batch_pdf_files
      
      if (is.null(uploaded) || nrow(uploaded) == 0) {
        return(empty_manifest())
      }
      
      return(data.frame(
        path = uploaded$datapath,
        source_file = uploaded$name,
        stringsAsFactors = FALSE
      ))
    }
    
    directory <- selected_directory()
    
    if (is.null(directory) || !dir.exists(directory)) {
      return(empty_manifest())
    }
    
    paths <- list.files(
      path = directory,
      pattern = "\\.pdf$",
      full.names = TRUE,
      recursive = isTRUE(
        input$recursive_folder_search
      ),
      ignore.case = TRUE
    )
    
    data.frame(
      path = paths,
      source_file = basename(paths),
      stringsAsFactors = FALSE
    )
  })
  
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
          "Selected template is invalid:",
          paste(problems, collapse = " ")
        ),
        type = "error",
        duration = NULL
      )
      return()
    }
    
    manifest <- build_batch_file_manifest()
    
    if (nrow(manifest) == 0) {
      showNotification(
        "No PDF files were selected.",
        type = "warning"
      )
      return()
    }
    
    batch_result(NULL)
    batch_quality_control(NULL)
    
    batch_status_message(
      paste(
        "Parsing",
        nrow(manifest),
        "PDF document(s)..."
      )
    )
    
    tryCatch(
      {
        bundle <- withProgress(
          message = "Parsing PDF documents",
          value = 0,
          {
            parse_pdf_batch_with_quality(
              file_manifest = manifest,
              template = template,
              progress_callback = function(
    index,
    total,
    source_file
              ) {
                setProgress(
                  value = index / total,
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
        
        result <- bundle$data
        quality <- bundle$quality
        batch_result(result)
        batch_quality_control(quality)
        
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
        
        field_flags <- sum(
          quality$status != "ok",
          na.rm = TRUE
        )
        
        batch_status_message(
          paste0(
            "Parsing complete.\n",
            "Documents parsed: ", nrow(result), "\n",
            "Successful: ", ok_count, "\n",
            "Warnings: ", warning_count, "\n",
            "Errors: ", error_count, "\n",
            "Fields flagged for review: ", field_flags
          )
        )
        
        showNotification(
          paste(
            "Parsing completed for",
            nrow(result),
            "document(s)."
          ),
          type = if (
            warning_count > 0 ||
            error_count > 0
          ) {
            "warning"
          } else {
            "message"
          }
        )
      },
    error = function(e) {
      batch_result(NULL)
      batch_quality_control(NULL)
      
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
  
  output$batch_status <- renderText({
    batch_status_message()
  })
  
  output$batch_preview_table <- DT::renderDT({
    result <- batch_result()
    
    if (is.null(result) || nrow(result) == 0) {
      return(
        DT::datatable(
          data.frame(
            Message = "No batch result is available."
          ),
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE)
        )
      )
    }
    
    DT::datatable(
      utils::head(result, 5),
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
      return(
        DT::datatable(
          data.frame(
            Message = "No parsing result is available."
          ),
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE)
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
        Message = "No warnings or errors were reported."
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
  
  output$quality_preview_table <- DT::renderDT({
    quality <- batch_quality_control()
    
    if (is.null(quality) || nrow(quality) == 0) {
      return(
        DT::datatable(
          data.frame(
            Message = "No field-level quality result is available."
          ),
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE)
        )
      )
    }
    
    display <- quality[
      quality$status != "ok" |
        !is.na(quality$recognition_confidence),
      ,
      drop = FALSE
    ]
    
    if (nrow(display) == 0) display <- quality
    
    table <- DT::datatable(
      display,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
    DT::formatRound(
      table,
      columns = "recognition_confidence",
      digits = 1
    )
  })
  
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
          "No parsed result is available.",
          call. = FALSE
        )
      }
      
      utils::write.csv(
        result,
        file,
        row.names = FALSE,
        na = ""
      )
    },
    
    contentType = "text/csv"
  )
  
  output$download_quality_csv <- downloadHandler(
    filename = function() {
      paste0(
        "pdf_recognition_quality_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      quality <- batch_quality_control()
      
      if (is.null(quality) || nrow(quality) == 0) {
        stop(
          "No quality-control result is available.",
          call. = FALSE
        )
      }
      
      utils::write.csv(
        quality,
        file,
        row.names = FALSE,
        na = ""
      )
    },
    
    contentType = "text/csv"
  )
}

shinyApp(
  ui = ui,
  server = server,
  options = list(launch.browser = TRUE)
)

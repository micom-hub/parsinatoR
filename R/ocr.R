# R/ocr.R
#
# Local OCR and experimental handwriting-recognition helpers.
#
# Dependencies:
#   - pdftools
#   - magick
#   - tesseract
#
# This file intentionally uses explicit package namespaces and does not
# attach any packages.

# =============================================================================
# Defaults and environment checks
# =============================================================================

ocr_default_settings <- function() {
  list(
    enabled = TRUE,
    language = "eng",
    dpi = 300L,
    default_extraction_mode = "auto",
    preprocessing_profile = "standard",
    printed_psm = 7L,
    handwriting_psm = 7L,
    minimum_confidence = 50,
    auto_fallback_min_characters = 1L,
    x_pad_pct = 0.005,
    y_pad_pct = 0.005,
    upscale_pct = 200L
  )
}

ocr_safe_scalar <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) {
    return(default)
  }
  
  x[[1]]
}

ocr_safe_character <- function(x, default = "") {
  value <- ocr_safe_scalar(x, default)
  
  if (is.null(value) || is.na(value)) {
    return(default)
  }
  
  as.character(value)
}

ocr_safe_numeric <- function(x, default = 0) {
  value <- suppressWarnings(
    as.numeric(ocr_safe_scalar(x, default))
  )
  
  if (
    length(value) != 1 ||
    is.na(value) ||
    !is.finite(value)
  ) {
    return(default)
  }
  
  value
}

ocr_safe_integer <- function(x, default = 0L) {
  value <- suppressWarnings(
    as.integer(ocr_safe_scalar(x, default))
  )
  
  if (length(value) != 1 || is.na(value)) {
    return(default)
  }
  
  value
}

ocr_safe_logical <- function(x, default = FALSE) {
  value <- ocr_safe_scalar(x, default)
  
  if (is.logical(value)) {
    return(isTRUE(value))
  }
  
  if (is.character(value)) {
    return(tolower(value) %in% c("true", "yes", "1"))
  }
  
  isTRUE(as.logical(value))
}

normalize_ocr_settings <- function(settings = NULL) {
  defaults <- ocr_default_settings()
  settings <- settings %||_ocr% list()
  
  default_mode <- ocr_safe_character(
    settings$default_extraction_mode,
    defaults$default_extraction_mode
  )
  
  valid_modes <- c(
    "native",
    "printed_ocr",
    "handwriting",
    "auto",
    "hybrid"
  )
  
  if (!default_mode %in% valid_modes) {
    default_mode <- defaults$default_extraction_mode
  }
  
  profile <- ocr_safe_character(
    settings$preprocessing_profile,
    defaults$preprocessing_profile
  )
  
  valid_profiles <- c(
    "none",
    "standard",
    "high_contrast",
    "faint_text",
    "handwriting"
  )
  
  if (!profile %in% valid_profiles) {
    profile <- defaults$preprocessing_profile
  }
  
  list(
    enabled = ocr_safe_logical(
      settings$enabled,
      defaults$enabled
    ),
    language = ocr_safe_character(
      settings$language,
      defaults$language
    ),
    dpi = max(
      72L,
      ocr_safe_integer(settings$dpi, defaults$dpi)
    ),
    default_extraction_mode = default_mode,
    preprocessing_profile = profile,
    printed_psm = ocr_safe_integer(
      settings$printed_psm,
      defaults$printed_psm
    ),
    handwriting_psm = ocr_safe_integer(
      settings$handwriting_psm,
      defaults$handwriting_psm
    ),
    minimum_confidence = min(
      100,
      max(
        0,
        ocr_safe_numeric(
          settings$minimum_confidence,
          defaults$minimum_confidence
        )
      )
    ),
    auto_fallback_min_characters = max(
      0L,
      ocr_safe_integer(
        settings$auto_fallback_min_characters,
        defaults$auto_fallback_min_characters
      )
    ),
    x_pad_pct = max(
      0,
      ocr_safe_numeric(
        settings$x_pad_pct,
        defaults$x_pad_pct
      )
    ),
    y_pad_pct = max(
      0,
      ocr_safe_numeric(
        settings$y_pad_pct,
        defaults$y_pad_pct
      )
    ),
    upscale_pct = max(
      100L,
      ocr_safe_integer(
        settings$upscale_pct,
        defaults$upscale_pct
      )
    )
  )
}

`%||_ocr%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

check_ocr_environment <- function(language = "eng") {
  problems <- character()
  available_languages <- character()
  
  if (!requireNamespace("magick", quietly = TRUE)) {
    problems <- c(
      problems,
      "The R package 'magick' is not installed."
    )
  }
  
  if (!requireNamespace("tesseract", quietly = TRUE)) {
    problems <- c(
      problems,
      "The R package 'tesseract' is not installed."
    )
  }
  
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    problems <- c(
      problems,
      "The R package 'pdftools' is not installed."
    )
  }
  
  if (requireNamespace("tesseract", quietly = TRUE)) {
    available_languages <- tryCatch(
      tesseract::tesseract_info()$available,
      error = function(e) character()
    )
    
    if (
      length(available_languages) > 0 &&
      !language %in% available_languages
    ) {
      problems <- c(
        problems,
        paste0(
          "Tesseract language '",
          language,
          "' is not installed. Available languages: ",
          paste(available_languages, collapse = ", "),
          "."
        )
      )
    }
    
    engine_test <- tryCatch(
      {
        tesseract::tesseract(language = language)
        TRUE
      },
      error = function(e) {
        problems <<- c(
          problems,
          paste(
            "Tesseract could not be initialized:",
            conditionMessage(e)
          )
        )
        
        FALSE
      }
    )
  } else {
    engine_test <- FALSE
  }
  
  list(
    ready = length(problems) == 0 && isTRUE(engine_test),
    problems = unique(problems),
    available_languages = available_languages
  )
}

# =============================================================================
# Text normalization and usability
# =============================================================================

normalize_recognized_text <- function(text) {
  if (
    is.null(text) ||
    length(text) == 0 ||
    is.na(text[[1]])
  ) {
    return("")
  }
  
  text <- as.character(text[[1]])
  text <- gsub("[\r\n\t]+", " ", text)
  text <- gsub("[[:space:]]+", " ", text)
  trimws(text)
}

recognition_alphanumeric_count <- function(text) {
  text <- normalize_recognized_text(text)
  
  if (!nzchar(text)) {
    return(0L)
  }
  
  cleaned <- gsub("[^[:alnum:]]", "", text)
  nchar(cleaned, type = "chars")
}

is_usable_recognized_text <- function(
    text,
    minimum_characters = 1L
) {
  recognition_alphanumeric_count(text) >= minimum_characters
}

# =============================================================================
# Page and engine caches
# =============================================================================

create_ocr_page_cache <- function() {
  new.env(parent = emptyenv())
}

create_ocr_engine_cache <- function() {
  new.env(parent = emptyenv())
}

clear_ocr_cache <- function(cache) {
  if (is.environment(cache)) {
    rm(
      list = ls(cache, all.names = TRUE),
      envir = cache
    )
  }
  
  invisible(NULL)
}

ocr_page_cache_key <- function(page, dpi) {
  paste0("page_", page, "_dpi_", dpi)
}

render_pdf_page_for_recognition <- function(
    pdf_file,
    page,
    dpi = 300L
) {
  if (!file.exists(pdf_file)) {
    stop(
      paste("PDF file does not exist:", pdf_file),
      call. = FALSE
    )
  }
  
  output_file <- tempfile(
    pattern = paste0("recognition_page_", page, "_"),
    fileext = ".png"
  )
  
  on.exit(
    unlink(output_file, force = TRUE),
    add = TRUE
  )
  
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
      paste(
        "Page",
        page,
        "could not be rendered for character recognition."
      ),
      call. = FALSE
    )
  }
  
  image <- magick::image_read(rendered[[1]])
  image_info <- magick::image_info(image)
  
  if (
    nrow(image_info) == 0 ||
    image_info$width[[1]] <= 0 ||
    image_info$height[[1]] <= 0
  ) {
    stop(
      paste(
        "Page",
        page,
        "was rendered with invalid image dimensions."
      ),
      call. = FALSE
    )
  }
  
  image
}

get_cached_recognition_page <- function(
    pdf_file,
    page,
    dpi,
    page_cache
) {
  key <- ocr_page_cache_key(page, dpi)
  
  if (exists(key, envir = page_cache, inherits = FALSE)) {
    return(get(key, envir = page_cache, inherits = FALSE))
  }
  
  image <- render_pdf_page_for_recognition(
    pdf_file = pdf_file,
    page = page,
    dpi = dpi
  )
  
  assign(
    key,
    image,
    envir = page_cache
  )
  
  image
}

# =============================================================================
# Coordinate conversion and image cropping
# =============================================================================

normalized_bounds_to_pixels <- function(
    field,
    image_width,
    image_height,
    x_pad_pct = 0,
    y_pad_pct = 0
) {
  xmin <- as.numeric(field$xmin_norm[[1]])
  ymin <- as.numeric(field$ymin_norm[[1]])
  xmax <- as.numeric(field$xmax_norm[[1]])
  ymax <- as.numeric(field$ymax_norm[[1]])
  
  coordinates <- c(xmin, ymin, xmax, ymax)
  
  if (
    length(coordinates) != 4 ||
    any(!is.finite(coordinates))
  ) {
    stop(
      "The field contains invalid normalized coordinates.",
      call. = FALSE
    )
  }
  
  left <- floor(
    xmin * image_width -
      x_pad_pct * image_width
  )
  
  top <- floor(
    ymin * image_height -
      y_pad_pct * image_height
  )
  
  right <- ceiling(
    xmax * image_width +
      x_pad_pct * image_width
  )
  
  bottom <- ceiling(
    ymax * image_height +
      y_pad_pct * image_height
  )
  
  left <- max(0, min(left, image_width - 1))
  top <- max(0, min(top, image_height - 1))
  right <- max(left + 1, min(right, image_width))
  bottom <- max(top + 1, min(bottom, image_height))
  
  crop_width <- right - left
  crop_height <- bottom - top
  
  if (crop_width <= 0 || crop_height <= 0) {
    stop(
      "The calculated recognition crop has no usable area.",
      call. = FALSE
    )
  }
  
  list(
    left = as.integer(left),
    top = as.integer(top),
    right = as.integer(right),
    bottom = as.integer(bottom),
    width = as.integer(crop_width),
    height = as.integer(crop_height)
  )
}

crop_recognition_field <- function(
    page_image,
    field,
    x_pad_pct = 0,
    y_pad_pct = 0
) {
  image_info <- magick::image_info(page_image)
  
  bounds <- normalized_bounds_to_pixels(
    field = field,
    image_width = image_info$width[[1]],
    image_height = image_info$height[[1]],
    x_pad_pct = x_pad_pct,
    y_pad_pct = y_pad_pct
  )
  
  geometry <- magick::geometry_area(
    width = bounds$width,
    height = bounds$height,
    x_off = bounds$left,
    y_off = bounds$top
  )
  
  crop <- magick::image_crop(
    page_image,
    geometry = geometry,
    repage = TRUE
  )
  
  list(
    image = crop,
    bounds = bounds
  )
}

# =============================================================================
# Image preprocessing
# =============================================================================

preprocess_recognition_image <- function(
    image,
    profile = "standard",
    upscale_pct = 200L
) {
  profile <- ocr_safe_character(profile, "standard")
  upscale_pct <- max(100L, as.integer(upscale_pct))
  
  processed <- image
  
  if (!identical(profile, "none")) {
    processed <- magick::image_convert(
      processed,
      colorspace = "gray"
    )
  }
  
  if (identical(profile, "standard")) {
    processed <- magick::image_normalize(processed)
    processed <- magick::image_contrast(
      processed,
      sharpen = 1
    )
  }
  
  if (identical(profile, "high_contrast")) {
    processed <- magick::image_normalize(processed)
    
    processed <- magick::image_contrast(
      processed,
      sharpen = 1
    )
    
    processed <- magick::image_contrast(
      processed,
      sharpen = 1
    )
  }
  
  if (identical(profile, "faint_text")) {
    processed <- magick::image_normalize(processed)
    processed <- magick::image_enhance(processed)
    processed <- magick::image_contrast(
      processed,
      sharpen = 1
    )
  }
  
  if (identical(profile, "handwriting")) {
    processed <- magick::image_normalize(processed)
    processed <- magick::image_enhance(processed)
  }
  
  if (upscale_pct > 100L) {
    processed <- magick::image_resize(
      processed,
      paste0(upscale_pct, "%")
    )
  }
  
  processed
}

# =============================================================================
# Tesseract engine configuration
# =============================================================================

content_type_to_psm <- function(
    content_type,
    recognition_type = "printed",
    settings = ocr_default_settings()
) {
  content_type <- ocr_safe_character(
    content_type,
    "single_line"
  )
  
  if (identical(content_type, "single_word")) {
    return(8L)
  }
  
  if (identical(content_type, "single_character")) {
    return(10L)
  }
  
  if (identical(content_type, "multiline")) {
    return(6L)
  }
  
  if (identical(content_type, "sparse_text")) {
    return(11L)
  }
  
  if (identical(recognition_type, "handwriting")) {
    return(as.integer(settings$handwriting_psm))
  }
  
  as.integer(settings$printed_psm)
}

content_type_to_whitelist <- function(content_type) {
  content_type <- ocr_safe_character(
    content_type,
    "single_line"
  )
  
  if (identical(content_type, "numeric")) {
    return("0123456789.,-+")
  }
  
  if (identical(content_type, "date")) {
    return("0123456789/-.")
  }
  
  if (identical(content_type, "identifier")) {
    return(
      paste0(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "abcdefghijklmnopqrstuvwxyz",
        "0123456789-_/"
      )
    )
  }
  
  if (identical(content_type, "single_character")) {
    return(
      paste0(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "abcdefghijklmnopqrstuvwxyz",
        "0123456789"
      )
    )
  }
  
  ""
}

get_cached_tesseract_engine <- function(
    language,
    psm,
    whitelist = "",
    engine_cache
) {
  key <- paste(
    language,
    psm,
    whitelist,
    sep = "__"
  )
  
  key <- gsub("[^A-Za-z0-9_]+", "_", key)
  
  if (exists(key, envir = engine_cache, inherits = FALSE)) {
    return(get(key, envir = engine_cache, inherits = FALSE))
  }
  
  options <- list(
    tessedit_pageseg_mode = as.character(psm),
    preserve_interword_spaces = "1"
  )
  
  if (nzchar(whitelist)) {
    options$tessedit_char_whitelist <- whitelist
  }
  
  engine <- tesseract::tesseract(
    language = language,
    options = options
  )
  
  assign(
    key,
    engine,
    envir = engine_cache
  )
  
  engine
}

# =============================================================================
# Recognition execution and confidence
# =============================================================================

calculate_ocr_confidence <- function(ocr_data) {
  if (
    is.null(ocr_data) ||
    !is.data.frame(ocr_data) ||
    nrow(ocr_data) == 0 ||
    !"confidence" %in% names(ocr_data)
  ) {
    return(NA_real_)
  }
  
  confidence <- suppressWarnings(
    as.numeric(ocr_data$confidence)
  )
  
  if ("word" %in% names(ocr_data)) {
    words <- as.character(ocr_data$word)
    
    keep <- !is.na(confidence) &
      confidence >= 0 &
      !is.na(words) &
      nzchar(trimws(words))
    
    weights <- pmax(nchar(words[keep]), 1)
  } else {
    keep <- !is.na(confidence) & confidence >= 0
    weights <- rep(1, sum(keep))
  }
  
  if (!any(keep)) {
    return(NA_real_)
  }
  
  stats::weighted.mean(
    confidence[keep],
    w = weights
  )
}

run_tesseract_recognition <- function(
    image,
    engine
) {
  ocr_data <- tryCatch(
    tesseract::ocr_data(
      image,
      engine = engine
    ),
    error = function(e) NULL
  )
  
  text <- ""
  
  if (
    is.data.frame(ocr_data) &&
    nrow(ocr_data) > 0 &&
    "word" %in% names(ocr_data)
  ) {
    words <- as.character(ocr_data$word)
    words <- words[
      !is.na(words) &
        nzchar(trimws(words))
    ]
    
    text <- paste(words, collapse = " ")
  }
  
  if (!nzchar(normalize_recognized_text(text))) {
    text <- tryCatch(
      tesseract::ocr(
        image,
        engine = engine
      ),
      error = function(e) ""
    )
  }
  
  list(
    text = normalize_recognized_text(text),
    confidence = calculate_ocr_confidence(ocr_data),
    data = ocr_data
  )
}

# =============================================================================
# Printed-character recognition
# =============================================================================

recognize_printed_field <- function(
    crop,
    language,
    content_type,
    preprocessing_profile,
    settings,
    engine_cache
) {
  psm <- content_type_to_psm(
    content_type = content_type,
    recognition_type = "printed",
    settings = settings
  )
  
  whitelist <- content_type_to_whitelist(content_type)
  
  engine <- get_cached_tesseract_engine(
    language = language,
    psm = psm,
    whitelist = whitelist,
    engine_cache = engine_cache
  )
  
  processed <- preprocess_recognition_image(
    image = crop,
    profile = preprocessing_profile,
    upscale_pct = settings$upscale_pct
  )
  
  result <- run_tesseract_recognition(
    image = processed,
    engine = engine
  )
  
  result$profile <- preprocessing_profile
  result$psm <- psm
  result$recognition_type <- "printed"
  result$processed_image <- processed
  result
}

# =============================================================================
# Experimental handwriting recognition
# =============================================================================

recognize_handwritten_field <- function(
    crop,
    language,
    content_type,
    settings,
    engine_cache
) {
  # Tesseract is not a dedicated handwriting model. This first-pass
  # implementation runs multiple conservative preprocessing variants and
  # selects the strongest usable result.
  
  profiles <- unique(c(
    "handwriting",
    "standard",
    "faint_text",
    "none"
  ))
  
  psm_candidates <- unique(c(
    content_type_to_psm(
      content_type = content_type,
      recognition_type = "handwriting",
      settings = settings
    ),
    if (content_type %in% c(
      "single_word",
      "numeric",
      "date",
      "identifier"
    )) {
      8L
    } else {
      7L
    },
    if (identical(content_type, "multiline")) {
      6L
    } else {
      7L
    }
  ))
  
  whitelist <- content_type_to_whitelist(content_type)
  candidates <- list()
  candidate_index <- 0L
  
  for (profile in profiles) {
    processed <- preprocess_recognition_image(
      image = crop,
      profile = profile,
      upscale_pct = settings$upscale_pct
    )
    
    for (psm in psm_candidates) {
      candidate_index <- candidate_index + 1L
      
      engine <- get_cached_tesseract_engine(
        language = language,
        psm = psm,
        whitelist = whitelist,
        engine_cache = engine_cache
      )
      
      result <- run_tesseract_recognition(
        image = processed,
        engine = engine
      )
      
      result$profile <- profile
      result$psm <- psm
      result$recognition_type <- "handwriting"
      result$processed_image <- processed
      
      candidates[[candidate_index]] <- result
    }
  }
  
  usable <- vapply(
    candidates,
    function(candidate) {
      is_usable_recognized_text(
        candidate$text,
        minimum_characters = 1L
      )
    },
    logical(1)
  )
  
  if (!any(usable)) {
    return(list(
      text = "",
      confidence = NA_real_,
      data = NULL,
      profile = "handwriting",
      psm = settings$handwriting_psm,
      recognition_type = "handwriting",
      processed_image = preprocess_recognition_image(
        crop,
        profile = "handwriting",
        upscale_pct = settings$upscale_pct
      ),
      candidates = candidates
    ))
  }
  
  usable_candidates <- candidates[usable]
  
  candidate_scores <- vapply(
    usable_candidates,
    function(candidate) {
      confidence_score <- candidate$confidence
      
      if (
        is.null(confidence_score) ||
        is.na(confidence_score)
      ) {
        confidence_score <- 0
      }
      
      length_score <- min(
        recognition_alphanumeric_count(candidate$text),
        20
      ) / 20
      
      confidence_score + length_score
    },
    numeric(1)
  )
  
  best_index <- which.max(candidate_scores)
  best_result <- usable_candidates[[best_index]]
  best_result$candidates <- candidates
  best_result
}

# =============================================================================
# Field-level settings and unified OCR extraction
# =============================================================================

resolve_field_recognition_settings <- function(
    field,
    ocr_settings
) {
  ocr_settings <- normalize_ocr_settings(ocr_settings)
  
  extraction_mode <- ocr_safe_character(
    field$extraction_mode,
    "inherit"
  )
  
  if (identical(extraction_mode, "inherit")) {
    extraction_mode <- ocr_settings$default_extraction_mode
  }
  
  valid_modes <- c(
    "native",
    "printed_ocr",
    "handwriting",
    "auto",
    "hybrid"
  )
  
  if (!extraction_mode %in% valid_modes) {
    extraction_mode <- ocr_settings$default_extraction_mode
  }
  
  content_type <- ocr_safe_character(
    field$content_type,
    "single_line"
  )
  
  valid_content_types <- c(
    "single_character",
    "single_word",
    "single_line",
    "multiline",
    "sparse_text",
    "numeric",
    "date",
    "identifier"
  )
  
  if (!content_type %in% valid_content_types) {
    content_type <- "single_line"
  }
  
  preprocessing_profile <- ocr_safe_character(
    field$preprocessing_profile,
    "inherit"
  )
  
  if (identical(preprocessing_profile, "inherit")) {
    preprocessing_profile <-
      ocr_settings$preprocessing_profile
  }
  
  language <- ocr_safe_character(
    field$ocr_language,
    ""
  )
  
  if (!nzchar(language)) {
    language <- ocr_settings$language
  }
  
  minimum_confidence <- ocr_safe_numeric(
    field$minimum_confidence,
    ocr_settings$minimum_confidence
  )
  
  list(
    extraction_mode = extraction_mode,
    content_type = content_type,
    preprocessing_profile = preprocessing_profile,
    language = language,
    minimum_confidence = minimum_confidence
  )
}

extract_field_with_recognition <- function(
    field,
    page_image,
    ocr_settings,
    engine_cache,
    recognition_type = c("printed", "handwriting")
) {
  recognition_type <- match.arg(recognition_type)
  ocr_settings <- normalize_ocr_settings(ocr_settings)
  
  field_settings <- resolve_field_recognition_settings(
    field = field,
    ocr_settings = ocr_settings
  )
  
  cropped <- crop_recognition_field(
    page_image = page_image,
    field = field,
    x_pad_pct = ocr_settings$x_pad_pct,
    y_pad_pct = ocr_settings$y_pad_pct
  )
  
  if (identical(recognition_type, "handwriting")) {
    recognized <- recognize_handwritten_field(
      crop = cropped$image,
      language = field_settings$language,
      content_type = field_settings$content_type,
      settings = ocr_settings,
      engine_cache = engine_cache
    )
  } else {
    recognized <- recognize_printed_field(
      crop = cropped$image,
      language = field_settings$language,
      content_type = field_settings$content_type,
      preprocessing_profile =
        field_settings$preprocessing_profile,
      settings = ocr_settings,
      engine_cache = engine_cache
    )
  }
  
  text <- normalize_recognized_text(recognized$text)
  confidence <- recognized$confidence
  status <- "ok"
  messages <- character()
  
  if (!nzchar(text)) {
    status <- "warning"
    messages <- c(
      messages,
      if (identical(recognition_type, "handwriting")) {
        "Handwriting recognition returned no text."
      } else {
        "OCR returned no text."
      }
    )
  }
  
  if (
    !is.na(confidence) &&
    confidence < field_settings$minimum_confidence
  ) {
    status <- "warning"
    
    messages <- c(
      messages,
      paste0(
        "Recognition confidence ",
        round(confidence, 1),
        " is below the configured threshold of ",
        round(field_settings$minimum_confidence, 1),
        "."
      )
    )
  }
  
  list(
    raw_text = text,
    confidence = confidence,
    status = status,
    message = paste(unique(messages), collapse = "; "),
    extraction_method = if (
      identical(recognition_type, "handwriting")
    ) {
      "handwriting_tesseract"
    } else {
      "printed_ocr"
    },
    original_crop = cropped$image,
    processed_crop = recognized$processed_image,
    crop_bounds = cropped$bounds,
    profile = recognized$profile,
    psm = recognized$psm
  )
}
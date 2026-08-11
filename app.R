# app.R - Simple PDF template box editor
# Requires: shiny, pdftools, png

# Package set-up
packages <- c("shiny", "pdftools", "png", "zip")

missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing)
}

invisible(lapply(packages, library, character.only = TRUE))

# Launch in browser so its bigger than the viewer pane
options(shiny.launch.browser = TRUE)

# helper to initialize an empty dataframe
empty_fields <- function() {
  data.frame(
    name = character(), # user provided name
    # Note that 0,0 is top left, X increases right, Y increases down
    xmin = numeric(), # left edge of box
    ymin = numeric(), # top edge
    xmax = numeric(), # right edge
    ymax = numeric(), # bottom edge
    width = numeric(), # = xmax - xmin
    height = numeric(), # = ymax - ymin
    # Same dimensions, normalized by as fraction of page width/height
    xmin_norm = numeric(),
    ymin_norm = numeric(),
    xmax_norm = numeric(),
    ymax_norm = numeric()
  )
}

# Helper to initialize an empty parsed sample
empty_parsed_sample <- function() {
  data.frame(
    name = character(),
    raw_text = character(),
    n_words = integer(),
    stringsAsFactors = FALSE
  )
}

# Parser preview function to extract fields
# An example of the full pdf parser to be built
extract_pdf_fields <- function(pdf_file, fields_df, settings, page = 1) {
  
  if (nrow(fields_df) == 0) {
    return(empty_parsed_sample())
  }
  
  # Run pdf_data on the original uploaded PDF
  words <- pdftools::pdf_data(pdf_file)[[page]]
  
  # Get actual PDF page size in pdf_data coordinate units
  page_sizes <- pdftools::pdf_pagesize(pdf_file)
  pdf_width <- page_sizes$width[page]
  pdf_height <- page_sizes$height[page]
  
  if (is.null(words) || nrow(words) == 0) {
    return(data.frame(
      name = fields_df$name,
      raw_text = "",
      n_words = 0L,
      stringsAsFactors = FALSE
    ))
  }
  
  words <- words[!is.na(words$text) & nzchar(words$text), , drop = FALSE]
  
  if (nrow(words) == 0) {
    return(data.frame(
      name = fields_df$name,
      raw_text = "",
      n_words = 0L,
      stringsAsFactors = FALSE
    ))
  }
  
  # Word bounding boxes from pdf_data
  words$word_xmin <- words$x
  words$word_ymin <- words$y
  words$word_xmax <- words$x + words$width
  words$word_ymax <- words$y + words$height
  words$word_xcenter <- words$x + words$width / 2
  words$word_ycenter <- words$y + words$height / 2
  
  results <- vector("list", nrow(fields_df))
  
  for (i in seq_len(nrow(fields_df))) {
    
    field <- fields_df[i, ]
    
    # Convert normalized template coordinates into pdf_data coordinate space.
    # Then expand using padding hyperparameters.
    field_xmin <- max(
      0,
      field$xmin_norm * pdf_width - settings$x_pad_pct * pdf_width
    )
    
    
    field_xmax <- min(
      pdf_width,
      field$xmax_norm * pdf_width + settings$x_pad_pct * pdf_width
    )

    
    field_ymin <- max(
      0,
      field$ymin_norm * pdf_height - settings$y_pad_pct * pdf_height
    )
    
    field_ymax <- min(
      pdf_height,
      field$ymax_norm * pdf_height + settings$y_pad_pct * pdf_height
    )
    
    if (settings$capture_mode == "center") {
      
      # Capture word if word center is inside field box
      keep <- words$word_xcenter >= field_xmin &
        words$word_xcenter <= field_xmax &
        words$word_ycenter >= field_ymin &
        words$word_ycenter <= field_ymax
      
    } else {
      
      # Capture word if enough of its bounding box overlaps field box
      overlap_width <- pmax(
        0,
        pmin(words$word_xmax, field_xmax) - pmax(words$word_xmin, field_xmin)
      )
      
      overlap_height <- pmax(
        0,
        pmin(words$word_ymax, field_ymax) - pmax(words$word_ymin, field_ymin)
      )
      
      overlap_area <- overlap_width * overlap_height
      word_area <- pmax(words$width * words$height, 1e-9)
      overlap_fraction <- overlap_area / word_area
      
      keep <- overlap_fraction >= settings$min_word_overlap_pct / 100
    }
    
    selected_words <- words[keep, , drop = FALSE]
    
    if (nrow(selected_words) > 0) {
      
      # Sort top-to-bottom, then left-to-right.
      # line_tolerance groups nearby y-values into the same visual line.
      line_key <- round(selected_words$y / settings$line_tolerance)
      
      selected_words <- selected_words[
        order(line_key, selected_words$x),
        ,
        drop = FALSE
      ]
      
      raw_text <- paste(selected_words$text, collapse = " ")
      n_words <- nrow(selected_words)
      
    } else {
      
      raw_text <- ""
      n_words <- 0L
    }
    
    results[[i]] <- data.frame(
      name = field$name,
      raw_text = raw_text,
      n_words = n_words,
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, results)
}


ui <- fluidPage(
  titlePanel("Interactive PDF Template Builder"),
  
  tabsetPanel(
    
  # First tab for template construction
  tabPanel(
    "Build Template",
  
  # Left sidebar for upload, field name, and input buttons
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "pdf_file",
        "Choose a PDF to build a template",
        accept = ".pdf"
      ),
      
      helpText("Drag a rectangle on the PDF to define a field."),
      
      hr(),
      
      textInput("field_name", "Field name"),
      
      # save current rectangle as a field
      actionButton("add_field", "Add Field", width = "100%"),
      
      br(),
      br(),
      
      # undo last built field
      actionButton("delete_last_field", "Delete Last Field", width = "100%"),
      
      br(),
      br(),
      # clear all fields
      actionButton("clear_fields", "Clear Fields", width = "100%"),
      
      hr(),
      
      downloadButton("download_csv", "Export CSV", width = "100%"),
      
      width = 3
    ),
    # Main panel for rendered PDF 
    mainPanel(
      plotOutput(
        "page_plot",
        height = "850px",
        # Interactive brush for drawing boxes
        brush = brushOpts(
          id = "page_brush",
          direction = "xy",
          resetOnNew = FALSE
        )
      ),
      
      hr(),
      
      h4("Saved Fields"),
      tableOutput("fields_table")
    )
  )
  ),
  
  tabPanel(
    "Preview & Tune Parser",
    
    sidebarLayout(
      sidebarPanel(
        h4("Parser Hyperparameters"),
        
        helpText("These settings control how text is captured around each saved field box."),
        
        sliderInput(
          "x_pad_pct",
          tags$span("Horizontal padding ", actionLink("help_x_pad", "?")),
          min = 0,
          max = 0.05,
          value = 0.005,
          step = 0.001
        ),
        
        sliderInput(
          "y_pad_pct",
          tags$span("Vertical padding ", actionLink("help_y_pad", "?")),
          min = 0,
          max = 0.05,
          value = 0.005,
          step = 0.001
        ),
        
        selectInput(
          "capture_mode",
          tags$span("Capture rule ", actionLink("help_capture_mode", "?")),
          choices = c(
            "Word center must be inside field" = "center",
            "Word must overlap field" = "overlap"
          ),
          selected = "center"
        ),
        
        sliderInput(
          "min_word_overlap_pct",
          tags$span("Minimum overlap ", actionLink("help_min_overlap", "?")),
          min = 0,
          max = 100,
          value = 1,
          step = 1
        ),
        
        sliderInput(
          "line_tolerance",
          tags$span("Line sorting ", actionLink("help_line_sorting", "?")),
          min = 1,
          max = 20,
          value = 3,
          step = 1
        ),
        
        hr(),
        
        actionButton("preview_parse", "Preview Parsing", width = "100%"),
        
        hr(),
        
        textInput(
          "template_file_name",
          "Template file name",
          value = "export"
        ),
        
        helpText("The final export will include the template CSV, parsedsample.csv, and parameters.csv."),        
        downloadButton(
          "finish_export",
          "Finish: Export Template + Parsed Sample",
          width = "100%"
        ),
        
        width = 3
      ),
      
      mainPanel(
        h4("Current Template Status"),
        verbatimTextOutput("template_status"),
        
        hr(),
        
        h4("Parsed Sample Preview"),
        tableOutput("parsed_sample_table"),
        
        hr(),
        
        h4("Export contents"),
        tags$ul(
          tags$li("Template CSV: saved field boxes only."),
          tags$li("parsedsample.csv: one row per field, with field name and raw captured text."),
          tags$li("parameters.csv: parser hyperparameters used for the parsed sample.")
        )
      )
    )
  )
  )
  )
server <- function(input, output, session) {
  
  # Stop listening in R if the browser is closed
  session$onSessionEnded(function() {
    stopApp()
  })
  
  
  # use 150 dpi
  dpi <- 150
  
  page_image <- reactiveVal(NULL)
  page_dims <- reactiveVal(NULL)
  fields <- reactiveVal(empty_fields())
  parsed_sample <- reactiveVal(empty_parsed_sample())
  
  # keep track of current parser hyperparameter settings
  current_parser_settings <- reactive({
    list(
      x_pad_pct = input$x_pad_pct,
      y_pad_pct = input$y_pad_pct,
      capture_mode = input$capture_mode,
      min_word_overlap_pct = input$min_word_overlap_pct,
      line_tolerance = input$line_tolerance
    )
  })
  
  # Help button information to describe hyperparameters
  observeEvent(input$help_x_pad, {
    showModal(modalDialog(
      title = "Horizontal padding",
      p("Horizontal padding expands each field box to the left and right before text is captured."),
      p("The value is a normalized fraction of the page width."),
      p("For example, 0.01 means 1% of the page width is added to both the left and right sides of each field box."),
      p("Increase this if text near the left or right edge of a field is being missed. Decrease it if nearby text from neighboring fields is being captured."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_y_pad, {
    showModal(modalDialog(
      title = "Vertical padding",
      p("Vertical padding expands each field box upward and downward before text is captured."),
      p("The value is a normalized fraction of the page height."),
      p("For example, 0.01 means 1% of the page height is added above and below each field box."),
      p("Increase this if text slightly above or below the box is being missed. Decrease it if text from nearby lines is being captured."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_capture_mode, {
    showModal(modalDialog(
      title = "Capture rule",
      p("The capture rule determines how the parser decides whether a word belongs inside a field box."),
      tags$ul(
        tags$li(strong("Center mode: "), "a word is captured if the center point of the word falls inside the field box."),
        tags$li(strong("Overlap mode: "), "a word is captured if enough of the word's bounding box overlaps the field box.")
      ),
      p("Center mode is usually stricter and safer. Overlap mode can be useful when words sit partly outside the drawn field box."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_min_overlap, {
    showModal(modalDialog(
      title = "Minimum overlap",
      p("Minimum overlap is used only when the capture rule is set to overlap mode."),
      p("It controls how much of a word's bounding box must overlap the field box before that word is captured."),
      p("The value is a percentage from 0 to 100."),
      tags$ul(
        tags$li("A low value, such as 1, captures words that only slightly touch the field box."),
        tags$li("A higher value, such as 50, requires at least half of the word to overlap the field box."),
        tags$li("A value of 100 requires the full word box to be inside the field box.")
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$help_line_sorting, {
    showModal(modalDialog(
      title = "Line sorting",
      p("Line sorting controls how captured words are ordered before they are pasted together into raw text."),
      p("PDF text extraction can assign slightly different y-coordinates to words that visually appear on the same line."),
      p("This setting groups nearby y-values together so words are sorted more naturally from top to bottom and left to right."),
      p("Increase this if words from the same line are being split into strange order. Decrease it if words from separate lines are being incorrectly grouped together."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Take a pdf file as input
  observeEvent(input$pdf_file, {
    req(input$pdf_file)
    
    # convert to png with a temporary file
    tmp_png <- paste0(tempfile(pattern = "pdf_page_"), "_%d.%s")
    
    # Convert input file to png - can raster into an R plot
    tryCatch({
      rendered_file <- pdf_convert(
        pdf = input$pdf_file$datapath,
        format = "png",
        pages = 1,
        dpi = dpi,
        filenames = tmp_png,
        verbose = FALSE
      )
      
      img <- readPNG(rendered_file[1])
      
      page_width <- dim(img)[2]
      page_height <- dim(img)[1]
      
      page_image(img)
      page_dims(list(width = page_width, height = page_height))
      
      fields(empty_fields())
      parsed_sample(empty_parsed_sample())
      session$resetBrush("page_brush")
      
    }, error = function(e) {
      page_image(NULL)
      page_dims(NULL)
      fields(empty_fields())
      parsed_sample(empty_parsed_sample())
      
      showNotification(
        paste("Could not render PDF:", e$message),
        type = "error"
      )
    })
  })
  
  output$page_plot <- renderPlot({
    img <- page_image()
    
    validate(
      need(!is.null(img), "Upload a PDF to begin.")
    )
    
    dims <- page_dims()
    w <- dims$width
    h <- dims$height
    
    par(mar = c(0, 0, 0, 0))
    
    plot(
      NA,
      xlim = c(0, w),
      ylim = c(h, 0),
      xaxs = "i",
      yaxs = "i",
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      asp = 1
    )
    
    rasterImage(
      img,
      xleft = 0,
      ybottom = h,
      xright = w,
      ytop = 0,
      interpolate = TRUE
    )
    
    saved_fields <- fields()
    
    if (nrow(saved_fields) > 0) {
      for (i in seq_len(nrow(saved_fields))) {
        rect(
          saved_fields$xmin[i],
          saved_fields$ymin[i],
          saved_fields$xmax[i],
          saved_fields$ymax[i],
          border = "red",
          lwd = 2
        )
        
        text(
          x = (saved_fields$xmin[i] + saved_fields$xmax[i]) / 2,
          y = (saved_fields$ymin[i] + saved_fields$ymax[i]) / 2,
          labels = saved_fields$name[i],
          col = "red",
          cex = 0.9
        )
      }
    }
    
    current_brush <- input$page_brush
    
    if (!is.null(current_brush)) {
      rect(
        current_brush$xmin,
        current_brush$ymin,
        current_brush$xmax,
        current_brush$ymax,
        border = "blue",
        lwd = 2,
        lty = 2
      )
    }
  })
  
  # logic for creating a field
  observeEvent(input$add_field, {
    dims <- page_dims()
    brush <- input$page_brush
    field_name <- trimws(input$field_name)
    
    req(dims)
    
    if (is.null(brush)) {
      showNotification("Drag a rectangle on the PDF first.", type = "warning")
      return()
    }
    
    if (!nzchar(field_name)) {
      showNotification("Enter a field name.", type = "warning")
      return()
    }
    
    w <- dims$width
    h <- dims$height
    
    xmin <- max(0, min(brush$xmin, brush$xmax))
    xmax <- min(w, max(brush$xmin, brush$xmax))
    ymin <- max(0, min(brush$ymin, brush$ymax))
    ymax <- min(h, max(brush$ymin, brush$ymax))
    
    if ((xmax - xmin) < 2 || (ymax - ymin) < 2) {
      showNotification("The selected rectangle is too small.", type = "warning")
      return()
    }
    
    new_field <- data.frame(
      name = field_name,
      xmin = xmin,
      ymin = ymin,
      xmax = xmax,
      ymax = ymax,
      width = xmax - xmin,
      height = ymax - ymin,
      xmin_norm = xmin / w,
      ymin_norm = ymin / h,
      xmax_norm = xmax / w,
      ymax_norm = ymax / h,
      stringsAsFactors = FALSE
    )
    
    fields(rbind(fields(), new_field))
    parsed_sample(empty_parsed_sample())
    
    
    updateTextInput(session, "field_name", value = "")
    session$resetBrush("page_brush")
  })
  
  # logic to delete last field of the current csv
  observeEvent(input$delete_last_field, {
    current_fields <- fields()
    
    if (nrow(current_fields) == 0) {
      showNotification("No fields to delete.", type = "warning")
      return()
    }
    
    deleted_name <- current_fields$name[nrow(current_fields)]
    
    # Remove the bottom row / most recently added field
    updated_fields <- current_fields[-nrow(current_fields), , drop = FALSE]
    
    fields(updated_fields)
    #reset parsed sample to avoid any misinterpretation
    parsed_sample(empty_parsed_sample())    
    
    # Optional convenience: put the deleted field's name back in the text box
    # so you can redraw the box and re-add it.
    updateTextInput(session, "field_name", value = deleted_name)
    
    session$resetBrush("page_brush")
    
    showNotification(
      paste("Deleted last field:", deleted_name),
      type = "message"
    )
  })
  
  observeEvent(input$clear_fields, {
    fields(empty_fields())
    parsed_sample(empty_parsed_sample())
    session$resetBrush("page_brush")
  })
  
  output$fields_table <- renderTable({
    saved_fields <- fields()
    
    if (nrow(saved_fields) == 0) {
      return(data.frame(Message = "No fields yet."))
    }
    
    saved_fields
  }, digits = 3)
  
  output$template_status <- renderText({
    current_fields <- fields()
    
    if (is.null(input$pdf_file)) {
      return("No PDF uploaded.")
    }
    
    paste0(
      "PDF: ", input$pdf_file$name, "\n",
      "Saved fields: ", nrow(current_fields), "\n",
      "Preview parser uses the current field boxes and current hyperparameter values."
    )
  })
  
  observeEvent(input$preview_parse, {
    req(input$pdf_file)
    
    current_fields <- fields()
    
    if (nrow(current_fields) == 0) {
      showNotification("No fields have been defined yet.", type = "warning")
      return()
    }
    
    settings <- current_parser_settings()
    
    tryCatch({
      
      result <- extract_pdf_fields(
        pdf_file = input$pdf_file$datapath,
        fields_df = current_fields,
        settings = settings,
        page = 1
      )
      
      parsed_sample(result)
      
      showNotification("Preview parsing complete.", type = "message")
      
    }, error = function(e) {
      
      parsed_sample(empty_parsed_sample())
      
      showNotification(
        paste("Preview parsing failed:", e$message),
        type = "error"
      )
    })
  })
  
  output$parsed_sample_table <- renderTable({
    result <- parsed_sample()
    
    if (nrow(result) == 0) {
      return(data.frame(Message = "No parsed sample yet. Click Preview Parsing."))
    }
    
    result
  }, digits = 3)
  
  output$finish_export <- downloadHandler(
    filename = function() {
      stem <- input$template_file_name
      paste0(stem, ".zip")
    },
    
    content = function(file) {
      
      req(input$pdf_file)
      
      current_fields <- fields()
      
      if (nrow(current_fields) == 0) {
        stop("No fields have been defined.")
      }
      
      settings <- current_parser_settings()
      
      # Re-run extraction at export time so parsedsample.csv matches
      # the current template and current hyperparameters.
      parsed <- extract_pdf_fields(
        pdf_file = input$pdf_file$datapath,
        fields_df = current_fields,
        settings = settings,
        page = 1
      )
      
      # Keep only the requested columns for parsedsample.csv
      parsed_out <- parsed[, c("name", "raw_text"), drop = FALSE]
      
      # save template output 
      template_out <- current_fields
      
      # Hyperparameters used in preview saved to parameters.csv
      parameters_out <- data.frame(
        parameter = c(
          "x_padding",
          "y_padding",
          "capture_mode",
          "min_word_overlap_pct",
          "line_tolerance",
          "source_pdf",
          "created_at"
        ),
        value = c(
          settings$x_pad_pct,
          settings$y_pad_pct,
          settings$capture_mode,
          settings$min_word_overlap_pct,
          settings$line_tolerance,
          input$pdf_file$name,
          as.character(Sys.time())
        ),
        stringsAsFactors = FALSE
      )
      
      # Create temp export directory
      export_dir <- tempfile("parser_export_")
      dir.create(export_dir)
      
      template_stem <- input$template_file_name
      
      template_csv <- file.path(export_dir, paste0(template_stem, ".csv"))
      parsed_csv <- file.path(export_dir, "parsedsample.csv")
      parameters_csv <- file.path(export_dir, "parameters.csv")
      
      write.csv(template_out, template_csv, row.names = FALSE)
      write.csv(parsed_out, parsed_csv, row.names = FALSE)
      write.csv(parameters_out, parameters_csv, row.names = FALSE)
      
      # zip csv files and export
      zipr(
        zipfile = file,
        files = c(
          basename(template_csv),
          basename(parsed_csv),
          basename(parameters_csv)
        ),
        root = export_dir
      )
    }
  )
  
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("pdf_template_fields_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(fields(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

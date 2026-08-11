# app.R - Simple PDF template box editor
# Requires: shiny, pdftools, png

# Package set-up
packages <- c("shiny", "pdftools", "png")

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

ui <- fluidPage(
  titlePanel("Interactive PDF Template Builder"),
  
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
)

server <- function(input, output, session) {
  # use 150 dpi
  dpi <- 150
  
  page_image <- reactiveVal(NULL)
  page_dims <- reactiveVal(NULL)
  fields <- reactiveVal(empty_fields())
  
  # Take a pdf file as input
  observeEvent(input$pdf_file, {
    req(input$pdf_file)
    
    # convert to png
    tmp_png <- tempfile(fileext = ".png")
    
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
      session$resetBrush("page_brush")
      
    }, error = function(e) {
      page_image(NULL)
      page_dims(NULL)
      fields(empty_fields())
      
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
    session$resetBrush("page_brush")
  })
  
  output$fields_table <- renderTable({
    saved_fields <- fields()
    
    if (nrow(saved_fields) == 0) {
      return(data.frame(Message = "No fields yet."))
    }
    
    saved_fields
  }, digits = 3)
  
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

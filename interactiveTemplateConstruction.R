#helper to build the template

##############################################################################
###################### Section 0: Set-up and data import #####################
##############################################################################

packages <-
  c(
    "pdftools",
    "tidyverse",
    "tcltk"
  )

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
invisible(lapply(packages, library, character.only = T))

# Import Data - use tk_choose.files for a cross-platform dialog box
file <- tk_choose.files(
  caption = "Select your report file to build template",
  multi = FALSE # Ensures they can only pick one file
)


# x increases L->R, y increases Top->Bottom
words_df <- as.data.frame(pdf_data(file)[[1]])

# plot words on the page
plot_words_page <- function(words) {
  xrange <- range(words$x, na.rm = TRUE)
  yrange <- range(words$y, na.rm = TRUE)
  # Use base plot with reversed y axis (so top is at top)
  plot(
    NA,
    xlim = xrange,
    ylim = c(yrange[2], yrange[1]),
    xlab = "x",
    ylab = "y",
    main = "Click top-left then bottom-right of each field (Press ESC to stop)."
  )
  # add text labels (smaller for many words)
  text(words$x, words$y, labels = words$text, cex = 0.7, adj = c(0, 1))
  # Add grid for guidance
  abline(h = pretty(words$y), v = pretty(words$x), col = "#dddddd", lty = 3)
}
plot_words_page(words_df)

collect_boxes_interactive <- function(
  words,
  out_csv = "field_boxes_template.csv"
) {
  # show the page
  plot_words_page(words)

  boxes <- data.frame(
    field = character(),
    x = numeric(),
    y = numeric(),
    width = numeric(),
    height = numeric(),
    stringsAsFactors = FALSE
  )

  message(
    "For each field: type the field name, press Enter, then click TOP-LEFT and BOTTOM-RIGHT corners on the plot."
  )
  message("When finished, press Enter on an empty field name to stop.")

  repeat {
    field_name <- readline(prompt = "Field name (or ENTER to finish): ")
    if (nchar(field_name) == 0) {
      break
    }

    cat("Now click TOP-LEFT then BOTTOM-RIGHT on the PLOT window.\n")
    # locator(2) waits for two clicks (or ESC to abort)
    pts <- locator(2)
    if (is.null(pts) || length(pts$x) < 2) {
      cat(
        "Clicking aborted or fewer than 2 clicks detected. Try again for this field.\n"
      )
      next
    }

    x1 <- pts$x[1]
    y1 <- pts$y[1]
    x2 <- pts$x[2]
    y2 <- pts$y[2]

    # Convert clicked corners to canonical top-left (smallest y) and bottom-right
    left <- min(x1, x2)
    right <- max(x1, x2)
    top <- min(y1, y2) # smaller y = closer to top in PDF coord
    bottom <- max(y1, y2)

    width <- right - left
    height <- bottom - top

    # Append to boxes
    boxes <- rbind(
      boxes,
      data.frame(
        field = field_name,
        x = left,
        y = top,
        width = width,
        height = height,
        stringsAsFactors = FALSE
      )
    )

    # Draw rectangle immediately for verification
    rect(left, top, left + width, top + height, border = "red", lwd = 2)
    text(left + width / 2, top + height / 2, labels = field_name, col = "red")
    cat(sprintf(
      "Recorded %s -> x=%.1f y=%.1f w=%.1f h=%.1f\n",
      field_name,
      left,
      top,
      width,
      height
    ))
  }

  # Save CSV
  write.csv(boxes, out_csv, row.names = FALSE)
  cat("Saved boxes to:", out_csv, "\n")
  return(boxes)
}

# Run the interactive collector (make sure the plot window is active)
box_template <- collect_boxes_interactive(
  words_df,
  out_csv = "field_boxes_template.csv"
)
box_template

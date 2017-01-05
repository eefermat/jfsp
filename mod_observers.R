observeEvent(input$plot1_dblclick, {
  brush <- input$plot1_brush
  if (!is.null(brush)) {
    rv_plot1$x <- c(brush$xmin, brush$xmax)
    rv_plot1$y <- c(brush$ymin, brush$ymax)
  } else {
    rv_plot1$x <- NULL
    rv_plot1$y <- NULL
  }
})

observeEvent(xdata(), {
  rv_plot1$keeprows <- rep(TRUE, nrow(xdata()))
})

# Toggle points that are clicked
observeEvent(input$plot1_click, {
  res <- nearPoints(xdata(), input$plot1_click, allRows=TRUE)
  rv_plot1$keeprows <- xor(rv_plot1$keeprows, res$selected_)
})

# Toggle points that are brushed, when button is clicked
observeEvent(input$exclude_toggle, {
  res <- brushedPoints(xdata(), input$plot1_brush, allRows=TRUE)
  rv_plot1$keeprows <- xor(rv_plot1$keeprows, res$selected_)
})

# Reset all points
observeEvent(input$exclude_reset, {
  rv_plot1$keeprows <- rep(TRUE, nrow(xdata()))
})

# Density plot

# Doubleclick observation
observeEvent(input$plot_density1_dblclick, {
  brush <- input$plotdensity1_brush
  if (!is.null(brush)) {
    rv_plots$xden <- c(brush$xmin, brush$xmax)
    rv_plots$yden <- c(brush$ymin, brush$ymax)
  } else {
    rv_plots$xden <- NULL
    rv_plots$yden <- NULL
  }
})

# Time series plot

# Doubleclick observation
observeEvent(input$plot1_dblclick, {
  brush <- input$plot1_brush
  if (!is.null(brush)) {
    rv_plots$x <- c(brush$xmin, brush$xmax)
    rv_plots$y <- c(brush$ymin, brush$ymax)
  } else {
    rv_plots$x <- NULL
    rv_plots$y <- NULL
  }
})

observeEvent(xdata(), {
  rv_plots$keeprows <- rep(TRUE, nrow(xdata()))
})

# Toggle points that are clicked
observeEvent(input$plot1_click, {
  res <- nearPoints(xdata(), input$plot1_click, allRows=TRUE)
  rv_plots$keeprows <- xor(rv_plots$keeprows, res$selected_)
})

# Toggle points that are brushed, when button is clicked
observeEvent(input$exclude_toggle, {
  res <- brushedPoints(xdata(), input$plot1_brush, allRows=TRUE)
  rv_plots$keeprows <- xor(rv_plots$keeprows, res$selected_)
})

# Reset all points
observeEvent(input$exclude_reset, {
  rv_plots$keeprows <- rep(TRUE, nrow(xdata()))
})

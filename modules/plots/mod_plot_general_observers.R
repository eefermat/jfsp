# Doubleclk observation
observeEvent(input$plot1_dblclk, {
  brush <- input$plot1_brush
  if (!is.null(brush)) {
    rv_plots$x1 <- c(brush$xmin, brush$xmax)
    rv_plots$y1 <- c(brush$ymin, brush$ymax)
  } else {
    rv_plots$x1 <- NULL
    rv_plots$y1 <- NULL
  }
})

# Distribution - histogram

# Doubleclk observation
observeEvent(input$plot2_dblclk, {
  brush <- input$plot2_brush
  if (!is.null(brush)) {
    rv_plots$x2 <- c(brush$xmin, brush$xmax)
    rv_plots$y2 <- c(brush$ymin, brush$ymax)
  } else {
    rv_plots$x2 <- NULL
    rv_plots$y2 <- NULL
  }
})

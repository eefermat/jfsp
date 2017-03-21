# Time series plot annual

observeEvent(d(), {
  rv_plots$keeprows <- rep(TRUE, nrow(d()))
})

# Toggle points that are clked
observeEvent(input$plot1_clk, {
  res <- nearPoints(d(), input$plot1_clk, allRows=TRUE)
  rv_plots$keeprows <- xor(rv_plots$keeprows, res$selected_)
})

# Toggle points that are brushed, when button is clked
observeEvent(input$exclude_toggle, {
  res <- brushedPoints(d(), input$plot1_brush, allRows=TRUE)
  rv_plots$keeprows <- xor(rv_plots$keeprows, res$selected_)
})

# Reset all points
observeEvent(input$exclude_reset, {
  rv_plots$keeprows <- rep(TRUE, nrow(d()))
})

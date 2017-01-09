# Distribution - density plot

# Doubleclk observation
observeEvent(input$plot_den1_dblclk, {
  brush <- input$plot_den1_brush
  if (!is.null(brush)) {
    rv_plots$xden1 <- c(brush$xmin, brush$xmax)
    rv_plots$yden1 <- c(brush$ymin, brush$ymax)
  } else {
    rv_plots$xden1 <- NULL
    rv_plots$yden1 <- NULL
  }
})

# Distribution - histogram

# Doubleclk observation
observeEvent(input$plot_den2_dblclk, {
  brush <- input$plot_den2_brush
  if (!is.null(brush)) {
    rv_plots$xden2 <- c(brush$xmin, brush$xmax)
    rv_plots$yden2 <- c(brush$ymin, brush$ymax)
  } else {
    rv_plots$xden2 <- NULL
    rv_plots$yden2 <- NULL
  }
})

# Time series plot annual

# Doubleclk observation
observeEvent(input$plot_ts1_dblclk, {
  brush <- input$plot_ts1_brush
  if (!is.null(brush)) {
    rv_plots$xts1 <- c(brush$xmin, brush$xmax)
    rv_plots$yts1 <- c(brush$ymin, brush$ymax)
  } else {
    rv_plots$xts1 <- NULL
    rv_plots$yts1 <- NULL
  }
})

observeEvent(d(), {
  rv_plots$keeprows <- rep(TRUE, nrow(d()))
})

# Toggle points that are clked
observeEvent(input$plot_ts1_clk, {
  res <- nearPoints(d(), input$plot_ts1_clk, allRows=TRUE)
  rv_plots$keeprows <- xor(rv_plots$keeprows, res$selected_)
})

# Toggle points that are brushed, when button is clked
observeEvent(input$exclude_toggle, {
  res <- brushedPoints(d(), input$plot_ts1_brush, allRows=TRUE)
  rv_plots$keeprows <- xor(rv_plots$keeprows, res$selected_)
})

# Reset all points
observeEvent(input$exclude_reset, {
  rv_plots$keeprows <- rep(TRUE, nrow(d()))
})

# Time series plot cumulative

# Doubleclk observation
observeEvent(input$plot_ts2_dblclk, {
  brush <- input$plot_ts2_brush
  if (!is.null(brush)) {
    rv_plots$xts2 <- c(brush$xmin, brush$xmax)
    rv_plots$yts2 <- c(brush$ymin, brush$ymax)
  } else {
    rv_plots$xts2 <- NULL
    rv_plots$yts2 <- NULL
  }
})

# Decadal series boxplot

# Doubleclk observation
observeEvent(input$plot_dec1_dblclk, {
  brush <- input$plot_dec1_brush
  if (!is.null(brush)) {
    rv_plots$xdec1 <- c(brush$xmin, brush$xmax)
    rv_plots$ydec1 <- c(brush$ymin, brush$ymax)
  } else {
    rv_plots$xdec1 <- NULL
    rv_plots$ydec1 <- NULL
  }
})

observeEvent(keep_dec(), {
  rv_plots$dec_keeprows <- rep(TRUE, nrow(keep_dec()))
})

# Toggle points that are clked
observeEvent(input$plot_dec1_clk, {
  res <- nearPoints(keep_dec(), input$plot_dec1_clk, allRows=TRUE)
  rv_plots$dec_keeprows <- xor(rv_plots$dec_keeprows, res$selected_)
})

# Toggle points that are brushed, when button is clked
observeEvent(input$exclude_toggle, {
  res <- brushedPoints(keep_dec(), input$plot_dec1_brush, allRows=TRUE)
  rv_plots$dec_keeprows <- xor(rv_plots$dec_keeprows, res$selected_)
})

# Reset all points
observeEvent(input$exclude_reset, {
  rv_plots$dec_keeprows <- rep(TRUE, nrow(keep_dec()))
})

# Decadal series barplot

# Doubleclk observation
observeEvent(input$plot_dec2_dblclk, {
  brush <- input$plot_dec2_brush
  if (!is.null(brush)) {
    rv_plots$xdec2 <- c(brush$xmin, brush$xmax)
    rv_plots$ydec2 <- c(brush$ymin, brush$ymax)
  } else {
    rv_plots$xdec2 <- NULL
    rv_plots$ydec2 <- NULL
  }
})

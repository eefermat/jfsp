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
  x <- input$plot_dec1_clk
  y <- rv_plots$dec_holdClick
  if(!is.null(x)){
    if(is.null(y) || y$x!=x$x) rv_plots$dec_holdClick <- x
  }
})

observeEvent(input$plot_dec1_clk, {
  x <- keep_dec()$Decade
  lvls <- levels(x)
  clk <- input$plot_dec1_clk
  if(is.null(clk)) y <- rv_plots$dec_holdClick else y <- clk
  keep_lvls <- lvls[round(y$x)]
  if(!length(keep_lvls) || is.na(keep_lvls)) keep_lvls <- lvls
  if(any(rv_plots$dec_keeprows!=(x==keep_lvls))) rv_plots$dec_keeprows <- x %in% keep_lvls
})

# Toggle points that are brushed in x axis direction (all y)
observeEvent(input$plot_dec1_brush, {
  x <- input$plot_dec1_brush
  #y <- rv_plots$dec_holdBrush
  if(!is.null(x)){
    rv_plots$dec_holdBrush <- x
  }
})

observeEvent(input$plot_dec1_brush, {
  x <- keep_dec()$Decade
  lvls <- levels(x)
  brush <- input$plot_dec1_brush
  if(is.null(brush)) y <- rv_plots$dec_holdBrush else y <- brush
  intlvls <- round(y$xmin):round(y$xmax)
  rv_plots$dec_keeprows <- x %in% lvls[intlvls]
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

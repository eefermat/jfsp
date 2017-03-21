# Decadal series boxplot

# Toggle points that are clked
observeEvent(input$plot1_clk, {
  x <- input$plot1_clk
  y <- rv_plots$holdClick
  if(!is.null(x)){
    if(is.null(y) || y$x!=x$x) rv_plots$holdClick <- x
  }
})

observeEvent(input$plot1_clk, {
  x <- keep()$Decade
  lvls <- levels(x)
  clk <- input$plot1_clk
  if(is.null(clk)) y <- rv_plots$holdClick else y <- clk
  keep_lvls <- lvls[round(y$x)]
  if(!length(keep_lvls) || is.na(keep_lvls)) keep_lvls <- lvls
  if(any(rv_plots$keeprows!=(x==keep_lvls))) rv_plots$keeprows <- x %in% keep_lvls
})

# Toggle points that are brushed in x axis direction (all y)
observeEvent(input$plot1_brush, {
  x <- input$plot1_brush
  if(!is.null(x)){
    rv_plots$holdBrush <- x
  }
})

observeEvent(input$plot1_brush, {
  x <- keep()$Decade
  lvls <- levels(x)
  brush <- input$plot1_brush
  if(is.null(brush)) y <- rv_plots$holdBrush else y <- brush
  intlvls <- round(y$xmin):round(y$xmax)
  rv_plots$keeprows <- x %in% lvls[intlvls]
})

observeEvent(keep(), {
  rv_plots$keeprows <- rep(TRUE, nrow(keep()))
})

# Decadal series barplot

# Toggle points that are clked
observeEvent(input$plot2_clk, {
  x <- input$plot2_clk
  y <- rv_plots$holdClick2
  if(!is.null(x)){
    if(is.null(y) || y$x!=x$x) rv_plots$holdClick2 <- x
  }
})

observeEvent(input$plot2_clk, {
  x <- keep_dec()$Decade
  lvls <- levels(x)
  clk <- input$plot2_clk
  if(is.null(clk)) y <- rv_plots$holdClick2 else y <- clk
  keep_lvls <- lvls[round(y$x)]
  if(!length(keep_lvls) || is.na(keep_lvls)) keep_lvls <- lvls
  if(any(rv_plots$keeprows2!=(x==keep_lvls))) rv_plots$keeprows2 <- x %in% keep_lvls
})

# Toggle points that are brushed in x axis direction (all y)
observeEvent(input$plot2_brush, {
  x <- input$plot2_brush
  if(!is.null(x)){
    rv_plots$holdBrush2 <- x
  }
})

observeEvent(input$plot2_brush, {
  x <- keep_dec()$Decade
  lvls <- levels(x)
  brush <- input$plot2_brush
  if(is.null(brush)) y <- rv_plots$holdBrush2 else y <- brush
  intlvls <- round(y$xmin):round(y$xmax)
  rv_plots$keeprows2 <- x %in% lvls[intlvls]
})

observeEvent(keep_dec(), {
  rv_plots$keeprows2 <- rep(TRUE, nrow(keep_dec()))
})

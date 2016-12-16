library(dplyr)
library(purrr)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  dsub <- reactive({
    filter(d, RCP %in% input$rcps & Model %in% input$gcms & Region %in% input$regions &
             Var %in% input$vars & Vegetation %in% input$veg & Year >= input$yrs[1] & Year <= input$yrs[2]) %>%
      select_(.dots=c("RCP", "Model", "Region", "Var", "Vegetation", "Year", input$stat))
  })
  
  rv_plot1 <- reactiveValues(x=NULL, y=NULL, keeprows=rep(TRUE, nrow(isolate(dsub()))))
  
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
  
  observeEvent(dsub(), {
    rv_plot1$keeprows <- rep(TRUE, nrow(dsub()))
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    if(input$settings_clickExclude){
      res <- nearPoints(dsub(), input$plot1_click, allRows=TRUE)
      rv_plot1$keeprows <- xor(rv_plot1$keeprows, res$selected_)
    } else {
      rv_plot1$keeprows <- rep(TRUE, nrow(dsub()))
    }
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(dsub(), input$plot1_brush, allRows=TRUE)
    rv_plot1$keeprows <- xor(rv_plot1$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    rv_plot1$keeprows <- rep(TRUE, nrow(dsub()))
  })
  
  output$plot1 <- renderPlot({
    if(nrow(dsub())!=length(rv_plot1$keeprows)) return()
    keep    <- filter(dsub(), rv_plot1$keeprows)
    exclude <- filter(dsub(), !rv_plot1$keeprows)
    
    ggplot(data=keep, aes_string("Year", input$stat, colour="Model")) + geom_point(size=3) +
      geom_point(data=exclude, shape=21, fill=NA, color="black", alpha=0.25) +
      coord_cartesian(xlim=rv_plot1$x, ylim=rv_plot1$y)
  })
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "click: ", xy_str(input$plot1_click),
      "dblclick: ", xy_str(input$plot1_dblclick),
      "hover: ", xy_str(input$plot1_hover),
      "brush: ", xy_range_str(input$plot1_brush)
    )
  })
  
  output$info2 <- renderPrint({
    if(is.null(input$plot1_brush)) nearPoints(dsub(), input$plot1_click, allRows=input$settings_allRows) else
    brushedPoints(dsub(), input$plot1_brush, allRows=input$settings_allRows)
    # nearPoints() also works with hover and dblclick events
  })
  
})

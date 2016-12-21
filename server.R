library(rgdal)
library(dplyr)
library(purrr)
library(ggplot2)

lon <- -155
lat <- 65
fmz <- readOGR("shapefiles/fmz_polygons.shp", verbose=FALSE)
fmz <- subset(fmz, !REGION %in% c("TNF", "HNS"))
flam <- readOGR("shapefiles/flam_polygon.shp", verbose=FALSE)

source("utils.R", local=TRUE)

shinyServer(function(input, output, session) {
  
  source("observers.R", local=TRUE)
  
  fmzsub <- reactive({
    x <- input$regions
    if(is.null(x)) return()
    if(length(x)==1){
      if(x=="") return()
      if(x=="AK") return(fmz)
    }
    subset(fmz, REGION %in% x)
  })
  
  # Initialize map
  output$Map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lon, lat, 4) %>%
      addPolygons(data=flam, stroke=TRUE, fillOpacity=0.2, weight=1, color="red", group="flammable")
  })
  
  dsub <- reactive({
    filter(d, GBM %in% input$gbms & RCP %in% input$rcps & Model %in% input$gcms & Region %in% input$regions &
             Var %in% input$vars & Vegetation %in% input$veg & Year >= input$yrs[1] & Year <= input$yrs[2]) %>%
      select_(.dots=c("GBM", "RCP", "Model", "Region", "Var", "Vegetation", "Year", input$stat))
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
  
  colorby <- reactive({ if(input$colorby=="") NULL else input$colorby })
  keep    <- reactive({
    req(rv_plot1$keeprows)
    if(length(rv_plot1$keeprows) != nrow(dsub())) return()
    filter(dsub(), rv_plot1$keeprows)
  })
  exclude <- reactive({ filter(dsub(), !rv_plot1$keeprows) })
  colorvec <- reactive({ if(is.null(colorby())) NULL else tolpal(length(unique(keep()[[colorby()]]))) })
  
  output$plot1 <- renderPlot({
    if(nrow(dsub())==0 | nrow(dsub())!=length(rv_plot1$keeprows)) return()
    
    g <- ggplot(data=keep(), aes_string("Year", input$stat, colour=colorby())) + geom_point(size=3) +
      geom_point(data=exclude(), shape=21, fill=NA, color="black", alpha=0.25) +
      coord_cartesian(xlim=rv_plot1$x, ylim=rv_plot1$y) +
      theme_bw(base_size = 14) #+ scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand=c(0,0.5))
    if(!is.null(colorvec())) g <- g + scale_colour_manual(values=colorvec())
    if(input$facetby!="") g <- g + facet_wrap(as.formula(paste0("~", input$facetby)), scales=input$facet_scales)
    g + tp_theme
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
  
  output$Selected_obs <- DT::renderDataTable({
    x <- if(is.null(input$plot1_brush)) nearPoints(dsub(), input$plot1_click, allRows=input$settings_allRows) else
      brushedPoints(dsub(), input$plot1_brush, allRows=input$settings_allRows)
    col <- if(input$colorby=="") "Var" else input$colorby
    clrs <- tableRowColors(keep(), input$colorby, colorvec())
    DT::datatable(x, options=list(
      lengthMenu=list(c(5, 10, 25, - 1), c('5', '10', '25', 'All')), pageLength=5, searching=FALSE)) %>%
        formatStyle(columns=col, backgroundColor=clrs, target='row')
  })
  
})

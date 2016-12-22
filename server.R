library(rgdal)
library(dplyr)
library(purrr)
library(ggplot2)

lon <- -155
lat <- 65
fmz <- readOGR("shapefiles/fmz_polygons.shp", verbose=FALSE)
fmz <- subset(fmz, !REGION %in% c("TNF", "HNS"))
flam <- readOGR("shapefiles/flam_polygon.shp", verbose=FALSE)

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
  
  callModule(dbmod, "mod1", xdata=dsub, rv_plot1=rv_plot1, stat=input$stat,
             allTableRows=input$settings_allRows, settings_clickExclude=reactive(input$settings_clickExclude))
  
  
})

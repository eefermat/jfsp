library(rgdal)
library(lazyeval)
library(dplyr)
library(purrr)
library(ggplot2)

lon <- -155
lat <- 65
fmz <- readOGR("shapefiles/fmz_polygons.shp", verbose=FALSE)
fmz <- subset(fmz, !REGION %in% c("TNF", "HNS"))
flam <- readOGR("shapefiles/flam_polygon.shp", verbose=FALSE)
tab_ids <- c("burnarea", "firefreq", "firesize", "vegarea", "vegage")

shinyServer(function(input, output, session) {
  
  source("observers.R", local=TRUE) # observers related to region selection using map and selectInput

  # Initialize map and add flammability polygon layer
  mapSelect <- leaflet() %>% addTiles() %>% setView(lon, lat, 4) %>%
    addPolygons(data=flam, stroke=TRUE, fillOpacity=0.2, weight=1, color="red", group="flammable")
  # Add background polygon region outlines after map is created
  for(i in fmz$REGION) mapSelect <- mapSelect %>%
         addPolygons(data=subset(fmz, REGION==i), stroke=TRUE, fillOpacity=0, weight=1, color="black", group="not_selected", layerId=i, label=i,
                     highlightOptions=highlightOptions(opacity=1, weight=2, fillOpacity=0, bringToFront=FALSE, sendToBack=FALSE))
  output$Map <- renderLeaflet(mapSelect)
  outputOptions(output ,"Map", suspendWhenHidden=FALSE)
  
  d1 <- reactive({
    filter(d, GBM %in% input$gbms & RCP %in% input$rcps & Model %in% input$gcms & Region %in% input$regions &
             Vegetation %in% input$veg & Year >= input$yrs[1] & Year <= input$yrs[2]) %>%
      select_(.dots=c("GBM", "RCP", "Model", "Region", "Var", "Vegetation", "Year", input$stat))
  })
  
  selected_var <- reactive({
    switch(input$tabs, "burnarea"="Burn Area", "firefreq"="Fire Count", "firesize"="Fire Size", "vegarea"="Vegetated Area", "vegage"="Vegetation Age")
  })
  
  dsub <- reactive({ filter(d1(), Var==selected_var()) %>% droplevels })

  map(tab_ids, ~callModule(dbmod, paste0("mod1_", .x), data=dsub, variable=selected_var(), stat=input$stat,
                           alpha=reactive(input$settings_alpha), showLines=reactive(input$settings_showLines),
                           jitterPoints=reactive(input$settings_jitterPoints), facetScales=reactive(input$facet_scales)))
  
})

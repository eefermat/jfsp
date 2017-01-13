library(rgdal)
library(lazyeval)
library(dplyr)
library(purrr)
library(ggplot2)

lon <- -155
lat <- 65
tab_ids <- c("burnarea", "firefreq", "firesize", "vegarea", "vegage")
mods <- paste0("mod_", tab_ids)

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
      select_(.dots=c("GBM", "RCP", "Model", "Region", "Var", "Vegetation", "Year", input$stat)) %>% split(.$Var) %>%
      map(~droplevels(.x))
  })
  
  #selected_var <- reactive({
  #  switch(input$tabs, "burnarea"="Burn Area", "firefreq"="Fire Count", "firesize"="Fire Size", "vegarea"="Vegetated Area", "vegage"="Vegetation Age")
  #})
  
  #dsub <- reactive({ filter(d1(), Var==selected_var()) %>% droplevels })

  #map(tab_ids, ~callModule(dbmod, paste0("mod1_", .x), data=dsub, variable=selected_var(), stat=input$stat,
  #                         alpha=reactive(input$settings_alpha), showLines=reactive(input$settings_showLines),
  #                         jitterPoints=reactive(input$settings_jitterPoints), facetScales=reactive(input$facet_scales)))
  
  d_ba <- reactive({ d1()[["Burn Area"]] })
  d_fc <- reactive({ d1()[["Fire Count"]] })
  d_fs <- reactive({ d1()[["Fire Size"]] })
  d_v <- reactive({ d1()[["Vegetated Area"]] })
  d_a <- reactive({ d1()[["Vegetation Age"]] })
  
  callModule(dbmod, mods[1], data=d_ba)
  callModule(dbmod, mods[2], data=d_fc)
  callModule(dbmod, mods[3], data=d_fs)
  callModule(dbmod, mods[4], data=d_v)
  callModule(dbmod, mods[5], data=d_a)
})

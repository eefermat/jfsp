# Map-related observers

# initial addition of background polygon region outlines after map is created
observe({
  walk(fmz$REGION, ~leafletProxy("Map") %>%
         addPolygons(data=subset(fmz, REGION==.x), stroke=TRUE, fillOpacity=0, weight=1, color="black", group="not_selected", layerId=.x, label=.x,
                     highlightOptions=highlightOptions(opacity=1, weight=2, fillOpacity=0, bringToFront=FALSE, sendToBack=FALSE)))
})

# observe if show/hide flammability mask is checked
observeEvent(input$flammable, {
  proxy <- leafletProxy("Map")
  if(!input$flammable){
    proxy %>% hideGroup("flammable")
  } else { # hiding and re-showing all layers maintains necessary layer order for map clicks
    proxy %>% hideGroup("selected") %>% hideGroup("not_selected") %>%
      showGroup("flammable") %>% showGroup("not_selected") %>% showGroup("selected")
  }
})

# observe region selectInput and update map polygons
observeEvent(input$regions, {
  x <- input$regions
  proxy <- leafletProxy("Map")
  not_selected <- setdiff(regions, x)
  if(length(not_selected)) walk(not_selected, ~proxy %>% removeShape(layerId=paste0("selected_", .x)))
  walk(x, ~proxy %>%
         addPolygons(data=subset(fmz, REGION==.x),
                     stroke = TRUE, fillOpacity=0.2, weight=1, group="selected", layerId=paste0("selected_", .x)))
}, ignoreNULL=FALSE)

# observe map shape click and add or remove selected polygons and update region selectInput
observeEvent(input$Map_shape_click, {
  p <- input$Map_shape_click$id
  x <- input$regions
  p1 <- strsplit(p, "_")[[1]][2]
  proxy <- leafletProxy("Map")
  
  if(substr(p, 1, 9)=="selected_"){
    proxy %>% removeShape(layerId=p)
  } else {
    proxy %>% addPolygons(data=subset(fmz, REGION==p),
                          stroke=TRUE, fillOpacity=0.2, weight=1,
                          group="selected", layerId=paste0("selected_", p))
  }
  
  if(!is.null(p)){
    if(is.na(p1) && (is.null(x) || !p %in% x)){
      updateSelectInput(session, "regions", selected=c(x, p))
    } else if(!is.na(p1) && p1 %in% x){
      updateSelectInput(session, "regions", selected=x[x!=p1])
    }
  }
})

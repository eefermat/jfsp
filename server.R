lon <- -155
lat <- 65
tab_ids <- c("burnarea", "firefreq", "firesize", "vegarea", "vegage")
mods <- paste0("mod_", tab_ids)

shinyServer(function(input, output, session) {
  
  source("observers.R", local=TRUE) # map and region selectInput observers
  source("tour.R", local=TRUE) # introjs tour
  
  # Initialize map and add flammability polygon layer
  mapSelect <- reactive({
    ptm <- proc.time()
    cat("Leaflet initialization time excluding renderLeaflet:\n")
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Generating zone map", value=0)
    z <- fmz$REGION
    n <- 1 + length(z)
    x <- leaflet() %>% addTiles() %>% setView(lon, lat, 4) %>%
      addPolygons(data=flam, stroke=TRUE, fillOpacity=0.2, weight=1, color="red", group="flammable")
    progress$inc(1/n, detail="Basemap built")
    #Sys.sleep(1)
    # Add background polygon region outlines after map is created
    for(i in seq_along(z)){
      x <- x %>% addPolygons(data=subset(fmz, REGION==z[i]), stroke=TRUE, fillOpacity=0, weight=1,
        color="black", group="not_selected", layerId=z[i], label=names(regions)[i],
        highlightOptions=highlightOptions(opacity=1, weight=2, fillOpacity=0, 
          bringToFront=FALSE, sendToBack=FALSE))
      progress$inc((i+1)/n, detail=paste("Adding polygon", i))
    }
    print(proc.time() - ptm)
    x
  })
  
  output$Map <- renderLeaflet(mapSelect())
  outputOptions(output ,"Map", suspendWhenHidden=FALSE)
  
  metric <- reactive(input$metric=="Metric")
  
  d1 <- reactive({withProgress({
    i <- list(input$rcps, input$gcms, input$regions, input$veg, input$yrs, input$stat)
    dots <- c("RCP", "Model", "Region", "Var", "Vegetation", "Year", i[[6]])
    if(any(sapply(i, is.null))){
      x <- slice(d, 0) 
    } else {
      getSubset <- function(x) filter(x, 
        RCP %in% c("Historical", i[[1]]) & Model %in% c("CRU 3.2", i[[2]]) &
          Region %in% i[[3]] & Vegetation %in% i[[4]] &
          Year >= i[[5]][1] & Year <= i[[5]][2]) %>%
        select_(.dots=dots)
      
      x <- getSubset(d)
      if(i[[5]][1] < 2014) x <- bind_rows(getSubset(h), x)
    }
    if(nrow(x) > 0 && !metric()){
      x <- mutate_(x, Converted=lazyeval::interp(
        ~ifelse(Var %in% c("Burn Area", "Fire Size", "Vegetated Area"), as.integer(round(247.105*x)), x), 
        x=as.name(i[[6]]))) %>% select_(i[[6]]) %>% rename_(.dots=setNames("Converted", i[[6]]))
    }
    x %>% split(.$Var) %>% map(~droplevels(.x))
    }, message="Subsetting data...", value=1)
  })
  
  d1sum <- reactive({ map(d1(), ~summary(.x)) })
  
  output$filtered_data <- DT::renderDataTable({
    DT::datatable(suppressWarnings(bind_rows(d1())), options=list(
      lengthMenu=list(c(5, 10, 25), c('5', '10', '25')), pageLength=5, searching=FALSE))
  })
  
  output$filtered_ba <- renderPrint({ d1sum()[["Burn Area"]] })
  output$filtered_fc <- renderPrint({ d1sum()[["Fire Count"]] })
  output$filtered_fs <- renderPrint({ d1sum()[["Fire Size"]] })
  output$filtered_v <- renderPrint({ d1sum()[["Vegetated Area"]] })
  output$filtered_a <- renderPrint({ d1sum()[["Vegetation Age"]] })
  
  observe({
    x <- NULL
    success <- "Explore fire and vegetation tabs..."
    if(is.null(input$rcps)) x <- "RCP selection missing"
    if(is.null(input$gcms)) x <- "Model selection missing"
    if(is.null(input$regions)) x <- "Fire Mgmt Zone selection missing"
    if(is.null(input$veg)) x <- "Vegetation selection missing"
    if(is.null(x)){
      toastr_success(title="Data subset updated", success, timeOut=2500, preventDuplicates=TRUE)
    } else {
      toastr_error(title="Empty data set", x, timeOut=2500, preventDuplicates=TRUE)
    }
  })
  
  d_ba <- reactive({ d1()[["Burn Area"]] })
  d_fc <- reactive({ d1()[["Fire Count"]] })
  d_fs <- reactive({ d1()[["Fire Size"]] })
  d_v <- reactive({ d1()[["Vegetated Area"]] })
  d_a <- reactive({ d1()[["Vegetation Age"]] })
  
  callModule(mainMod, mods[1], data=d_ba, metric=metric)
  callModule(mainMod, mods[2], data=d_fc, metric=metric)
  callModule(mainMod, mods[3], data=d_fs, metric=metric)
  callModule(mainMod, mods[4], data=d_v, metric=metric)
  callModule(mainMod, mods[5], data=d_a, metric=metric)
})

lon <- -155
lat <- 65
tab_ids <- c("burnarea", "firefreq", "firesize", "vegarea", "vegage")
mods <- paste0("mod_", tab_ids)

shinyServer(function(input, output, session) {
  
  mapset_workspace <- reactive({
    if(is.null(input$mapset) || input$mapset=="AK") return()
    file.path(dataloc, "mapData.RData")
  })
  
  rv <- reactiveValues(d=d, h=h, regions=regions, shp=shp)
  
  load_file <- function(file, source="local"){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    if(source=="local"){
      progress$set(message="Loading local data", value=1)
      load(file, envir=environment())
      progress$set(message="Loading local data", value=2)
    }
    if(source=="aws"){
      progress$set(message="Fetching AWS data", value=1)
      s3load(mapset_workspace(), envir=environment())
      progress$set(message="Fetching AWS data", value=2)
    }
    rv[["d"]] <- d
    rv[["h"]] <- h
    rv[["regions"]] <- regions
    rv[["shp"]] <- shp
  }
  
  updateData <- reactive({
    local_cache <- paste0("mapset_data_", input$mapset)
    if(is.null(mapset_workspace())){
      rv[["d"]] <- d
      rv[["h"]] <- h
      rv[["regions"]] <- regions
      rv[["shp"]] <- shp
    } else if(exists(local_cache, envir=.GlobalEnv)){
      rv[["d"]] <- get(local_cache, envir=.GlobalEnv)$d
      rv[["h"]] <- get(local_cache, envir=.GlobalEnv)$h
      rv[["regions"]] <- get(local_cache, envir=.GlobalEnv)$regions
      rv[["shp"]] <- get(local_cache, envir=.GlobalEnv)$shp
    } else {
      load_file(mapset_workspace(), source="aws")
      assign(local_cache, list(d=rv$d, h=rv$h, regions=rv$regions, shp=rv$shp), envir=.GlobalEnv)
    }
  })
  observe({ updateData() })
  
  mapset_labs <- reactive({ names(mapsets)[match(input$mapset, mapsets)] })
  
  output$mapset_regions <- renderUI({
    reg <- rv$regions
    mult <- FALSE
    if(input$mapset!="AK"){
      reg <- c("", reg)
      mult <- TRUE
    }
    if(length(mapset_labs()))
      selectInput("regions", mapset_labs(), choices=reg, selected=reg[1], multiple=mult, width="100%")
  })
  
  source("observers.R", local=TRUE) # map and region selectInput observers
  source("tour.R", local=TRUE) # introjs tour
  
  # Initialize map and add flammability polygon layer
  mapSelect <- reactive({
    ptm <- proc.time()
    cat("Leaflet initialization time excluding renderLeaflet:\n")
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message="Generating zone map", value=0)
    ak <- input$mapset=="AK"
    z <- if(ak) "Alaska" else rv$shp$REGION
    n <- 1 + length(z)
    x <- leaflet() %>% addTiles() %>% setView(lon, lat, 4) %>%
      addPolygons(data=flam, stroke=TRUE, fillOpacity=0.2, weight=1, color="red", group="flammable")
    progress$inc(1/n, detail="Basemap built")
    # Add background polygon region outlines after map is created
    if(!ak){
      for(i in seq_along(z)){
        x <- x %>% addPolygons(data=subset(rv$shp, REGION==z[i]), stroke=TRUE, fillOpacity=0, weight=1,
          color="black", group="not_selected", layerId=z[i], label=names(rv$regions)[i],
          highlightOptions=highlightOptions(opacity=1, weight=2, fillOpacity=0, 
            bringToFront=FALSE, sendToBack=FALSE))
        progress$inc((i+1)/n, detail=paste("Adding polygon", i))
      }
    }
    print(proc.time() - ptm)
    x
  })
  
  output$Map <- renderLeaflet(mapSelect())
  outputOptions(output ,"Map", suspendWhenHidden=FALSE)
  
  metric <- reactive({ input$metric=="Metric" })
  axis_scale <- reactive({ as.numeric(substring(input$area_axis_scale, 2)) })
  i <- reactive({ list(rcps=input$rcps, gcms=input$gcms, reg=input$regions,
                       veg=input$veg, yrs=input$yrs, stat=input$stat,
                       reg.names=names(rv$regions)[match(input$regions, rv$regions)]) 
  })
  noData <- reactive({ any(sapply(i(), is.null)) || is.null(rv$d) })
  
  d1 <- reactive({withProgress({
    dots <- c("RCP", "Model", "Region", "Var", "Vegetation", "Year", i()[[6]])
    if(noData()){
      x <- slice(d, 0) 
    } else {
      getSubset <- function(x) filter(x, 
        RCP %in% c("Historical", i()[[1]]) & Model %in% c("CRU 3.2", i()[[2]]) &
          Region %in% i()[[3]] & Vegetation %in% i()[[4]] &
          Year >= i()[[5]][1] & Year <= i()[[5]][2]) %>%
        select_(.dots=dots)
      
      x <- getSubset(rv$d)
      if(i()[[5]][1] < 2014) x <- bind_rows(getSubset(rv$h), x)
    }
    
    if(nrow(x) > 0 && !metric()){
      area.vars <- c("Burn Area", "Fire Size", "Vegetated Area")
      x <- mutate_(x, Converted=lazyeval::interp(
        ~ifelse(Var %in% area.vars, as.integer(round(247.105*x)), x), x=as.name(i()[[6]]))) %>%
        select_(.dots=paste0("-", i()[[6]])) %>% rename_(.dots=setNames("Converted", i()[[6]]))
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
    input$rcps; input$gcms; input$regions; input$veg
    isolate({
      if(is.null(input$rcps)) x <- "RCP selection missing"
      if(is.null(input$gcms)) x <- "Model selection missing"
      if(!is.null(input$mapset) && input$mapset!="AK" && is.null(input$regions)) 
        x <- paste("No", tolower(mapset_labs()), "selected")
      if(is.null(input$veg)) x <- "Vegetation selection missing"
      if(is.null(x) && all(input$regions %in% rv$regions)){
        toastr_success(title="Data subset updated", success, timeOut=2500, preventDuplicates=TRUE)
      } else {
        toastr_error(title="Empty data set", x, timeOut=2500, preventDuplicates=TRUE)
      }
    })
  })
  
  d_ba <- reactive({ d1()[["Burn Area"]] })
  d_fc <- reactive({ d1()[["Fire Count"]] })
  d_fs <- reactive({ d1()[["Fire Size"]] })
  d_v <- reactive({ d1()[["Vegetated Area"]] })
  d_a <- reactive({ d1()[["Vegetation Age"]] })
  
  report_ba <- callModule(mainMod, mods[1], data=d_ba, inp=i, metric=metric, axis_scale=axis_scale)
  report_fc <- callModule(mainMod, mods[2], data=d_fc, inp=i, metric=metric, axis_scale=axis_scale)
  report_fs <- callModule(mainMod, mods[3], data=d_fs, inp=i, metric=metric, axis_scale=axis_scale)
  report_v <- callModule(mainMod, mods[4], data=d_v, inp=i, metric=metric, axis_scale=axis_scale)
  report_a <- callModule(mainMod, mods[5], data=d_a, inp=i, metric=metric, axis_scale=axis_scale)
  
  output$download_report <- renderUI({
    if(!input$tabs %in% tab_ids || noData()) return()
    allow_report <- switch(input$tabs,
      "burnarea"=report_ba(), "firefreq"=report_fc(), "firesize"=report_fs(),
      "vegarea"=report_v(), "vegage"=report_a())
    if(!allow_report) return()
    btn <- paste0("mod_", input$tabs, "-report")
    tip <- "Download a customized fact sheet based on currently selected data and displayed plots. See the FAQ section under the Information tab above for details."
    
    tagList(
      hr(style="margin: 15px;"),
      fluidRow(
        column(12,
          p("Download custom reports.", style="text-align: justify; margin: 0px 15px 0px 15px;"),
          tipify(downloadButton(btn, "Fact sheet", style=action_btn_style), tip, placement="right", options=list(container="body"))
        )
      )
    )
  })
})

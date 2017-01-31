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
    getSubset <- function(x) filter(x, 
      RCP %in% c("Historical", input$rcps) & Model %in% c("CRU 3.2", input$gcms) &
        Region %in% input$regions & Vegetation %in% input$veg &
        Year >= input$yrs[1] & Year <= input$yrs[2]) %>%
      select_(.dots=c("RCP", "Model", "Region", "Var", "Vegetation", "Year", input$stat))
    x <- getSubset(d)
    if(input$yrs[1] < 2014) x <- bind_rows(getSubset(h), x)
    x %>% split(.$Var) %>% map(~droplevels(.x))
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
  
  callModule(mainMod, mods[1], data=d_ba)
  callModule(mainMod, mods[2], data=d_fc)
  callModule(mainMod, mods[3], data=d_fs)
  callModule(mainMod, mods[4], data=d_v)
  callModule(mainMod, mods[5], data=d_a)
})

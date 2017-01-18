mainModUI <- function(id, tab_name){
  ns <- NS(id)
  tabItem(tabName=tab_name,
    fluidRow(
      column(8, tsModUI(id=ns("mod_ts"),
        titles=c("Raw Observations", "Cumulative"), values=c("annual", "cumulative"), main="Annual time series", mouselog=TRUE)),
      column(4, denModUI(id=ns("mod_den"),
        titles=c("Density", "Histogram"), values=c("density", "histogram"), main="Aggregate distribution", mouselog=TRUE))
    ),
    fluidRow(
      decModUI(id=ns("mod_dec"),
        titles=c("Observations", "Averages"), values=c("dec_boxplot", "dec_barplot"), main="Decadal change", mouselog=TRUE))
  )
}

mainMod <- function(input, output, session, data){
  ns <- session$ns
  callModule(tsMod, "mod_ts", data=data)
  callModule(denMod, "mod_den", data=data)
  callModule(decMod, "mod_dec", data=data)
}

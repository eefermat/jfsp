mainModUI <- function(id, tab_name){
  ns <- NS(id)
  tabItem(tabName=tab_name,
    fluidRow(
      column(8, tsModUI(id=ns("mod_ts"))),
      column(4, denModUI(id=ns("mod_den")))
    ),
    fluidRow(decModUI(id=ns("mod_dec")))
  )
}

mainMod <- function(input, output, session, data){
  ns <- session$ns
  callModule(tsMod, "mod_ts", data=data)
  callModule(denMod, "mod_den", data=data)
  callModule(decMod, "mod_dec", data=data)
}

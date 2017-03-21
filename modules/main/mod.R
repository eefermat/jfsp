mainModUI <- function(id, tab_name){
  ns <- NS(id)
  main <- c("Annual time series", "Aggregate distribution", "Decadal change")
  tabItem(tabName=tab_name,
    fluidRow(
      tsModUI(id=ns("mod_ts"), type="plot", titles=c("Raw Observations", "Cumulative"),
              values=c("annual", "cumulative"), main=main[1], width=8),
      denModUI(id=ns("mod_den"), type="plot", titles=c("Density", "Histogram"),
               values=c("density", "histogram"), main=main[2], width=4)
    ),
    fluidRow(
      decModUI(id=ns("mod_dec"), type="plot", titles=c("Observations", "Averages"),
               values=c("dec_boxplot", "dec_barplot"), main=main[3])),
    fluidRow(
      box(
        tabBox(
          tabPanel(main[3], decModUI(id=ns("mod_dec"), type="inputs", main=main[3]), value=ns("decadal")),
          tabPanel(main[2], denModUI(id=ns("mod_den"), type="inputs", main=main[2]), value=ns("aggregate")),
          tabPanel(main[1], tsModUI(id=ns("mod_ts"), type="inputs", main=main[1]), value=ns("annual")),
          ns(id="tb_inputs"), selected=ns("annual"), title="Plot-specific controls and options", width=12, side="right"),
        title="Plot settings", status="primary", solidHeader=TRUE, width=12, collapsible=TRUE, collapsed=TRUE
      )
    )
  )
}

mainMod <- function(input, output, session, data, metric, axis_scale){
  ns <- session$ns
  callModule(tsMod, "mod_ts", data=data, metric=metric, axis_scale=axis_scale)
  callModule(denMod, "mod_den", data=data, metric=metric, axis_scale=axis_scale)
  callModule(decMod, "mod_dec", data=data, metric=metric, axis_scale=axis_scale)
}

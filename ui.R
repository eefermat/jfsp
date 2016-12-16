navbarPage(theme="http://bootswatch.com/spacelab/bootstrap.css", inverse=TRUE,
           title=HTML("Alfreesco output"),#HTML('<div><a href="http://snap.uaf.edu" target="_blank"><img src="./img/SNAP_acronym_100px.png" width="100%"></a></div>'),
           windowTitle="JFSP",
           collapsible=TRUE,
           id="nb",
           tabPanel("Plots", value="vis",
                    #tags$head(includeScript("ga-nwtapp.js"), includeScript("ga-allapps.js")),
                    bsModal("modal_data", "Data filter", "btn_modal_data", size = "large",
                            fluidRow(
                              column(6, selectInput("rcps", "RCP", choices=rcps, selected=rcps, multiple=TRUE, width="100%")),
                              column(6, sliderInput("yrs", "Years", min=period[1], max=period[2], value=period, step=1, sep="", width="100%"))
                            ),
                            selectInput("gcms", "GCM", choices=gcms, selected=gcms, multiple=TRUE, width="100%"),
                            selectInput("regions", "Region", choices=regions, selected="AK", multiple=TRUE, width="100%"),
                            selectInput("veg", "Vegetation", choices=veg, selected=veg, multiple=TRUE, width="100%"),
                            fluidRow(
                              column(6, selectInput("vars", "Variable", choices=variables, selected=variables[1], width="100%")),
                              column(6, selectInput("stat", "Stat", choices=stats, selected="Mean", width="100%"))
                            )
                    ),
                    bsModal("modal_settings", "Settings", "btn_modal_settings", size = "large",
                            h4("Plot interaction"),
                            fluidRow(
                              column(6, checkboxInput("settings_allRows", "Show all table rows on observation selection", FALSE, width="100%")),
                              column(6, checkboxInput("settings_clickExclude", "Clicks exclude observations", FALSE, width="100%"))
                            )
                    ),
                    fluidRow(
                      column(4, h4("Testing")),
                      column(4, actionButton("btn_modal_data", "Data filter", class="btn-block")),
                      column(4, actionButton("btn_modal_settings", "Settings", class="btn-block"))
                    ),
                    plotOutput("plot1", click="plot1_click", dblclick="plot1_dblclick", hover="plot1_hover",
                                        brush=brushOpts(id="plot1_brush", resetOnNew=TRUE)),
                    br(),
                    fluidRow(
                      column(4, h4("Selected observations")),
                      column(4, actionButton("exclude_toggle", "Toggle points", class="btn-block")),
                      column(4, actionButton("exclude_reset", "Reset", class="btn-block"))
                    ),
                    br(),
                    verbatimTextOutput("info"),
                    verbatimTextOutput("info2")
           )
)
                                       
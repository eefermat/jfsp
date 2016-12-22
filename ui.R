dashboardPage(
  dashboardHeader(
    title=HTML("Alfresco output")#HTML('<div><a href="http://snap.uaf.edu" target="_blank"><img src="./img/SNAP_acronym_100px.png" width="100%"></a></div>'),
    #tags$head(includeScript("ga-nwtapp.js"), includeScript("ga-allapps.js")),
  ),
  dashboardSidebar(
    fluidRow(
      column(12, align="center",
        actionButton("btn_modal_data", "Data filter", icon("paper-plane"),
          style=action_btn_style, class="btn-flat action-button btn-block"),
        actionButton("btn_modal_settings", "Settings", icon("paper-plane"), 
          style=action_btn_style, class="btn-flat action-button btn-block")
      )
    ),
    selectInput("gcms2", "GCM", choices=gcms, selected=gcms, multiple=TRUE, width="100%"),
    sidebarMenu(
      id="tabs",
      menuItem("Plot", tabName="main_plot", icon=icon("dashboard")),
      menuItem("Tab2", icon = icon("th"), tabName="tab2", badgeLabel="new",
               badgeColor="green"),
      menuItem("Tab3", icon = icon("bar-chart-o"),
               menuSubItem("Sub-item 1", tabName="subitem1"),
               menuSubItem("Sub-item 2", tabName="subitem2")
      )
    )
  ),
  dashboardBody(
    bsModal("modal_data", "Data filter", "btn_modal_data", size = "large",
            fluidRow(
              column(3, selectInput("gbms", "GBM", choices=gbms, selected=gbms, multiple=TRUE, width="100%")),
              column(3, selectInput("rcps", "RCP", choices=rcps, selected=rcps, multiple=TRUE, width="100%")),
              column(6, sliderInput("yrs", "Years", min=period[1], max=period[2], value=period, step=1, sep="", width="100%"))
            ),
            selectInput("gcms", "GCM", choices=gcms, selected=gcms, multiple=TRUE, width="100%"),
            selectInput("regions", "Region", choices=regions, selected="", multiple=TRUE, width="100%"),
            selectInput("veg", "Vegetation", choices=veg, selected=veg, multiple=TRUE, width="100%"),
            fluidRow(
              column(6, selectInput("vars", "Variable", choices=variables, selected=variables[1], width="100%")),
              column(6, selectInput("stat", "Stat", choices=stats, selected="Mean", width="100%"))
            )
    ),
    bsModal("modal_settings", "Settings", "btn_modal_settings", size = "large",
            h4("Map display"),
            fluidRow(
              column(6, checkboxInput("flammable", "Show flammable region", TRUE, width="100%"))
            ),
            h4("Plot interaction"),
            fluidRow(
              column(6, selectInput("facet_scales", "Axis scales", choices=axis_scales, selected="fixed", width="100%"))
            ),
            fluidRow(
              column(6, checkboxInput("settings_allRows", "Show all table rows on observation selection", FALSE, width="100%")),
              column(6, checkboxInput("settings_clickExclude", "Clicks exclude observations", FALSE, width="100%"))
            )
    ),
    leafletOutput("Map", width="100%"),
    tabItems(
      dbmodUI(id="mod1", tab_name="main_plot"),
      tabItem("tab2",
              "Tab2 content"
      ),
      tabItem("subitem1",
              "Tab3 Sub-item 1 content"
      ),
      tabItem("subitem2",
              "Tab3 Sub-item 2 content"
      )
    )
  ),
  title="JFSP"
)
                                       
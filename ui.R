faq <- source("faq.R", local=TRUE)[[1]]

function(request){
  dashboardPage(
    dashboardHeader(
      title="JFSP Alfresco",
      tags$li(class="dropdown",
        tags$a(href="http://snap.uaf.edu", target="_blank",
          tags$img(src="SNAP_acronym_100px.png", width="100%", alt="SNAP"), style="padding: 10px; margin: 0px;"))
      #tags$head(includeScript("ga-nwtapp.js"), includeScript("ga-allapps.js")),
    ),
    dashboardSidebar(
      useToastr(),
      introjsUI(),
      #tags$style(HTML(".myijs { min-width: 800px; max-width: 800px; }")),
      fluidRow(column(12, p("Select data, then explore.", style="text-align: justify; margin: 15px;"))),
      sidebarMenu(
        id="tabs",
        menuItem("Data", icon=icon("sliders"), tabName="data"),
        menuItem("Fire", icon=icon("fire", lib="glyphicon"),
          menuSubItem("Burn area", tabName="burnarea"),
          menuSubItem("Fire count", tabName="firefreq"),
          menuSubItem("Fire size", tabName="firesize")
        ),
        menuItem("Vegetation", icon=icon("tree"), badgeColor="green",
          menuSubItem("Cover area", tabName="vegarea"),
          menuSubItem("Stand age", tabName="vegage")
        ),
        menuItem("Information", icon=icon("info-circle"), tabName="info")
      ),
      conditionalPanel("output.Map != null",
        actionButton("help", "Take tour", style=action_btn_style, icon=icon("question-circle")),
        #bookmarkButton(style=action_btn_style)
        actionButton("fake", "Bookmark", style=action_btn_style, icon=icon("link")), # placeholder
        bsTooltip("help", "Note: Tour incomplete, under development.", placement="right", options=list(container="body")),
        bsTooltip("fake", "Note: Server-side bookmarking not yet available.", placement="right", options=list(container="body"))
      ),
      hr(style="margin: 15px;"),
      fluidRow(
        column(12, 
          p("Global / semi-global options.", style="text-align: justify; margin: 0px 15px 0px 15px;"),
          selectInput("metric", "Units", c("US", "Metric"), width="100%"),
          selectInput("area_axis_scale", "Axis scale", c("x1", "x100", "x1000"), width="100%")
        )
      ),
      bsTooltip("metric", "Units apply to all data.", placement="right", options=list(container="body")),
      bsTooltip("area_axis_scale", "Scale affects plot axes for burn area, fire size and vegetation cover area.",
        placement="right", options=list(container="body")),
      uiOutput("download_report")
    ),
    dashboardBody(
      includeCSS("www/styles.css"),
      bsModal("staticmap", "Fire Management Zones", "btn_staticmap", size="large",
        img(src='Fire_Mgmt_Areas.png', align="center", style="width: 100%")
      ),
      tabItems(
        tabItem(tabName="data",
          fluidRow(column(12, temptext)),
          fluidRow(
            column(6,
              div(id="plot-container",
                leafletOutput("Map", width="100%", height="500px"),
                conditionalPanel("output.Map == null", 
                  h4("Loading map", style="position: absolute; left: 0; top: 35%; right: 0; text-align: center;"),
                  tags$img(src="spinner.gif", id="loading-spinner")
                )
              )
            ),
            column(6,
              selectInput("regions", "Fire Mgmt Zones", choices=regions, selected="", multiple=TRUE, width="100%"),
              selectInput("veg", "Vegetation", choices=veg, selected="Black Spruce", multiple=TRUE, width="100%"),
              selectInput("gcms", "GCM", choices=gcms, selected=gcms, multiple=TRUE, width="100%"),
              fluidRow(
                column(6, selectInput("rcps", "RCP", choices=rcps, selected=rcps, multiple=TRUE, width="100%")),
                column(6, selectInput("stat", "Simulations", choices=stats, selected="Mean", width="100%"))
              ),
              sliderInput("yrs", "Years", min=period[1], max=period[2], value=c(2014, 2099), step=1, sep="", width="100%"),
              fluidRow(
                column(4, actionButton("btn_staticmap", "Detailed map", class="btn-block", icon("globe"))),
                column(8,
                  conditionalPanel("output.Map != null",
                    checkboxInput("flammable", "Show flammable region", TRUE, width="100%"))
                )
                
              )
            )
          ),
          br(),
          fluidRow(
            box(title="", status="primary", width=12, height=0),
            tabBox(
              tabPanel("Stand age", verbatimTextOutput("filtered_a"), icon=icon("tree")),
              tabPanel("Cover area", verbatimTextOutput("filtered_v"), icon=icon("tree")),
              tabPanel("Fire size", verbatimTextOutput("filtered_fs"), icon=icon("fire", lib="glyphicon")),
              tabPanel("Fire count", verbatimTextOutput("filtered_fc"), icon=icon("fire", lib="glyphicon")),
              tabPanel("Burn area", verbatimTextOutput("filtered_ba"), icon=icon("fire", lib="glyphicon")),
              tabPanel("Full table", div(DT::dataTableOutput("filtered_data"), style="font-size: 100%"), icon=icon("sliders")),
              id="summary", selected="Full table", title="Full table subset and summaries", width=12, side="right"
            )
          )
        ),
        mainModUI(id="mod_burnarea", tab_name="burnarea"),
        mainModUI(id="mod_firefreq", tab_name="firefreq"),
        mainModUI(id="mod_firesize", tab_name="firesize"),
        mainModUI(id="mod_vegarea", tab_name="vegarea"),
        mainModUI(id="mod_vegage", tab_name="vegage"),
        tabItem(tabName="info",
          h2("Frequently asked questions"),
          faq,
          h2("Contact information"),
          p("For questions about this application, please email paul.duffy@neptuneinc.org")
        )
      )
    ),
    title="JFSP"
  )
}

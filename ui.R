function(request){
  dashboardPage(
    dashboardHeader(
      title="JFSP Alfresco",
      tags$li(class="dropdown",
              tags$a(href="http://snap.uaf.edu", target="_blank",
                     tags$img(src="SNAP_acronym_100px.png", width="100%", alt="SNAP"), style="margin: 10px; padding: 0px;")
      )
      #tags$head(includeScript("ga-nwtapp.js"), includeScript("ga-allapps.js")),
    ),
    dashboardSidebar(
      actionButton("btn_modal_data", "Data filter", icon("paper-plane"),
        style=action_btn_style, class="btn-flat action-button btn-block"),
      sidebarMenu(
        id="tabs",
        #menuItem("Data", icon=icon("th"), tabName="data"),
        menuItem("Fire", icon=icon("dashboard"),
          menuSubItem("Burn area", tabName="burnarea"),
          menuSubItem("Fire count", tabName="firefreq"),
          menuSubItem("Fire size", tabName="firesize")
        ),
        menuItem("Vegetation", icon=icon("dashboard"),
          menuSubItem("Cover area", tabName="vegarea"),
          menuSubItem("Stand age", tabName="vegage")
        )
      ),
      bookmarkButton(style=action_btn_style, class="btn-flat action-button btn-block")
    ),
    dashboardBody(
      includeCSS("www/styles.css"),
      bsModal("modal_data", "Data filter", "btn_modal_data", size="large",
              leafletOutput("Map", width="100%"),
              fluidRow(
                column(3, selectInput("gbms", "GBM", choices=gbms, selected=gbms, multiple=TRUE, width="100%")),
                column(3, selectInput("rcps", "RCP", choices=rcps, selected=rcps, multiple=TRUE, width="100%")),
                column(6, sliderInput("yrs", "Years", min=period[1], max=period[2], value=period, step=1, sep="", width="100%"))
              ),
              selectInput("gcms", "GCM", choices=gcms, selected=gcms, multiple=TRUE, width="100%"),
              selectInput("regions", "Region", choices=regions, selected="", multiple=TRUE, width="100%"),
              selectInput("veg", "Vegetation", choices=veg, selected=veg, multiple=TRUE, width="100%"),
              fluidRow(
                column(6, selectInput("stat", "Stat", choices=stats, selected="Mean", width="100%")),
                column(6, checkboxInput("flammable", "Show flammable region", TRUE, width="100%"))
              )
      ),
      tabItems(
        dbmodUI(id="mod_burnarea", tab_name="burnarea"),
        dbmodUI(id="mod_firefreq", tab_name="firefreq"),
        dbmodUI(id="mod_firesize", tab_name="firesize"),
        dbmodUI(id="mod_vegarea", tab_name="vegarea"),
        dbmodUI(id="mod_vegage", tab_name="vegage")
      )
    ),
    title="JFSP"
  )
}

# calback convenience functions
stepEquals <- function(i) paste0("this._currentStep==", i-1, collapse=" || ")

dv <- function(x, quote=TRUE){
  x <- paste0("a[data-value=\"", x, "\"]")
  if(quote) x <- paste0("'", x, "'")
  x
}

rmClass <- function(x) paste0(paste0(
  "$(", dv(x), ").removeClass('active');", collapse="\n"), "\n")

goClass <- function(x){
  if(length(x) > 1) stop("Only add and trigger one class at a time.")
  paste0("$(", dv(x), ").addClass('active');\n$(", dv(x), ").trigger('click');\n")
}

ttWidth <- function(steps, minWidth, maxWidth, class="myijs"){ # not functional
  n <- sapply(list(steps, minWidth, maxWidth), length)
  stperr <- "length must be 1 or number of steps"
  if(n[2] != n[1] & n[2] != 1) stop(paste("'minWidth'", stperr))
  if(n[3] != n[1] & n[3] != 1) stop(paste("'maxWidth'", stperr))
  if(n[2]==1) minWidth <- rep(minWidth, n[1])
  if(n[3]==1) maxWidth <- rep(maxWidth, n[1])
  paste0("if (targetElement.getAttribute(\"data-step\")===\"", steps-1, 
         "\") { $(\".", class, "\").css(\"max-width\", \"",
         maxWidth, "\").css(\"min-width\",\"", minWidth, "\"); }", collapse="\n")
}

stepcb <- function(condition, action){
  paste0("if (", condition, ") {", paste0(action, collapse="\n"), "}")
}

# tour steps
tour.text <- c(
  "01"="Welcome to the JFSP ALFRESCO fire simulations data exploration and analysis app.
  Begin with the data tab. Here you can select variables and subset a data table
  of ALFRESCO model outputs based on your selections to focus on the data you are most interested.
  Then explore and compare your data using graphs, tables and various summary statistics.",
  "02"="The data tab is shown at launch, but if you leave the tab, use the sidebar to find it
  any time you want to update your data selections.",
  "03"="An interactive map allows you to select one or more Alaska fire management zones of interest.
  No default zone is selected when you launch the app so there are no data to display.
  Any categorical variable with none of its levels selected will result in an empty data table.
  Most variables have sensible defaults but fire management zone is left blank for you to fill.
  There are many zones so selecting them all is impractical. Additionally, some of the southernmost
  zones have only vegetation cover area and stand age data, but no fire data because ALFRESCO
  fire simulations are not run that far south.",
  "04"="A semi-transparent red flammable region map sublayer underlies the fire management zone
  polygons in the interactive map. It is provided for context and shows the spatial domain 
  where fire is permitted in the ALFRESCO simulations.",
  "05"="Note that some zones lie outside the flammable domain and thus do not have fire model outputs.
  These zones do have vegetation outputs.
  Other zones intersect the ALFRESCO simulations domain; they do have fire model outputs in addition to
  vegetation outputs, but fire variables and associated statistics pertain only to the subregion
  of the fire management zone inside the flammable domain despite the formally defined zone boundaries.",
  "06"="Fire management zone names show when hovering over polygons in the map.
  Clicking on the map is convenient for selecting or deselecting zones, but this can also be done
  using the fire management zone selection menu.
  The dropdown menu and interactive map selections are synchronized",
  "07"="A separate static map layer is available for viewing here.
  It shows all Alaska fire management zones with names and formal abbreviations and provides a link
  to source data on fire management zones.",
  "08"="Like fire management zones, categorical variables available for subsetting your data
  can have mutliple levels selected simultaneously.
  The six available vegetation classes - black and white spruce, deciduous tree species,
  and shrub, graminoid and wetland tundra - are not all present in all zones
  or with similar spatial coverage so only the first is selected by default.
  There is also an aggregate option. This refers to an integration across all vegetation classes.
  Aggregate data apply to an entire fire management zone rather than being conditional on a given
  vegetation class.",
  "09"="Aggregated regional data only applies to the three fire variables, burn area,
  fire count, and fire size, available under the fire tab in the sidebar menu.
  It does not apply to the vegetation cover area or stand age variables available under the
  vegetation tab because across-species stand age is not meaningful in this context and
  the sum of each area covered by each vegetation class is simply the zone area.",
  "10"="Before covering the remaining variables, this is a good time to discuss the data preview panel.
  You have already seen some ways in which you can select levels of variables that will return no data.
  If no levels are selected at all for any variable, nothing is returned. You are effectively saying,
  for example, that you want data from none of the fire management zones or none of the vegetation classes.
  Another way to have no data is to limit your selections to mutually exclusive options.
  For example, selecting only zones outside the ALFRESCO simulations flammable domain
  results in no fire-related data. The table below will show only vegetation cover and stand age data.",
  "11"="Moving through the different tabs shows summaries of the data available based on your selections
  for each output variable.",
  "12"="The full table shows all data for all five ALFRESCO output variables based on the subset you define
  by your selections",
  "13"="In the previous example, those for fire vairables would be empty.",
  "14"="On the other hand, if you select only the aggregate option for vegetation, fire data is
  returned for each selected region where it is available, but the cover area and stand age tabs
  will have nothing to display.",
  "15"="Now let's return to the remaining variables available for filtering the ALFRESCO output.
  There are five General Circulation Models (GCMs) available. These climate models inform a wider a range
  of projected simulations of Alaska wildfire and associated vegetation transitions over time than
  just the stochastic component of the ALFRESCO model alone. Using multiple climate models provides
  a more robust representation of future uncertainty. More information about these GCMs can be found
  at the Information tab in the sidebar.",
  "16"="Next are the Representative Concentration Pathways (RCPs), which refer to different levels of
  projected future greenhouse gas emmissions based on different assumptions about population growth,
  economic development and energy usage. Three RCPs are available for each GCM.
  All 15 combinations of each climate model under all three RCP emmissions scenarios
  are used to inform ALFRESCO so that simulation output may account for more future uncertainty.
  See the Information tab for more details about RCPs.",
  "17"="The mean is the default statistic for summarizing each of the five ALFRESCO output variables
  across all 200 simulations. Other available statistics include the minimum and maximum.
  It is important to remember that when looking at plots and statistics relating to averages
  of your selected data over time, across space, or across multiple GCMs, RCPs, etc.,
  that while you may be looking at an average, this is not your selected statistic.
  For example, you might be looking decadal, multi-GCM averages for the maximum burn area across simulations.",
  "18"="The available time frame is 1950 - 2099. ALFRESCO is informed by historical climate data
  from the Climatological Research Unit (CRU) through 2013. GCMs inform ALFRESCO from 2014 - 2099.
  This is another time to note the impact of your data selections. For example, dragging the slider
  endpoints so that the range only covers historical years will lead to no GCM/RCP-based data being returned.
  Alternatively, selecting only future years means no outputs informed by CRU data will be available.
  Note also that in decadal summaries available on the each of the five output variables tabs
  the 2010s decade must be composed of ALFRESCO output from four CRU-based, calibrated historical
  simulation years and six GCM/RCP-based projected simulation years.",
  "19"= "You have completed the tour of the data selection tab. You now know how to filter or subset
  ALFRESCO model output to suit your needs based on the available variables and you understand
  the underlying data better. Now select some data if you had not done so earlier.
  Up to now this tour did not require a valid (non-empty) data subset be defined,
  but one is needed to continue or you will have nothing to look at. Make sure your selections
  return fire data. Press Next to continue to the burn area tab...",
  "20"="Since you have defined a valid data set there are now several plots and statistical
  summaries to look at. If you don't see anything, go back to the previous step in the tour
  and update your data subset. The rest of this tour will only focus on the burn area tab
  because the other four vairable tabs display similar content. [Last tour step written for now...]"
)

tour.pos <- c("left", "right", "bottom", "left", "bottom", "bottom", "top", "bottom", "right", "top", "top", "left", "left", "left",
             "bottom", "bottom", "bottom", "bottom", "left", "left")

tour.element <- c(
  "#shiny-tab-data", 
  "#tabs", "#Map", "#flammable", "#Map", "#regions + .selectize-control", "#btn_staticmap",
  "#veg + .selectize-control", "#tabs",
  "#summary", "#summary", dv("Full table", quote=F), dv("Burn area", quote=F), dv("Cover area", quote=F),
  "#gcms + .selectize-control", "#rcps + .selectize-control", "#stat + .selectize-control", ".js-irs-0", 
  "#shiny-tab-data", "#shiny-tab-burnarea"
)

steps <- reactive({
  data.frame(element=tour.element, intro=tour.text, position=tour.pos)
})



# begin tour on button click
observeEvent(input$help, {
  not.db.data <- c("burnarea", "firefreq", "firesize", "vegarea", "vegage")
  not.db.burnarea <- c("data", "firefreq", "firesize", "vegarea", "vegage")
  not.tb.fulltable <- c("Burn area", "Fire count", "Fire size", "Cover area", "Stand age")
  not.tb.burnarea <- c("Full table", "Fire count", "Fire size", "Cover area", "Stand age")
  not.tb.coverarea <- c("Full table", "Burn area", "Fire count", "Fire size", "Stand age")
  
  tour.options <- list(steps=steps(), 
    #"tooltipClass"="myijs",
    "showBullets"="false", "showProgress"="true", "showStepNumbers"="false")
  tour.events <- list(
    "onchange"=I(paste0(
      stepcb(stepEquals(c(1:19)), c(rmClass(not.db.data), goClass("data"))),
      stepcb(stepEquals(c(12)), c(rmClass(not.tb.fulltable), goClass("Full table"))),
      stepcb(stepEquals(c(13)), c(rmClass(not.tb.burnarea), goClass("Burn area"))),
      stepcb(stepEquals(c(14)), c(rmClass(not.tb.coverarea), goClass("Cover area"))),
      stepcb(stepEquals(c(20)), c(rmClass(not.db.burnarea), goClass("burnarea"))),
      collapse="\n"))#, ttWidth(c(3,5), "50%", "50%")
  )
  introjs(session, options=tour.options, events=tour.events)
})

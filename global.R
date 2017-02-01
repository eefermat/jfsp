library(rintrojs)
library(leaflet)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinytoastr)
library(DT)

library(rgdal)
library(lazyeval)
library(dplyr)
library(purrr)
library(ggplot2)
load("appData.RData")

action_btn_style <- "margin: 10px 15px 10px 15px; width: 200px"
groupby_vars <- c("", "RCP", "GCM"="Model", "Fire Mgmt Zone"="Region", "Vegetation")
pooled_options <- c("Average observations", "Unique observations")
axis_scales <- c("Fixed"="fixed", "Free"="free", "Free X"="free_x", "Free Y"="free_y")

source("override.R")

# load modules
source("modules/main/mod_utils.R")
source("modules/main/mod.R")
source("modules/plots/mod_plots.R")

#enableBookmarking(store="server") # not yet available on shinyapps.io

library(shiny)
library(shinyBS)
library(leaflet)
library(shinydashboard)
library(DT)
load("appData.RData")

action_btn_style <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"
groupby_vars <- c("", "GBM", "RCP", "Model", "Region", "Var", "Vegetation")
axis_scales <- c("Fixed"="fixed", "Free"="free", "Free X"="free_x", "Free Y"="free_y")

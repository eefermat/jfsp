library(leaflet)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(DT)
load("appData.RData")

action_btn_style <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"
groupby_vars <- c("", "GBM", "RCP", "Model", "Region", "Vegetation")
pooled_vars <- c("Average observations", "Unique observations")
axis_scales <- c("Fixed"="fixed", "Free"="free", "Free X"="free_x", "Free Y"="free_y")

source("mod.R")

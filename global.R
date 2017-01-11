library(leaflet)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(DT)
load("appData.RData")

action_btn_style <- "color: #fff; background-color: #337ab7; border-color: #2e6da4"
groupby_vars <- c("", "GBM", "RCP", "Model", "Region", "Vegetation")
pooled_options <- c("Average observations", "Unique observations")
axis_scales <- c("Fixed"="fixed", "Free"="free", "Free X"="free_x", "Free Y"="free_y")

valueBox <- function (value, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL){
  shinydashboard:::validateColor(color)
  if (!is.null(icon)) 
    shinydashboard:::tagAssert(icon, type = icon$name)
  if(!is.null(icon)){
    if(!icon$name %in% c("i", "img")) stop("'icon$name' must be 'i' or 'img'.")
    iconClass <- if(icon$name=="i") "icon-large" else "img"
  }
  boxContent <- div(class = paste0("small-box bg-", color), 
                    div(class = "inner", h3(value), p(subtitle)), if (!is.null(icon)) 
                      div(class = iconClass, icon))
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}

icon <- function (name, class = NULL, lib = "font-awesome"){
  if(lib=="local"){
    if(is.null(name$src)) stop("If lib='local', 'name' must be a named list with at least a 'src' element.")
    if(is.null(name$width)) name$width <- "100%"
    return(tags$img(class="img img-local", src=name$src, width=name$width))
  }
  
  prefixes <- list(`font-awesome` = "fa", glyphicon = "glyphicon")
  prefix <- prefixes[[lib]]
  if (is.null(prefix)) {
    stop("Unknown font library '", lib, "' specified. Must be one of ", 
         paste0("\"", names(prefixes), "\"", collapse = ", "))
  }
  iconClass <- ""
  if (!is.null(name)) 
    iconClass <- paste0(prefix, " ", prefix, "-", name)
  if (!is.null(class)) 
    iconClass <- paste(iconClass, class)
  iconTag <- tags$i(class = iconClass)
  if (lib == "font-awesome") {
    htmltools::htmlDependencies(iconTag) <- htmltools::htmlDependency("font-awesome", 
                                                "4.6.3", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css")
  }
  iconTag
}

source("mod.R")

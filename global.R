ptm <- proc.time()
library(rintrojs)
library(leaflet)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinytoastr)
library(DT)

library(sp)
library(lazyeval)
library(dplyr)
library(purrr)
library(ggplot2)
library(ggpmisc)
cat("Total library load time:\n")
print(proc.time() - ptm)

ptm <- proc.time()
load("appData/appData.RData")
cat("Total data load time:\n")
print(proc.time() - ptm)

ptm <- proc.time()
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
temptext <- p(em("This website was developed as part of project (#16-1-01-18) funded by the Joint Fire Science Program.
If you would be interested in participating in a more in-depth discussion in late Feb-early Mar about the design/utility
of this website and to explore future fire management options please fill out", 
              a("this doodle poll", href="http://doodle.com/poll/74ktffkxqnvnmevc", target="_blank")))

cat("Remainder global.R time:\n")
print(proc.time() - ptm)

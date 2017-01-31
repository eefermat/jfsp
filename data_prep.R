library(dplyr)
library(purrr)
library(ggplot2)

# setup
gbm <- c("3m", "5m")
rcp <- c("Historical", "RCP 4.5", "RCP 6.0", "RCP 8.5")
models <- c("CRU 3.2", "NCAR-CCSM4", "GFDL-CM3", "GISS-E2-R", "IPSL-CM5A-LR", "MRI-CGCM3")
veg.drop <- c("Barren lichen-moss", "Temperate Rainforest")
lev <- c("Black Spruce", "White Spruce", "Deciduous", "Shrub Tundra", "Graminoid Tundra", "Wetland Tundra", "All")
stats.drop <- paste0("-Pct_", c("05", 10, 25, 50, 75, 90, 95))

# workspace files
files.h <- unlist(map(gbm, ~list.files(file.path("CMIP5_Statewide", .x, "stats"),
                                        pattern="historical_.*.RData", full.names=TRUE, recursive=TRUE)))
files.p <- unlist(map(gbm, ~list.files(file.path("CMIP5_Statewide", .x, "stats"),
                                     pattern="projected_.*.RData", full.names=TRUE, recursive=TRUE)))

# load data
loadData <- function(files){
  e <- environment()
  d <- vector("list", length(files))
  for(i in seq_along(files)){
    load(files[i], envir=e)
    d[[i]] <- tbl_df(get(ls(pattern="^stats.alf", envir=e), envir=e))
  }
  d
}

dh <- loadData(files.h)
dp <- loadData(files.p)

# prep data
prepData <- function(d, files){
  d <- bind_rows(map2(d, rep(gbm, each=length(files)/2),
                     ~select_(.x, .dots=stats.drop) %>% filter(!Vegetation %in% veg.drop & Location!="AK") %>%
                       mutate(Phase=.y,
                              Model=ifelse(Model=="CCSM4", "NCAR-CCSM4", Model),
                              Vegetation=factor(Vegetation, levels=lev)) %>%
                       rename(GBM=Phase, RCP=Scenario, Region=Location))) %>%
    mutate(GBM=factor(GBM), Region=factor(Region), RCP=factor(RCP, levels=rcp), 
           Model=factor(Model, levels=models), Var=factor(Var)) %>%
    arrange(GBM, Region, RCP, Model, Var, Vegetation, Year)
}

h <- prepData(dh, files.h)
d <- prepData(dp, files.p)

rcps <- c("4.5"=rcp[1], "6.0"=rcp[2], "8.5"=rcp[3])
gbms <- levels(d$GBM)
gcms <- levels(d$Model)
regions <- c("Chugach N.F."="CGF", "Copper River"="CRS", "Delta"="DAS", "Fairbanks"="FAS",
             "Galena"="GAD", "Military"="MID", "Southwest"="SWS", "Tanana"="TAD", 
             "Tok"="TAS", "Upper Yukon"="UYD")
#regions <- levels(d$Region)
veg <- levels(d$Vegetation)
period <- range(d$Year)
variables <- levels(d$Var)
stats <- c("Mean", "Min", "Max")

# Shapefiles
library(rgdal)
flam <- readOGR("shapefiles/flam_polygon.shp", verbose=FALSE)
fmz <- readOGR("shapefiles/fmz_polygons.shp", verbose=FALSE)

# Save app workspace
save(d, h, gbms, rcps, gcms, regions, veg, period, variables, stats, flam, fmz, file="appData.RData")

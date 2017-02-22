library(dplyr)
library(purrr)
library(ggplot2)

# setup
gbm <- "5m" #c("3m", "5m")
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
    rm(list=ls(pattern="^stats.alf", envir=e), envir=e)
  }
  d
}

dh <- loadData(files.h)
dp <- loadData(files.p)

# prep data
prepData <- function(d, files){
  d <- bind_rows(map2(d, rep(gbm, each=length(files)/length(gbm)),
                     ~select_(.x, .dots=stats.drop) %>% filter(!Vegetation %in% veg.drop & Location!="AK") %>%
                       mutate(Phase=.y,
                              Model=ifelse(Model=="CCSM4", "NCAR-CCSM4", Model),
                              Vegetation=factor(Vegetation, levels=lev)) %>%
                       rename(GBM=Phase, RCP=Scenario, Region=Location))) %>%
    mutate(GBM=factor(GBM), Region=factor(Region), RCP=factor(RCP, levels=rcp), 
           Model=factor(Model, levels=models), Var=factor(Var)) %>%
    arrange(GBM, Region, RCP, Model, Var, Vegetation, Year)
  if(length(gbm)==1) d <- select(d, -GBM)
  d
}

h <- prepData(dh, files.h)
d <- prepData(dp, files.p)

rcps <- c("4.5"=rcp[2], "6.0"=rcp[3], "8.5"=rcp[4])
if(length(gbm) > 1) gbms <- levels(d$GBM)
gcms <- levels(d$Model)[-1]
regions <- c("Delta"="DAS", "Fairbanks"="FAS", "Galena"="GAD", "Military"="MID",
             "Southwest"="SWS", "Tanana"="TAD", "Tok"="TAS", "Upper Yukon"="UYD")
veg <- levels(d$Vegetation)
names(veg) <- veg
names(veg)[which(veg=="All")] <- "Aggregate"
period <- range(c(h$Year, d$Year))
variables <- levels(d$Var)
stats <- c("Mean", "Min", "Max")

# Shapefiles
library(rgdal)
flam <- readOGR("shapefiles/flam_polygon.shp", verbose=FALSE)
fmz <- readOGR("shapefiles/FireManagementZonesV3.shp", verbose=FALSE)

# Save app workspace
objs <- c('d', 'h', 'rcps', 'gcms', 'regions', 'veg', 'period', 'variables', 'stats', 'flam', 'fmz')
if(length(gbm) > 1) objs <- c(objs, gbms)
save(list=objs, file="appData.RData")

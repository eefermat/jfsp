library(dplyr)
library(purrr)
library(ggplot2)

gbm <- c("3m", "5m")
rcp <- c("RCP 4.5", "RCP 6.0", "RCP 8.5")
files <- unlist(map(gbm, ~list.files(file.path("CMIP5_Statewide", .x, "stats"),
                                        pattern="projected_.*.RData", full.names=TRUE, recursive=TRUE)))

d <- vector("list", length(files))
for(i in seq_along(files)){
  load(files[i])
  d[[i]] <- tbl_df(get(ls(pattern="^stats.alf")))
  rm(list=ls(pattern="^stats.alf"))
}

veg.drop <- c("Barren lichen-moss", "Temperate Rainforest")
lev <- c("Black Spruce", "White Spruce", "Deciduous", "Shrub Tundra", "Graminoid Tundra", "Wetland Tundra", "All")
stats.drop <- paste0("-Pct_", c("05", 10, 25, 50, 75, 90, 95))
d <- bind_rows(map2(d, rep(gbm, each=length(files)/2),
                   ~select_(.x, .dots=stats.drop) %>% filter(!Vegetation %in% veg.drop & Location!="AK" & Year >= 2020) %>%
                     mutate(Phase=.y,
                            Model=ifelse(Model=="CCSM4", "NCAR-CCSM4", Model),
                            Vegetation=factor(Vegetation, levels=lev)) %>%
                     rename(GBM=Phase, RCP=Scenario, Region=Location))) %>%
  mutate(GBM=factor(GBM), Region=factor(Region), RCP=factor(RCP, levels=rcp), Model=factor(Model), Var=factor(Var)) %>%
  arrange(GBM, Region, RCP, Model, Var, Vegetation, Year)

rcps <- c("4.5"=rcp[1], "6.0"=rcp[2], "8.5"=rcp[3])
gbms <- levels(d$GBM)
gcms <- levels(d$Model)
#regions <- c("CGF", "CRS", "DAS", "FAS", "GAD", "KKS", "MID", "MSS", "SWS", "TAD", "TAS", "UYD")
regions <- levels(d$Region)
veg <- levels(d$Vegetation)
period <- range(d$Year)
variables <- levels(d$Var)
stats <- c("Mean", "SD", "Min", "Max")

save(d, gbms, rcps, gcms, regions, veg, period, variables, stats, file="appData.RData")

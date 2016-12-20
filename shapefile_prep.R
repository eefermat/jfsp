library(raster)
library(maptools)

# Alfresco runs flammability area polygon shapefile
proj4 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
flam <- raster("source_data/gbm.flamm_1950.tif") %>% aggregate(5) %>% reclassify(cbind(-Inf, Inf, 1))
flam <- rasterToPolygons(flam, dissolve=TRUE) %>% spTransform(proj4)
shapefile(flam, "shapefiles/flam_polygon.shp")

# Prepped fire management zones shapefile
fmz <- readOGR("shapefiles/FireManagementZones_simplified.shp", verbose=FALSE)
fmz <- spTransform(fmz, proj4)
fmz@data$Shape_Area <- NULL
fmz@data$Shape_Leng <- NULL
shapefile(fmz, "shapefiles/fmz_polygons.shp")

# below code produces shapefiles not currently in use.
id <- cut(coordinates(fmz)[,1], quantile(coordinates(fmz)[,1]), include.lowest=TRUE)
ak <- unionSpatialPolygons(fmz, id)

id <- unlist(map(1:4, ~ak@polygons[[.x]]@ID))
d <- data.frame(OBJECTID=factor(id), REGION=factor(paste0("AK", 1:4)), AGENCY=factor(paste0("AK", 1:4)))
rownames(d) <- id
ak <- SpatialPolygonsDataFrame(ak, d)
shapefile(ak, "shapefiles/fmz_union_polygon.shp")

shp <- rbind(fmz, ak)
shapefile(shp, "shapefiles/fmz_final_polygons.shp")

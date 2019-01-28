#setwd("//ad.uillinois.edu/aces/ACE/personal/zhou100/")
# setwd("~/Box Sync/Research")

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(ggplot2)
library(raster)
library(gdalUtils)
library(parallel)
library(RCurl)
library(R.utils)
library(rgeos)
library(dplyr)

package = c("maptools", "rgdal", "PBSmapping", "raster", "snow")
lapply(package, require, character.only = TRUE)


cropland <- raster("glcrop1992_5min.asc")
# proj.latlon <- CRS("+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")

# proj.latlon<-CRS("+proj=moll +lon_0=-300.9375")
crs(cropland) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

#cropland_proj <- spTransform(cropland, CRS = proj.latlon)
#cropland_proj <- projectRaster(cropland, crs = proj.latlon)

cropland_polygon <- rasterToPolygons(cropland, dissolve=TRUE)
clip1_zam_overlap <- crop(r[[i]], extent(Zam_overlap)) #crop to extent of polygon


# look at the raster attributes. 

DEM[DEM==0]<-NA
plot(DEM)]


plot(p)
writeOGR(p, ".", "counties-rgdal", driver="ESRI Shapefile")



subset =readOGR("subset_globe.shp")
subset$Countryeng

poly =readOGR("/Users/yujunzhou/Box Sync/Research/Baylis_lab/before_merge.shp")
plot(poly)


CropDissolve   <- unionSpatialPolygons(poly ,IDs = poly$Countryeng)
unique(poly$Countryeng)
CropDissolve

poly_df <- as.data.frame(CropDissolve)
# do some staff with "poly_df" that doesn't support SpatialPolygonsDataFrame
# then convert it to SPDF back again
s_poly <- SpatialPolygonsDataFrame(poly, CropDissolve)
library(PBSmapping)
CropDissolvePSFour <- SpatialPolygons2PolySet(CropDissolve)

CropDissolve$
  unique(CropDissolvePSFour$PID)
unique(CropDissolvePSFour$SID)
unique(CropDissolvePSFour$POS)


clip1 <- raster("clip_cropland.tif")



library(polyclip)

require(rgdal)
# Read SHAPEFILE.shp from the current working directory (".")
shape <- readOGR(dsn = ".", layer = "SHAPEFILE")

diff_set = polyclip(A, B, op=c("minus")

library(lidR)
library(plyr)
library(stringr)
library(terra)
library(sf)
library(fasterize)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
stands <- st_read('data/stands.shp')
mastervht <- 0


folderlist <- unique((stands$folder))
i=16
path <- paste0('output/',folderlist[i])

canopy <- rast(paste0(path,'/','canopy.tif'))

canopy50m <- aggregate(canopy, fun = 'max', fact=50, na.rm=T);names(canopy50m)<-'canopy50m'
canopy50m <-  disaggregate(canopy50m,  method="near", fact=50); canopy50m <- crop(canopy50m, canopy)
plot(canopy)

canopy50m[canopy50m > 60] <- NA
canopy50m[!is.na(canopy50m)] <- 1
writeRaster(canopy,paste0(path,'/','canopyX.tif'))
canopy <- canopy * canopy50m

writeRaster(canopy,paste0(path,'/','canopy.tif'), overwrite=T)

library(lidR)
library(rgdal)
library(rlas)
library(progress)
library(future)
library(viridis)
library(dplyr)
library(stringr)
library(terra)
library(sf)
library(fasterize)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
stands <- st_read('data/stands.shp')

folderlist <- unique((stands$folder))

for (i in 1:length(folderlist)){}

path <- paste0('output/',folderlist[1])


canopy <- rast(paste0(path,'/','canopy.tif'))

veg <- subset(stands, folder %in% folderlist[1])
veg$id <- as.numeric(rownames(veg))
veg.t <- st_drop_geometry(veg)

veg <- st_transform(veg, crs = crs(canopy))
veg.r <- fasterize(veg, raster(canopy), field = 'id')
writeRaster(veg.r, 'output/veg.r.tif', overwrite=TRUE)
veg.r <- rast('output/veg.r.tif')
plot(veg.r)

vht <- c(veg.r, canopy)
vht <- as.data.frame(vht, xy=T)
colnames(vht) <- c('x', 'y', 'id', 'ht')
vht <- subset(vht, !ht >100)
vht <- merge(vht, veg.t, by='id')

#calculate percentage above 5 meters
#get median and 5-95 quantiles

library(lidR)
library(plyr)
library(stringr)
library(terra)
library(sf)
library(fasterize)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
stands <- st_read('data/stands.shp')

folderlist <- unique((stands$folder))

crowndist <- data.frame(site = 'zzz',crown = 'zzz', area=0.1, ht50=0.1, htmax=0.1, width=0.1)

for (i in 1:length(folderlist)){ #

path <- paste0('output/',folderlist[i])

canopy <- rast(paste0(path,'/','canopy.tif'))
crowns <-  rast(paste0(path,'/','crowns.tif'))
veg <- subset(stands, folder %in% folderlist[i])
veg$id <- as.numeric(rownames(veg))
veg.t <- st_drop_geometry(veg)

veg <- st_transform(veg, crs = crs(canopy))
veg.r <- fasterize(veg, raster(canopy), field = 'id')
writeRaster(veg.r, 'output/veg.r.tif', overwrite=TRUE)
veg.r <- rast('output/veg.r.tif')
#plot(veg.r)
##frequency of gaps and emergents ----

  vht <- c(veg.r, canopy, crowns)
  vht <- as.data.frame(vht, xy=T)
  colnames(vht) <- c('x', 'y', 'id', 'ht', 'crown')

vht <- subset(vht, !ht >116)
vht <- merge(vht, veg.t, by='id')

#calculate percentage above 5 meters ----


#determine and remove outliers -----
#get median and 5-95 quantiles ----
percentiles.trees <- ddply(vht, .(site, crown), summarise,
                           area = length(ht), 
                           ht50 = quantile(ht, 0.5),
                           htmax = max(ht)
)
percentiles.trees$width <-  (percentiles.trees$area/3.141592)^0.5*2
crowndist <- rbind(crowndist, percentiles.trees)
}; crowndist <- crowndist[-1,]
  write.csv(crowndist, 'output/crowndist.csv', row.names = F)
  
  
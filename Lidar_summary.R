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
path <- 'data/porters2'
path.new <- 'output/porters2'
ground <- rast(paste0(path.new,'/','ground.tif'))
surface <- rast(paste0(path.new,'/','surface.tif'))
canopy <- rast(paste0(path.new,'/','canopy.tif'))

veg <- st_read('data/vegpolygons/GRSM_veg.shp')
veg$id <- as.numeric(rownames(veg))
veg.t <- st_drop_geometry(veg)
veg.r <- fasterize(veg, raster(canopy), field = 'id')
writeRaster(veg.r, 'output/veg.r.tif', overwrite=TRUE)
veg.r <- rast('output/veg.r.tif')


vht <- c(veg.r, canopy)
vht <- as.data.frame(vht)
colnames(vht) <- c('id', 'ht')
vht <- subset(vht, !ht >100)
vht <- merge(vht, veg.t[,c('id', 'Short_name')], by='id' )
 
covertab <- data.frame(community='zzz',cover=0, ht=0)

htclass <- c(1,2,3,5,10,15,20,25,30,35,40,45,50,60,70)
 grptl <- aggregate(list(total = vht$ht), by=list(community = vht$Short_name), FUN='length')
for (i in 1:length(htclass))
 { vht.sub <- subset(vht, ht < htclass[i])
 grcnt <- aggregate(list(count = vht.sub$ht), by=list(community = vht.sub$Short_name), FUN='length')
 grcnt <- merge(grcnt, grptl, by='community', all.x=T)
 grcnt$cover <- (1-grcnt$count/grcnt$total)*100
 covertab1 <- cbind(grcnt[,c('community', 'cover')], ht=htclass[i])
 covertab <- rbind(covertab, covertab1)}
 unique(covertab$community)
 covertab.select <- subset(covertab, community %in% c('Cove forest (typic type)','Northern hardwood/acid hardwood forest', 'Yellow pine forest', 'Oak-hickory forest (typic acidic type)', 'Successional hardwood forest'))
 ht.select <- subset(vht, Short_name %in% c('Cove forest (typic type)','Northern hardwood/acid hardwood forest', 'Yellow pine forest', 'Oak-hickory forest (typic acidic type)', 'Successional hardwood forest'))
 
 ggplot(covertab.select, aes(x=cover, y=ht, color=community))+
   geom_line()
 
 
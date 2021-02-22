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

vhtsum <- data.frame(site = 'zzz', open=0.1, tshrub.cover=0.1, tree.cover=0.1, tree05=0.1, tree15=0.1, tree30=0.1, tree45=0.1, tree60=0.1, ht05=0.1, ht50=0.1, ht95=0.1, htmax=0.1)

for (i in 1:length(folderlist)){

path <- paste0('output/',folderlist[i])


canopy <- rast(paste0(path,'/','canopy.tif'))

veg <- subset(stands, folder %in% folderlist[i])
veg$id <- as.numeric(rownames(veg))
veg.t <- st_drop_geometry(veg)

veg <- st_transform(veg, crs = crs(canopy))
veg.r <- fasterize(veg, raster(canopy), field = 'id')
writeRaster(veg.r, 'output/veg.r.tif', overwrite=TRUE)
veg.r <- rast('output/veg.r.tif')
#plot(veg.r)

vht <- c(veg.r, canopy)
vht <- as.data.frame(vht, xy=T)
colnames(vht) <- c('x', 'y', 'id', 'ht')
vht <- subset(vht, !ht >100)
vht <- merge(vht, veg.t, by='id')

#calculate percentage above 5 meters ----

vht$tree <- (vht$ht >= 5)*1
vht$tscrub <- (vht$ht >= 2 & vht$ht < 5)*1
vht$open <- (vht$ht < 2)*1
vht$tree05 <- (vht$ht >= 5& vht$ht < 15)*1
vht$tree15 <- (vht$ht >= 15& vht$ht < 30)*1
vht$tree30 <- (vht$ht >= 30& vht$ht < 45)*1
vht$tree45 <- (vht$ht >= 45& vht$ht < 60)*1
vht$tree60 <- (vht$ht >= 60)*1

#determine and remove outliers -----
percentiles.trees1 <- subset(vht, tree %in% 1)
percentiles.trees1 <- ddply(percentiles.trees1, .(site), summarise, ht50 = quantile(ht, 0.5), ht95 = quantile(ht, 0.95))
percentiles.trees1$outlier <- percentiles.trees1$ht95 + (percentiles.trees1$ht95 - percentiles.trees1$ht50)*2
vht <-  merge(vht, percentiles.trees1[,c('site','outlier')], all.x=T)
vht <- subset(vht, !ht > outlier)

cover <- aggregate(list(open= vht$open, tshrub.cover= vht$tscrub, tree.cover= vht$tree,
                        tree05 = vht$tree05,tree15 = vht$tree15,
                        tree30 = vht$tree30,tree45 = vht$tree45,
                        tree60 = vht$tree60), by=list(site= vht$site), FUN='mean')
cover[,2:ncol(cover)] <- round(cover[,2:ncol(cover)]*100,1)
#get median and 5-95 quantiles ----
percentiles.trees <- subset(vht, tree %in% 1)
percentiles.trees <- ddply(percentiles.trees, .(site), summarise,
                           ht05 = round(quantile(ht, 0.05),1), ht50 = round(quantile(ht, 0.5),1), ht95 = round(quantile(ht, 0.95),1), htmax = round(max(ht),1)
)
vhtsum1 <- merge(cover, percentiles.trees, by='site', all.x=T)
vhtsum <- rbind(vhtsum, vhtsum1)
};vhtsum <- vhtsum[-1,];rm(vhtsum1)
write.csv(vhtsum, 'output/vhtsum.csv', row.names = F)

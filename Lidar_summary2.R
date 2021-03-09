library(lidR)
library(plyr)
library(stringr)
library(terra)
library(sf)
library(fasterize)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
stands <- st_read('data/stands.shp')
hectarefreq <- FALSE # TRUE if analyze gap and emergent frequency
mastervht <- 0


folderlist <- unique((stands$folder))
if(!hectarefreq){
  vhtsum <- data.frame(site = 'zzz', open=0.1, tshrub.cover=0.1, tree.cover=0.1, tree05=0.1, tree15=0.1, tree30=0.1, tree45=0.1, tree60=0.1, ht05=0.1, ht25=0.1,ht50=0.1,ht75=0.1, ht95=0.1, htmax=0.1)
}else{
  vhtsum <- data.frame(site = 'zzz', open=0.1, tshrub.cover=0.1, tree.cover=0.1, tree05=0.1, tree15=0.1, tree30=0.1, tree45=0.1, tree60=0.1, ht05=0.1, ht25=0.1,ht50=0.1,ht75=0.1, ht95=0.1, htmax=0.1,  gapAre=0.1, gapHa=0.1, emergAre=0.1, emergHa=0.1)
}

for (i in 1:length(folderlist)){#i=19

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
##frequency of gaps and emergents ----
if(hectarefreq){
  canopy10m <- aggregate(canopy, fun = 'max', fact=10)
  canopy100.gap <- aggregate(canopy10m, fun = 'min', fact=10)
  canopy100m <- aggregate(canopy10m, fun = 'max', fact=10)
  gapAre <- canopy10m < 5
  gapHa <- canopy100.gap < 5
  emergAre <- canopy10m >= 45
  emergHa <- canopy100m >= 45
  names(gapAre) = 'gapAre'
  names(gapHa) = 'gapHa'
  names(emergAre) = 'emergAre'
  names(emergHa) = 'emergHa'
  gapAre <-  disaggregate(gapAre,  method="near", fact=10); gapAre <- crop(gapAre, canopy)
  gapHa <-  disaggregate(gapHa,  method="near", fact=100); gapHa <- crop(gapHa, canopy)
  emergAre <-  disaggregate(emergAre,  method="near", fact=10); emergAre <- crop(emergAre, canopy)
  emergHa <-  disaggregate(emergHa,  method="near", fact=100); emergHa <- crop(emergHa, canopy)
}

if(!hectarefreq){
  vht <- c(veg.r, canopy)
  vht <- as.data.frame(vht, xy=T)
  colnames(vht) <- c('x', 'y', 'id', 'ht')
}else{
  vht <- c(veg.r, canopy, gapAre, gapHa, emergAre, emergHa)
  vht <- as.data.frame(vht, xy=T)
  colnames(vht) <- c('x', 'y', 'id', 'ht', 'gapAre', 'gapHa', 'emergAre', 'emergHa')
}

# canopy2 <-  crop(canopy, extent(veg))
# plot(canopy >50); plot(veg, add=T)
#canopy2 <- (veg.r *0 )+canopy

vht <- subset(vht, !ht >116)
vht <- merge(vht, veg.t, by='id')
if(length(mastervht) < 2){
  mastervht <- vht
}else{
  mastervht <- rbind(mastervht, vht)
}
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
percentiles.trees1$outlier <- percentiles.trees1$ht95 + (percentiles.trees1$ht95 - percentiles.trees1$ht50)*2.5
vht <-  merge(vht, percentiles.trees1[,c('site','outlier')], all.x=T)
vht <- subset(vht, !ht > outlier)

cover <- aggregate(list(open= vht$open, tshrub.cover= vht$tscrub, tree.cover= vht$tree,
                        tree05 = vht$tree05,tree15 = vht$tree15,
                        tree30 = vht$tree30,tree45 = vht$tree45,
                        tree60 = vht$tree60), by=list(site= vht$site), FUN='mean')
cover[,2:ncol(cover)] <- round(cover[,2:ncol(cover)]*100,2)
#get median and 5-95 quantiles ----
percentiles.trees <- subset(vht, tree %in% 1)
percentiles.trees <- ddply(percentiles.trees, .(site), summarise,
                           ht05 = round(quantile(ht, 0.05),2), 
                           ht25 = round(quantile(ht, 0.25),2),
                           ht50 = round(quantile(ht, 0.5),2),
                           ht75 = round(quantile(ht, 0.75),2),
                           ht95 = round(quantile(ht, 0.95),2), htmax = round(max(ht),2)
)
if(hectarefreq){
  freqs <- aggregate(list(gapAre = vht$gapAre,
                          gapHa = vht$gapHa,
                          emergAre = vht$emergAre,
                          emergHa = vht$emergHa), by=list(site= vht$site), FUN='mean')
  freqs[,2:ncol(freqs)] <- round(freqs[,2:ncol(freqs)]*100,2)
}
vhtsum1 <- merge(cover, percentiles.trees, by='site', all.x=T)
if(hectarefreq){vhtsum1 <- merge(vhtsum1, freqs, by='site', all.x=T)}  
vhtsum <- rbind(vhtsum, vhtsum1)
};vhtsum <- vhtsum[-1,];rm(vhtsum1)

if(!hectarefreq){
  write.csv(vhtsum, 'output/vhtsum.csv', row.names = F)
}else{
  write.csv(vhtsum, 'output/vhtsumfreq.csv', row.names = F)
}

saveRDS(mastervht, 'output/vht.RDS')

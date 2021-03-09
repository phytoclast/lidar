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

for (i in 1:length(folderlist)){#i=16

path <- paste0('output/',folderlist[i])

canopy <- rast(paste0(path,'/','canopy.tif'))

veg <- subset(stands, folder %in% folderlist[i])
veg$id <- as.numeric(rownames(veg))
veg.t <- st_drop_geometry(veg)

veg <- st_transform(veg, crs = crs(canopy))
veg.r <- fasterize(veg, raster(canopy), field = 'id')
writeRaster(veg.r, 'output/veg.r.tif', overwrite=TRUE)
veg.r <- rast('output/veg.r.tif')

##frequency of gaps and emergent ----
canopy1m <- canopy;canopy1m[canopy1m<5 |canopy>116]<- NA
# canopy5m <- aggregate(canopy1m, fun = 'max', fact=5, na.rm=T);names(canopy5m)<-'canopy5m'
# canopy10min2 <- aggregate(canopy5m, fun = 'min', fact=2, na.rm=T);names(canopy10min2)<-'canopy10min2'
# canopy10min <- aggregate(canopy1m, fun = 'min', fact=10, na.rm=T);names(canopy10min)<-'canopy10min'
canopy10m <- aggregate(canopy1m, fun = 'max', fact=10, na.rm=T);names(canopy10m)<-'canopy10m'
canopy20m <- aggregate(canopy10m, fun = 'max', fact=2, na.rm=T);names(canopy20m)<-'canopy20m'
canopy50m <- aggregate(canopy10m, fun = 'max', fact=5, na.rm=T);names(canopy50m)<-'canopy50m'
canopy80m <- aggregate(canopy10m, fun = 'max', fact=8, na.rm=T);names(canopy80m)<-'canopy80m'
canopy100m <- aggregate(canopy10m, fun = 'max', fact=10, na.rm=T);names(canopy100m)<-'canopy100m'
canopy125m <- aggregate(canopy1m, fun = 'max', fact=125, na.rm=T);names(canopy125m)<-'canopy125m'
  
# canopy5m <-  disaggregate(canopy5m,  method="near", fact=5); canopy5m <- crop(canopy5m, canopy)
# canopy10min <-  disaggregate(canopy10min,  method="near", fact=10); canopy10min <- crop(canopy10min, canopy)
# canopy10min2 <-  disaggregate(canopy10min2,  method="near", fact=10); canopy10min2 <- crop(canopy10min2, canopy)
canopy10m <-  disaggregate(canopy10m,  method="near", fact=10); canopy10m <- crop(canopy10m, canopy)
canopy20m <-  disaggregate(canopy20m,  method="near", fact=20); canopy20m <- crop(canopy20m, canopy)
canopy50m <-  disaggregate(canopy50m,  method="near", fact=50); canopy50m <- crop(canopy50m, canopy)
canopy80m <-  disaggregate(canopy80m,  method="near", fact=80); canopy80m <- crop(canopy80m, canopy)
canopy100m <-  disaggregate(canopy100m,  method="near", fact=100); canopy100m <- crop(canopy100m, canopy)
canopy125m <-  disaggregate(canopy125m,  method="near", fact=125); canopy125m <- crop(canopy125m, canopy)
canopy100m <- (canopy100m*2+canopy80m+canopy125m)/4




#vht <- c(veg.r, canopy, canopy10min, canopy10min2, canopy5m, canopy10m, canopy20m, canopy50m, canopy100m)
vht <- c(veg.r, canopy, canopy10m, canopy20m, canopy50m, canopy100m)
vht <- as.data.frame(vht, xy=T, na.rm=FALSE)
#colnames(vht) <- c('x', 'y', 'id', 'ht', 'c10min', 'c10min2', 'c5m', 'c10m', 'c20m','c50m','c100m')
colnames(vht) <- c('x', 'y', 'id', 'ht', 'c10m', 'c20m','c50m','c100m')
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
# percentiles.trees1 <- subset(vht, tree %in% 1)
# percentiles.trees1 <- ddply(percentiles.trees1, .(site), summarise, ht50 = quantile(c10m, 0.5), ht95 = quantile(c10m, 0.95))
# percentiles.trees1$outlier <- percentiles.trees1$ht95 + (percentiles.trees1$ht95 - percentiles.trees1$ht50)*2.5
# vht <-  merge(vht, percentiles.trees1[,c('site','outlier')], all.x=T)
# vht <- subset(vht, !ht > outlier &
#                 !c10m > outlier &
#                 !c20m > outlier &
#                 !c50m > outlier &
#                 !c100m > outlier)

#get median and 5-95 quantiles ----
percentiles.trees <- vht
percentiles.trees$httree <- ifelse(percentiles.trees$ht >=5, percentiles.trees$ht, NA)
percentiles.trees <- ddply(percentiles.trees, .(site), summarise,
                           open = round(mean(open, na.rm=T)*100,2), 
                           tscrub = round(mean(tscrub, na.rm=T)*100,2), 
                           tree05 = round(mean(tree05, na.rm=T)*100,2), 
                           tree15 = round(mean(tree15, na.rm=T)*100,2), 
                           tree30 = round(mean(tree30, na.rm=T)*100,2), 
                           tree45 = round(mean(tree45, na.rm=T)*100,2), 
                           tree60 = round(mean(tree60, na.rm=T)*100,2),
                           tree = round(mean(tree, na.rm=T)*100,2),
                           # c10min = round(mean(c10min, na.rm=T),2),
                           # c10min2 = round(mean(c10min2, na.rm=T),2),
                           # c5m = round(mean(c5m, na.rm=T),2),
                           c10m = round(mean(c10m, na.rm=T),2),
                           c20m = round(mean(c20m, na.rm=T),2),
                           c50m = round(mean(c50m, na.rm=T),2),
                           c100m = round(mean(c100m, na.rm=T),2),
                           ht05 = round(quantile(httree, 0.05, na.rm=T),2),
                           ht25 = round(quantile(httree, 0.25, na.rm=T),2),
                           ht50 = round(quantile(httree, 0.50,  na.rm=T),2),
                           ht75 = round(quantile(httree, 0.75,  na.rm=T),2),
                           ht95 = round(quantile(httree, 0.95,  na.rm=T),2),
                           htmax = round(max(ht, na.rm=T),2)
)
if(length(mastervht)>1){
  mastervht <- rbind(mastervht, percentiles.trees)
}else{mastervht <- percentiles.trees}
}

  write.csv(mastervht, 'output/vhthectare.csv', row.names = F)

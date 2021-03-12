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
focalmax <- function(r,w){#r=ras;w=3
  rmax <-  aggregate(r, fact=w, fun='max', na.rm=T) %>% disaggregate(fact=w,method="near") %>% crop(r)
  m <- as.matrix(rbind(c(round(w/3,0),0,0,0),
                       c(round(w/3,0),0,0,round(w/3,0)),
                       c(0,0,0,round(w/3,0)),
                       c(round(w*2/3,0),0,0,0),
                       c(round(w*2/3,0),0,0,round(w*2/3,0)),
                       c(0,0,0,round(w*2/3,0))
  ))
  for (i in 1:6){#i=1
    fe <- m[i,]
    e <- ext(r)+fe
    re <- expand(r, e)
    remax <- aggregate(re, fact=w, fun='max', na.rm=T) %>% disaggregate(fact=w,method="near") %>% crop(r)
    rmax <- rmax+remax
  };rmax=rmax/7
}



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
canopy10m <- focalmax(canopy1m, 10);names(canopy10m)<-'canopy10m'
canopy20m <- focalmax(canopy1m, 20);names(canopy20m)<-'canopy20m'
canopy50m <- focalmax(canopy1m, 50);names(canopy50m)<-'canopy50m'
canopy100m <- focalmax(canopy1m, 100);names(canopy100m)<-'canopy100m'

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

#get median and 5-95 quantiles ----
percentiles.trees <- vht
percentiles.trees$httree <- ifelse(percentiles.trees$ht >=5, percentiles.trees$ht, NA)
percentiles.trees <- ddply(percentiles.trees, .(site), summarise,
                           strat00 = round(mean(open, na.rm=T)*100,2), 
                           strat02 = round(mean(tscrub, na.rm=T)*100,2), 
                           strat05 = round(mean(tree05, na.rm=T)*100,2), 
                           strat15 = round(mean(tree15, na.rm=T)*100,2), 
                           strat30 = round(mean(tree30, na.rm=T)*100,2), 
                           strat45 = round(mean(tree45, na.rm=T)*100,2), 
                           strat60 = round(mean(tree60, na.rm=T)*100,2),
                           strattree = round(mean(tree, na.rm=T)*100,3),
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

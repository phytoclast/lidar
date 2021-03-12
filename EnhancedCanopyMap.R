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
  rmax <-  aggregate(r, fact=w, fun='max', na.rm=T) %>% disaggregate(fact=w,method="bilinear") %>% crop(r)
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
    remax <- aggregate(re, fact=w, fun='max', na.rm=T) %>% disaggregate(fact=w,method="bilinear") %>% crop(r)
    rmax <- rmax+remax
  };rmax=rmax/7
}



folderlist <- unique((stands$folder))

for (i in 1:length(folderlist)){#i=6
  path <- paste0('output/',folderlist[i])
  canopy <- rast(paste0(path,'/','canopy.tif'))
  # canopy1m <- canopy;canopy1m[canopy1m<5 |canopy>116]<- NA
  # canopy3m <- focalmax(canopy1m, 3);names(canopy3m)<-'canopy3m'
  # lowtree <- canopy3m;lowtree[lowtree < 15]<- NA
  # canopy10m <- focalmax(lowtree, 15);names(canopy10m)<-'canopy10m'
  # ecanopy <- max((canopy1m*0)+canopy3m, (canopy1m*0)+(lowtree*0)+canopy10m, canopy, na.rm = TRUE)
  canopy <- rast(paste0(path,'/','canopy.tif'))
  canopy1m <- canopy;canopy1m[canopy1m<5 |canopy>116]<- NA
  canopy10m <- focalmax(canopy1m, 10);names(canopy10m)<-'canopy10m'
  netcanopy <- min(canopy10m, canopy1m*1.2)
  ecanopy <- max((canopy1m*0)+netcanopy, canopy, na.rm = TRUE)
  writeRaster(ecanopy, paste0(path,'/','ecanopy.tif'), overwrite=TRUE)
}

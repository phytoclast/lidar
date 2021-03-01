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
  
 
  for (i in 1:length(folderlist)){i=11
  
  path <- paste0('output/',folderlist[i])
  
  
  canopy <- rast(paste0(path,'/','canopy.tif'))
  plot(canopy9m)
  
  
  
  timeA <- Sys.time() 
  canopy3m <- aggregate(canopy, fun = 'max', fact=3)
  canopy3m <- resample(canopy3m, canopy, method='bilinear')
  canopy.15m <- (canopy >= 15)
  canopymax <- (canopy3m * canopy.15m) + (canopy * (canopy.15m*-1+1))
  Sys.time()-timeA  
  
  writeRaster(canopymax, filename= paste0(path,'/','canopymax.tif'),
              overwrite=TRUE, wopt=list(gdal=c("COMPRESS=LZW")))
  }
  
  canopy9m <- aggregate(canopy, fun = 'max', fact=15)
  canopy.5m <- (canopy9m >= 5)
  canopy.5m[canopy.5m == 1] <- NA
  plot(canopy.5m)
  
  timeA <- Sys.time() 
  
  d1 <- distance(canopy.5m)
  Sys.time()-timeA 
  
plot(d1)

writeRaster(canopy.5m, filename= paste0(path,'/','canopy.5m.tif'),
              overwrite=TRUE, wopt=list(gdal=c("COMPRESS=LZW")))

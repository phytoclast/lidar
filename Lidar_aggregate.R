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
  
 
  for (i in 1:length(folderlist)){
  
  path <- paste0('output/',folderlist[i])
  
  
  canopy <- rast(paste0(path,'/','canopy.tif'))
  canopy3m <- aggregate(canopy, fun = 'max', fact=3, filename= paste0(path,'/','canopy3m.tif'),
                        overwrite=TRUE, wopt=list(gdal=c("COMPRESS=LZW")))
  }
  
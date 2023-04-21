  library(lidR)
  library(plyr)
  library(stringr)
  library(raster)
  library(terra)
  library(sf)
  library(fasterize)
  library(ggplot2)
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  stands <- st_read('data/stands.shp')

  folderlist <- unique((stands$folder))


  for (i in 1:length(folderlist)){#i=33
  path <- paste0('output/','gollwoods')
  path <- paste0('output/',folderlist[i])
 if(!file.exists(paste0(path,'/','gshade.tif'))){
  canopy <- raster(paste0(path,'/','canopy.tif'))
  ground <- raster(paste0(path,'/','ground.tif'))
  g.slope <- terrain(ground,opt="slope")
  g.aspect <- terrain(ground,opt="aspect")
  gshade <- hillShade(g.slope, g.aspect, angle = 45, normalize=TRUE)
  writeRaster(gshade, filename= paste0(path,'/','gshade.tif'),
              overwrite=TRUE, wopt=list(gdal=c("COMPRESS=LZW")))
  surface <- canopy+ground
  s.slope <- terrain(surface,opt="slope")
  s.aspect <- terrain(surface,opt="aspect")
  sshade <- hillShade(s.slope, s.aspect, angle = 45, normalize=TRUE)
  writeRaster(sshade, filename= paste0(path,'/','sshade.tif'),
              overwrite=TRUE, wopt=list(gdal=c("COMPRESS=LZW")))}
  }

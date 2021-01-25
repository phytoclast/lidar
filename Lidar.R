#remotes::install_github("Jean-Romain/lidR", ref = "devel")
library(lidR)
library(rgdal)
library(rlas)
library(mapview)
library(progress)
library(future)
library(viridis)
library(dplyr)
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Batch Processing  ----
#Take a compressed LAZ files, metricate, then move to new folder as decompressed LAS files
#
path <- 'data/hartwick'
path.new <- 'output/hartwick'
fl <- list.files(path)
file.original <- fl[1]
Las <- readLAS(paste0(path,"/",file.original), filter="-drop_class 7 18")
crs.old <- as.character(Las@proj4string)
crs.new <- str_replace(crs.old, '\\+units\\=ft','\\+units\\=m')
crs.new <- str_replace(crs.old, '\\+vunits\\=ft','\\+vunits\\=m')
isvfeet <- grepl('+vunits=ft', crs.old)
ishmeter <- grepl('+units=m', crs.old)
for (i in 1:length(fl)){
  file.original <- fl[i]
  Las <- readLAS(paste0(path,"/",file.original), filter="-drop_class 7 18")
  if(ishmeter){
    Las2 <- Las
    }else{
  Las2 <- lastransform(Las, CRS(crs.new))}
  if(isvfeet){
  Las2$Z <- Las2$Z *.3048}
  file.new <- paste0(stringr::str_split_fixed(file.original,'\\.',2)[1],'.las')
  writeLAS(Las2, paste0(path.new,'/',file.new))
}
#create a catalog of pre-metricated lidar tiles ----
#
las.collection <- readLAScatalog(path.new)

#opt_filter(las.collection) = '2'

dtm_tin <- grid_terrain(las.collection, res = 5, algorithm = tin())
writeRaster(dtm_tin, paste0(path.new,'/','ground.tif'), overwrite=T)
dsm <- grid_canopy(las.collection, res = 5, algorithm = dsmtin())
writeRaster(dsm, paste0(path.new,'/','surface.tif'), overwrite=T)
plot(dsm)
canopy <- dsm - dtm_tin
canopy[canopy > 115] <- NA
writeRaster(canopy, paste0(path.new,'/','canopy.tif'), overwrite=T)
plot(canopy)
plot(canopy, breaks = c(0,5,10,20,30,40,50), col=c('white', 'lightgreen', 'darkgreen', 'yellow', 'orange', 'red', 'purple'))




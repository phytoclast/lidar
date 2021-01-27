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
circle <- function(grid, radius){ # creates a weighted in the shape of a circle for focal analysis
  nc = floor(radius/(res(grid)[1])+1)*2+1
  mat <- matrix(1,nrow = nc, ncol = nc)
  mid = floor(nc/2)+1
  for (i in 1:nc){
    for(j in 1:nc){
      x = i
      y = j
      mat[x,y] <- 
        (ifelse((((x-mid)^2+(y-mid)^2)^0.5/(mid-1) >= 1), 0, 1)+
           ifelse((((x+.5-mid)^2+(y-mid)^2)^0.5/(mid-1) >= 1), 0, 1)+
           ifelse((((x-.5-mid)^2+(y-mid)^2)^0.5/(mid-1) >= 1), 0, 1)+
           ifelse((((x-mid)^2+(y+.5-mid)^2)^0.5/(mid-1) >= 1), 0, 1)+
           ifelse((((x-mid)^2+(y-.5-mid)^2)^0.5/(mid-1) >= 1), 0, 1)+
           ifelse((((x+.5-mid)^2+(y+.5-mid)^2)^0.5/(mid-1) >= 1), 0, 1)+
           ifelse((((x-.5-mid)^2+(y-.5-mid)^2)^0.5/(mid-1) >= 1), 0, 1)+
           ifelse((((x+.5-mid)^2+(y-.5-mid)^2)^0.5/(mid-1) >= 1), 0, 1)+
           ifelse((((x-.5-mid)^2+(y+.5-mid)^2)^0.5/(mid-1) >= 1), 0, 1))/9}}
  return(mat)}

#Batch Processing  ----
#Take a compressed LAZ files, metricate, then move to new folder as decompressed LAS files
#
path <- 'data/gourdneck'
path.new <- 'output/gourdneck'
fl <- list.files(path)
file.original <- fl[1]
Las <- readLAS(paste0(path,"/",file.original), filter="-drop_class 7 18")
crs.old <- as.character(Las@proj4string)
crs.new <- str_replace(crs.old, '\\+units\\=ft','\\+units\\=m')
crs.new <- str_replace(crs.new, '\\+vunits\\=ft','\\+vunits\\=m')
isvfeet <- grepl('+vunits=ft', crs.old)
ishmeter <- grepl('+units=m', crs.old)
if(F){ #disabled, because it doesn't work
for (i in 1:length(fl)){
  file.original <- fl[i]
  Las <- readLAS(paste0(path,"/",file.original), filter="-drop_class 7 18")
  if(ishmeter){
    Las2 <- Las
    }else{
  Las2 <- spTransform(Las, CRS(crs.new))}
  if(isvfeet){
  Las2$Z <- Las2$Z *.3048}
  #fix header ....
  #header.old <- wkt(Las)
  #header.new <- str_replace_all(header.old, 'foot','meter')
  #header.new <- str_replace_all(header.new, '\\(ft\\)','\\(m\\)')
  #header.new <- str_replace_all(header.new, "0.3048", "1")
  #wkt(Las2) <- header.new
  wkt(Las2) <- wkt(crs(Las2))
  file.new <- paste0(stringr::str_split_fixed(file.original,'\\.',2)[1],'.las')
  writeLAS(Las2, paste0(path.new,'/',file.new))
}}
#create a catalog of pre-metricated lidar tiles ----
#
#las.collection <- readLAScatalog(path.new)
las.collection <- readLAScatalog(path)
res <- 5
res.new <- res
if(!ishmeter){res.new <- res/0.3048}

ground <- grid_terrain(las.collection, res = res.new/3, algorithm = tin())
if(isvfeet){ground <- ground * 0.3048}
ground[ground > 10000 | ground < -10000] <- NA
if(!ishmeter){ground <- projectRaster(ground, crs = CRS(crs.new), method = 'bilinear', res = res/3)}
writeRaster(ground, paste0(path.new,'/','ground.tif'), overwrite=T)

surface <- grid_canopy(las.collection, res = res.new/3, algorithm = p2r())
if(isvfeet){surface <- surface * 0.3048}
surface[surface > 10000 | surface < -10000] <- NA
if(!ishmeter){surface <- projectRaster(surface, crs = CRS(crs.new), method = 'bilinear', res = res/3)}
writeRaster(surface, paste0(path.new,'/','surface.tif'), overwrite=T)
plot(surface)

ground <- raster(paste0(path.new,'/','ground.tif'))
surface <- raster(paste0(path.new,'/','surface.tif'))

canopy <- surface - ground
canopy[canopy > 150 | canopy < -1] <- NA

#canopy.max <- focal(canopy, w=matrix(c(6,10,6,10,10,10,6,10,6)/74*9,nrow = 3), fun = max, na.rm=T)
canopy.max <- focal(canopy, w = circle(canopy, 5), fun = max, na.rm=T)
canopy.light <- focal(canopy, w = circle(canopy, 2.5), fun = max, na.rm=T)

writeRaster(canopy, paste0(path.new,'/','canopy.tif'), overwrite=T)
writeRaster(canopy.max, paste0(path.new,'/','canopy.max.tif'), overwrite=T)
plot(canopy, breaks = c(0,5,10,20,30,40,50), col=c('white', 'lightgreen', 'darkgreen', 'yellow', 'orange', 'red', 'purple'))
overstory0 <- canopy < 5
overstory1 <- canopy >= 5 & canopy < 15
overstory2 <- canopy >= 15
overstory.max <- (canopy.max * overstory2) + (canopy.light * overstory1) + (canopy * overstory0)
overstory <- (canopy >= 5) + (canopy >= 15)
writeRaster(overstory, paste0(path.new,'/','overstory.tif'), overwrite=T)
writeRaster(overstory.max, paste0(path.new,'/','overstory.max.tif'), overwrite=T)

plot(overstory.max)
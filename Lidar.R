library(lidR)
library(rgdal)
library(rlas)
library(mapview)
library(progress)
library(future)
library(viridis)
library(dplyr)
library(stringr)
library(terra)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
circle <- function(grid, radius){ # creates a weighted in the shape of a circle for focal analysis# grid=ground; radius=5
  nc = floor(radius/(res(grid)[1]))*2+1
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
  mat=mat/sum(mat)
  return(mat)}

#Batch Processing  ----
#Take a compressed LAZ files, metricate, then move to new folder as decompressed LAS files
#
path <- 'data/yunque/laz'
path.new <- 'output/yunque'
if(!dir.exists(path.new)){dir.create(path.new)}
fl <- list.files(path)
file.original <- fl[1]
Las <- readLAS(paste0(path,"/",file.original), filter="-drop_class 7 18")
crs.old <- as.character(Las@proj4string)
crs.new <- str_replace(crs.old, '\\+units\\=ft','\\+units\\=m')
crs.new <- str_replace(crs.new, '\\+vunits\\=ft','\\+vunits\\=m')
crs.new <- str_replace(crs.old, '\\+units\\=us-ft','\\+units\\=m')
crs.new <- str_replace(crs.new, '\\+vunits\\=us-ft','\\+vunits\\=m')
isvfeet <- grepl('+vunits=ft', crs.old) | (grepl('+units=ft', crs.old) & !grepl('+vunits', crs.old))
isvusfeet <- grepl('+vunits=us-ft', crs.old) | (grepl('+units=us-ft', crs.old) & !grepl('+vunits', crs.old))
zfactor <- ifelse(isvfeet, 0.3048, ifelse(isvusfeet, 1200/3937, 1))
ishmeter <- grepl('+units=m', crs.old)
if(F){ #disabled, because it doesn't work
for (i in 1:length(fl)){
  file.original <- fl[i]
  Las <- readLAS(paste0(path,"/",file.original), filter="-drop_class 7 18")
  if(ishmeter){
    Las2 <- Las
    }else{
  Las2 <- spTransform(Las, CRS(crs.new))}
  
  Las2$Z <- Las2$Z * zfactor
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
las.collection <- readLAScatalog(path, filter="-drop_class 7 18")
res <- 5
res.new <- res
if(!ishmeter){res.new <- res/0.3048}

#generate ground and surface rasters from LAS dataset ----
ground <- grid_terrain(las.collection, res = res.new/3, algorithm = tin())
ground <- ground * zfactor
ground[ground > 10000 | ground < -10000] <- NA
if(!ishmeter){ground <- projectRaster(ground, crs = CRS(crs.new), method = 'bilinear', res = res/3)}
writeRaster(ground, paste0(path.new,'/','ground.tif'), overwrite=T)
plot(ground)

surface <- grid_canopy(las.collection, res = res.new/3, algorithm = p2r(3))
surface <- surface * zfactor
surface[surface > 10000 | surface < -10000] <- NA
if(!ishmeter){surface <- projectRaster(surface, crs = CRS(crs.new), method = 'bilinear', res = res/3)}
writeRaster(surface, paste0(path.new,'/','surface.tif'), overwrite=T)
plot(surface)

#Create canopy layer, with filters to clean up outliers ----
ground <- rast(paste0(path.new,'/','ground.tif'))
surface <- rast(paste0(path.new,'/','surface.tif'))

ground.min <- focal(ground, w=3, fun = min)
ground.mean <- focal(ground, w=3, fun = mean)


#plot((ground.mean - ground.min) )
correction <- ground.mean - ground.min
canopy <- surface - ground - correction/2
canopy.focmax <- focal(canopy, w=3, fun = max)
canopy.focmean <- focal(canopy, w=3, fun = mean)
outliers <- canopy.focmax - canopy.focmean
outliers[outliers >= 50] <- 0
outliers[outliers < 50] <- 1
outliers <- focal(outliers, w=3, fun = min)
outliers[outliers == 0] <- NA
canopy <- canopy * outliers
canopy[canopy < 0] <- 0
canopy[canopy > 115] <- NA
writeRaster(canopy, paste0(path.new,'/','canopy.tif'), overwrite=T)
writeRaster(correction, paste0(path.new,'/','correction.tif'), overwrite=T)

#canopy.max <- focal(canopy, w=matrix(c(6,10,6,10,10,10,6,10,6)/74*9,nrow = 3), fun = max, na.rm=T)
#canopy.max <- focal(canopy, w = circle(canopy, 5), fun = max, na.rm=T)
#canopy.light <- focal(canopy, w = circle(canopy, 2.5), fun = max, na.rm=T)

#writeRaster(canopy.max, paste0(path.new,'/','canopy.max.tif'), overwrite=T)
plot(canopy, breaks = c(0,5,10,20,30,40,50), col=c('white', 'lightgreen', 'darkgreen', 'yellow', 'orange', 'red', 'purple'), maxcell=1000000)
#overstory0 <- canopy < 5
#overstory1 <- canopy >= 5 & canopy < 15
#overstory2 <- canopy >= 15
#overstory.max <- (canopy.max * overstory2) + (canopy.light * overstory1) + (canopy * overstory0)
#overstory <- (canopy >= 5) + (canopy >= 15)
#writeRaster(overstory, paste0(path.new,'/','overstory.tif'), overwrite=T)
#writeRaster(overstory.max, paste0(path.new,'/','overstory.max.tif'), overwrite=T)

df <- as.data.frame(canopy, xy=TRUE)
hist(df$surface)

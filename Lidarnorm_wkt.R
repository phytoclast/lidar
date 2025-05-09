library(lidR)
library(rlas)
library(mapview)
library(progress)
library(future)
library(viridis)
library(dplyr)
library(stringr)
library(terra)
library(sf)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
folder = 'lepard'
troubled = F  #TRUE if data source is of sparse poor quality
canopyonly = FALSE
notsquare = FALSE #TRUE if data source is of is irregularly shaped
single = FALSE #TRUE if consists of a single tile
projection.override <- FALSE #TRUE if need to override poorly formatted source CRS
normalizeerror = FALSE #TRUE normalization step fails. This generates surface raster first instead of modifying Las data, then subtracts ground

path <- paste0('data/', folder,'/laz')
path.norm <- paste0('data/', folder,'/laz.norm')
path.new <- paste0('output/', folder)
override  = file.exists(paste0('data/', folder,'/epsg.txt'))
if(override){
override.epsg <- read.delim( paste0('data/', folder,'/epsg.txt'))
if(names(override.epsg)[1]%in% 'esri'){override.epsg <- sf::st_crs(paste0('ESRI:',override.epsg[1,1]))}else{
override.epsg <- sf::st_crs(paste0('EPSG:',override.epsg[1,1]))}
}

if(!dir.exists(path.new)){dir.create(path.new)}
if(!dir.exists(path.norm)){dir.create(path.norm)}
plan(multisession)#initiate multi-core processing using the future package
#function to filter outliers  ----
filter_noise.LAS = function(las, sensitivity, hfactor)
{
  p95 <- grid_metrics(las, ~quantile(Z, probs = 0.99), 100/hfactor)
  las <- merge_spatial(las, p95, "p95")
  las <- filter_poi(las, Z < p95*sensitivity)
  las$p95 <- NULL
  return(las)
}

fl <- list.files(path)
#determine projection and units ----
file.original <- fl[1]
Las <- readLAS(paste0(path,"/",file.original), filter="-drop_class 7 18")
st_crs(Las)




crs.old <- st_crs(Las)[[2]]
as.data.frame(crs.old)
grepl('VERT_CS\\[',crs.old)
grepl('UNIT.*foot.*VERT_CS.',crs.old)
grepl('VERT_CS.*UNIT.*foot',crs.old)
x=stringr::str_replace(crs.old, '\n',";")
grepl('vertcrs.*unit.*us survey foot',tolower(crs.old))
grepl('unit.*us survey foot.*vertcrs',tolower(crs.old))
str_extract(crs.old,'VERTCRS.*')

x = as.data.frame(str_split(crs.old,'VERTCRS'))
x = str_extract(x[2,], 'LENGTHUNIT\\[\".*\"')
x
# if(is.na(crs.old)[1]){crs.old <- override.epsg}
if(override){crs.old <- override.epsg}
isvfeet <- grepl('vertcrs.*unit.*"foot"',tolower(crs.old)) | !grepl('vertcrs',tolower(st_crs(crs.old))) & grepl('unit.*"foot"',tolower(st_crs(crs.old)))
isvusfeet <- grepl('vertcrs.*unit.*"us survey foot"',tolower(st_crs(crs.old)))| !grepl('vertcrs',tolower(st_crs(crs.old))) & grepl('unit.*"us survey foot"',tolower(st_crs(crs.old)))

ishfeet <- grepl('unit.*"foot".*vertcrs',tolower(st_crs(crs.old))) | !grepl('vertcrs',tolower(st_crs(crs.old))) & grepl('unit.*"foot"',tolower(st_crs(crs.old)))
ishusfeet <- grepl('unit.*"us survey foot".*vertcrs',tolower(st_crs(crs.old)))| !grepl('vertcrs',tolower(st_crs(crs.old))) & grepl('unit.*"us survey foot"',tolower(crs.old))


zfactor <- ifelse(isvfeet[length(st_crs(crs.old))], 0.3048, ifelse(isvusfeet[length(st_crs(crs.old))], 0.304800609601219, 1))
hfactor <- ifelse(ishfeet[length(st_crs(crs.old))], 0.3048, ifelse(ishusfeet[length(st_crs(crs.old))], 0.304800609601219, 1))


# establish resolution----

res <- 1
subcircle <- res

#generate ground and surface rasters from LAS dataset ----
if(!canopyonly){
  if(single){
  las.collection <- readLAS(paste0(path,"/",file.original), filter="-drop_class 7 18")}else{
las.collection <- readLAScatalog(path, filter="-drop_class 7 18")}
if(notsquare){
  ground.original <- rasterize_terrain(las.collection, res = res/hfactor, algorithm = knnidw(k = 10, p = 2, rmax = 50/hfactor))
}else{
  ground.original <- rasterize_terrain(las.collection, res = res/hfactor, algorithm = tin())
}
terra::crs(ground.original) <- crs.old[1] |> unlist()  
ground <- ground.original * zfactor
ground[ground > 10000 | ground < -10000] <- NA

writeRaster(ground, paste0(path.new,'/','ground0.tif'), overwrite=T)
ground <- rast(paste0(path.new,'/','ground0.tif'))
plot(ground)

#normalize height, remove outliers, generate canopy height model ----

for (i in 1:length(fl)){
  file.original <- fl[i]
  Las <- readLAS(paste0(path,"/",file.original), filter="-drop_class 7 18")
  st_crs(Las) <- crs.old[1] |> unlist()
  las.norm <- normalize_height(Las,  ground.original, na.rm = TRUE)
  las.norm <- filter_noise.LAS(las.norm, 1.5, hfactor)
  file.new <- paste0(stringr::str_split_fixed(file.original,'\\.',2)[1],'.laz')
  writeLAS(las.norm, paste0(path.norm,'/',file.new))
}

}

if(single){
  las.norm <- readLAS(paste0(path.norm,"/",file.original))}else{
    las.norm <- readLAScatalog(path.norm)}

timeA <- Sys.time()
#Create canopy model depending on whether data source has problems ----


if(!troubled){
  canopy <- rasterize_canopy(las.norm, res = res/hfactor, algorithm =
                          pitfree(thresholds = c(0, 5, 10, 15, 20, 25, 30, 45, 60)/zfactor, max_edge = c(0, 3)/hfactor))
}else{
  canopy <- rasterize_canopy(las.norm, res = res/hfactor, algorithm =
                          pitfree(thresholds = c(0, 5, 10, 15, 20, 25, 30, 45, 60)/zfactor, max_edge = c(0, 5)/hfactor, subcircle = subcircle/hfactor*1))
}

terra::crs(canopy) <- crs.old[1] |> unlist()
canopy <- canopy * zfactor
canopy[canopy > 116 | canopy < -1] <- NA

Sys.time()-timeA

#reproject raster to metric units centered to middle of raster ----

xcoord =(ext(ground)[1] + ext(ground)[2])/2
ycoord =(ext(ground)[3] + ext(ground)[4])/2

pt = data.frame(
  xcoord,ycoord
)
pt <- sf::st_as_sf(as.data.frame(pt), coords = c("xcoord","ycoord"), crs=st_crs(ground))
pt.trans <- (st_transform(pt,crs=st_crs('EPSG:4326')))
pt.trans <- st_coordinates(pt.trans)
pt.trans[,2]


wkt.new <- paste0('PROJCS["Centered Equal Area",
    GEOGCS["WGS 84",
        DATUM["WGS_1984",
            SPHEROID["WGS 84",6378137,298.257223563]],
        PRIMEM["Greenwich",0],
        UNIT["Degree",0.0174532925199433]],
    PROJECTION["Lambert_Azimuthal_Equal_Area"],
    PARAMETER["latitude_of_center",',pt.trans[,2],'],
    PARAMETER["longitude_of_center",',pt.trans[,1],'],
    PARAMETER["false_easting",0],
    PARAMETER["false_northing",0],
    UNIT["metre",1]]')

ex <- data.frame(rname=c('ul','ll','ur','lr'),
                 xcoord=c(ext(ground)[1],ext(ground)[1],ext(ground)[2],ext(ground)[2]),
                 ycoord=c(ext(ground)[3],ext(ground)[4],ext(ground)[3],ext(ground)[4])
)
ex <- sf::st_as_sf(as.data.frame(ex), coords = c("xcoord","ycoord"), crs=st_crs(ground))
ex.trans <- (st_transform(ex,crs=wkt.new))
ex.trans <- as.data.frame(st_coordinates(ex.trans))
xmn= min(ex.trans$X)
xmx= max(ex.trans$X)
ymn= min(ex.trans$Y)
ymx= max(ex.trans$Y)

y.rast <- rast(xmin=xmn, xmax=xmx,
               ymin=ymn, ymax=ymx, crs=wkt.new, res=res)

writeRaster(canopy, paste0(path.new,'/','canopy.tif'), overwrite=T) 
writeRaster(ground, paste0(path.new,'/','ground.tif'), overwrite=T)
ground2 <- rast(paste0(path.new,'/','ground.tif'))
canopy2 <- rast(paste0(path.new,'/','canopy.tif'))
if(projection.override){
crs(ground2) <- wkt(override.epsg)
crs(canopy2) <- wkt(override.epsg)}
ground<- project(ground2, y.rast, method = 'bilinear')
canopy<- project(canopy2, y.rast, method = 'bilinear')
if(normalizeerror){
  surface <- rasterize_canopy(las.collection, algorithm = dsmtin(max_edge = 0), res = res/hfactor)
  surface <- surface * zfactor
  writeRaster(surface, paste0(path.new,'/','surface.tif'), overwrite=T, options="COMPRESS=LZW")
  surface2 <- rast(paste0(path.new,'/','surface.tif'))
  surface<- project(surface2, y.rast, method = 'bilinear')
  canopy <- surface - ground
}
writeRaster(canopy, paste0(path.new,'/','canopy.tif'), overwrite=T, gdal=c("COMPRESS=LZW"))
writeRaster(ground, paste0(path.new,'/','ground.tif'), overwrite=T, gdal=c("COMPRESS=LZW"))

plot(canopy, breaks = c(0,2, 5,15,30,45,60, 115), col=c('white', 'pink', 'yellowgreen', 'green', 'darkgreen', 'cyan', 'blue'), maxcell=100000)

df <- as.data.frame(canopy, xy=TRUE)
hist(df[,3])

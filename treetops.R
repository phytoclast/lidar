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
  #function to determine crown search width
  f <- function(x) {
    y <- x^0.5/30^0.5*15
    y[y < 5] <- 5
    y[y > 25] <- 25
    return(y)
  }
  
  heights <- seq(-5,100,0.5)
  ws <- f(heights)
  plot(heights, ws, type = "l",  ylim = c(0,30))
  
 
  for (i in 1:length(folderlist)){i=11
  
  path <- paste0('output/',folderlist[i])
  
  canopy <- raster(paste0(path,'/','canopy.tif'))
  #find tops of trees
  ttops <- find_trees(canopy, lmf(f, hmin=5, shape='circular'))
  
  ttops2 <-  st_as_sf(ttops)
  st_write(ttops2,paste0(path,'/','ttps.shp'), driver="ESRI Shapefile", overwrite =TRUE, append = FALSE)
  #delineate crown boundaries
  canopy.trees <- dalponte2016(canopy, ttops, th_tree = 5, th_seed = 0.45, th_cr = 0.55, max_cr = 25, ID = "treeID")
  crowns <- canopy.trees()
  plot(crowns, col = pastel.colors(1000))
  
  v <- terra::as.polygons(rast(crowns))
  crowns.sf <- st_as_sf(as.data.frame(v, geom=TRUE), wkt="geometry", crs=crs(canopy))
  
  crowns.sf <- merge(crowns.sf, st_drop_geometry(ttops2), by.x='layer', by.y='treeID')
  st_write(crowns.sf,paste0(path,'/','crowns.shp'), driver="ESRI Shapefile", overwrite =TRUE, append = FALSE)
  }
  
  
library(lidR)
library(rgdal)
library(rlas)
library(progress)
library(future)
library(viridis)
library(dplyr)
library(stringr)
library(terra)
library(sf)
library(fasterize)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path.new <- 'output/hartwick'
ground <- rast(paste0(path.new,'/','ground.tif'))
surface <- rast(paste0(path.new,'/','surface.tif'))
canopy <- rast(paste0(path.new,'/','canopy.tif'))
#veg <- st_read('output/muir/muirwoods.shp') ; vegfield <- 'community'
#veg <- st_read('data/vegpolygons/GRSM_veg.shp') ; vegfield <- 'Short_name'
veg <- st_read('output/warren/mnfi.shp') ; vegfield <- 'SNAME'
veg$id <- as.numeric(rownames(veg))
veg.t <- st_drop_geometry(veg)
colnames(veg.t)[colnames(veg.t) == vegfield] <-  'community'
veg2 <- st_transform(veg, crs = crs(canopy))
veg.r <- fasterize(veg2, raster(canopy), field = 'id')
writeRaster(veg.r, 'output/veg.r.tif', overwrite=TRUE)
veg.r <- rast('output/veg.r.tif')
plot(canopy)

vht <- c(veg.r, canopy)
vht <- as.data.frame(vht, xy=T)
colnames(vht) <- c('x', 'y', 'id', 'ht')
vht <- subset(vht, !ht >100)
vht <- merge(vht, veg.t[,c('id', 'community')], by='id')

# cross section -----------------------------------------------------------

#vht$ny <- vht$x;vht$nx <- vht$y;vht$y <- vht$ny;vht$x <- vht$nx
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

#vht2 <- subset(vht, grepl('Cove', community))
vhtxs <-  subset(vht, x == getmode(vht$x))
q <- quantile(vhtxs$ht, 0.05); q05 <- data.frame(x=c(min(vht$y),max(vht$y)), y=c(q,q))
q <- quantile(vhtxs$ht, 0.1); q10 <- data.frame(x=c(min(vht$y),max(vht$y)), y=c(q,q))
q <- quantile(vhtxs$ht, 0.2); q20 <- data.frame(x=c(min(vht$y),max(vht$y)), y=c(q,q))
q <- quantile(vhtxs$ht, 0.5); q50 <- data.frame(x=c(min(vht$y),max(vht$y)), y=c(q,q))
q <- quantile(vhtxs$ht, 0.8); q80 <- data.frame(x=c(min(vht$y),max(vht$y)), y=c(q,q))
q <- quantile(vhtxs$ht, 0.9); q90 <- data.frame(x=c(min(vht$y),max(vht$y)), y=c(q,q))
q <- quantile(vhtxs$ht, 0.95); q95 <- data.frame(x=c(min(vht$y),max(vht$y)), y=c(q,q))

ggplot()+
    geom_line(data=vhtxs, mapping=aes(x=y, y=ht), col='darkgreen')+
    geom_line(data=q05, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
    geom_line(data=q10, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
    geom_line(data=q20, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
    geom_line(data=q50, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
    geom_line(data=q80, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
    geom_line(data=q90, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
    geom_line(data=q95, mapping=aes(x=x, y=y),alpha = 0.3, color='black', linetype='solid')+
    scale_y_continuous(name= "Canopy Height (m)", breaks=c(0,10,20,30,40,50,60,70,80,90,100))
    

# get cover by height -----------------------------------------------------------

covertab <- data.frame(community='zzz',cover=0, ht=0)

htclass <- c(1,2,3,5,10,15,20,25,30,35,40,45,50,55,60, 70, 80, 90)
 grptl <- aggregate(list(total = vht$ht), by=list(community = vht$community), FUN='length')
for (i in 1:length(htclass))
 { vht.sub <- subset(vht, ht < htclass[i])
 grcnt <- aggregate(list(count = vht.sub$ht), by=list(community = vht.sub$community), FUN='length')
 grcnt <- merge(grcnt, grptl, by='community', all.x=T)
 grcnt$cover <- (1-grcnt$count/grcnt$total)*100
 covertab1 <- cbind(grcnt[,c('community', 'cover')], ht=htclass[i])
 covertab <- rbind(covertab, covertab1)}
 covertab <- covertab[-1,]
 unique(covertab$community)
 # covertab.select <- subset(covertab, community %in% c('Cove forest (typic type)','Northern hardwood/acid hardwood forest', 'Yellow pine forest', 'Oak-hickory forest (typic acidic type)', 'Successional hardwood forest'))
 # ht.select <- subset(vht, Short_name %in% c('Cove forest (typic type)','Northern hardwood/acid hardwood forest', 'Yellow pine forest', 'Oak-hickory forest (typic acidic type)', 'Successional hardwood forest'))
 covertab.select <- covertab
 ht.select <- vht
 
 ggplot(covertab.select, aes(x=cover, y=ht, color=community))+
   geom_line()
 
 #porters <- covertab.select
 #hartwick <- covertab.select
 #warren <- covertab.select
 #muir <- covertab.select
 allstands <- rbind(hartwick,warren,muir,porters)
 #saveRDS(allstands, 'output/allstands.RDS')
 allstands <- readRDS('output/allstands.RDS')
 unique(allstands$community)
 
 selectedcoms  <- c('Floodplain Forest', 'Cove forest (typic type)','redwoods core', 'Yellow pine forest', 'Dry-mesic Northern Forest', 'Mesic Southern Forest', 'Successional hardwood forest',  "Ericaceous shrubs (heath bald type)")
 
 #selectedcoms  <- c("Cove forest (rich type)",  'Cove forest (typic type)')
 covertab.select <- subset(allstands, community %in% selectedcoms)
 
 renamed <-  data.frame(community = c('Floodplain Forest','Cove forest (typic type)','redwoods core', 'Yellow pine forest', 'Dry-mesic Northern Forest', 'Mesic Southern Forest', 'Successional hardwood forest',  "Ericaceous shrubs (heath bald type)"),Community = c('Floodplain Forest (Warren Woods)','Cove forest (GSMNP)','Redwoods (Muir Woods)', 'Yellow pine forest (GSMNP)', 'Dry-mesic Northern Forest (Hartwick Pines)', 'Mesic Southern Forest (Warren Woods))', 'Successional hardwood forest (GSMNP)',  "Heath Bald (GSMNP)"))
 
 covertab.select <-  merge(covertab.select, renamed, by='community', all.x=T)
 
 ggplot(covertab.select, aes(x=cover, y=ht, color=Community))+
    geom_line(size=1)+
    scale_x_continuous(name= "Canopy Cover (%)", 
                       breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
                           scale_y_continuous(name= "Canopy Height (m)", breaks=c(0,10,20,30,40,50,60,70,80,90,100))
 
 
 # ggplot(ht.select, aes(x=community, y=ht, color=community))+
 #    geom_boxplot() 
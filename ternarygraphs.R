library(ggplot2)
library(ggtern)
library(plyr)
vht.sum <- read.csv('output/vhtsum.csv')

vht.sum$gap <- vht.sum$open +vht.sum$tshrub.cover +vht.sum$tree05/2
vht.sum$canopy <- vht.sum$tree15 +vht.sum$tree30/2+vht.sum$tree05/2
vht.sum$emergent <- vht.sum$tree45 +vht.sum$tree60+vht.sum$tree30/2


ggtern(data=vht.sum,aes(x=gap,y=canopy, z=emergent)) +
  geom_point()+
  geom_text(aes(label=site), size=2)

vht.sum$magnitude <- (
  vht.sum$tshrub.cover*2+
  vht.sum$tree05*5+
  vht.sum$tree15*15+
  vht.sum$tree30*30+
  vht.sum$tree45*45+
  vht.sum$tree60*60)/60

vht.sum$stdev <- (
  (vht.sum$magnitude - 0)^2*vht.sum$open+ 
    (vht.sum$magnitude - 2)^2*vht.sum$tshrub.cover+
    (vht.sum$magnitude - 5)^2*vht.sum$tree05+
    (vht.sum$magnitude - 15)^2*vht.sum$tree15+
    (vht.sum$magnitude - 30)^2*vht.sum$tree30+
    (vht.sum$magnitude - 45)^2*vht.sum$tree45+
    (vht.sum$magnitude - 60)^2*vht.sum$tree60
  )^0.5/(vht.sum$magnitude+30)*2

detach(package:ggtern, unload=TRUE)
detach(package:ggplot2, unload=TRUE)
library(ggplot2)


  vht.trans <- as.data.frame(rbind(
    cbind(site=vht.sum$site, ht=0, cover=vht.sum$open),
    cbind(site=vht.sum$site, ht=2, cover=vht.sum$tshrub.cover),
    cbind(site=vht.sum$site, ht=5, cover=vht.sum$tree05),
    cbind(site=vht.sum$site, ht=15, cover=vht.sum$tree15),
    cbind(site=vht.sum$site, ht=30, cover=vht.sum$tree30),
    cbind(site=vht.sum$site, ht=45, cover=vht.sum$tree45),
    cbind(site=vht.sum$site, ht=60, cover=vht.sum$tree60)
  ))
  vht.trans$ht <- as.numeric(vht.trans$ht)
  vht.trans$cover <- as.numeric(vht.trans$cover)
  vht.select <- subset(vht.trans, site %in% 
                         c('cove hardwoods', 
                           'ramsey cove hardwood', 
                           'successional cove hardwoods'))

  ggplot(vht.select, aes(y=cover, x=ht, color=site, fill=site))+
    geom_area(alpha=0.1,   position = "identity")+
    scale_x_continuous(name= "Tree Height (m)", 
                       breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60))+
    labs(caption = 'Tree height distribution')+
    scale_y_continuous(name= "Cover (%)", 
                       breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
    coord_flip()
#master vht dataset ----
vht <- readRDS('output/vht.RDS')
  sort(unique(vht$site))
  a.cove <- c('cove hardwoods', 
              'ramsey cove hardwood', 
              'successional cove hardwoods')
  n.cove <- c('old-growth cove hardwood', 
              'old-growth cove hardwood', 
              'successional cove hardwoods')
  
  a.beech <- c('warren beech-maple', 'baker beech-maple','russ beech-maple-tulip',
               'clinton successional beech-maple','eaton successional beech-maple',
               'ruderal woodlot'
  )
  n.beech <- c('1. old-growth beech-maple', '1. old-growth beech-maple','1. old-growth beech-maple',
               '2. successional beech-maple','2. successional beech-maple',
               '3. ruderal woodlot'
  )

  
  a.westcoast <-  c('redwoods humbolt', 
                    'sitka spruce rainforest', 
                    'redwoods')
  n.westcoast <-  c('Humbolt Redwoods', 
                    'Olympic Rainforest', 
                    'Muir Redwoods')
  a.smoky <- c("cove hardwoods","ramsey cove hardwood", "ramsey northern hardwoods" ,
               "oak-chestnut","ramsey oak-chestnut", "yellow pine","heath bald", "ramsey heath bald")
  
  n.smoky <- c("cove hardwoods","cove hardwoods", "northern hardwoods" ,
               "oak-chestnut","oak-chestnut", "yellow pine","heath bald", "heath bald"  )
  
  a.mich <- c('warren beech-maple', 'baker beech-maple','russ beech-maple-tulip',"dry-mesic forest","hartwick white pine-red pine","mio jack pine 19 y", "mio jack pine 39 y", "northern hardwoods cove", "northern mesic forest" )
  n.mich <- c('southern mesic forest', 'southern mesic forest','southern mesic forest',"southern dry-mesic forest","white pine forest","jack pine barrens", "jack pine barrens", "northern mesic forest", "northern mesic forest" )
  
  a.briar <- c('northern hardwoods sunslope','northern hardwoods cove')
  n.briar <- c('northern hardwoods sunslope','northern hardwoods cove')
  
  a.fl <- c('florida bottomland','florida ravine','longleaf pine')
  n.fl <- c('florida bottomland','florida ravine','longleaf pine')
  
  a.yunque <- c('yunque 2015 high','yunque 2015 low','yunque 2018 high','yunque 2018 low')
  n.yunque <- c('cloud forest','rainforest','cloud forest post-Maria','rainforest post-Maria')
  
  
  a.pr <- c('yunque 2015 high','prdry upland','prdry lowland', 'prrainforest', 'selva lowland')
  n.pr <- c('3.dwarf cloud forest','4.tropical dry forest','4.tropical dry forest', '2.island tropical rainforest', '1.mainland tropical rainforest')
  
mygroup <- 'briar'
a.group <- get(paste0('a.',mygroup))
n.group <- get(paste0('n.',mygroup))

vht.select <- subset(vht, site %in% 
                       c(a.group)
            )

vht.select$vegetation <- vht.select$site
for(i in 1:length(a.group)){
vht.select$vegetation <-  gsub(paste0('^',a.group[i],'$'), n.group[i], vht.select$vegetation)
}

vht.select.sum <- ddply(vht.select, "vegetation", summarise,
                        mean=mean(ht),
                        median=median(ht),
                        sd =sd(ht),
                        sdm =sd(ht)/(mean(ht)+1)*100 )


profile <- 
ggplot(vht.select, aes(y=ht, color=vegetation, fill=vegetation))+
  geom_density(alpha=0.2, bw=5/4)+
  scale_y_continuous(name= "Canopy Height (m)", 
                     breaks=c(0:22*5))

profile

filename <- paste0('output/profile.',mygroup,'.png')
png(filename=filename,width = 600, height =600, units = "px", pointsize = 3)
par(mar = c(1,1,1,1))
profile
dev.off()


boxplot <- 
ggplot(vht.select, aes(y=ht, x=vegetation, color=vegetation, fill=vegetation))+
  geom_boxplot(alpha=0.1)+
  scale_y_continuous(name= "Canopy Height (m)", 
                     breaks=c(0:22*5))+  
  scale_x_discrete(name= "Vegetation")+
  labs(title = 'Canopy height distribution')
boxplot
filename <- paste0('output/boxplot.',mygroup,'.png')
png(filename=filename,width = 600, height =600, units = "px", pointsize = 3)
par(mar = c(1,1,1,1))
boxplot
dev.off()



cummulative <- 
ggplot(vht.select, aes(x=ht, color=vegetation))+
  stat_ecdf()+
  scale_x_continuous(name= "Canopy Height (m)", 
                     breaks=c(0:22*5))+
  scale_y_reverse(name= "Cumulative Cover (%)", 
                     breaks=c(0:10/10),labels=c((10-(0:10))*10))+
    coord_flip()
  
cummulative
filename <- paste0('output/cummulative.',mygroup,'.png')
png(filename=filename,width = 600, height =600, units = "px", pointsize = 3)
par(mar = c(1,1,1,1))
cummulative
dev.off()

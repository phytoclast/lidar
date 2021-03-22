library(plyr)
library(Hmisc)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))#crowns

crownsum <- read.csv('output/crownsum.csv')
vhthectare <- read.csv('output/vhthectare.csv')

combsum <- merge(crownsum, vhthectare, by='site')

combsum$tree05 <- round(combsum$pretree05*combsum$strattree/100,2)
combsum$tree15 <- round(combsum$pretree15*combsum$strattree/100,2)
combsum$tree30 <- round(combsum$pretree30*combsum$strattree/100,2)
combsum$tree45 <- round(combsum$pretree45*combsum$strattree/100,2)
combsum$tree60 <- round(combsum$pretree60*combsum$strattree/100,2)
combsum <- subset(combsum, select = -c(pretree05, pretree15, pretree30, pretree45, pretree60))

write.csv(combsum,'output/combsum.csv', row.names = F)

v <- read.csv('output/combsum.csv')

v$base <- ifelse((v$strattree >= 10), 
                ifelse(v$strattree >= 65, 'forest','woodland'),
                ifelse(v$strat02 >= 10, 'shrubland','open'))

v$mod1 <- ifelse(v$base %in%c('woodland','forest'), 
                 ifelse(v$crown50 < 15, 'low',
                        ifelse(v$crown50 < 30, 'medium',
                               ifelse(v$c100m < 45, 'high',
                                      ifelse(v$crown50 < 45|v$c100m < 60, 'tall','giant')))),'')

v$mod2 <- ifelse(v$base %in%c('woodland'), 
                 ifelse(v$strat02 >= 10,
                        ifelse(v$strat00 < 25,'shrubby','open shrubby')
                        ,'open'),
                 ifelse(v$base %in%c('shrubland'),
                        ifelse(v$strat00 < 25,'dense','open'),''))

v$veg <-stringr::str_squish(paste(v$mod2, v$mod1, v$base))
             
write.csv(v,'output/keyveg.csv', row.names = F)

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
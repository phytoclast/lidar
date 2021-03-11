library(plyr)
library(Hmisc)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))#crowns

crown.dist <- read.csv('output/crowndist.csv')
crown.dist$strat05 <- ifelse(crown.dist$htmax >= 5 & crown.dist$htmax < 15,1,0)
crown.dist$strat15 <- ifelse(crown.dist$htmax >= 15 & crown.dist$htmax < 30,1,0)
crown.dist$strat30 <- ifelse(crown.dist$htmax >= 30 & crown.dist$htmax < 45,1,0)
crown.dist$strat45 <- ifelse(crown.dist$htmax >= 45 & crown.dist$htmax < 60,1,0)
crown.dist$strat60 <- ifelse(crown.dist$htmax >= 60, 1,0)
percentiles.crowns <- ddply(crown.dist, .(site), summarise,
                            width50 = round(wtd.quantile(width,weights=area, 0.5),2),
                            canopy50 = round(wtd.quantile(ht50,weights=area, 0.5),2),
                            pretree05 = round(wtd.mean(strat05,weights=area)*100,3),
                            pretree15 = round(wtd.mean(strat15,weights=area)*100,3),
                            pretree30 = round(wtd.mean(strat30,weights=area)*100,3),
                            pretree45 = round(wtd.mean(strat45,weights=area)*100,3),
                            pretree60 = round(wtd.mean(strat60,weights=area)*100,3),
                            crown05 = round(wtd.quantile(htmax,weights=area, 0.05),2),
                            crown25 = round(wtd.quantile(htmax,weights=area, 0.25),2),
                            crown50 = round(wtd.quantile(htmax,weights=area, 0.50),2),
                            crown75 = round(wtd.quantile(htmax,weights=area, 0.75),2),
                            crown95 = round(wtd.quantile(htmax,weights=area, 0.95),2),
                            crownmax = round(max(htmax),2)
)
write.csv(percentiles.crowns,'output/crownsum.csv', row.names = F)

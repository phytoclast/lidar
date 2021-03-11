library(plyr)
library(Hmisc)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))#crowns

crown.dist <- read.csv('output/crowndist.csv')

percentiles.crowns <- ddply(crown.dist, .(site), summarise,
                            width50 = round(wtd.quantile(width,weights=area, 0.5),2),
                            canopy50 = round(wtd.quantile(ht50,weights=area, 0.5),2),
                            crown05 = round(wtd.quantile(htmax,weights=area, 0.05),2),
                            crown25 = round(wtd.quantile(htmax,weights=area, 0.25),2),
                            crown50 = round(wtd.quantile(htmax,weights=area, 0.50),2),
                            crown75 = round(wtd.quantile(htmax,weights=area, 0.75),2),
                            crown95 = round(wtd.quantile(htmax,weights=area, 0.95),2),
                            crownmax = round(max(htmax),2)
)
write.csv(percentiles.crowns,'output/crownsum.csv', row.names = F)

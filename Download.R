library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
wd <- 'udell'
fn <- 'data.txt'
laz.path <- paste0('data/', wd,'/laz')
filelist <- paste0('data/', wd,'/',fn)
urls <- read.delim(filelist, header = F)
urls <- subset(urls, grepl('.laz$|.LAZ$',V1))
urls$fcount <- str_count(urls$V1, '/')
urls$fn <-  str_split_fixed(urls$V1, '/', urls$fcount[1]+1)[,urls$fcount[1]+1]
if(!dir.exists(laz.path)){dir.create(laz.path)}
for (i in 1:nrow(urls)){
  download.file(urls$V1[i], paste0('data/', wd,'/laz/', urls$fn[i]), method = 'curl')
}

##### download metadata ----
urls <- read.delim(filelist, header = F)
urls <- subset(urls, grepl('.zip$|.ZIP$',V1))
urls$fcount <- str_count(urls$V1, '/')
urls$fn <-  str_split_fixed(urls$V1, '/', urls$fcount[1]+1)[,urls$fcount[1]+1]

download.file(urls$V1[1], paste0('data/', wd,'/', urls$fn[1]), method = 'curl')

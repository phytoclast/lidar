#evaluating lidR package for individual tree segmentation at point cloud level.
# Trevor Hobbs, Resource Information Manager, Huron-Manistee NF.
# Movember 2020
#remotes::install_github("Jean-Romain/lidR", ref = "devel")
library(lidR)
library(rgdal)
library(rlas)
library(mapview)
library(progress)
library(future)
library(viridis)
library(dplyr)
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##############  Batch Processing  ###############

#Build a LASCatalog class to first process a height normalized set of .las files...
#
#
#
path <- 'data/gourdneck'
path.new <- 'output/gourdneck'
fl <- list.files(path)
for (i in 1:length(fl)){
  file.original <- fl[i]
  Las <- readLAS(paste0(path,"/",file.original), filter="-drop_class 7 18")
  Las2 <- lastransform(Las, CRS('+proj=lcc +lat_1=45.7 +lat_2=44.18333333333333 +lat_0=43.31666666666667 +lon_0=-84.36666666666666 +x_0=6000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +vunits=m +no_defs'))
  Las2$Z <- Las2$Z *.3048
  file.new <- paste0(stringr::str_split_fixed(file.original,'\\.',2)[1],'.las')
  
  writeLAS(Las2, paste0(path.new,'/',file.new))
}

Las2@proj4string

las.collection <- readLAScatalog(path.new)

#opt_filter(las.collection) = '2'

dtm_tin <- grid_terrain(las.collection, res = 5, algorithm = tin())
writeRaster(dtm_tin, paste0(path.new,'/','ground.tif'))

normalize_height(las.collection, tin())
chm<- chm(chm_ctg, res = 5, p2r())
writeRaster(dtm_tin, paste0(path.new,'/','canopy.tif'))
#specify output directory...


#specify options like las class, loading specific attributes (to minimize processing time), and optimal chunk size...
opt_filter(las.collection)<- "-drop_class 7 18"
opt_select(las.collection)<- "xyz"
opt_chunk_size(las.collection)<- 625

#initiate multi-core processing using the future package
plan(multisession)

#subtract ground elevations across catalog, using no discretization/rasterization (more memory intensive, but no intermediate raster files or artifacts)
normalize_height(las.collection, tin())

#build a new catalog for processing chm and tree tops from the normalized height .las files...
chm_ctg<- readLAScatalog("output/a")

#specify output directory...
opt_output_files(chm_ctg)<- "output/a/{ORIGINALFILENAME}_CHM"

#create canopy height model.  Output is written to disk in tile
chm<- grid_canopy(chm_ctg, res = 5, p2r())

#Detect tree tops to be used in segmentation algorithm later. *Note, ttops in this case is built from the rasterized chm, not the point cloud-based LAScatalog. This saves the output as a spatial points data frame successfully...
ttops <- find_trees(chm, lmf(25), uniqueness = "bitmerge")

#create value for the desired algorithm, passed to 78 below...
algo<- dalponte2016(chm, ttops, th_tree = 5, th_seed = 0.45, th_cr = 0.55, max_cr = 100, ID = "treeID")

#perform tree segmentation at point cloud level on the canopy height model catalog.  Outputs will be a new set of las files with "treeID" saved as new attribute.
segment_trees(chm_ctg, algo)

#build a new catalog for evaluating results, based on the outputs of segmentation...
segs_ctg<- readLAScatalog("G:/LiDAR_Luzerne/TreeSeg/CHM")

#computer standard metrics on the indivdiual segmented trees
TreeMetricsDF = tree_metrics(segs_ctg, .stdtreemetrics, attribute = "treeID")

#check to confirm that the individual trees that span across chunk boundaries have one unique ID (rather than two).  Needs further testing, but appears to work.  The coordinates should span across tile boundaries...
lasplot1 <- clip_circle(segs_ctg, 19705625, 467500, 200)
lasplot2 <- clip_circle(segs_ctg, 19704375, 466250, 200)
lasplot3 <- clip_circle(segs_ctg, 19704375, 463750, 200)
lasplot4 <- clip_circle(segs_ctg, 19705000, 465000, 200)


plot(lasplot1, color = "treeID", bg = "black", size = 2)
plot(lasplot2, color = "treeID", bg = "black", size = 2)
plot(lasplot3, color = "treeID", bg = "black", size = 2)
plot(lasplot4, color = "treeID", bg = "black", size = 2)



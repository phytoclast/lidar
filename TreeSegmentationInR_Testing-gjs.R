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

############### WORKING ON AN INDIVIDUAL TILE ###############

#read in a las file, selecting only needed attributes, and filtering low and high noise...
Las<- readLAS("data/USGS_LPC_MI_FEMA_2019_C19_605517.las", filter="-drop_class 7 18")

#initiate multi-core processing using the future package
plan(multisession)

#subtract ground elevations to normalize height...
normLas<- normalize_height(Las, tin())

#create a canopy height model from the normalized point cloud...
chm <- grid_canopy(normLas, res = 2, pitfree(thresholds = c(0, 2, 6, 40, 75), max_edge = c(0, 1), subcircle = 0))

#detect tree high points...
ttops <- find_trees(normLas, lmf(13, hmin = 10, shape = c("circular")))

#plot tree high points...
plot(ttops)

#write tree tops to shapefiles...
writeOGR(obj = ttops, dsn = "output", layer = "Stand_66Ttops_lmf12", driver = "ESRI Shapefile")

#write normalized height las
writeLAS(normLas, "output/Stand_5_normalized.las", index = FALSE)

#segment trees using dalponte2016 algorithm...
segmentedLas <- segment_trees(normLas, dalponte2016(chm, ttops, th_tree = 5, th_seed = 0.65, th_cr = 0.55, max_cr = 18, ID = "treeID"))

#Generate random color scheme for visualizing tree IDs...
col <- random.colors(6900)

#view segmented treees...
plot(segmentedLas, color = "treeID", clear_artifacts = TRUE, colorPalette = col)

#create a value out of the point cloud plot, colorized by tree ID, which can be passed on below (line 55)...
PointsAndTrees = plot(segmentedLas, color = "treeID", clear_artifacts = TRUE, colorPalette = col)

#view the segmented trees with the tree tops points...
add_treetops3d(PointsAndTrees, ttops, z = "Z", clear_artifacts = TRUE)

##############  Use this code to export the point cloud so that it can be viewed in GIS by TreeID  ###############

#remove points that are not assigned to a tree
trees = filter_poi(segmentedLas, !is.na(treeID))

#examine
plot(trees, color = "treeID", colorPalette = col)

#format for copying rows to overwrite - this essentially assigns the tree ID to the intensity field
trees$Intensity = trees$treeID

#check to see if it worked
head(trees)

#remove all NA values from here too
trees = filter_poi(segmentedLas, !is.na(Intensity))

#re-add ground heights if desired...
ElevSegmented<- unnormalize_height(trees)

# check max number of identified trees, because the intensity attribute is 16 bit, the max number cannot be more than 65,535. Write this number down, so you can set it as the max value in the symbology for ArcPRO (draw using intensity, random color scheme, stretch type=none, value source=custom, set max to this number...)

maxNumTrees<- max(ElevSegmented$Intensity, na.rm = TRUE)

# for more clarity in rendering, it is necessary to "scramble" the treeID numbers, which are sequential by default, resulting in tree IDs next to each other appearing as though they are part of the same tree in almost any color scheme.  The following code creates a vector of random numbers between 1-65535(the max possibilities for 16-bit) for the max number of trees in the segmented point cloud. Then re-assigns points to new randomly generated tree IDs...

TreeIDList<- sample.int(65535, maxNumTrees)

TreeIDListDF<- data.frame(ElevSegmented$Intensity)

RandomTreeIDList<-TreeIDListDF %>%
  group_by(ElevSegmented.Intensity) %>%
  mutate(random = sample(maxNumTrees,1))

#create a new las class object with an added custom attribute, which can be used to swap into the intensity slot
ElevSegmentedRandomTreeID<- add_lasattribute(ElevSegmented, RandomTreeIDList$random, "TreeIDRandom", "random integer assigned to Tree")

head(ElevSegmentedRandomTreeID)

#replace the Intensity with Randomized treeIDList...
ElevSegmentedRandomTreeID$Intensity=ElevSegmentedRandomTreeID$TreeIDRandom

#write Las file...
writeLAS(las = ElevSegmentedRandomTreeID, file = "output/Stand__5_ntrees.las")


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



norm_ctg1<- readLAScatalog("data/gourdneck")


#specify output directory...
opt_output_files(norm_ctg)<- "output/gourdneck"

#specify options like las class, loading specific attributes (to minimize processing time), and optimal chunk size...
opt_filter(norm_ctg)<- "-drop_class 7 18"
opt_select(norm_ctg)<- "xyz"
opt_chunk_size(norm_ctg)<-625

#initiate multi-core processing using the future package
plan(multisession)

#subtract ground elevations across catalog, using no discretization/rasterization (more memory intensive, but no intermediate raster files or artifacts)
normalize_height(norm_ctg, tin())

#build a new catalog for processing chm and tree tops from the normalized height .las files...
chm_ctg<- readLAScatalog("J:/Huron_NF_LiDAR_Intensity_Normalized/point_cloud/Normalized")

#specify output directory...
opt_output_files(chm_ctg)<- "G:/LiDAR_Luzerne/TreeSeg/CHM/{ORIGINALFILENAME}_CHM"

#create canopy height model.  Output is written to disk in tile
chm<- grid_canopy(chm_ctg, res = 2, p2r())

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



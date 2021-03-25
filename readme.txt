General sequence of scripts
1. Download.R #Downloads several lidar tiles by supplying a list of file urls from the USGS download site.
2. Lidarnorm.R #Renders a bare earth DEM, then uses this to normalize point clouds, then renders a canopy height model.
3. Make_Crowns.R #Delineates crowns for later analysis.
4. PreCrown_summary.R #Summarizes each canopy height model by crown.
5. Crown_summary.R #Summarizes each stand as defined by a shapefile in terms of each crown.
6. Hectare_summary.R #Summarizes each stand as defined by a shapefile in terms of maximum by fixed areas.
7. combined_summary.R #Combines the two summary tables so that they can later be selected and displayed in a spreadsheet.
8. Lidar_shade.R #Generates a shaded relief from the initial canopy height models and DEMs generated in step 2.
8. ternarygraphs.R #optional export of canopy height model as tables by stand as defined by a shapefile, for use in some of the optional graphs.
10. Lidar_summary2.R #optional graphs.
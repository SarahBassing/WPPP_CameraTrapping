    ##  Study Area Maps & Grid
    ##  February 26, 2018
################################################################################
  ####  Initial set up  ####

  #  Load packages
  library(rgdal)
  library(rgeos)
  library(raster)
  library(tidyr)
  library(dplyr)
  
  #  Read in study area shapefiles
  MW_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "METHOW_SA") #MW
  NE_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "NE_SA")  #NE
  GMUS <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "GMU_Generalized") #gmus
  
  #  Set study area projection
  #sa_proj <- projection(MW)
  
  #  Reproject shapefiles to be in meters, not feet
  #  +proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 
  #  +lon_0=-120.5 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs 
  new_proj <- CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  
  MW <- spTransform(MW_SA, new_proj)
  NE <- spTransform(NE_SA, new_proj)
  gmus <- spTransform(GMUS, new_proj)
    
  #  Set bounding boxes for each study area
  bb_MW <- bbox(MW)
  bb_NE <- bbox(NE)
  
################################################################################  
  ####  Superimpose grid across study areas  ####
  
  #  Grid cell size: 190 km2 (= 1.9e+08 m2)
  #  => 13.78405km x 13.78405km cell = 13784.05m x 13784.05m cell
  #  Cell size represents mean size of female cougar home range in both study areas
  #  Means are slightly different per study area but statistically no real diff.
  #  in size.  Mean cougar homerange data courtesy of B. Kertson
  
  #  Middle grid cell size resolution: 57 km2 (= 5.7e+07 m2)
  #  => 7.55km x 7.55km cell = 7550m x 7550m cell
  #  Cell size represents mean size of black bear home range in Washington
  #  Data courtesy of R. Beausoleil
  
  #  Small grid size resolution: 25 km2 (= 2.5e+7 m2)
  #  => 5km x5km cell = 5000m x 5000m cell
  #  Cell size intended to represent approximate coyote territory in WA??? Total guess.
  
  #  Smallest grid size resolution: 1 km2 (= 1e+6 m2)
  #  ==> 1km x 1km cell = 1000m x 1000m cell
  #  Cell size is finest resolution I can realistically go.  Using this so that camera 
  #  site is as representative of the entire cell as possible (compared to larger
  #  resolutions).  Also, 1 km2 is about the average size of a deer homerange.
################################################################################  
  ####  Make grids  ####
  
  #  Create reference raster to which everything is sampled
  #  Resolution equal to average female cougar home range (13784.05m x 13784.05m)
  # cell_res <- 190
  # MW_ref <- raster(extent(bb_MW), crs = projection(new_proj), res = 13784.05)
  # NE_ref <- raster(extent(bb_NE), crs = projection(new_proj), res = 13784.05)
  #  Resolution equal to average black bear home range (7550m x 7550m)
  # cell_res <- 57
  # MW_ref <- raster(extent(bb_MW), crs = projection(new_proj), res = 7550)
  # NE_ref <- raster(extent(bb_NE), crs = projection(new_proj), res = 7550)
  #  Resolution equal to made up coyote territory size (5000m x 5000m)
  # cell_res <- 25
  # MW_ref <- raster(extent(bb_MW), crs = projection(new_proj), res = 5000)
  # NE_ref <- raster(extent(bb_NE), crs = projection(new_proj), res = 5000)
  cell_res <- 1
  MW_ref <- raster(extent(bb_MW), crs = projection(new_proj), res = 1000)
  NE_ref <- raster(extent(bb_NE), crs = projection(new_proj), res = 1000)


  #  Transform rasters into polygons to help visualize
  MW_poly <- rasterToPolygons(MW_ref)
  NE_poly <- rasterToPolygons(NE_ref)

  #  Add cell id to new polygons
  MW_cells <- 1:ncell(MW_poly)
  NE_cells <- 1:ncell(NE_poly)

  MW_poly@data$cell_ID <- c(1:length(MW_cells))
  NE_poly@data$cell_ID <- c(1:length(NE_cells))

  #  Intersect both polygons
  MW_inter <- raster::intersect(MW, MW_poly)
  NE_inter <- raster::intersect(NE, NE_poly)
  #  Save it: be sure you are saving the correct grid cell size to the correct shapefile name!
  #writeOGR(MW_inter, dsn = "./Shapefiles", layer = "MW_190kmgrid", driver = "ESRI Shapefile", overwrite = T)
  #writeOGR(NE_inter, dsn = "./Shapefiles", layer = "NE_190kmgrid", driver = "ESRI Shapefile", overwrite = T)
  #writeOGR(MW_inter, dsn = "./Shapefiles", layer = "MW_57kmgrid", driver = "ESRI Shapefile", overwrite = T)
  #writeOGR(NE_inter, dsn = "./Shapefiles", layer = "NE_57kmgrid", driver = "ESRI Shapefile", overwrite = T)
  #writeOGR(MW_inter, dsn = "./Shapefiles", layer = "MW_25kmgrid", driver = "ESRI Shapefile", overwrite = T)
  #writeOGR(NE_inter, dsn = "./Shapefiles", layer = "NE_25kmgrid", driver = "ESRI Shapefile", overwrite = T)
  # writeOGR(MW_inter, dsn = "./Shapefiles", layer = "MW_1kmgrid", driver = "ESRI Shapefile", overwrite = T)
  # writeOGR(NE_inter, dsn = "./Shapefiles", layer = "NE_1kmgrid", driver = "ESRI Shapefile", overwrite = T)

################################################################################
  ####  Extract information from your grid  ####
  
  #  Read in the grid cell shapefiles
  MW_inter <- readOGR("./Shapefiles", layer = "MW_1kmgrid")
  NE_inter <- readOGR("./Shapefiles", layer = "NE_1kmgrid")
  
  #  Calculate area of each "intersected cell" in m2
  MW_area <- rgeos::gArea(MW_inter, byid = T)
  NE_area <- rgeos::gArea(NE_inter, byid = T)
  
  #  Combine intersected polygons with areas
  MW_edge <- cbind(MW_inter@data, MW_area)
  NE_edge <- cbind(NE_inter@data, NE_area)
  
  #  Calculate proportion of each cell within study area
  MW_prop <- MW_edge %>%
    mutate(MW_area_km2 = MW_area/1000000,
           Prop = MW_area_km2/cell_res)
  NE_prop <- NE_edge %>%
    mutate(NE_area_km2 = NE_area/1000000,
           Prop = NE_area_km2/cell_res)
  
  #  Count the number of cells (including partials) within study areas
  mw_cells <- MW_inter@data$cell_ID
  ncells_MW <- length(unique(mw_cells))
  
  ne_cells <- NE_inter@data$cell_ID
  ncells_NE <- length(unique(ne_cells))
  
  #  How many cells overlap with the study area by less than 99% (originally 25%)?
  # prop25_MW <- MW_prop[MW_prop$Prop < "0.25", ]  # tells you which ones
  # prop25_NE <- NE_prop[NE_prop$Prop < "0.25", ]  # tells you which ones
  prop99_MW <- MW_prop[MW_prop$Prop < "0.99", ]  # tells you which ones
  prop99_NE <- NE_prop[NE_prop$Prop < "0.99", ]  # tells you which ones
  
  # MW_partial <- length(unique(prop25_MW$cell_ID))  # gives you a count
  # NE_partial <- length(unique(prop25_NE$cell_ID))  # gives you a count
  MW_partial <- length(unique(prop99_MW$cell_ID))  # gives you a count
  NE_partial <- length(unique(prop99_NE$cell_ID))  # gives you a count
  
  
  
  #  With 65 cameras in the MW and 55 in the NE, what does that look like?
  #  (Originally: With 60 cameras per study area what does this look like?)
  #  Toss cells that are less than 99% within study area
  #  What percent of sample units are being surveyed?
  MW_cells <- ncells_MW - MW_partial; 65/MW_cells
  NE_cells <- ncells_NE - NE_partial; 55/NE_cells
  
################################################################################
  ####  Simple Random Sample  ####
  
  #  Randomly select sample units to survey
  #  But first: toss sample units with less than 99% (25%) falls within study area (prop99_)
  #  And exclude sample units within 1 km of town limits
  
################################################################################
  
  #  First: create sequential cell nmbr corresponding to cell_ID from original grid
  nmb <- 1:MW_cells
  nmb2 <- 1:NE_cells
  
  #  Identify cells where at least 99% (25%) of cell falls within study area
  #  The partial cells cannot be chosen for sampling now
  prop100_MW <- MW_prop[MW_prop$Prop >= "0.99", 3]
  prop100_NE <- NE_prop[NE_prop$Prop >= "0.99", 3]
  
  #  New dataframe listing out new cell number and original cell_ID
  nMW_cells <- as.data.frame(cbind(nmb, prop100_MW))
  colnames(nMW_cells) <- c("ncell", "cell_ID")
  nNE_cells <- as.data.frame(cbind(nmb2, prop100_NE))
  colnames(nNE_cells) <- c("ncell", "cell_ID")
  
  #  Now pick 60 random cells from each study area
  set.seed(24)
  MW_rand <- sample(nMW_cells$cell_ID, 65, replace = F)
  set.seed(24)
  NE_rand <- sample(nNE_cells$cell_ID, 55, replace = F)
  
  #  Annoying formating so I can merge selected cells with full list of cells
  mw_selected <- rep(1, 65)
  MW_sites <- cbind(MW_rand, mw_selected)
  MW_sites <- as.data.frame(MW_sites)
  colnames(MW_sites) <- c("cell_ID", "selected")
  ne_selected <- rep(1, 55)
  NE_sites <- cbind(NE_rand, ne_selected)
  NE_sites <- as.data.frame(NE_sites)
  colnames(NE_sites) <- c("cell_ID", "selected")
  
  #  Create dataframe showing which sites have been selected across study areas
  mw_grid <- as.data.frame(MW_poly@data$cell_ID)
  #mw_cells <- as.data.frame(mw_cells) # if I only focus on grid cells within study area
  colnames(mw_grid) <- "cell_ID"
  MW_cams <- mw_grid %>%
    full_join(MW_sites, by = "cell_ID") %>%
    transmute(
      cell_ID = as.numeric(cell_ID),
      selected = ifelse(is.na(selected), 0, 1)
    )
  
  ne_grid <- as.data.frame(NE_poly@data$cell_ID)
  #ne_cells <- as.data.frame(ne_cells)  # if I only focus on grid cells within study area
  colnames(ne_grid) <- "cell_ID"
  NE_cams <- ne_grid %>%
    full_join(NE_sites, by = "cell_ID") %>%
    transmute(
      cell_ID = as.numeric(cell_ID),
      selected = ifelse(is.na(selected), 0, 1)
    )
  
  #  Add selected camera dataframe to study area polygon shapefiles
  #  keep in mind this is the full bounding box grid so cells outside study area are missing values
  MW_poly@data$cam_sites <- MW_cams  #MW_inter if I only focus on grid cells within study area
  NE_poly@data$cam_sites <- NE_cams  #NE_inter
  
  #  Rasterize study area polygon shapefiles
  MW_camlocs <- rasterize(MW_poly, MW_ref, field = MW_poly@data$cam_sites$selected)  #MW_inter
  NE_camlocs <- rasterize(NE_poly, NE_ref, field = NE_poly@data$cam_sites$selected)  #NE_inter

  #  Write Raster
  # writeRaster(MW_camlocs, filename = "./Shapefiles/MW_camlocs.grd", format = 'raster', overwrite = T)
  # writeRaster(MW_camlocs, filename = "./Shapefiles/MW_camlocs_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  # writeRaster(NE_camlocs, filename = "./Shapefiles/NE_camlocs.grd", format = 'raster', overwrite = T)
  # writeRaster(NE_camlocs, filename = "./Shapefiles/NE_camlocs_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  # writeRaster(MW_camlocs, filename = "./Shapefiles/MW_camlocs_mid.grd", format = 'raster', overwrite = T)
  # writeRaster(MW_camlocs, filename = "./Shapefiles/MW_camlocs_mid_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  # writeRaster(NE_camlocs, filename = "./Shapefiles/NE_camlocs_mid.grd", format = 'raster', overwrite = T)
  # writeRaster(NE_camlocs, filename = "./Shapefiles/NE_camlocs_mid_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  # writeRaster(MW_camlocs, filename = "./Shapefiles/MW_camlocs_small.grd", format = 'raster', overwrite = T)
  # writeRaster(MW_camlocs, filename = "./Shapefiles/MW_camlocs_small_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  # writeRaster(NE_camlocs, filename = "./Shapefiles/NE_camlocs_small.grd", format = 'raster', overwrite = T)
  # writeRaster(NE_camlocs, filename = "./Shapefiles/NE_camlocs_small_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  # writeRaster(MW_camlocs, filename = "./Shapefiles/MW_camlocs_1km.grd", format = 'raster', overwrite = T)
  # writeRaster(MW_camlocs, filename = "./Shapefiles/MW_camlocs_1km_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  # writeRaster(NE_camlocs, filename = "./Shapefiles/NE_camlocs_1km.grd", format = 'raster', overwrite = T)
  # writeRaster(NE_camlocs, filename = "./Shapefiles/NE_camlocs_1km_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  
  #  Double check it worked!
  #tst <- raster("./Shapefiles/NE_camlocs_mid.grd")
  
  # #  Plot to show which cells were randomly selected
  # plot(MW_camlocs, main = "Randomly selected camera sites 
  # in the Methow")
  # plot(MW_poly, border = "gray25", add = T)
  # plot(MW, add = T)
  # #plot(MW_inter, border = "gray25", add = T)
  # 
  # plot(NE_camlocs, main = "Randomly selected camera sites 
  # in the Northeast")
  # plot(NE_poly, border = "gray25", add = T)
  # plot(NE, add = T)
  # #plot(NE_inter, border = "gray25", add = T)
  

################################################################################
  ####  Fun with centroids  ####
  
  #  Need to extract the centroid of each cell so we know where to place cameras
  #  and pull data from these locations from rasters
  
  #  Centroids of selected cells
  MW_centroids <- rasterToPoints(MW_camlocs, spatial = T)
  NE_centroids <- rasterToPoints(NE_camlocs, spatial = T)
  
  #  Check to make sure they're plotting correctly
  #  Don't do this with the 1 km2 grid cell...
  # plot(MW_poly, border = "gray25")
  # plot(MW_centroids, pch = 16, add = T)
  # plot(NE_poly, border = "gray25")
  # plot(NE_centroids, pch = 16, add = T)
  
  #  Identify which cells were randomly selected
  which(MW_poly@data$cam_sites$selected == 1)
  which(NE_poly@data$cam_sites$selected == 1)
  
  #  Create spatial polygons of just those selected locations
  mw_cam_sites <- MW_poly[which(MW_poly@data$cam_sites$selected == 1),]
  ne_cam_sites <- NE_poly[which(NE_poly@data$cam_sites$selected == 1),]
  
  #  Extract centroids of those polygons
  mw_cents <- as.data.frame(coordinates(mw_cam_sites))
  ne_cents <- as.data.frame(coordinates(ne_cam_sites))
  
  #write.csv(ne_cents, file = "C:/Users/sb89/1 Predator Prey Project/Maps")
  
  #  Turn into a spatial points dataframe
  mw_cam_points <- SpatialPointsDataFrame(mw_cents, data = mw_cents, proj4string = new_proj)
  ne_cam_points <- SpatialPointsDataFrame(ne_cents, data = ne_cents, proj4string = new_proj)
  
  # #  Take a look
  # plot(NE)
  # plot(ne_cam_sites, add = T)
  # plot(ne_cam_points, add = T, col = "blue")
  
  #  Save this shapefile
  # writeOGR(mw_cam_points, dsn = "./Shapefiles", layer = "mw_cam_points", driver = "ESRI Shapefile", overwrite = T )
  # writeOGR(ne_cam_points, dsn = "./Shapefiles", layer = "ne_cam_points", driver = "ESRI Shapefile", overwrite = T )
  
  WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

  ne_cam_pnts_WGS84 <- spTransform(ne_cam_points, WGS84)
  # writeOGR(ne_cam_pnts_WGS84, dsn = "./Shapefiles", layer = "ne_cam_pnts_WGS84", driver = "ESRI Shapefile", overwrite = T)
  
   #  THEN SOMEHOW EXTRACT CENTROIDS OF THE POINTS I WANT TO SAMPLE
  #  AND THE POINTS OF NEIGHBORING CELLS
################################################################################
  ####  Rasterizing grids with cell ID information  ####
  
  #  Merge cell ID information with the cells that fall within study areas
  #  Rasterize that into the grid so each cell holds its own cell ID information
  #  To be used for knowing which cell a camera falls in if we have to randomly
  #  select a neighboring cell for deployment
  
  #  Start by working with the correct dataframe (ncell is for cells in study area)
  View(nMW_cells)
  View(nNE_cells)
  
  #  Create dataframe showing which sites have been selected across study areas
  mw_grid <- as.data.frame(MW_poly@data$cell_ID)
  colnames(mw_grid) <- "cell_ID"
  
  ne_grid <- as.data.frame(NE_poly@data$cell_ID)
  colnames(ne_grid) <- "cell_ID"
  
  #  Create a dataframe that joins the grid with cell ID information
  MW_IDs <- mw_grid %>% 
    full_join(nMW_cells, by = "cell_ID")
  #View(MW_IDs)
  
  NE_IDs <- ne_grid %>%
    full_join(nNE_cells, by = "cell_ID")
  #View(NE_IDs)
  
  #  Create a new SpatialPolygonsDataFrame with the cell ID information
  OK_temp <- MW_poly
  OK_temp@data$cell_info <- MW_IDs
  
  NE_temp <- NE_poly
  NE_temp@data$cell_info <- NE_IDs
  
  #  Rasterize the spatial data with only "cell_ID" (too much cell info if ncell included)
  OK_rasterized <- rasterize(OK_temp, MW_ref, field = OK_temp@data$cell_info$cell_ID)
  NE_rasterized <- rasterize(NE_temp, NE_ref, field = NE_temp@data$cell_info$cell_ID)
  
  #  Write Raster
  writeRaster(OK_rasterized, filename = "./Shapefiles/OK_rasterized.grd", format = 'raster', overwrite = T)
  writeRaster(OK_rasterized, filename = "./Shapefiles/OK_rasterized_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  writeRaster(NE_rasterized, filename = "./Shapefiles/NE_rasterized.grd", format = 'raster', overwrite = T)
  writeRaster(NE_rasterized, filename = "./Shapefiles/NE_rasterized_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  
################################################################################
  ####  Systematic Random Sampling  ####
  
  #  Having already excluded cells were < 25% fall within study area from above
  #  Start with a random location
  #  After random start, palce a cameras in ever 5th cell
################################################################################
  
  # #  Figure out systematic interval to place cameras
  # MW_cells; NE_cells
  # ncells <- mean(c(MW_cells, NE_cells))  # average number of cells in each study area
  # ith_cell_mw <- round(MW_cells/60, 0)  #  place camera at this interval in Methow
  # ith_cell_ne <- round(NE_cells/60, 0)  #  place camera at this interval in Northeast
  # # For MW: every 84th cell; for NE: every 72nd cell
  # 
  # #  Select random start out of the first ith_cell cells
  # #fst5_MW <- nMW_cells$cell_ID[1:5]  # if I want to use the actual cell_ID for indexing
  # #fst5_NE <- nNE_cells$cell_ID[1:5]
  # set.seed(624)
  # MW_rnd_start <- sample(1:ith_cell_mw, 1, replace = F)  # indexing for nMW_cells 79 (cell_ID = 1287)
  # set.seed(16)
  # NE_rnd_start <- sample(1:ith_cell_ne, 1, replace = F)  # indexing for nNE_cells 50 (cell_ID = 776)
  # 
  # #  From that starting point, select every ith_cell to sample according to each study area
  # cellList_MW <- 1:length(nMW_cells$ncell)
  # i <- MW_rnd_start
  # MW_syst_cams <- cellList_MW[seq(i, length(cellList_MW), ith_cell_mw)]
  # cellList_NE <- 1:length(nNE_cells$ncell)
  # j <- NE_rnd_start
  # NE_syst_cams <- cellList_NE[seq(j, length(cellList_NE), ith_cell_ne)]
  # 
  # #  Too many sites selected for MW so need to randomly toss 4
  # # set.seed(20)
  # # toss <- sample(cellList_MW, 4, replace = F)
  # # print(toss)
  # # MW_syst_rnd <- cellList_MW[-c(which(cellList_MW == 90 | cellList_MW == 165 | cellList_MW == 245 | cellList_MW == 285))]
  # # #  Too few sites selected for NE so need to randomly add 5
  # # set.seed(4)
  # # add <- sample(nNE_cells$ncell, 5, replace = F)
  # # print(add)
  # # NE_syst_rnd <- sort(c(cellList_NE, add), decreasing = F)
  # # #  Double check there are no duplicates
  # # which(duplicated(NE_syst_rnd))
  #  
  # #  Annoying formating so I can merge selected cells with full list of cells
  # selected <- rep(1, 60)
  # MW_sites_syst <- cbind(MW_syst_cams, selected)
  # MW_sites_syst <- as.data.frame(MW_sites_syst)
  # colnames(MW_sites_syst) <- c("ncell", "selected")
  # NE_sites_syst <- cbind(NE_syst_cams, selected)
  # NE_sites_syst <- as.data.frame(NE_sites_syst)
  # colnames(NE_sites_syst) <- c("ncell", "selected")
  # 
  # #  Merge selected cells with 
  # # MW_selsites_syst <- dplyr::right_join(nMW_cells, MW_sites_syst, by = "ncell")
  # # NE_selsites_syst <- dplyr::right_join(nNE_cells, NE_sites_syst, by = "ncell")
  # MW_selsites_syst <- dplyr::full_join(nMW_cells, MW_sites_syst, by = "ncell")
  # NE_selsites_syst <- dplyr::full_join(nNE_cells, NE_sites_syst, by = "ncell")
  # 
  # #  Create dataframe showing which sites have been selected across study areas
  # mw_grid <- as.data.frame(MW_poly@data$cell_ID)
  # #mw_cells <- as.data.frame(mw_cells) # if I only focus on grid cells within study area
  # colnames(mw_grid) <- "cell_ID"
  # MW_cams_syst <- mw_grid %>%
  #   full_join(MW_selsites_syst, by = "cell_ID") %>%
  #   transmute(
  #     sa_ncell = as.numeric(ncell),  # cell index for the group of cells that fall within the study area (nMW_cells)
  #     cell_ID = as.numeric(cell_ID),
  #     selected = ifelse(is.na(selected), 0, 1)
  #   )
  # 
  # ne_grid <- as.data.frame(NE_poly@data$cell_ID)
  # #ne_cells <- as.data.frame(ne_cells)  # if I only focus on grid cells within study area
  # colnames(ne_grid) <- "cell_ID"
  # NE_cams_syst <- ne_grid %>%
  #   full_join(NE_selsites_syst, by = "cell_ID") %>%
  #   transmute(
  #     sa_ncell = as.numeric(ncell),  # cell index for the group of cells that fall within the study area (nNE_cells)
  #     cell_ID = as.numeric(cell_ID),
  #     selected = ifelse(is.na(selected), 0, 1)
  #   )
  # 
  # #  Add selected camera dataframe to study area polygon shapefiles
  # MW_poly@data$cam_sites_syst <- MW_cams_syst  #MW_inter if I only focus on grid cells within study area
  # NE_poly@data$cam_sites_syst <- NE_cams_syst  #NE_inter
  # 
  # #  Rasterize study area polygon shapefiles
  # MW_camlocs_syst <- rasterize(MW_poly, MW_ref, field = MW_poly@data$cam_sites_syst$selected)  #MW_inter
  # NE_camlocs_syst <- rasterize(NE_poly, NE_ref, field = NE_poly@data$cam_sites_syst$selected)  #NE_inter
  # 
  # #  Write Raster
  # writeRaster(MW_camlocs_syst, filename = "./Shapefiles/MW_camlocs_syst_1km.grd", format = 'raster', overwrite = T)
  # writeRaster(MW_camlocs_syst, filename = "./Shapefiles/MW_camlocs_syst_1km_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  # writeRaster(NE_camlocs_syst, filename = "./Shapefiles/NE_camlocs_syst_1km.grd", format = 'raster', overwrite = T)
  # writeRaster(NE_camlocs_syst, filename = "./Shapefiles/NE_camlocs_syst_1km_arc.img", format = 'HFA', overwrite = T)  # use this one if planning to map in ArcGIS at all
  # 
  #   
  # #  Plot to show which cells were randomly selected
  # #png('tst_systRS_Methow.png')
  # plot(MW_camlocs_syst, main = "Systematic Randomly selected 
  # camera sites in the Methow")
  # plot(MW_poly, border = "gray25", add = T)
  # plot(MW, add = T)
  # #plot(MW_inter, border = "gray25", add = T)
  # #dev.off()
  # 
  # #png('tst_systRS_NE.png')
  # plot(NE_camlocs_syst, main = "Systematically Randomly selected 
  # camera sites in the Northeast")
  # plot(NE_poly, border = "gray25", add = T)
  # plot(NE, add = T)
  # #plot(NE_inter, border = "gray25", add = T)
  # #dev.copy(png, 'test_systRS_NE.png')
  # #dev.off()
  # 
  # cells <- cellFromRowColCombine(r, 1:10, 1:10) 
  # xy <- xyFromCell(r, cells) 
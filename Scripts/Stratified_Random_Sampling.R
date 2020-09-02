    ##  Stratified Random Sampling for WA Predator-Prey Project
    ##  Camera trap placement
    ##  Sarah Bassing 
    ##  May 2018
################################################################################
  #  Script to randomly sample elevational strata within each study area
  #  Each study area was broken into four quartiles (DEM_minipulations.R)
  #  Alpine habitat was excluded in the OK study area (above ~7000 ft.)
  #  Camera sites are randomly selected in each strata proportional to the area
  #  represented within each elevational strata for each study area
################################################################################

  #  Load packages and read in data
  library(rgdal)
  library(raster)
  library(dplyr)
  
  OK_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "METHOW_SA") #MW
  NE_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "NE_SA")  #NE
  
  #  Set new projection
  new_proj <- CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  
  #  Reproject study areas
  OK <- spTransform(OK_SA, new_proj)
  NE <- spTransform(NE_SA, new_proj)
  
  #  Extract bounding box data for each study area 
  bb_OK <- bbox(OK)
  bb_NE <- bbox(NE)
  
  #  Read in new DEM files
  OK_DEM <- raster("./Shapefiles/WA_DEM_files/WA_DEM/MW_DEM.img")
  NE_DEM <- raster("./Shapefiles/WA_DEM_files/WA_DEM/NE_DEM.img")
  
################################################################################
  ####  Make base grids for each study area  ####
  OK_ref <- raster(extent(bb_OK), crs = projection(new_proj), res = 1000)
  NE_ref <- raster(extent(bb_NE), crs = projection(new_proj), res = 1000)
  
################################################################################
  ####  Extract cell info from grids per study area  ####
  
  #  Double check the resolution and extent of grids and DEMs (they don't match)
  res(OK_ref)
  res(OK_DEM)
  extent(OK_ref)
  extent(OK_DEM)
  
  #  Resample the DEM files to make resolution match grid
  #  Only use this for selecting random points, don't use this for extracting
  #  covariate information (possibly biases pixel values)
  OK_DEM_resamp <- resample(OK_DEM, OK_ref, method = "bilinear")
  res(OK_DEM_resamp)
  NE_DEM_resamp <- resample(NE_DEM, NE_ref, method = "bilinear")
  res(NE_DEM_resamp)
  
  #  30 cells in NE raster are missing values (NAs) on outer edge of extent
  #  Need to provide some value to these cells for later data wrangling
  which(is.na(NE_DEM_resamp@data@values))
  new_val <- 859  # using mean value
  NE_DEM_resamp[is.na(NE_DEM_resamp@data@values)] <- new_val
  
  #  Transform rasters into polygons to help visualize
  OK_poly <- rasterToPolygons(OK_DEM_resamp)
  NE_poly <- rasterToPolygons(NE_DEM_resamp)  # somehow I lose 30 grid cells in this step... how?
  
  #  Add cell id to new polygons
  OK_cells <- 1:ncell(OK_poly)
  NE_cells <- 1:ncell(NE_poly)
  
  OK_poly@data$cell_ID <- c(1:length(OK_cells))
  NE_poly@data$cell_ID <- c(1:length(NE_cells))
  
  #  Intersect both polygons
  OK_inter <- raster::intersect(OK, OK_poly)
  NE_inter <- raster::intersect(NE, NE_poly)
  
################################################################################
  ####  Extract information from your grid  ####
  
  #  Read in the grid cell shapefiles
  #OK_inter <- readOGR("./Shapefiles", layer = "MW_1kmgrid")
  #NE_inter <- readOGR("./Shapefiles", layer = "NE_1kmgrid")
  
  #  Calculate area of each "intersected cell" in m2
  OK_area <- rgeos::gArea(OK_inter, byid = T)
  NE_area <- rgeos::gArea(NE_inter, byid = T)
  
  #  Combine intersected polygons with areas
  OK_edge <- cbind(OK_inter@data, OK_area)
  NE_edge <- cbind(NE_inter@data, NE_area)
  
  #  Calculate proportion of each cell within study area
  cell_res <- 1
  OK_prop <- OK_edge %>%
    mutate(OK_area_km2 = OK_area/1000000,
           Prop = OK_area_km2/cell_res)
  NE_prop <- NE_edge %>%
    mutate(NE_area_km2 = NE_area/1000000,
           Prop = NE_area_km2/cell_res)
  
  #  Count the number of cells (including partials) within study areas
  ok_cells <- OK_inter@data$cell_ID
  ncells_OK <- length(unique(ok_cells))
  
  ne_cells <- NE_inter@data$cell_ID
  ncells_NE <- length(unique(ne_cells))
  
  #  How many cells overlap with the study area by less than 99%?
  prop99_OK <- OK_prop[OK_prop$Prop < "0.99", ]  # tells you which ones
  prop99_NE <- NE_prop[NE_prop$Prop < "0.99", ]  # tells you which ones
  
  OK_partial <- length(unique(prop99_OK$cell_ID))  # gives you a count
  NE_partial <- length(unique(prop99_NE$cell_ID))  # gives you a count
  
  #  With 65 cameras in the OK and 55 in the NE, what does that look like?
  #  Toss cells that are less than 99% within study area
  #  What percent of sample units are being surveyed?
  OKsa_cells <- ncells_OK - OK_partial; 65/OKsa_cells
  NEsa_cells <- ncells_NE - NE_partial; 55/NEsa_cells  

  #  Create sequential cell nmbr corresponding to cell_ID from original grid 
  #  (within study area only)
  nmb <- 1:OKsa_cells
  nmb2 <- 1:NEsa_cells
  
  #  Identify cells where at least 99% of cell falls within study area
  #  The partial cells cannot be chosen for sampling now
  prop100_OK <- OK_prop[OK_prop$Prop >= "0.99", 3]
  prop100_NE <- NE_prop[NE_prop$Prop >= "0.99", 3]
  
  #  New dataframe listing out new cell number and original cell_ID within study areas
  nOK_cells <- as.data.frame(cbind(nmb, prop100_OK))
  colnames(nOK_cells) <- c("ncell", "cell_ID")
  nNE_cells <- as.data.frame(cbind(nmb2, prop100_NE))
  colnames(nNE_cells) <- c("ncell", "cell_ID")
  
  head(nOK_cells)
  head(nNE_cells)
  
  #  Add cell numbers back to DEM rasters
  #  Join study area specific cell infomation with full extent of grids
  #  Okanogan
  OK_cells_df <- as.data.frame(OK_cells)
  colnames(OK_cells_df) <- "cell_ID"
  OK_gridcells <- OK_cells_df %>%
    left_join(nOK_cells, by = "cell_ID") 
  #colnames(OK_gridcells) <- c("cell_count", "ncell")
  OK_poly@data$ncell <- OK_gridcells$ncell
  
  #  Northeast
  NE_cells_df <- as.data.frame(NE_cells)
  colnames(NE_cells_df) <- "cell_ID"
  NE_gridcells <- NE_cells_df %>%
    left_join(nNE_cells, by = "cell_ID") 
  #colnames(NE_gridcells) <- c("cell_count", "ncell")
  NE_poly@data$ncell <- NE_gridcells$ncell

  #  Rasterize data- adds spatial polygon dataframe as a dataframe in the raster
  OK_DEM_cell_ID <- rasterize(OK_poly, OK_DEM_resamp, field = OK_poly@data)
  NE_DEM_cell_ID <- rasterize(NE_poly, NE_DEM_resamp, field = NE_poly@data)

################################################################################
  #### Break up DEM into 4 quartiles that expand range of elevations  ####
  
  #  Look at elevation range- this will give me my 4 quartiles per study area
  #  Extract center points & their elevation from DEM values within study area
  OK_mask <- mask(OK_DEM, OK)
  OK_elevs <- rasterToPoints(OK_mask, spatial = T)
  
  NE_mask <- mask(NE_DEM, NE)
  NE_elevs <- rasterToPoints(NE_mask, spatial = T)
  
  summary(OK_elevs); summary(NE_elevs)
  
  #  Okanogan DEM based on elevational strata
  
  #  Low elevation strata: Q1 (0 - 777.8 meters elev; 0 - 2551.8 feet elev)
  #low_ok <- c(0, 738.5, TRUE, 738.5, 2636.3, NA)
  low_ok <- c(0, 777.8, TRUE, 777.8, 2689.4, NA)
  Q1_OK_reclass <- reclassify(OK_DEM_cell_ID, low_ok)
  Q1_OK <- mask(Q1_OK_reclass, OK)
  Q1_OK_cells <- as.data.frame(Q1_OK@data@values)
  colnames(Q1_OK_cells) <- "Q1"
  #tst <- rasterize(OK_poly, Q1_OK, field = OK_poly@data)
  #writeRaster(Q1_OK, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q1_OK", "HFA")
  
  #  Medium low elevation strata: Q2 (777.8 - 1188.4 meters elev; 2551.8 - 3899 feet elev)
  # medlow_ok <- c(0, 738.5, NA, 738.5, 1179.5, TRUE, 1179.5, 2636.3, NA)
  medlo_ok <- c(0, 777.8, NA, 777.8, 1188.4, TRUE, 1188.4, 2689.4, NA)
  Q2_OK_reclass <- reclassify(OK_DEM_cell_ID, medlo_ok)
  Q2_OK <- mask(Q2_OK_reclass, OK)
  Q2_OK_cells <- as.data.frame(Q2_OK@data@values) 
  colnames(Q2_OK_cells) <- "Q2"
  #writeRaster(Q2_OK, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q2_OK", "HFA")
  
  #  Medium high elevation strata: Q3 (1188.4 - 1605.2 meters elev; 3899- 5266.4 feet elev)
  #medhi_ok <- c(0, 1179.5, NA, 1179.5, 1682.4, TRUE, 1682.4, 2636.3, NA)
  medhi_ok <- c(0, 1188.4, NA, 1188.4, 1605.2, TRUE, 1605.2, 2689.4, NA)
  Q3_OK_reclass <- reclassify(OK_DEM_cell_ID, medhi_ok)
  Q3_OK <- mask(Q3_OK_reclass, OK)
  Q3_OK_cells <- as.data.frame(Q3_OK@data@values)
  colnames(Q3_OK_cells) <- "Q3"
  #writeRaster(Q3_OK, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q3_OK", "HFA")
  
  #  Hi elevation strata: Q4 (1682.4 - 2100.0 meters elev; 5266.4 - 6889.8 feet elve)
  #hi_ok <- c(0, 1682.4, NA, 1682.4, 2100, TRUE, 2100, 2636.3, NA)  # capped at 2100m instead of max elev
  hi_ok <- c(0, 1605.2, NA, 1605.2, 2100, TRUE, 2100, 2689.4, NA)  # capped at 2100m instead of max elev
  Q4_OK_reclass <- reclassify(OK_DEM_cell_ID, hi_ok)
  Q4_OK <- mask(Q4_OK_reclass, OK)
  Q4_OK_cells <- as.data.frame(Q4_OK@data@values)
  colnames(Q4_OK_cells) <- "Q4"
  #writeRaster(Q4_OK, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q4_OK", "HFA")
  
  #  Visualize
  plot(OK)
  plot(Q4_OK, add = T, col = "lightblue")
  plot(Q3_OK, add = T, col = "darkgreen")
  plot(Q2_OK, add = T, col = "green")
  plot(Q1_OK, add = T, col = "yellow")
  
  
  #  Northeast DEM based on elevational strata
  
  #  Low elevation strata: Q1 (0 - 679.1 meters elev)
  low_ne <- c(0, 679.1, TRUE, 679.1, 2072.0, NA)   
  Q1_NE_reclass <- reclassify(NE_DEM_cell_ID, low_ne)
  Q1_NE <- mask(Q1_NE_reclass, NE)
  Q1_NE_cells <- as.data.frame(Q1_NE@data@values)
  colnames(Q1_NE_cells) <- "Q1"
  #writeRaster(Q1_NE, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q1_NE", "HFA")
  
  #  Medium low elevation strata: Q2 (679.1 - 859.8 meters elev)
  medlo_ne <- c(0, 679.1, NA, 679.1, 859.8, TRUE, 859.8, 2072.0, NA)
  Q2_NE_reclass <- reclassify(NE_DEM_cell_ID, medlo_ne)
  Q2_NE <- mask(Q2_NE_reclass, NE)
  Q2_NE_cells <- as.data.frame(Q2_NE@data@values) 
  colnames(Q2_NE_cells) <- "Q2"
  #writeRaster(Q2_NE, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q2_NE", "HFA")
  
  #  Medium high elevation strata: Q3 (859.8 - 1015.8 meters elev)
  medhi_ne <- c(0, 859.8, NA, 859.8, 1015.8, TRUE, 1015.8, 2072.0, NA)
  Q3_NE_reclass <- reclassify(NE_DEM_cell_ID, medhi_ne)
  Q3_NE <- mask(Q3_NE_reclass, NE)
  Q3_NE_cells <- as.data.frame(Q3_NE@data@values) 
  colnames(Q3_NE_cells) <- "Q3"
  #writeRaster(Q3_NE, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q3_NE", "HFA")
  
  #  Hi elevation strata: Q4 (1015.8 - 2072.0 meters elev)
  hi_ne <- c(0, 1015.8, NA, 1015.8, 2072.0, TRUE)
  Q4_NE_reclass <- reclassify(NE_DEM_cell_ID, hi_ne)
  Q4_NE <- mask(Q4_NE_reclass, NE)
  Q4_NE_cells <- as.data.frame(Q4_NE@data@values) 
  colnames(Q4_NE_cells) <- "Q4"
  #writeRaster(Q4_NE, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q4_NE", "HFA")
  
  #  Visualize
  plot(NE)
  plot(Q1_NE, add = T, col = "yellow") 
  plot(Q2_NE, add = T, col = "green")
  plot(Q3_NE, add = T, col = "darkgreen")
  plot(Q4_NE, add = T, col = "lightblue")


################################################################################
  ####  Calculate area of each quartile per study area  ####
  
  #  Function to calculate area of each DEM quartile 
  #  Step 1: convert quartile DEM rasters to spatialpolygons
  #  Step 2: calculate areas of each spatialpolygon (in m2)
  #  Step 3: calculate areas in km2
  quart_area <- function(Q) {
    Q_polys <- rasterToPolygons(Q)
    #return(Q_polys)
    Q_area <- rgeos::gArea(Q_polys, byid = F)
    #  byid = F means it calucates the area of all cells in the polygon,
    #  not the area of individual attribues (cells)
    Q_area_km2 <- Q_area/1000000
    return(Q_area_km2)
  }
  
  #  Combine all the DEM quartile rasters together
  Q_OK <- c(Q1_OK, Q2_OK, Q3_OK, Q4_OK)
  Q_NE <- c(Q1_NE, Q2_NE, Q3_NE, Q4_NE)
  
  #  Feed the rasters through the funciton
  #  Output is a list of areas in km2
  #  Quadrats specific to each study area
  Q <- Q_OK
  Quart_OK_km <- lapply(Q, quart_area)
  Q_OK_km <- unlist(Quart_OK_km)
  Q <- Q_NE 
  Quart_NE_km <- lapply(Q, quart_area)
  Q_NE_km <- unlist(Quart_NE_km)
  
  #  Combine all the pieces
  #  Based on the strata specific to each study area
  OK_quadrats <- cbind(OK_cells_df, Q1_OK_cells, Q2_OK_cells, Q3_OK_cells, Q4_OK_cells)
  OK_strata <- left_join(OK_quadrats, nOK_cells, by = "cell_ID")
  NE_quadrats <- cbind(NE_cells_df, Q1_NE_cells, Q2_NE_cells, Q3_NE_cells, Q4_NE_cells)
  NE_strata <- left_join(NE_quadrats, nNE_cells, by = "cell_ID")

  
################################################################################
  ####  Number of cameras per strata per study area  ####
  
  #  Calculate proportion of area represented by each strata
  #  Based on study area specific strata
  Q_OK_total <- sum(Q_OK_km); Q_NE_total <- sum(Q_NE_km)
  prop_Q_OK <- Q_OK_km/Q_OK_total; prop_Q_NE <- Q_NE_km/Q_NE_total
  
  #  Calculate the number of cameras that should be deployed per strata 
  #  proportional to the area covered by each strata
  #  Originally made a mistake of how many cameras could be randomly selected
  #  from each strata in the OK so 2018 sampling was based on Q1:16, Q2:22, Q3:11, Q4:16
  
  #  Okanogan
  prop_cam_OK <- round(65*prop_Q_OK, 0)
  print(prop_cam_OK); sum(prop_cam_OK)
  
  OKQ1_cams <- prop_cam_OK[1]
  OKQ2_cams <- prop_cam_OK[2]
  OKQ3_cams <- prop_cam_OK[3]
  OKQ4_cams <- prop_cam_OK[4]
  
  
  #  Northeast
  prop_cam_NE <- round(55*prop_Q_NE, 0)
  print(prop_cam_NE); sum(prop_cam_NE)
  
  NEQ1_cams <- prop_cam_NE[1]
  NEQ2_cams <- prop_cam_NE[2]
  NEQ3_cams <- prop_cam_NE[3]
  NEQ4_cams <- prop_cam_NE[4]
  
  
  #  If I keep the number of cameras even across all strata (not deploying proportional to area)
  # #  OKanogan
  # 65/4  # 16.25; will need to randomly select 1 quartile to get an extra camera (this can change with each iteration)
  # OK_cells_a <- 16
  # OK_cells_b <- 17
  # #  Northeast
  # 55/4  #  13.75; will need to randomly select 3 quartiles to get an extra camera (this can change with each iteration)
  # NE_cells_a <- 13
  # NE_cells_b <- 14  
  
################################################################################
  ####  Pick random cells from each study area for each strata  ####
  
  #  Randomly select grid cells within each strata
  #  added the [& OK_strata$ncell != "NA"] part 3/25/2019 to ensure sites that 
  #  only partially overlap the study area edge are not selected

  #  Running tab on which seeds I used for randomization:
  #  OK & NE 2020: 19 (COVID-19 4.23.20); 2020 (summer 2020 backups)
  #  OK & NE 2019: 16 (4.18.19); 2019 (summer 2019 backups)
  #  Okanogan 2018: 24 (6.10.18); 6 (summer 2018 backups)
  #  Northeast 2018: 16 (4.18.19); 24 (5.20.18)
  #  Current seed
  seed <- 2020
  
  #  Okanogan
  OK_Q1_cells <- OK_strata[which(OK_strata$Q1 == 1 & OK_strata$ncell != "NA"),]
  set.seed(seed)  
  OK_rand_Q1 <- sample(OK_Q1_cells$cell_ID, OKQ1_cams, replace = F)
  OK_Q2_cells <- OK_strata[which(OK_strata$Q2 == 1 & OK_strata$ncell != "NA"),]
  set.seed(seed)  
  OK_rand_Q2 <- sample(OK_Q2_cells$cell_ID, OKQ2_cams, replace = F)
  OK_Q3_cells <- OK_strata[which(OK_strata$Q3 == 1 & OK_strata$ncell != "NA"),]
  set.seed(seed)  
  OK_rand_Q3 <- sample(OK_Q3_cells$cell_ID, OKQ3_cams, replace = F)
  OK_Q4_cells <- OK_strata[which(OK_strata$Q4 == 1 & OK_strata$ncell != "NA"),]
  set.seed(seed)  
  OK_rand_Q4 <- sample(OK_Q4_cells$cell_ID, OKQ4_cams, replace = F)
  
  #  Northeast
  NE_Q1_cells <- NE_strata[which(NE_strata$Q1 == 1 & NE_strata$ncell != "NA"),]
  set.seed(seed)  
  NE_rand_Q1 <- sample(NE_Q1_cells$cell_ID, NEQ1_cams, replace = F)
  NE_Q2_cells <- NE_strata[which(NE_strata$Q2 == 1 & NE_strata$ncell != "NA"),]
  set.seed(seed)  
  NE_rand_Q2 <- sample(NE_Q2_cells$cell_ID, NEQ2_cams, replace = F)
  NE_Q3_cells <- NE_strata[which(NE_strata$Q3 == 1 & NE_strata$ncell != "NA"),]
  set.seed(seed)  
  NE_rand_Q3 <- sample(NE_Q3_cells$cell_ID, NEQ3_cams, replace = F)
  NE_Q4_cells <- NE_strata[which(NE_strata$Q4== 1 & NE_strata$ncell != "NA"),]
  set.seed(seed)  
  NE_rand_Q4 <- sample(NE_Q4_cells$cell_ID, NEQ4_cams, replace = F)

  
  #  Annoying formating so I can merge selected cells with full list of cells
  OK_Q1_select <- rep(1, OKQ1_cams); OK_Q2_select <- rep(1, OKQ2_cams); 
  OK_Q3_select <- rep(1, OKQ3_cams); OK_Q4_select <- rep(1, OKQ4_cams)
  NE_Q1_select <- rep(1, NEQ1_cams); NE_Q2_select <- rep(1, NEQ2_cams); 
  NE_Q3_select <- rep(1, NEQ3_cams); NE_Q4_select <- rep(1, NEQ4_cams)
  
  
  #  Okanogan elevation bands
  OK_Band_lo <- rep(1, 17)
  OK_sites_Q1 <- cbind(OK_rand_Q1, OK_Q1_select, OK_Band_lo)
  OK_sites_Q1 <- as.data.frame(OK_sites_Q1)
  colnames(OK_sites_Q1) <- c("cell_ID", "selected", "Elev_Band")

  OK_Band_medlo <- rep(2, 19)
  OK_sites_Q2 <- cbind(OK_rand_Q2, OK_Q2_select, OK_Band_medlo)
  OK_sites_Q2 <- as.data.frame(OK_sites_Q2)
  colnames(OK_sites_Q2) <- c("cell_ID", "selected", "Elev_Band")
  
  OK_Band_medhi <- rep(3, 14)
  OK_sites_Q3 <- cbind(OK_rand_Q3, OK_Q3_select, OK_Band_medhi)
  OK_sites_Q3 <- as.data.frame(OK_sites_Q3)
  colnames(OK_sites_Q3) <- c("cell_ID", "selected", "Elev_Band")
  
  OK_Band_hi <- rep(4, 15)
  OK_sites_Q4 <- cbind(OK_rand_Q4, OK_Q4_select, OK_Band_hi)
  OK_sites_Q4 <- as.data.frame(OK_sites_Q4)
  colnames(OK_sites_Q4) <- c("cell_ID", "selected", "Elev_Band")
  
  #  Merge into a single dataframe of the randomly selected sites
  OK_cam_sites <- as.data.frame(rbind(OK_sites_Q1, OK_sites_Q2, OK_sites_Q3, OK_sites_Q4))
  
  
  #  Northeast elevation bands
  NE_Band_lo <- rep(1, 13)
  NE_sites_Q1 <- cbind(NE_rand_Q1, NE_Q1_select, NE_Band_lo)
  NE_sites_Q1 <- as.data.frame(NE_sites_Q1)
  colnames(NE_sites_Q1) <- c("cell_ID", "selected", "Elev_Band")
  
  NE_Band_medlo <- rep(2, 19)
  NE_sites_Q2 <- cbind(NE_rand_Q2, NE_Q2_select, NE_Band_medlo)
  NE_sites_Q2 <- as.data.frame(NE_sites_Q2)
  colnames(NE_sites_Q2) <- c("cell_ID", "selected", "Elev_Band")
  
  NE_Band_medhi <- rep(3, 9)
  NE_sites_Q3 <- cbind(NE_rand_Q3, NE_Q3_select, NE_Band_medhi)
  NE_sites_Q3 <- as.data.frame(NE_sites_Q3)
  colnames(NE_sites_Q3) <- c("cell_ID", "selected", "Elev_Band")
  
  NE_Band_hi <- rep(4, 14)
  NE_sites_Q4 <- cbind(NE_rand_Q4, NE_Q4_select, NE_Band_hi)
  NE_sites_Q4 <- as.data.frame(NE_sites_Q4)
  colnames(NE_sites_Q4) <- c("cell_ID", "selected",  "Elev_Band")
  
  #  Merge into a single dataframe of the randomly selected sites
  NE_cam_sites <- as.data.frame(rbind(NE_sites_Q1, NE_sites_Q2, NE_sites_Q3, NE_sites_Q4))
  
################################################################################
  ####  Extract centroids for selected cells  ####
  
  #  Set up dataframe to be rasterized
  #  Indicates which grid cells (cell_ID) were selected and what elevational band
  #  they fall into
  OK_cams <- OK_cells_df %>%
    full_join(OK_cam_sites, by = "cell_ID") %>%
    transmute(
      cell_ID = as.numeric(cell_ID),
      selected = ifelse(is.na(selected), 0, 1),
      Elev_Band = ifelse(is.na(Elev_Band), 0, Elev_Band)
    )
  NE_cams <- NE_cells_df %>%
    full_join(NE_cam_sites, by = "cell_ID") %>%
    transmute(
      cell_ID = as.numeric(cell_ID),
      selected = ifelse(is.na(selected), 0, 1),
      Elev_Band = ifelse(is.na(Elev_Band), 0, Elev_Band)
    )
  
  #  Add selected camera dataframe to study area polygon shapefiles
  #  keep in mind this is the full bounding box grid so cells outside study area are missing values
  #  At this point only one data frame is being attached to the same sp polygon
  #  Either locations based on study area-specific or unified strata
  OK_poly@data$cam_sites <- OK_cams  #OK_inter if I only focus on grid cells within study area
  NE_poly@data$cam_sites <- NE_cams  
  
  #  Rasterize study area polygon shapefiles
  OK_camlocs <- rasterize(OK_poly, OK_ref, field = OK_poly@data$cam_sites$selected)
  NE_camlocs <- rasterize(NE_poly, NE_ref, field = NE_poly@data$cam_sites$selected)
  
  #  Identify which cells were randomly selected
  OK_cell_ID <- which(OK_poly@data$cam_sites$selected == 1)
  NE_cell_ID <- which(NE_poly@data$cam_sites$selected == 1)
  
  #  Create study area-specific cell_ID
  OKprefix <- rep("OK", length(OK_cell_ID))
  NEprefix <- rep("NE", length(NE_cell_ID))
  OK_Cell_ID <- paste(OKprefix, OK_cell_ID, sep = "")
  NE_Cell_ID <- paste(NEprefix, NE_cell_ID, sep = "")
  
  #  Create SpatialPolygonDataFrame of just grid cells that were selected
  ok_cam_sites <- OK_poly[which(OK_poly@data$cam_sites$selected == 1),]
  ne_cam_sites <- NE_poly[which(NE_poly@data$cam_sites$selected == 1),]
  
  #  Extract centroids of those polygons
  #  Gives the coordinates (UTM) at the center of each spatialpolygon
  #  Could also use rgeos::gCentroid(object, byid = T) to extract centroids
  #  And add elevational strata to dataframe
  ok_cents <- as.data.frame(coordinates(ok_cam_sites))
  elev_strata <- ok_cam_sites@data$cam_sites$Elev_Band
  ok_cents <- cbind(ok_cents, elev_strata)
  colnames(ok_cents) <- c("X", "Y", "Strata")
  ne_cents <- as.data.frame(coordinates(ne_cam_sites))
  elev_strata <- ne_cam_sites@data$cam_sites$Elev_Band
  ne_cents <- cbind(ne_cents, elev_strata)
  colnames(ne_cents) <- c("X", "Y", "Strata")

  
  ####  SpatialPointDataFrames for selected camera locations  ####
  
  #  Turn into a spatial points dataframe
  OK_cam_points <- SpatialPointsDataFrame(ok_cents, data = ok_cents, proj4string = new_proj)
  NE_cam_points <- SpatialPointsDataFrame(ne_cents, data = ne_cents, proj4string = new_proj)
  
  #  Double check the projection
  projection(OK_cam_points)
  

  ####  Reprojecting for OnX & sharing  ####
  #  Using WGS84 geographic coordinate system (Lat/Long)
  WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  
  #  Reproject to WGS84
  OK_cam_points_WGS84 <- spTransform(OK_cam_points, WGS84)
  NE_cam_points_WGS84 <- spTransform(NE_cam_points, WGS84)
  
  #  Create dataframes of lat/long coordinates for selected grid cell centroids
  #  and elevational strata that each grid cell falls within
  OK_WGS84_dd <- as.data.frame(coordinates(OK_cam_points_WGS84))
  colnames(OK_WGS84_dd) <- c("X", "Y", "Strata")
  NE_WGS84_dd <- as.data.frame(coordinates(NE_cam_points_WGS84))
  colnames(NE_WGS84_dd) <- c("X", "Y", "Strata")
  
  #  Separately extract longitude & latitude in decimal degrees
  #  Longitude (X)
  OK_WGS84_long <- as.vector(coordinates(OK_cam_points_WGS84)[,1])
  NE_WGS84_long <- as.vector(coordinates(NE_cam_points_WGS84)[,1])
  #  Latitude (Y)
  OK_WGS84_lat <- as.vector(coordinates(OK_cam_points_WGS84)[,2])
  NE_WGS84_lat <- as.vector(coordinates(NE_cam_points_WGS84)[,2])
  
  #  Add cell_ID to @data so available as attributes in ArcGIS
  OK_cam_points@data$Cell_ID <- OK_Cell_ID
  NE_cam_points@data$Cell_ID <- NE_Cell_ID
  OK_cam_points_WGS84@data$Name <- OK_Cell_ID
  NE_cam_points_WGS84@data$Name <- NE_Cell_ID
  
  #  Add lat/long coordinates to @data so available as attribures in ArcGIS
  OK_cam_points_WGS84@data$X <- OK_WGS84_long
  OK_cam_points_WGS84@data$Y <- OK_WGS84_lat
  NE_cam_points_WGS84@data$X <- NE_WGS84_long
  NE_cam_points_WGS84@data$Y <- NE_WGS84_lat
  
  ####  SAVE  ####

  ##  Be sure to change the object names so you're saving correct csv. files &
  ##  shapefiles for each new randomization!!!

  #  Change names for csv. files
  OK_cents_summer20_bckup <- ok_cents
  NE_cents_summer20_bckup <- ne_cents
  OK_cents_WGS84_summer20_bckup <- OK_WGS84_dd
  NE_cents_WGS84_summer20_bckup <- NE_WGS84_dd
  
  #  Change names for shapefiles
  OK_cam_points_summer20_bckup <- OK_cam_points
  NE_cam_points_summer20_bckup <- NE_cam_points
  OK_cam_points_WGS84_summer20_bckup <- OK_cam_points_WGS84
  NE_cam_points_WGS84_summer20_bckup <- NE_cam_points_WGS84
  
  #  Save dataframes
  write.csv(OK_cents_summer20_bckup, file = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/OK_cents_summer20_bckup.csv")
  write.csv(NE_cents_summer20_bckup, file = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/NE_cents_summer20_bckup.csv")
  write.csv(OK_cents_WGS84_summer20_bckup, file = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/OK_cents_WGS84_summer20_bckup.csv")
  write.csv(NE_cents_WGS84_summer20_bckup, file = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/NE_cents_WGS84_summer20_bckup.csv")
  
  
  #  Save shapefile
  writeOGR(OK_cam_points_summer20_bckup, dsn = "./Shapefiles/Camera_Locations", layer = "OK_cam_points_summer20_bckup", driver = "ESRI Shapefile", overwrite = F )
  writeOGR(NE_cam_points_summer20_bckup, dsn = "./Shapefiles/Camera_Locations", layer = "NE_cam_points_summer20_bckup", driver = "ESRI Shapefile", overwrite = F )
  writeOGR(OK_cam_points_WGS84_summer20_bckup, dsn = "./Shapefiles/Camera_Locations", layer = "OK_cam_points_WGS84_summer20_bckup", driver = "ESRI Shapefile", overwrite = F)
  writeOGR(NE_cam_points_WGS84_summer20_bckup, dsn = "./Shapefiles/Camera_Locations", layer = "NE_cam_points_WGS84_summer20_bckup", driver = "ESRI Shapefile", overwrite = F)
  
  
  
  ####  Visualize your hard work  ####
  #  Plot Okanogan study area and randomly selected camera locations
  plot(OK)
  plot(ok_cam_sites, add = T)  # selected grid cells (polygon)
  plot(OK_cam_points, add = T, col = "blue")  # centroid locations
  #  Plot Northeast study area and randomly selected camera locations
  plot(NE)
  plot(ne_cam_sites, add = T)  # selected grid cells (polygon)
  plot(NE_cam_points, add = T, col = "blue")  # centroid locations
  

  
  #  Save as GPX files (must be in WGS84)
  writeOGR(OK_cam_points_WGS84_summer20, dsn="./Shapefiles/Camera_Locations/OK_Centroids_04202020.gpx",
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
  writeOGR(NE_cam_points_WGS84_summer20, dsn="./Shapefiles/Camera_Locations/NE_Centroids_04202020.gpx",
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
  writeOGR(OK_cam_points_WGS84_summer20_bckup, dsn="./Shapefiles/Camera_Locations/OK_Centroids_bckup_04202020.gpx",
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
  writeOGR(NE_cam_points_WGS84_summer20_bckup, dsn="./Shapefiles/Camera_Locations/NE_Centroids_bckup_04202020.gpx",
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
  #  Warning message: In fld_names == attr(res, "ofld_nms"): 
  #  longer object length is not a multiple of shorter object length
  #  No clue what this warning message means but it still works

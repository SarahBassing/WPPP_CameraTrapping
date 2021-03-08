  #'  ============================================
  #'  Covariate extraction
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  February 2021
  #'  ============================================
  #'  Extract prepared covariate data at each camera station. Pulls in rasters
  #'  from various sources and extracts data at each camera site. Script also 
  #'  uses DEM raster to create multiple rasters describing terrain features
  #'  (slope, aspect, roughness, and terrain ruggedness index). Reads in covariate
  #'  data collected during camera deployment as well. Finally, reads in covariate
  #'  data calculated in other scripts (distance to nearest water & road). These
  #'  data are generated in the following scripts:
  #'  
  #'  "Camera_Station_Covariates_DATE.csv" from Detections_by_Camera_Station.R 
  #'  "dist2water.csv" & "dist2road.csv" from Covariate_Hydro_Density.R
  #'  
  #'  Rasters include:
  #'       -DEM (Cascadia, 30m res)
  #'         Calculate elevation, slope, aspect, roughness, terrain ruggedness index
  #'       -Landcover type 1 (2016 National Landcover Database, 30m res)
  #'       -Percent canopy cover (Global Forest Change, 0.00025 res in degrees)
  #'         Raster created with Covariate_GFC_TreeCanopyCover.R
  #'       -Landcover type 2 (Cascadia, 30m res)
  #'         Annual landcover type
  #'       -vegIndicies (Cascadia, 30m res)
  #'         Band 1: Annual NDVI values for both Spring & Summer
  #'         Band 2: Annual dNBR (burn severity) values for both Spring & Summer 
  #'         Helpful background on dNBR: https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-modis/normalized-burn-index-dNBR/
  #'       -vegDisturbance (Cascadia, 30m res)
  #'         Band 1: Annual disturbance types include burned, timber harvest, or other
  #'         Band 3: Annual burn perimeters 
  #'       -Road density (Cascadia, 1km res)
  #'         Raster created with Covariate_Road_Density.R
  #'         sum_km: total kilometers of roads (primary, secondary, logging, etc.)
  #'         within a given 1 sq-km pixel (units are in kilometers even though
  #'         sum_km says "[m]")
  #'       -Water density (Raster derived from WA Dept. Ecology shapefile, 1km res)
  #'         Raster created with Covariate_Hydro_Density.R
  #'         sum_km: total kilometers of flowlines (streams, rivers, etc.) within
  #'         a given 1 sq-km pixel (even though sum_km says "[m]" the units are 
  #'         in kilometers)
  #'         Metadata found here: https://fortress.wa.gov/ecy/gispublic/DataDownload/ECY_WAT_NHDWA.htm
  #'       -Landfire Percent Canopy Cover (USGS, 30m res, excludes Canada)
  #'         Data found here: https://landfire.gov/getdata.php
  #'       -Human population (WorldPop, prepared by T. Ganz, 1km res)
  #'         Pixels represent number of people per 1 sq-km based on country totals
  #'         and adjusted to match UN population estimates
  #'         Data found here: https://www.worldpop.org/geodata/listing?id=77
  #'  ============================================

  
  #'  Load libraries
  library(sf)
  library(stars)
  library(rgeos)
  library(raster)
  library(tidyverse)
  
  #'  Read in camera locations
  # station_covs <- read.csv("./Output/Camera_Station_Covariates_2021-02-05.csv")
  # station_covs <- read.csv("./Output/Camera_StationYr2_Covariates_2021-03-02.csv")
  station_covs <- read.csv("./Output/Camera_Station18-20_Covariates_2021-03-04.csv")
  CameraLocation <- station_covs$CameraLocation
  
  #'  Read in covariate data extracted from other sources
  dist2water18 <- read.csv("./Output/dist2water.csv") %>%
    mutate(
      km2water = dist2water/1000
    )
  dist2water19 <- read.csv("./Output/dist2waterYr2.csv") %>%
    mutate(
      km2water = dist2water/1000
    )
  dist2water <- rbind(dist2water18, dist2water19) %>%
    arrange(CameraLocation) %>%
    dplyr::select(-X)
  
  dist2road <- read.csv("./Output/dist2road18-20.csv") %>%
    mutate(
      km2road = dist2road/1000
    ) %>%
    dplyr::select(-X)
  nearest <- full_join(dist2water, dist2road, by = "CameraLocation") %>%
    mutate(
      ID = seq(1:nrow(.))
    )
  
  #'  Read in spatial data
  wppp_bound <- st_read("./Shapefiles/WPPP_CovariateBoundary", layer = "WPPP_CovariateBoundary")
  #'  Terrain rasters
  dem <- raster("./Shapefiles/WA DEM rasters/WPPP_DEM_30m.tif")
  slope <- raster("./Shapefiles/WA DEM rasters/WPPP_slope_aspect.tif", band = 1)
  aspect <- raster("./Shapefiles/WA DEM rasters/WPPP_slope_aspect.tif", band = 2)
  TRI <- raster("./Shapefiles/WA DEM rasters/WPPP_TRI.tif")
  rough <- raster("./Shapefiles/WA DEM rasters/WPPP_roughness.tif")
  #'  NLCD raster
  nlcd <- raster("./Shapefiles/Land_cover/NLCD_2016_Land_Cover/NLCD_2016_Land_Cover_L48_20190424.img")
  #'  Water density raster
  waterden <- raster("./Shapefiles/WA_DeptEcology_HydroWA/WaterDensity_1km.tif")
  #'  Road density raster
  roadden <- raster("./Shapefiles/Cascadia_layers/roadsForTaylor/RoadDensity_1km.tif")
  #'  Percent canopy cover rasters
  canopy18 <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/treecov_2018.tif")
  canopy19 <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/treecov_2019.tif")
  landfire <- raster("./Shapefiles/Additional_WPPP_Layers/WPPP_landfire_CC_2019.tif")
  #'  Cascadia Biodiveristy Watch rasters
  landcov18 <- raster("./Shapefiles/Cascadia_layers/landcover_2018.tif")
  landcov19 <- raster("./Shapefiles/Cascadia_layers/landcover_2019.tif")
  ndvi_sp18 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2018_spring.tif")
  dnbr_sp18 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2018_spring.tif", band = 2)
  ndvi_sm18 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2018_summer.tif")
  dnbr_sm18 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2018_summer.tif", band = 2)
  ndvi_sp19 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2019_spring.tif")
  dnbr_sp19 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2019_spring.tif", band = 2)
  ndvi_sm19 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2019_summer.tif")
  dnbr_sm19 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2019_summer.tif", band = 2)
  disturb18 <- raster("./Shapefiles/Cascadia_layers/vegDisturbance_2018.tif")
  burnPerim18 <- raster("./Shapefiles/Cascadia_layers/vegDisturbance_2018.tif", band = 3)
  disturb19 <- raster("./Shapefiles/Cascadia_layers/vegDisturbance_2019.tif")
  burnPerim19 <- raster("./Shapefiles/Cascadia_layers/vegDisturbance_2019.tif", band = 3)
  #'  Human density
  human <- raster("./Shapefiles/Additional_WPPP_Layers/WPPP_pop.tif")
  
  #'  Identify projections & resolutions of relevant features
  sa_proj <- projection("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  wgs84 <- projection("+proj=longlat +datum=WGS84 +no_defs")
  projection(wppp_bound)
  projection(dem)
  projection(nlcd)
  projection(waterden)
  projection(roadden)
  projection(canopy18)
  projection(landfire)
  projection(landcov18)
  projection(ndvi_sp18)
  projection(burnPerim18)
  projection(human)

  res(dem)
  res(nlcd)
  res(waterden)
  res(landfire)
  res(landcov18)
  res(ndvi_sp18)
  res(human)
  
  #'  Make camera location data spatial
  cams <- st_as_sf(station_covs[,6:8], coords = c("Longitude", "Latitude"), crs = wgs84)
  
  
  #'  Extract landcover data from NLCD
  #'  Remember this is in a different projection
  cams_reproj <- st_transform(cams, crs = crs(nlcd))
  landcov_nlcd <- raster::extract(nlcd, cams_reproj, df = TRUE)
  colnames(landcov_nlcd) <- c("ID", "NLCD_landcov")
  #'  I think raster::extract can actually reproject spatial points on the fly?!
  
  
  #'  Extract water density (km of flowlines/1 sq-km)
  #'  Remember this is a different projection
  cams_reproj <- st_transform(cams, crs(crs(waterden)))
  water_density <- raster::extract(waterden, cams_reproj, df = TRUE)
  colnames(water_density) <- c("ID", "water_density")
  #'  Replace NAs with 0's because no water features within pixel camera fell in
  water_density[is.na(water_density[])] <- 0
  
  #'  Extract road density (km of roads/1 sq-km)
  #'  Remember this is in same projection as water density
  road_density <- raster::extract(roadden, cams_reproj, df = TRUE)
  colnames(road_density) <- c("ID", "road_density")
  #'  Replace NAs with 0's because no road features within pixel camera fell in
  road_density[is.na(road_density[])] <- 0
  
  
  #'  Extract percent canopy cover from GFC-derived raster
  canopy_stack <- stack(canopy18, canopy19)
  canopy_cov <- raster::extract(canopy_stack, cams, df = TRUE)
  colnames(canopy_cov) <- c("ID", "canopy18", "canopy19")
  
  #'  Extract percent canopy cover from Landfire data
  landfire <- raster::extract(landfire, cams, df = TRUE)
  colnames(landfire) <- c("ID", "landfire")
  
  #'  Extract human population
  human_density <- raster::extract(human, cams, df = TRUE)
  colnames(human_density) <- c("ID", "human_density")
  
  
  
  #'  Stack Cascadia rasters
  cascadia_stack <- stack(landcov18, landcov19, ndvi_sp18, ndvi_sm18, ndvi_sp19, ndvi_sm19, 
                     dnbr_sp18, dnbr_sm18, dnbr_sp19, dnbr_sm19, disturb18, disturb19, 
                     burnPerim18, burnPerim19)

  #'  Extract Cascadia covariates at camera locations
  #'  1 or 2 after repeat columns names refers to relevant band for rasters, e.g.,
  #'  vegIndices_2018_spring.1 = NDVI; vegIndices_2018_spring.2 = dNBR
  #'  vegDisturbance_2018.1 = disturbance type; vegDisturbance_2018.2 = burn perimeter
  Cascadia <- raster::extract(cascadia_stack, cams, df = TRUE)
  colnames(Cascadia) <- c("ID", "landcov18", "landcov19", "ndvi_sp18", "ndvi_sm18", 
                          "ndvi_sp19", "ndvi_sm19", "dnbr_sp18", "dnbr_sm18", 
                          "dnbr_sp19", "dnbr_sm19", "disturb18", "disturb19", 
                          "burnPerim18", "burnPerim19")
  #'  Get NDVI & dNBR values back on original scale (Cascadia multiplied indices by 10000)
  cascadia_covs <- Cascadia %>%
    mutate(
      ndvi_sp18 = ndvi_sp18/10000,
      ndvi_sm18 = ndvi_sm18/10000,
      ndvi_sp19 = ndvi_sp19/10000,
      ndvi_sm19 = ndvi_sm19/10000,
      dnbr_sp18 = dnbr_sp18/10000,
      dnbr_sm18 = dnbr_sm18/10000,
      dnbr_sp19 = dnbr_sp19/10000,
      dnbr_sm19 = dnbr_sm19/10000
    )
  #'  FYI, not sure how reliable dNBR or disturbance type really are given 
  #'  timing of when remotely sense data were collected and actual fires- 
  #'  3 cameras burned in 2018 but dNBR and disturbance values don't reflect this
  
  
  #'  Extract terrain characteristics at each camera site based on rasters
  #'  created from DEM and Covariate_DEM_to_TerrainFeatures.R
  #'  Stack terrain features
  dem_stack <- stack(slope, aspect, TRI, rough)
  #'  Extract terrain features & join into single df
  elevation <- raster::extract(dem, cams, df = TRUE)
  terrain <- raster::extract(dem_stack, cams, df = TRUE) %>%
    full_join(elevation, by = "ID") %>%
    dplyr:: mutate(
      slope = round(WPPP_slope_aspect.1, digits = 2),
      aspect = round(WPPP_slope_aspect.2, digits = 2),
      tri = round(WPPP_TRI, digits = 2),
      roughness = WPPP_roughness,
      elevation = WPPP_DEM_30m
    ) %>%
    dplyr::select(ID, elevation, slope, aspect, tri, roughness)
  #'  KEEP IN MIND:
  #'  Where slope = 0, aspect is set to 90 degrees!!!!
  #'  For slope & aspect, used 8 closest cells to calculate values (better for rough surfaces)
  #'  For TRI, scale of neighbor window set to 3 (8 closest cells)
  

  #'  Create dataframe with extracted covariate values
  nearest <- dplyr::select(nearest, -c(dist2water, dist2road))
  covs_df <- full_join(cascadia_covs, terrain, by = "ID") %>%
    full_join(landcov_nlcd, by = "ID") %>%
    full_join(canopy_cov, by = "ID") %>%
    full_join(landfire, by = "ID") %>%
    full_join(water_density, by = "ID") %>%
    full_join(road_density, by = "ID") %>%
    full_join(nearest, by = "ID") %>%
    full_join(human_density, by = "ID") %>%
    full_join(station_covs, by = "CameraLocation") %>%
    #  Slight rearranging of columns
    relocate(c(Year, Study_Area, CameraLocation), .before = ID) %>%
    relocate(c(NLCD_landcov, Habitat_Type), .after= "landcov19") %>%
    relocate(Canopy_Cov, .after = "canopy19") %>%
    relocate(c(Latitude, Longitude), .after = last_col()) %>%
    dplyr::select(-c(ID, X, Cell_ID, Camera_ID))

  #'  Save annual covariate data
  covs18_df <- covs_df[covs_df$Year == "Year1",] #%>%
    # dplyr::select(-c(landcov19, ndvi_sp19, ndvi_sm19, dnbr_sp19, dnbr_sm19, disturb19, burnPerim19, canopy19))
  covs19_df <- covs_df[covs_df$Year == "Year2",] #%>%
    # dplyr::select(-c(landcov18, ndvi_sp18, ndvi_sm18, dnbr_sp18, dnbr_sm18, disturb18, canopy18))
  
  #'  Merge annual data into a larger complete covariate dataframe
  #'  Requires renaming columns so variables from different years have same header
  #'  Keep in mind each year has slightly different number of covariates so need to add NA columns
  covs18 <- covs18_df %>%
    mutate(
      landcov = landcov18,
      ndvi_sp = ndvi_sp18,
      ndvi_sm = ndvi_sm18,
      dnbr_sp = dnbr_sp18,
      dnbr_sm = dnbr_sm18, 
      disturb = disturb18,
      burnPerim = burnPerim18,
      canopy = canopy18, 
      landcov19 = "NA", 
      ndvi_sp19 = "NA", 
      ndvi_sm19 = "NA",
      dnbr_sp19 = "NA",
      dnbr_sm19 = "NA",
      disturb19 = "NA",
      burnPerim19 = "NA",
      canopy19 = "NA"
    )
  
  covs19 <- covs19_df %>%
    mutate(
      landcov = landcov19,
      ndvi_sp = ndvi_sp19,
      ndvi_sm = ndvi_sm19,
      dnbr_sp = dnbr_sp19,
      dnbr_sm = dnbr_sm19, 
      disturb = disturb19,
      burnPerim = burnPerim19,
      canopy = canopy19, 
      landcov18 = "NA", 
      ndvi_sp18 = "NA", 
      ndvi_sm18 = "NA",
      dnbr_sp18 = "NA",
      dnbr_sm18 = "NA",
      disturb18 = "NA",
      burnPerim18 = "NA",
      canopy18 = "NA"
    )
  
  covs_df <- rbind(covs18, covs19)
    

  #'  Save for occupancy analyses
  # write.csv(covs18_df, paste0('./Output/CameraLocation_Covariates18_', Sys.Date(), '.csv'))  
  # write.csv(covs19_df, paste0('./Output/CameraLocation_Covariates19_', Sys.Date(), '.csv'))
  write.csv(covs_df, paste0('./Output/CameraLocation_Covariates18-20_', Sys.Date(), '.csv')) 
  
  
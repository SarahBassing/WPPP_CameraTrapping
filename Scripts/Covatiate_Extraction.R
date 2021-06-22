  #'  ============================================
  #'  Covariate extraction at Camera Trap Sites
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
  #'       -Landcover, type 1 (2016 National Landcover Database, 30m res)
  #'       -Percent canopy cover (Global Forest Change, 0.00025 res in degrees)
  #'         Raster created with Covariate_GFC_TreeCanopyCover.R
  #'       -Landcover, type 2 (Cascadia, 0.000269 degree res)
  #'         Annual landcover type
  #'       -Interpolated Landcover, type 3 (Cascadia, 30m res reprojected)
  #'         Annual landcover type interpolated to match NLCD
  #'         Raster created with Covariate_Landcover_Reproject.R
  #'       -Percent Landcover, type 4 (Cascadia, 30 m res after reprojection)
  #'         Percent landcover class within 250m radius of each 30m pixel using 
  #'         a moving window analysis based on interpolated Landcover raster 
  #'         Rasters created with Satterfield_ProportionalCoverAllocation.R
  #'       -vegIndicies (Cascadia, 0.000269 degree res)
  #'         Band 1: Annual NDVI values for both Spring & Summer
  #'         Band 2: Annual dNBR (burn severity) values for both Spring & Summer 
  #'         Helpful background on dNBR: https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-modis/normalized-burn-index-dNBR/
  #'       -vegDisturbance (Cascadia, 0.000269 degree res)
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
  #'       -NARR Daily max precipitation (mm) and mean temperature (K) (32 km res)
  #'         Extracted from North American Regional Reanalysis, data produced by 
  #'         the National Centers for Environmental Prediction. 
  #'         Data kindly extracted by O.Sanderfoot. 
  #'       -Landfire Percent Canopy Cover (USGS, 30m res, excludes Canada)
  #'         Data found here: https://landfire.gov/getdata.php
  #'       -Human population (WorldPop, prepared by T. Ganz, 1km res)
  #'         Pixels represent number of people per 1 sq-km based on country totals
  #'         and adjusted to match UN population estimates
  #'         Data found here: https://www.worldpop.org/geodata/listing?id=77
  #'       -Human Modification (Kennedy et al. 2019; 1 km res)
  #'         Pixels represent cumulative measure of human modification of 
  #'         terrestrial lands, values range 0-1 & represent proportion of 
  #'         landscape modified based on 13 anthropogenic stressors & estimated 
  #'         impacts, Data: https://figshare.com/articles/dataset/Global_Human_Modification/7283087
  #'  ============================================

  
  #'  Load libraries
  library(sf)
  library(stars)
  library(rgeos)
  library(raster)
  library(tidyverse)
  
  
  #'  Read in camera locations
  station_covs <- read.csv("./Output/Camera_Station18-20_Covariates_2021-03-04.csv") #2021-04-25
  CameraLocation <- station_covs$CameraLocation
  Year <- station_covs$Year
  
  
  #'  Define desired projections
  sa_proj <- projection("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  wgs84 <- projection("+proj=longlat +datum=WGS84 +no_defs")
  
  
  #'  Make camera location data spatial
  cams <- st_as_sf(station_covs[,6:8], coords = c("Longitude", "Latitude"), crs = wgs84)
  #'  Reproject to match study area projection
  cams_reproj <- st_transform(cams, crs = crs(sa_proj))
  
  
  #'  Read in rasterized spatial data
  wppp_bound <- st_read("./Shapefiles/WPPP_CovariateBoundary", layer = "WPPP_CovariateBoundary") %>%
    st_transform(crs = crs(sa_proj))
  OK_SA <- st_read("./Shapefiles/fwdstudyareamaps", layer = "METHOW_SA") %>%
    st_transform(crs = crs(sa_proj))
  NE_SA <- st_read("./Shapefiles/fwdstudyareamaps", layer = "NE_SA") %>%
    st_transform(crs = crs(sa_proj))
  #'  Terrain rasters (based on rasters projected in study area projection)
  dem <- raster("./Shapefiles/WA DEM rasters/WPPP_DEM_30m_reproj.tif")
  slope <- raster("./Shapefiles/WA DEM rasters/WPPP_slope_aspect_reproj.tif", band = 1)
  aspect <- raster("./Shapefiles/WA DEM rasters/WPPP_slope_aspect_reproj.tif", band = 2)
  TRI <- raster("./Shapefiles/WA DEM rasters/WPPP_TRI_reproj.tif")
  rough <- raster("./Shapefiles/WA DEM rasters/WPPP_roughness_reproj.tif")
  #'  Human density
  human <- raster("./Shapefiles/Additional_WPPP_Layers/WPPP_pop.tif")
  #'  Human modified landscape
  HM <- raster("./Shapefiles/Additional_WPPP_Layers/WPPP_gHM.tif") 
  #'  Reproject (and resample) human modified raster to match that of DEM reproj
  # HM_reproj <- projectRaster(HM, crs = crs(sa_proj), res = res(dem), method = "bilinear")
  # writeRaster(HM_reproj, filename = "./Shapefiles/Additional_WPPP_Layers/WPPP_gHM_reproj.tif", format="GTiff", overwrite=TRUE)
  HM_reproj <- raster("./Shapefiles/Additional_WPPP_Layers/WPPP_gHM_reproj.tif")
  #'  Road density raster
  roadden <- raster("./Shapefiles/roaddensity/road.density_km2_TIF.tif")
  #roadden <- raster("./Shapefiles/Cascadia_layers/roadsForTaylor/RoadDensity_1km.tif") 
  #'  Cascadia Biodiveristy interpolated rasters
  interp_landcov18 <- raster("./Shapefiles/Cascadia_layers/interpolated_landcover_2018.tif")
  interp_landcov19 <- raster("./Shapefiles/Cascadia_layers/interpolated_landcover_2019.tif")
  formix2prop18 <- raster("./Shapefiles/Cascadia_layers/forestmix2prop_18.tif")
  formix2prop19 <- raster("./Shapefiles/Cascadia_layers/forestmix2prop_19.tif")
  xgrassprop18 <- raster("./Shapefiles/Cascadia_layers/xgrassprop_18.tif")
  xgrassprop19 <- raster("./Shapefiles/Cascadia_layers/xgrassprop_19.tif")
  xshrubprop18 <- raster("./Shapefiles/Cascadia_layers/xshrubprop_18.tif")
  xshrubprop19 <- raster("./Shapefiles/Cascadia_layers/xshrubprop_19.tif")
  
  #' #'  Other rasters, likely won't use
  #' #'  NLCD raster
  #' nlcd <- raster("./Shapefiles/Land_cover/NLCD_2016_Land_Cover/NLCD_2016_Land_Cover_L48_20190424.img")
  #' #'  Water density raster
  #' waterden <- raster("./Shapefiles/WA_DeptEcology_HydroWA/WaterDensity_1km.tif")
  #' #'  Percent canopy cover rasters
  #' canopy18 <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/treecov_2018.tif")
  #' canopy19 <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/treecov_2019.tif")
  #' landfire <- raster("./Shapefiles/Additional_WPPP_Layers/WPPP_landfire_CC_2019.tif")
  #' #'  Cascadia Biodiveristy Watch rasters
  #' landcov18 <- raster("./Shapefiles/Cascadia_layers/landcover_2018.tif")
  #' landcov19 <- raster("./Shapefiles/Cascadia_layers/landcover_2019.tif")
  #' interp_landcov18 <- raster("./Shapefiles/Cascadia_layers/interpolated_landcover_2018.tif")
  #' interp_landcov19 <- raster("./Shapefiles/Cascadia_layers/interpolated_landcover_2019.tif")
  #' formix2prop18 <- raster("./Shapefiles/Cascadia_layers/forestmix2prop_18.tif")
  #' formix2prop19 <- raster("./Shapefiles/Cascadia_layers/forestmix2prop_19.tif")
  #' xgrassprop18 <- raster("./Shapefiles/Cascadia_layers/xgrassprop_18.tif")
  #' xgrassprop19 <- raster("./Shapefiles/Cascadia_layers/xgrassprop_19.tif")
  #' xshrubprop18 <- raster("./Shapefiles/Cascadia_layers/xshrubprop_18.tif")
  #' xshrubprop19 <- raster("./Shapefiles/Cascadia_layers/xshrubprop_19.tif")
  #' ndvi_sp18 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2018_spring.tif")
  #' dnbr_sp18 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2018_spring.tif", band = 2)
  #' ndvi_sm18 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2018_summer.tif")
  #' dnbr_sm18 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2018_summer.tif", band = 2)
  #' ndvi_sp19 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2019_spring.tif")
  #' dnbr_sp19 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2019_spring.tif", band = 2)
  #' ndvi_sm19 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2019_summer.tif")
  #' dnbr_sm19 <- raster("./Shapefiles/Cascadia_layers/vegIndices_2019_summer.tif", band = 2)
  #' disturb18 <- raster("./Shapefiles/Cascadia_layers/vegDisturbance_2018.tif")
  #' burnPerim18 <- raster("./Shapefiles/Cascadia_layers/vegDisturbance_2018.tif", band = 3)
  #' disturb19 <- raster("./Shapefiles/Cascadia_layers/vegDisturbance_2019.tif")
  #' burnPerim19 <- raster("./Shapefiles/Cascadia_layers/vegDisturbance_2019.tif", band = 3)
  
  
  #'  Identify projection, resolution, & spatial extent of relevant rasters
  projection(wppp_bound)
  projection(OK_SA)
  projection(dem)
  projection(HM_reproj)
  projection(interp_landcov18)
  projection(xshrubprop18)
  projection(roadden)
  # projection(nlcd)
  # projection(waterden)
  # projection(canopy18)
  # projection(landfire)
  # projection(landcov18)
  # projection(ndvi_sp18)
  # projection(burnPerim18)
  # projection(human)

  res(dem)
  res(HM_reproj)
  res(interp_landcov18)
  res(xshrubprop18)
  res(roadden)
  # res(nlcd)
  # res(landfire)
  # res(landcov18)
  # res(ndvi_sp18)
  # res(human)
  
  extent(dem)
  extent(HM_reproj)
  extent(xshrubprop18)
  extent(roadden)
  
  
  #'  Quick summary data about elevation for publications
  OK <- st_transform(OK_SA, crs = crs(wgs84))
  NE <- st_transform(NE_SA, crs = crs(wgs84))
  # elev_OK <- raster::extract(x = dem, y = OK)
  # elev_NE <- raster::extract(x = dem, y = NE)
  elev_minOK <- raster::extract(dem, OK, fun = min, na.rm=FALSE)
  elev_maxOK <- raster::extract(dem, OK, fun = max, na.rm=FALSE)
  elev_minNE <- raster::extract(dem, NE, fun = min, na.rm=FALSE)
  elev_maxNE <- raster::extract(dem, NE, fun = max, na.rm=FALSE)

  
  #'  Covariate data from other sources (non-raster based)
  #'  ====================================================
  #'  Distance to nearest water
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
  #'  Distance to nearest road
  dist2road <- read.csv("./Output/dist2road18-20.csv") %>%
    mutate(
      km2road = dist2road/1000
    ) %>%
    dplyr::select(-X)
  nearest <- full_join(dist2water, dist2road, by = "CameraLocation") %>%
    mutate(
      ID = seq(1:nrow(.))
    )
  #'  Daily max precip and mean temp; extracted from NARR by O.Sanderfoot
  #'  Will need to summarize/average based on desired date range & sampling occasions
  narr <- read.csv("./Output/WPPP_weather_data_all.csv") %>%
    dplyr::select(-X)
  
  
  #'  Extract covariate values at each camera site from all rasters
  #'  =============================================================
  #'  Stack terrain rasters
  terra_stack <- stack(dem, slope)
  #'  Extract terrain variables
  terra_covs <- raster::extract(terra_stack, cams_reproj, df = TRUE)
  
  #'  Extract anthropogenic variables
  road_den <- raster::extract(roadden, cams_reproj, df = TRUE)
  modified <- raster::extract(HM_reproj, cams_reproj, df = TRUE)
  
  cam_covs <- terra_covs %>%
    full_join(road_den, by = "ID") %>%
    full_join(modified, by = "ID") %>%
    transmute(
      obs = ID,
      Elev = round(WPPP_DEM_30m_reproj, digits = 2),
      Slope = round(WPPP_slope_aspect_reproj, digits = 2),
      RoadDen = round(road.density_km2_TIF, digits = 2),
      HumanMod = round(WPPP_gHM_reproj, digits = 2)
    ) %>%
    #'  Need to change NA to 0 for road density (if NA it means there are no
    #'  roads within that 1km pixel and raster pixel was empty)
    mutate(
      RoadDen = ifelse(is.na(RoadDen), 0, RoadDen)
    )
  
  
  #'  Extract percent landcover type using 250m moving window at each camera site
  #'  Make sure camera data are in correct format
  cams_reproj <- st_transform(cams, crs(crs(formix2prop18)))
  ID <- as.data.frame(as.numeric(seq(1:nrow(cams_reproj))))
  Cameras <- cbind(ID, CameraLocation)
  colnames(Cameras) <- c("ID", "CameraLocation")
  #'  Create raster stacks of 2018 and 2019 landcover data
  perc_stack18 <- stack(formix2prop18, xgrassprop18, xshrubprop18)
  perc_stack19 <- stack(formix2prop19, xgrassprop19, xshrubprop19)
  #'  Extract and organize
  perc_landcover18 <- raster::extract(perc_stack18, cams_reproj, df = TRUE) %>%
    full_join(Cameras, by = "ID") %>%
    cbind(Year) %>%
    transmute(
      obs = ID,
      CameraLocation = CameraLocation,
      Year = Year,
      PercForestMix2 = round(forestmix2prop_18, 2),
      PercXericGrass = round(xgrassprop_18, 2),
      PercXericShrub = round(xshrubprop_18, 2)
    ) %>%
    #'  Only retain observations from first year of sampling
    filter(Year == "Year1")
  # landcover18 <- cbind(perc_landcover18)
  perc_landcover19 <- raster::extract(perc_stack19, cams_reproj, df = TRUE) %>%
    full_join(Cameras, by = "ID") %>%
    cbind(Year) %>%
    transmute(
      obs = ID,
      CameraLocation = CameraLocation,
      Year = Year,
      PercForestMix2 = round(forestmix2prop_19, 2),
      PercXericGrass = round(xgrassprop_19, 2),
      PercXericShrub = round(xshrubprop_19, 2)
    ) %>%
    #'  Only retain observations from second year of sampling
    filter(Year == "Year2")
  tbl_landcover <- rbind(perc_landcover18, perc_landcover19)
  
  
  
  #'  Combine extracted covaraites into a single dataframe
  #'  ====================================================
  nearest <- dplyr::select(nearest, -c(dist2water, dist2road))
  covs_df <- full_join(cam_covs, tbl_landcover, by = "obs") %>%
    full_join(station_covs, by = "CameraLocation") %>%
    relocate(c(Year.x, Study_Area, CameraLocation), .before = obs) %>%
    relocate(c(Latitude, Longitude), .after = last_col()) %>%
    dplyr::select(-c(obs, Year.y, X, Cell_ID, Camera_ID))
  colnames(covs_df)[colnames(covs_df) == "Year.x"] <- "Year"

  #'  Don't forget that you have daily precip & temp data but these need to be 
  #'  filtered and summarized based on specific analyses, not here.
  
  #'  Save annual covariate data
  covs18_df <- covs_df[covs_df$Year == "Year1",]   
  covs19_df <- covs_df[covs_df$Year == "Year2",] 
  
  
  #'  Save for occupancy analyses
  write.csv(covs_df, paste0('./Output/CameraLocation_Covariates18-20_', Sys.Date(), '.csv'))
  
  
  
  #' #'  Extract landcover data from NLCD
  #' #'  Remember this is in a different projection
  #' cams_reproj <- st_transform(cams, crs = crs(nlcd))
  #' landcov_nlcd <- raster::extract(nlcd, cams_reproj, df = TRUE)
  #' colnames(landcov_nlcd) <- c("ID", "NLCD_landcov")
  #' #'  I think raster::extract can actually reproject spatial points on the fly?!
  #' 
  #' #'  Extract water density (km of flowlines/1 sq-km)
  #' #'  Remember this is a different projection
  #' cams_reproj <- st_transform(cams, crs(crs(waterden)))
  #' water_density <- raster::extract(waterden, cams_reproj, df = TRUE)
  #' colnames(water_density) <- c("ID", "water_density")
  #' #'  Replace NAs with 0's because no water features within pixel camera fell in
  #' water_density[is.na(water_density[])] <- 0
  #' 
  #' #'  Extract road density (km of roads/1 sq-km)
  #' #'  Remember this is in same projection as water density
  #' road_density <- raster::extract(roadden, cams_reproj, df = TRUE)
  #' colnames(road_density) <- c("ID", "road_density")
  #' #'  Replace NAs with 0's because no road features within pixel camera fell in
  #' road_density[is.na(road_density[])] <- 0
  #' 
  #' #'  Extract percent canopy cover from GFC-derived raster
  #' canopy_stack <- stack(canopy18, canopy19)
  #' canopy_cov <- raster::extract(canopy_stack, cams, df = TRUE)
  #' colnames(canopy_cov) <- c("ID", "canopy18", "canopy19")
  #' 
  #' #'  Extract percent canopy cover from Landfire data
  #' landfire <- raster::extract(landfire, cams, df = TRUE)
  #' colnames(landfire) <- c("ID", "landfire")
  #' 
  #' #'  Extract human population
  #' human_density <- raster::extract(human, cams, df = TRUE)
  #' colnames(human_density) <- c("ID", "human_density")
  #' 
  #' #'  Extract human modified lands
  #' modified <- raster::extract(HM, cams, df = TRUE)
  #' colnames(modified) <- c("ID", "modified")
  #' 
  #' #'  Extract landcover value from each pixel within 250m radius of camera site
  #' #'  Using interpolated landcover rasters derived from Cascadia landcover
  #' cams_reproj <- st_transform(cams, crs(crs(interp_landcov18)))
  #' pixvals18 <- raster::extract(interp_landcov18, cams_reproj, factors = TRUE, buffer = 250, df = TRUE)
  #' pixvals_df18 <- as.data.frame(pixvals18)
  #' pixvals19 <- raster::extract(interp_landcov19, cams_reproj, factors = TRUE, buffer = 250, df = TRUE)
  #' pixvals_df19 <- as.data.frame(pixvals19)
  #' #'  Merge together
  #' ID <- as.data.frame(as.numeric(seq(1:nrow(cams_reproj))))
  #' Cameras <- cbind(ID, CameraLocation)
  #' colnames(Cameras) <- c("ID", "CameraLocation")
  #' landcover_250m <- cbind(pixvals_df18, pixvals_df19$interpolated_landcover_2019) %>%
  #'   full_join(Cameras, by = "ID")
  #' colnames(landcover_250m) <- c("ID", "landcover_2018", "landcover_2019", "CameraLocation")
  #' 
  #' #'  Count the number of cells in each landcover category by CameraLocation
  #' tbl_landcover18 <- landcover_250m %>%
  #'   group_by(CameraLocation) %>%
  #'   count(landcover_2018) %>%
  #'   ungroup() %>%
  #'   pivot_wider(names_from = landcover_2018, values_from = n) %>%
  #'   replace(is.na(.), 0) %>% 
  #'   mutate(
  #'     sumPixels = rowSums(.[2:13])
  #'     )
  #' #'  Drop landcover data from 2019
  #' tbl_landcover18 <- cbind(Year, tbl_landcover18) %>%
  #'   filter(Year == "Year1")
  #' #'  Reogranize so it's easier to keep track of each category
  #' tbl_landcover18 <- tbl_landcover18[, order(colnames(tbl_landcover18), decreasing = TRUE)] %>%
  #'   relocate(sumPixels, .after = last_col()) 
  #' colnames(tbl_landcover18) <- c("Year", "CameraLocation", "Residential",  
  #'                                "Commercial", "Agriculture", "Forest",  
  #'                                "XericShrub", "MesicShrub", "XericGrass", 
  #'                                "MesicGrass", "WoodyWetland", "EmergentWetland", 
  #'                                "Barren", "Water", "sumPixels")
  #' tbl_landcover19 <- landcover_250m %>%
  #'   group_by(CameraLocation) %>%
  #'   count(landcover_2019) %>%
  #'   ungroup() %>%
  #'   pivot_wider(names_from = landcover_2019, values_from = n) %>%
  #'   replace(is.na(.), 0) %>% 
  #'   mutate(
  #'     sumPixels = rowSums(.[2:13])
  #'   )
  #' #'  Drop landcover data from 2018
  #' tbl_landcover19 <- cbind(Year, tbl_landcover19) %>%
  #'   filter(Year == "Year2")
  #' #'  Reogranize so it's easier to keep track of each category
  #' tbl_landcover19 <- tbl_landcover19[, order(colnames(tbl_landcover19), decreasing = TRUE)] %>%
  #'   relocate(sumPixels, .after = last_col())
  #' colnames(tbl_landcover19) <- c("Year", "CameraLocation", "Residential",  
  #'                                "Commercial", "Agriculture", "Forest",  
  #'                                "XericShrub", "MesicShrub", "XericGrass", 
  #'                                "MesicGrass", "WoodyWetland", "EmergentWetland", 
  #'                                "Barren", "Water", "sumPixels")
  #' #'  Merge annual landcover values together
  #' tbl_landcover <- rbind(tbl_landcover18, tbl_landcover19) %>%
  #'   #'  Consolidate categories
  #'   #'  Keeping Water to include in percent calculation but will not use for analyses
  #'   mutate(
  #'     Forest =  Forest + WoodyWetland + EmergentWetland,
  #'     MesicGrass = MesicGrass + Barren,
  #'     Developed = Residential + Commercial + Agriculture,
  #'     MesicMix = MesicShrub + MesicGrass,
  #'     ForestMix = Forest + MesicMix,
  #'     ForestMix2 = Forest + MesicShrub
  #'   ) %>%
  #'   dplyr::select(-c(WoodyWetland, EmergentWetland, Barren, Residential, Commercial, Agriculture)) %>%
  #'   relocate(sumPixels, .after = last_col()) %>%
  #'   #'  Calculate percent landcover type within 250m of each camera site
  #'   mutate(
  #'     PercForest = round(Forest/sumPixels, 2),
  #'     PercForestMix = round(ForestMix/sumPixels,2),     # Cannot be used in conjunction with Forest or any Mesic landcover types
  #'     PercForestMix2 = round(ForestMix2/sumPixels, 2),
  #'     PercXericShrub = round(XericShrub/sumPixels, 2),
  #'     PercMesicShrub = round(MesicShrub/sumPixels, 2),  # Cannot be used in conjunction with MesicMix
  #'     PercXericGrass = round(XericGrass/sumPixels, 2),
  #'     PercMesicGrass = round(MesicGrass/sumPixels, 2),  # Cannot be used in conjunction with MesicMix
  #'     PercMesicMix = round(MesicMix/sumPixels, 2),      # Cannot be used in conjunction with other Mesic landcover types
  #'     PercWater = round(Water/sumPixels, 2),            # Don't use for analyses (I have better data for H2o)
  #'     PercDeveloped = round(Developed/sumPixels, 2)
  #'   )
  #' 
  #' #'  Stack Cascadia rasters
  #' cascadia_stack <- stack(landcov18, landcov19, ndvi_sp18, ndvi_sm18, ndvi_sp19, ndvi_sm19, 
  #'                    dnbr_sp18, dnbr_sm18, dnbr_sp19, dnbr_sm19, disturb18, disturb19, 
  #'                    burnPerim18, burnPerim19)
  #' 
  #' #'  Extract Cascadia covariates at camera locations
  #' #'  1 or 2 after repeat columns names refers to relevant band for rasters, e.g.,
  #' #'  vegIndices_2018_spring.1 = NDVI; vegIndices_2018_spring.2 = dNBR
  #' #'  vegDisturbance_2018.1 = disturbance type; vegDisturbance_2018.2 = burn perimeter
  #' Cascadia <- raster::extract(cascadia_stack, cams, df = TRUE)
  #' colnames(Cascadia) <- c("ID", "landcov18", "landcov19", "ndvi_sp18", "ndvi_sm18", 
  #'                         "ndvi_sp19", "ndvi_sm19", "dnbr_sp18", "dnbr_sm18", 
  #'                         "dnbr_sp19", "dnbr_sm19", "disturb18", "disturb19", 
  #'                         "burnPerim18", "burnPerim19")
  #' #'  Get NDVI & dNBR values back on original scale (Cascadia multiplied indices by 10000)
  #' cascadia_covs <- Cascadia %>%
  #'   mutate(
  #'     ndvi_sp18 = ndvi_sp18/10000,
  #'     ndvi_sm18 = ndvi_sm18/10000,
  #'     ndvi_sp19 = ndvi_sp19/10000,
  #'     ndvi_sm19 = ndvi_sm19/10000,
  #'     dnbr_sp18 = dnbr_sp18/10000,
  #'     dnbr_sm18 = dnbr_sm18/10000,
  #'     dnbr_sp19 = dnbr_sp19/10000,
  #'     dnbr_sm19 = dnbr_sm19/10000
  #'   )
  #' #'  FYI, not sure how reliable dNBR or disturbance type really are given 
  #' #'  timing of when remotely sense data were collected and actual fires- 
  #' #'  3 cameras burned in 2018 but dNBR and disturbance values don't reflect this
  #' 
  #' #'  Extract terrain characteristics at each camera site based on rasters
  #' #'  created from DEM and Covariate_DEM_to_TerrainFeatures.R
  #' #'  Stack terrain features
  #' dem_stack <- stack(slope, aspect, TRI, rough)
  #' #'  Extract terrain features & join into single df
  #' elevation <- raster::extract(dem, cams, df = TRUE)
  #' terrain <- raster::extract(dem_stack, cams, df = TRUE) %>%
  #'   full_join(elevation, by = "ID") %>%
  #'   dplyr:: mutate(
  #'     slope = round(WPPP_slope_aspect.1, digits = 2),
  #'     aspect = round(WPPP_slope_aspect.2, digits = 2),
  #'     tri = round(WPPP_TRI, digits = 2),
  #'     roughness = WPPP_roughness,
  #'     elevation = WPPP_DEM_30m
  #'   ) %>%
  #'   dplyr::select(ID, elevation, slope, aspect, tri, roughness)
  #' #'  KEEP IN MIND:
  #' #'  Where slope = 0, aspect is set to 90 degrees!!!!
  #' #'  For slope & aspect, used 8 closest cells to calculate values (better for rough surfaces)
  #' #'  For TRI, scale of neighbor window set to 3 (8 closest cells)
  

  

 
  
  
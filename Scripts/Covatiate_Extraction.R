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
  #'       -Landcover type 1 (Cascadia, 30m res)
  #'         Annual landcover type
  #'       -Landcover type 2 (2016 National Landcover Database, 30m res)
  #'       -Percent canopy cover (Global Forest Change, 0.00025 res in degrees)
  #'         Raster created with Covariate_GFC_TreeCanopyCover.R
  #'       -vegIndicies (Cascadia, 30m res)
  #'         Band 1: Annual NDVI values for both Spring & Summer
  #'         Band 2: Annual dNBR (burn severity) values for both Spring & Summer 
  #'         Helpful background on dNBR: https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-modis/normalized-burn-index-dNBR/
  #'       -Road density (Raster created by L.Satterfield, 1km res)
  #'       -Water density (Raster derived from WA Dept. Ecology shapefile, 1km res)
  #'         Raster created with Covariate_Hydro_Density.R
  #'         Relevant data: LengthKM, GNIS_Name
  #'         Metadata found here: https://fortress.wa.gov/ecy/gispublic/DataDownload/ECY_WAT_NHDWA.htm
  #'       -vegDisturbance (Cascadia, 30m res)
  #'         Band 1: Annual disturbance types include burned, timber harvest, or other
  #'         Band 3: Annual burn perimeters 

  
  #'  Load libraries
  library(sf)
  library(stars)
  library(rgeos)
  library(raster)
  
  #'  Read in camera locations
  # deployed <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/All_Camera_Stations_18-19_updated_1.21.21.csv")
  station_covs <- read.csv("./Output/Camera_Station_Covariates_2021-02-05.csv")
  # CameraLocation <- cams$CameraLocation
  CameraLocation <- station_covs$CameraLocation
  
  #'  Read in covariate data extracted from other sources
  dist2water <- read.csv("./Output/dist2water.csv") %>%
    mutate(
      km2water = dist2water/1000
    )
  dist2road <- read.csv("./Output/dist2road.csv") %>%
    mutate(
      km2road = dist2road/1000
    )
  
  #'  Read in spatial data
  wppp_bound <- st_read("./Shapefiles/WPPP_CovariateBoundary", layer = "WPPP_CovariateBoundary")
  # wppp_grid <- raster("./Shapefiles/ref_grid_1k.img")
  dem <- raster("./Shapefiles/WA DEM rasters/WPPP_DEM_30m.tif")
  nlcd <- raster("./Shapefiles/Land_cover/NLCD_2016_Land_Cover/NLCD_2016_Land_Cover_L48_20190424.img")
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
  
  #'  Identify projections & resolutions of relevant features
  sa_proj <- projection("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  wgs84 <- projection("+proj=longlat +datum=WGS84 +no_defs")
  projection(wppp_bound)
  projection(dem)
  projection(nlcd)
  projection(landcov18)
  projection(ndvi_sp18)
  projection(burnPerim18)

  res(dem)
  res(nlcd)
  res(landcov18)
  res(ndvi_sp18)
  
  #'  Make camera location data spatial
  # cams <- st_as_sf(deployed[,3:5], coords = c("Longitude", "Latitude"), crs = wgs84)
  cams <- st_as_sf(station_covs[,6:8], coords = c("Longitude", "Latitude"), crs = wgs84)
  
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
    # dplyr::select(-"ID") %>%
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
  # cascadia_covs <- cbind(CameraLocation, cascadia_covs)
  #'  FYI, not sure how reliable dNBR or disturbance type really are given 
  #'  timing of when remotely sense data were collected and actual fires- 
  #'  3 cameras burned in 2018 but dNBR and disturbance values don't reflect this
  
  #'  Calculate slope & aspect
  #'  Where slope = 0, aspect is set to 90 degrees if unit = 'degrees'
  #'  neighbor = 8 best for rough surfaces, neighbor = 4 better for smoother surfaces
  # slope_aspect <- raster::terrain(dem, opt = c("slope", "aspect"), unit = "degrees", neighbors = 8) 
  # writeRaster(slope_aspect, filename = "./Shapefiles/WA DEM rasters/WPPP_slope_aspect.tif", format="GTiff", overwrite=TRUE)
  slope_aspect <- raster("./Shapefiles/WA DEM rasters/WPPP_slope_aspect.tif")
  
  #'  Calculate Terrain Ruggedness Index (TRI)
  #'  TRI: mean of the absolute differences between the value of a cell and the 
  #'  value of its 8 surrounding cells
  #'  spatialEco tri function allows you to set scale of neighbor window around 
  #'  each cell- 3 is default, 5 (etc) expands neighbor cells included in 
  #'  calculation and generates wider range of TRI values at camera sites 
  # require(spatialEco)
  # TRI <- spatialEco::tri(dem, s = 3, exact = TRUE, file.name = NULL)
  # writeRaster(TRI, filename = "./Shapefiles/WA DEM rasters/WPPP_TRI.tif", format="GTiff", overwrite=TRUE)
  TRI <- raster("./Shapefiles/WA DEM rasters/WPPP_TRI.tif")
  
  #'  Calculate Roughness
  #'  Roughness: the difference between the maximum and the minimum value of a 
  #'  cell and its 8 surrounding cells
  # rough <- raster::terrain(dem, opt = "roughness")
  # writeRaster(rough, filename = "./Shapefiles/WA DEM rasters/WPPP_roughness.tif", format="GTiff", overwrite=TRUE)
  rough <- raster("./Shapefiles/WA DEM rasters/WPPP_roughness.tif")
  
  #'  Stack, extract, & join elevation, slope, aspect, tri, & roughness into 
  #'  a single df
  dem_stack <- stack(slope_aspect, TRI, rough)
  elevation <- raster::extract(dem, cams, df = TRUE)
  terrain <- raster::extract(dem_stack, cams, df = TRUE) %>%
    full_join(elevation, by = "ID") %>%
    dplyr:: mutate(
      slope = round(slope, digits = 2),
      aspect = round(aspect, digits = 2),
      tri = round(layer, digits = 2),
      roughness = roughness,
      elevation = WPPP_DEM_30m
    ) %>%
    dplyr::select(-c(layer, WPPP_DEM_30m)) 

  #'  Create dataframe with extracted covariate values
  km2water <- dplyr::select(dist2water, -dist2water)
  # km2road <- dplyr::select(dist2road, -dist2road)
  # nearest <- full_join(km2water, km2road, by = "CameraLocation")
  covs_df <- full_join(cascadia_covs, terrain, by = "ID") %>%  
    full_join(km2water, by = c("ID" = "X")) %>% # use nearest eventually
    full_join(station_covs, by = "CameraLocation") %>%
    #  Slight rearranging of columns
    relocate(c(Year, Study_Area, CameraLocation), .before = ID) %>%
    relocate(c(Latitude, Longitude), .after = last_col()) %>%
    dplyr::select(-c(ID, X, Cell_ID, Camera_ID))

  #'  Remove annual data from 2019 (only working with 2018 cameras right now)
  covs18_df <- covs_df %>%
    dplyr::select(-c(landcov19, ndvi_sp19, ndvi_sm19, dnbr_sp19, dnbr_sm19, disturb19, burnPerim19))

  #'  Save for occupancy analyses
  write.csv(covs18_df, paste0('./Output/CameraLocation_Covariates18_', Sys.Date(), '.csv'))  
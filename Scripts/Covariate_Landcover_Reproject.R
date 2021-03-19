  #'  ============================================
  #'  Reproject & Interpolate Landcover
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  February 2021
  #'  ============================================
  
  #'  Load libraries
  library(sf)
  library(stars)
  library(rgeos)
  library(raster)
  library(tidyverse)
  
  #'  Read in camera locations
  station_covs <- read.csv("./Camera_Station18-20_Covariates_2021-03-04.csv")
  # station_covs <- read.csv("./Output/Camera_Station18-20_Covariates_2021-03-04.csv")
  CameraLocation <- dplyr::select(station_covs, c(X, CameraLocation))
  colnames(CameraLocation) <- c("ID", "CameraLocation")
  
  #'  Read in and inspect landcover data
  landcov18 <- raster("./Landcover/landcover_2018.tif")
  landcov19 <- raster("./Landcover/landcover_2019.tif")
  nlcd <- raster("./Landcover/NLCD_2016_Land_Cover_L48_20190424.img")
  # landcov18 <- raster("./Shapefiles/Cascadia_layers/landcover_2018.tif")
  # landcov19 <- raster("./Shapefiles/Cascadia_layers/landcover_2019.tif")
  # nlcd <- raster("./Shapefiles/Land_cover/NLCD_2016_Land_Cover/NLCD_2016_Land_Cover_L48_20190424.img")
  projection(landcov18)
  res(landcov18)
  
  #'  Identify projections & resolutions of relevant features
  sa_proj <- projection("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  wgs84 <- projection("+proj=longlat +datum=WGS84 +no_defs")
  
  #'  Make camera location data spatial
  cams <- st_as_sf(station_covs[,6:8], coords = c("Longitude", "Latitude"), crs = wgs84)
  cams_reproj <- st_transform(cams, crs = crs(sa_proj))
  
  #'  Reproject landcover rasters to match 30x30m resolution of nlcd raster
  #'  Use nearest neighbor method to interpolate values during reprojecting stage
  #'  Nearest neighbor method appropriate for categorical variables 
  #'  (use bilinear interpolation with continuous variables)
  as.factor(landcov18)
  as.factor(landcov19)
  reproj_landcov18 <- projectRaster(landcov18, crs = crs(sa_proj), res = res(nlcd), method = "ngb")
  reproj_landcov19 <- projectRaster(landcov19, crs = crs(sa_proj), res = res(nlcd), method = "ngb")
  
  #'  Save for future analyses
  writeRaster(reproj_landcov18, "./Landcover/interpolated_landcover_2018.tif")
  writeRaster(reproj_landcov19, "./Landcover/interpolated_landcover_2019.tif")
  
  #'  Check it out
  projection(reproj_landcov18)
  res(reproj_landcov18)
  plot(reproj_landcov18)
  plot(cams_reproj, add = TRUE)
  
  #'  Extract landcover value from each pixel within 250m radius of camera site
  pixvals18 <- raster::extract(reproj_landcov18, cams_reproj, factors = TRUE, buffer = 250, df = TRUE)
  pixvals_df18 <- as.data.frame(pixvals18)
  pixvals19 <- raster::extract(reproj_landcov19, cams_reproj, factors = TRUE, buffer = 250, df = TRUE)
  pixvals_df19 <- as.data.frame(pixvals19)
  
  #'  Merge together
  landcover <- cbind(pixvals_df18, pixvals_df19$landcover_2019) %>%
    full_join(CameraLocation, by = "ID")
  
  
  
  
  
  
  
  
  
  
  
  

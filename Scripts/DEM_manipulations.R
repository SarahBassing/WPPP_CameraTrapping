    ##  DEM manipulation
    ##  Sarah Bassing
    ##  May 2018
################################################################################
  #  Script to reproject and crop a DEM for the State of Washington into smaller 
  #  files specific to the Predator-Prey Project study areas.
  #  Then break apart into quartiles for stratified random sampling.
################################################################################
  
  #  Load packages and read in data
  library(rgdal)
  library(raster)

  #source("G:/My Drive/1 Dissertation/Analyses/Scripts/Study_Design/StudyAreas_and_Grid.R")  # run the entire study areas and grid script- will take awhile
  
  MW_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "METHOW_SA") #MW
  NE_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "NE_SA")  #NE
  dem <- raster("./Shapefiles/WA_DEM_files/WA_DEM/wa_dem1.img")  #WA DEM
  uslandcov <- raster("./Shapefiles/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img")
  #res(uslandcov)
  walandcov <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/NationalLandCoverData_WA_UTM10/land_use_land_cover/nlcd_wa_utm10.tif")
  landcovproj <- projection(uslandcov)
  
  #  Set new projection
  new_proj <- CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  
  #  Reproject study areas
  MW <- spTransform(MW_SA, new_proj)
  NE <- spTransform(NE_SA, new_proj)
  landcov <- projectRaster(walandcov, crs = new_proj) #crs(MW)
  
  #  Extract bounding box data for each study area 
  bb_MW <- bbox(MW)
  bb_NE <- bbox(NE)
  #  alternative way to extract bounding box
  #ext_MW <- extent(MW@bbox)
  #ext_NE <- extent(NE@bbox)
  
  #  DON'T NEED TO DO THIS AGAIN!
  # #  Reproject DEM raster to match new study area projection
  # dem.proj=projectRaster(dem, crs=new_proj)
  # 
  # # Crop WA DEM to the study areas
  # MW_DEM <- crop(dem.proj, bb_MW)
  # NE_DEM <- crop(dem.proj, bb_NE)
  # 
  # #  Write study area-specific DEM rasters
  # writeRaster(MW_DEM, filename="./Shapefiles/WA_DEM_files/WA_DEM/MW_DEM", "HFA")   # HFA is Erdas Imagine Images w/ file extension .img
  # writeRaster(NE_DEM, filename="./Shapefiles/WA_DEM_files/WA_DEM/NE_DEM", "HFA")   # HFA is Erdas Imagine Images w/ file extension .img
  
  #  Read in new DEM files
  MW_DEM <- raster("./Shapefiles/WA_DEM_files/WA_DEM/MW_DEM.img")
  NE_DEM <- raster("./Shapefiles/WA_DEM_files/WA_DEM/NE_DEM.img")
  
  #  Plot them to make sure it all worked
  plot(MW_DEM)
  plot(MW, add = T, border = "black")
  
  plot(NE_DEM)
  plot(NE, add = T, border = "black")
  
################################################################################
  #  Extract quartiles for elevational range on both study areas
  #  Look at distribution of elevations
  #  Compare to where random points land- is it representative of the proportional
  #  range of elevations represented in each study area

  #  Extract elevation (in meters) for each grid cell centroid
  #  bilinear takes the mean value for the centroid point and 4 surrounding DEM cells
  #  simple takes the elevation value for the centrod point alone
  #  Both give almost identical quartile values
  MW_elev <- raster::extract(MW_DEM, MW_centroids, method = "bilinear", df = T) #"simple"
  NE_elev <- raster::extract(NE_DEM, NE_centroids, method = "bilinear", df = T)
  
  #  Take a look
  summary(MW_elev); summary(NE_elev)  # why are there NAs in the NE_elev?
  
  ## BREAK UP EACH DEM BY ITS QUARTILES AND RANDOMLY SAMPLE ~15 SITES PER QUARTILE
  #  Break up each DEM by its quartiles
  
  #  Methow DEM
  low_mw <- c(0, 738.5, TRUE, 738.5, 2636.3, NA)
  Q1_MW_reclass <- reclassify(MW_DEM, low_mw)
  Q1_MW <- mask(Q1_MW_reclass, MW)
  writeRaster(Q1_MW, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q1_MW", "HFA")
  
  mid_mw <- c(0, 738.5, NA, 738.5, 1179.5, TRUE, 1179.5, 2636.3, NA)
  Q2_MW_reclass <- reclassify(MW_DEM, mid_mw)
  Q2_MW <- mask(Q2_MW_reclass, MW)
  writeRaster(Q2_MW, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q2_MW", "HFA")
  
  midhi_mw <- c(0, 1179.5, NA, 1179.5, 1682.4, TRUE, 1682.4, 2636.3, NA)
  Q3_MW_reclass <- reclassify(MW_DEM, midhi_mw)
  Q3_MW <- mask(Q3_MW_reclass, MW)
  writeRaster(Q3_MW, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q3_MW", "HFA")
  
  hi_mw <- c(0, 1682.4, NA, 1682.4, 2100, TRUE, 2100, 2636.3, NA)  # capped at 2100m instead of max elev
  Q4_MW_reclass <- reclassify(MW_DEM, hi_mw)
  Q4_MW <- mask(Q4_MW_reclass, MW)
  writeRaster(Q4_MW, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q4_MW", "HFA")
  
  #  Northeast DEM
  low_ne <- c(0, 681.9, TRUE, 681.9, 2636.3, NA)
  Q1_NE_reclass <- reclassify(NE_DEM, low_ne)
  Q1_NE <- mask(Q1_NE_reclass, NE)
  writeRaster(Q1_NE, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q1_NE", "HFA")
  
  mid_ne <- c(0, 681.9, NA, 681.9, 826.3, TRUE, 826.3, 1989.5, NA)
  Q2_NE_reclass <- reclassify(NE_DEM, mid_ne)
  Q2_NE <- mask(Q2_NE_reclass, NE)
  writeRaster(Q2_NE, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q2_NE", "HFA")
  
  midhi_ne <- c(0, 826.9, NA, 826.9, 1047.1, TRUE, 1047.1, 1989.5, NA)
  Q3_NE_reclass <- reclassify(NE_DEM, midhi_ne)
  Q3_NE <- mask(Q3_NE_reclass, NE)
  writeRaster(Q3_NE, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q3_NE", "HFA")
  
  hi_ne <- c(0, 1047.1, NA, 1047.1, 1989.5, TRUE)
  Q4_NE_reclass <- reclassify(NE_DEM, hi_ne)
  Q4_NE <- mask(Q4_NE_reclass, NE)
  writeRaster(Q4_NE, filename="./Shapefiles/WA_DEM_files/WA_DEM/Q4_NE", "HFA")
  
  
  #  Take a look!
  plot(MW)
  plot(Q4_MW, col = "gray85", add = T)
  plot(Q3_MW, col = "yellow", add = T)
  plot(Q2_MW, col = "green", add = T)
  plot(Q1_MW, col = "blue", add = T)
  plot(MW, lwd = 2, add = T)
  
  #  Extract elevation (in meters) for each camera location
  mw_cam_elev <- raster::extract(MW_DEM, mw_cam_points, method = "simple", df = T)
  ne_cam_elev <- raster::extract(NE_DEM, ne_cam_points, method = "simple", df = T)
  
  #  Compare full elevational range to sampled elevational range
  summary(MW_elev); summary(mw_cam_elev)
  summary(NE_elev); summary(ne_cam_elev)
  

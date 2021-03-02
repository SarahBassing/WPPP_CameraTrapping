  #'  ============================================
  #'  Flowlines to Water Density Raster
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  February 2021
  #'  ============================================
  #'  Use National Hydrology Database Flowlines shapefile to create a raster
  #'  representing water density, i.e., the kilometers of stream lengths per
  #'  1 sq-km pixel, as well as calculate distance to nearest flowline for each
  #'  camera station. Script reads in NHD Flowline data from larger GDB, crops
  #'  to the extent of the extended WPPP boundary, intersects flowline polylines
  #'  with 1 km2 grid, sums length of flowlines per grid cell, and rasterizes to
  #'  create a tif for futher data extraction.
  #'  
  #'  Flowlines include perennial, intermittent, ephemera, and pertinent streams/rivers
  #'  Waterbodies (e.g., lakes, ponds, marshes) represented by a single flowline
  #'  through their center. This underrepresents volume of water in these locations.
  #'  ============================================

  #'  Load libraries
  library(sf)
  library(stars)
  library(rgeos)
  library(rgdal)
  library(raster)
  
  #'  Set working directory when working on Gardner Lab computer
  # setwd("C:/Users/sb89/Desktop/Spatial Analyses") 
  
  #'  Read in spatial data
  wppp_bound <- st_read("./Shapefiles/WPPP_CovariateBoundary", layer = "WPPP_CovariateBoundary")
  wppp_grid <- raster("./Shapefiles/ref_grid_1k.img")
  # wppp_bound <- st_read("./WPPP_CovariateBoundary", layer = "WPPP_CovariateBoundary")
  # wppp_grid <- raster("./WPPP_CovariateBoundary/ref_grid_1k.img")
  
  #'  Identify projections of relevant features
  sa_proj <- projection("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  projection(sa_proj)
  projection(wppp_bound)
  projection(wppp_grid)

  
  
  ####  CROP AND SAVE HYDROLOGY SHAPEFILE  ####
  
  #'  ONLY NEED TO DO THIS ONCE!
  
  #' #'  Crop and save NHD Flowline shapefile to match WPPP extended boundary
  #' #'  Read in and review data
  #' streams <- sf::st_read("G:/My Drive/1 Dissertation/Analyses/Shapefiles/WA_DeptEcology_HydroWA/ECY_WAT_NHDWA/NHDWA.gdb", layer = "NHDFlowline")
  #' head(streams)
  #' projection(streams)
  #' 
  #' #'  Reproject extended WPPP boundary to match NHD projection
  #' reproj_bound <- st_transform(wppp_bound, crs = st_crs(streams))
  #' 
  #' #'  Identify bounding box of boundary in new projection
  #' bb <- st_bbox(reproj_bound)
  #' 
  #' #'  Drop M dimension
  #' streams <- st_zm(streams)
  #' 
  #' #'  Crop hydrology features to the extent of the WPPP extended boundary
  #' hydro_crop <- st_crop(streams, bb)
  #' 
  #' #'  Reproject to desired WA projection
  #' hydro_reproj <- st_transform(hydro_crop, crs = st_crs(sa_proj))
  #' 
  #' #'  Save so I never have to do this again!
  #' st_write(hydro_crop,
  #'          dsn = "G:/My Drive/1 Dissertation/Analyses/Shapefiles/WA_DeptEcology_HydroWA",
  #'          layer = "WPPP_hydro.shp", driver = "ESRI Shapefile")

  
  
  ####  CREATE WATER DENSITY RASTER  ####
  
  #'  Read in cropped hydrology layer in preferred WA projection
  hydro <- st_read("./Shapefiles/WA_DeptEcology_HydroWA", layer = "WPPP_hydro")
  # hydro <- st_read("./WA_DeptEcology_HydroWA", layer = "WPPP_hydro")
  hydro_reproj <- st_transform(hydro, crs = st_crs(sa_proj))
  projection(hydro_reproj)

  #'  Convert raster grid to polygon and then sf object
  grid <- rasterToPolygons(wppp_grid)
  grid_sf <- st_as_sf(grid)

  #'  Intersect hydro lines with sf grid
  hydro_grid_sf <- st_intersection(hydro_reproj, grid_sf)

  #'  Measure the length of each line in a grid cell
  #'  st_length measures in meters by default
  hydro_grid_sf$length <- st_length(hydro_grid_sf)  
  summary(hydro_grid_sf)
  head(hydro_grid_sf)
  
  #'  Calculate water density/1 sq-km by summing total length of lines per cell
  #'  Heads up, this takes awhile on the ol' laptop
  require(dplyr)
  water_density <- hydro_grid_sf %>%
    group_by(ref_grid_1k) %>%
    summarise(sum_length = sum(length), .groups = 'drop') %>%
    #'  Change length measurement from meters to kilometers
    mutate(
      sum_km = sum_length / 1000) %>%
    ungroup()
  summary(water_density)
  head(water_density)
  
  #'  Convert to raster
  #'  stars package allows you to rasterize an sf object
  require(stars)    
  #'  Create template grid based on dimensions of extended WPPP boundary
  dim(wppp_grid)
  grid <- st_as_stars(st_bbox(wppp_grid), nx = ncol(wppp_grid), ny = nrow(wppp_grid), values = NA_real_)
  #  Note the x,y dims seem backwards but this is what works
  
  #'  Rasterize sf object to match stars object and save as TIF
  WaterDensity <- st_rasterize(sf = water_density[,"sum_km"], template = grid, 
                               driver = "GTiff", file = "./Shapefiles/WA_DeptEcology_HydroWA/WaterDensity_1km.tif")
                               # driver = "GTiff", file = "./WA_DeptEcology_HydroWA/WaterDensity.tif")
  structure(WaterDensity)
  #'  Double check pixel area of new water density raster
  grid_area <- st_area(WaterDensity)
  structure(grid_area)
  
  #'  Read back in and double check it all worked!
  H2O <- raster("./Shapefiles/WA_DeptEcology_HydroWA/WaterDensity_1km.tif")
  projection(H2O)
  extent(H2O)
  dim(H2O)
  summary(H2O)
  plot(H2O, main = "Water Density (km water/1 sq-km)")
  plot(cams_reproj, add = TRUE, col = "black", pch = 6)

  #'  KEEP IN MIND: the original hydrology gdb lacks any data in the NE corner of 
  #'  the extended WPPP boundary (outside of watershed so no data included in 
  #'  original hydro layer) so all cell values in this region will be NA
  #'  Other large gaps around Spokane and central WA reflect real areas of little
  #'  water (e.g., massive urbanization, arid regions) and should be convtered to 0

  #'  Replace NAs with 0's since no flowlines documented in these pixels
  #WaterDensity[is.na(WaterDensity[])] <- 0
  #### NOT SURE IF I SHOULD DO THIS SINCE NE CORNER SHOULD TRULY BE NA
  
  

  ####  DISTANCE TO NEAREST HYDROLOGY FEATURE  ####
  
  #'  Calculate distance from each camera station to nearest stream and take the 
  #'  minimum value -- default units of measurement are in METERS
  #'  Make sure features are in either Equidistant or State-specific projection
  #'  to preserve distances between features (DO NOT use WGS84)
  #'  Albers Equal Area projection good for measuring area
  
  #'  Read in camera location data
  camera_stations <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/All_Camera_Stations_19-20.csv")
  # camera_stations <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/All_Camera_Stations_18-19_updated_1.21.21.csv")
  xy <- camera_stations[,c(5,4)]
  cams <- SpatialPointsDataFrame(coords = xy, data = camera_stations, 
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  #'  Convert spdf to sf object
  cams <- st_as_sf(cams)
  #'  Reproject camera stations to match hydro projection and get it out of lat/long
  reproj_cams <- st_transform(cams, crs = st_crs(sa_proj))
  
  #'  Calculate distance to nearest flowline
  #'  Test with one camera station
  dist2water <- min(st_distance(hydro_reproj, reproj_cams[1,]))
  dist2water <- as.data.frame(dist2water)
  dist2water$CameraLocation <- camera_stations$CameraLocation[1]
  
  #'  Iterate over all points (this will take awhile!)
  dist2water <- sapply(1:nrow(reproj_cams), function(x) min(st_distance(hydro_reproj, reproj_cams[x, ])))
  dist2water <- as.data.frame(dist2water)
  dist2water$CameraLocation <- camera_stations$CameraLocation
  
  #'  Save save save
  write.csv(dist2water, "./Output/dist2waterYr2.csv")

  
 
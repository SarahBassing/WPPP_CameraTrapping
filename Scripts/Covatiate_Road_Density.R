  #'  ============================================
  #'  Road Density Raster
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  March 2021
  #'  ============================================
  #'  Use roads shapefile provided by Cascadia Biodiversity Watch to create a 
  #'  raster representing raod density, i.e., the kilometers of road lengths per
  #'  1 sq-km pixel, as well as calculate distance to nearest road for each
  #'  camera station. Script reads in roads shapefile (already cropped to extended 
  #'  WPPP boundary extent), intersects polylines with 1 km2 grid, sums length 
  #'  of roads per grid cell, and rasterizes to create a tif for futher data extraction.
  #'  
  #'  Road layers include:
  #'  ============================================
  
  #'  Load libraries
  library(sf)
  library(stars)
  library(rgeos)
  library(rgdal)
  library(raster)
  
  #' #'  Set working directory when working on Madrona
  #' setwd("//udrive.uw.edu/udrive/Mapping") 
  
  #'  Read in spatial data
  dem <- raster("./Shapefiles/WA DEM rasters/WPPP_DEM_30m_reproj.tif")
  rd <- st_read("./Shapefiles/Cascadia_layers/roadsForTaylor", layer = "roadsForTaylor")
  # wppp_bound <- st_read("./Shapefiles/WPPP_CovariateBoundary", layer = "WPPP_CovariateBoundary")
  # wppp_grid <- raster("./Shapefiles/ref_grid_1k.img")
  # wppp_bound <- st_read("./WPPP_CovariateBoundary/WPPP_CovariateBoundary", layer = "WPPP_CovariateBoundary")
  # wppp_grid <- raster("./WPPP_CovariateBoundary/WPPP_CovariateBoundary/ref_grid_1k.img")
  
  #'  Identify projections of relevant features
  sa_proj <- projection("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  
  #'  Read in cropped road layer in preferred WA projection
  # rd <- st_read("./Shapefiles/Cascadia_layers/roadsForTaylor", layer = "roadsForTaylor")
  # rd <- st_read("./roadsForTaylor", layer = "roadsForTaylor")
  
  #'  Reproject road layer to study area projection
  rd_reproj <- st_transform(rd, crs = st_crs(sa_proj))
  projection(rd_reproj)
  projection(dem)

  #'  Create an empty raster with specified extent, resolution, and projection
  r <- raster(extent(rd_reproj), res = res(dem), crs = crs(sa_proj))
  
  #'  Merge all road features into a single polyline for easier manipulation
  #'  rgeos needs shapefiles to be an sp object, not an sf object
  rd_sp <- as(rd_reproj, Class = "Spatial")
  rd_union <- rgeos::gUnion(rd_sp, rd_sp)
  
  #'  Compute distance from each line feature to each raster point
  dd <- rgeos::gDistance(rd_union, as(r, "SpatialPoints"), byid = TRUE)
  
  #'  Save only minimum distance to each raster point
  r[] <- apply(dd, 1, min)
  plot(r)
  
  
  
  
  ####  CREATE ROAD DENSITY RASTER  ####
  #' #'  Convert raster grid to polygon and then sf object
  #' grid <- rasterToPolygons(wppp_grid)
  #' grid_sf <- st_as_sf(grid)
  #' 
  #' #'  Intersect road lines with sf grid
  #' rd_grid_sf <- st_intersection(rd_reproj, grid_sf)
  #' 
  #' #'  Measure the length of each line in a grid cell
  #' #'  st_length measures in meters by default
  #' rd_grid_sf$length <- st_length(rd_grid_sf)  
  #' summary(rd_grid_sf)
  #' head(rd_grid_sf)
  #' 
  #' #'  Calculate road density/1 sq-km by summing total length of lines per cell
  #' #'  Heads up, this takes awhile on the ol' laptop
  #' require(dplyr)
  #' road_density <- rd_grid_sf %>%
  #'   group_by(ref_grid_1k) %>%
  #'   summarise(sum_length = sum(length), .groups = 'drop') %>%
  #'   #'  Change length measurement from meters to kilometers
  #'   mutate(
  #'     sum_km = sum_length / 1000) %>%
  #'   ungroup()
  #' summary(road_density)
  #' head(road_density)
  #' 
  #' 
  #' #'  Convert to raster
  #' #'  stars package allows you to rasterize an sf object
  #' require(stars)    
  #' #'  Create template grid based on dimensions of extended WPPP boundary
  #' dim(wppp_grid)
  #' grid <- st_as_stars(st_bbox(wppp_grid), nx = ncol(wppp_grid), ny = nrow(wppp_grid), values = NA_real_)
  #' #  Note the x,y dims seem backwards but this is what works
  #' 
  #' 
  #' #'  Rasterize sf object to match stars object and save as TIF
  #' RoadDensity <- st_rasterize(sf = road_density[,"sum_km"], template = grid, 
  #'                              driver = "GTiff", file = "./roadsForTaylor/RoadDensity_1km.tif")
  #' 
  #' #'  Replace NAs with 0's since no roads documented in these pixels
  #' RoadDensity[is.na(RoadDensity[])] <- 0
  #' structure(RoadDensity)
  #' 
  #' #'  Double check pixel area of new road density raster
  #' grid_area <- st_area(RoadDensity)
  #' structure(grid_area)
  
  
  #'  Read back in and double check it all worked!
  road <- raster("./Shapefiles/Cascadia_layers/roadsForTaylor/RoadDensity_1km.tif")
  # road <- raster("./roadsForTaylor/RoadDensity_1km.tif")
  
  #'  Replace NAs with 0's since no roads documented in these pixels
  road[is.na(road[])] <- 0
  
  #'  Check it out
  projection(road)
  extent(road)
  dim(road)
  summary(road)
  plot(road, main = "Road Density (km road/1 sq-km)")
  

  
  ####  DISTANCE TO NEAREST ROAD FEATURE  ####
  
  #'  Calculate distance from each camera station to nearest road and take the 
  #'  minimum value -- default units of measurement are in METERS
  #'  Make sure features are in either Equidistant or State-specific projection
  #'  to preserve distances between features (DO NOT use WGS84)
  #'  Albers Equal Area projection good for measuring area
  
  #'  Read in camera location data
  camera_stations18 <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/All_Camera_Stations_19-20.csv")
  camera_stations19 <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/All_Camera_Stations_18-19_updated_1.21.21.csv")
  camera_stations <- rbind(camera_stations18, camera_stations19)
  xy <- camera_stations[,c(5,4)]
  cams <- SpatialPointsDataFrame(coords = xy, data = camera_stations, 
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  #'  Convert spdf to sf object
  cams <- st_as_sf(cams)
  #'  Reproject camera stations to match hydro projection and get it out of lat/long
  reproj_cams <- st_transform(cams, crs = st_crs(sa_proj))
  
  #'  Calculate distance to nearest road feature
  #'  Test with one camera station
  dist2road <- min(st_distance(rd_reproj, reproj_cams[1,]))
  dist2road <- as.data.frame(dist2road)
  dist2road$CameraLocation <- camera_stations$CameraLocation[1]
  
  #'  Iterate over all points (this will take awhile!)
  dist2road <- sapply(1:nrow(reproj_cams), function(x) min(st_distance(rd_reproj, reproj_cams[x, ])))
  dist2road <- as.data.frame(dist2road)
  dist2road$CameraLocation <- camera_stations$CameraLocation
  
  #'  Save save save
  write.csv(dist2road, "./Output/dist2road18-20.csv")
  
  

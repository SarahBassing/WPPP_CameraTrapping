  #'  ============================================
  #'  DEM to Terrain features
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  February 2021
  #'  ============================================
  #'  Use DEM raster to create various rasters describing terrain features across
  #'  extended WPPP study area, including slope, aspect, roughness, and terrain 
  #'  ruggedness index (TRI). Roughness and TRI represent the complexity of the
  #'  terrain in different ways. Keep in mind there are multiple ways to measure
  #'  slope and aspect, depending on the number of neighbor cells included in the
  #'  calculation. Similarly, the scale of window used to calculate TRI will
  #'  affect the values. Consider the scale of interest when calculating these 
  #'  metrics to represent terrain features important to research questions.
  #'  
  #'  Original Digital Elevation Map created by Cascadia Biodiversity Watch
  #'  https://cpf.users.earthengine.app/view/cascadia-biodiversity-watch
  #'  
  #'  HEADS UP: THESE TAKE AWHILE TO CREATE
    
  #'  Load libraries
  library(spatialEco)
  #'  For some weird reason raster version 3.4-10 won't reproject WGS84 to sa_proj
  #'  Uninstall current version and re-install older working version if needed
  # remove.packages("raster")
  # install.packages("remotes")
  # library(remotes)
  # install_version("raster", "3.4-5")
  library(raster)

  #'  Read in Digital Elevation Map raster & landcover raster to match DEM to
  dem <- raster("./Shapefiles/WA DEM rasters/WPPP_DEM_30m.tif")
  landcov18 <- raster("./Shapefiles/Cascadia_layers/interpolated_landcover_2018.tif")
  
  #'  Check out projections and resolutions
  projection(dem)
  projection(landcov18)
  res(dem)
  res(landcov18)
  extent(dem)
  extent(landcov18)
  
  #'  Define desired projection and resolution (in meters)
  sa_proj <- projection("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  
  #' #'  Re-project to study area projection and 30m resolution
  #' #'  Use bilinear interpolation method for continuous variables
  #' dem_reproj <- raster::projectRaster(dem, crs = crs(landcov18), res = res(landcov18), method = "bilinear")
  #' projection(dem_reproj)
  #' res(dem_reproj)
  #' writeRaster(dem_reproj, filename = "./Shapefiles/WA DEM rasters/WPPP_DEM_30m_reproj.tif", format="GTiff", overwrite=TRUE)
  dem_reproj <- raster("./Shapefiles/WA DEM rasters/WPPP_DEM_30m_reproj.tif")
  
  #'  Calculate slope & aspect
  #'  Where slope = 0, aspect is set to 90 degrees if unit = 'degrees'
  #'  neighbor = 8 best for rough surfaces
  #'  neighbor = 4 better for smoother surfaces
  slope_aspect <- raster::terrain(dem, opt = c("slope", "aspect"), unit = "degrees", neighbors = 8)
  writeRaster(slope_aspect, filename = "./Shapefiles/WA DEM rasters/WPPP_slope_aspect.tif", format="GTiff", overwrite=TRUE)
  slope_aspect_reproj <- raster::terrain(dem_reproj, opt = c("slope", "aspect"), unit = "degrees", neighbors = 8)
  writeRaster(slope_aspect_reproj, filename = "./Shapefiles/WA DEM rasters/WPPP_slope_aspect_reproj.tif", format="GTiff", overwrite=TRUE)
  
  #'  Calculate Terrain Ruggedness Index (TRI)
  #'  TRI: mean of the absolute differences between the value of a cell and the 
  #'  value of its 8 surrounding cells
  #'  spatialEco tri function allows you to set scale of neighbor window around 
  #'  each cell: 3 is default, 5 (etc.) expands neighbor cells included in 
  #'  calculation and generates wider range of TRI values at sites 
  TRI <- spatialEco::tri(dem, s = 3, exact = TRUE, file.name = NULL)
  writeRaster(TRI, filename = "./Shapefiles/WA DEM rasters/WPPP_TRI.tif", format="GTiff", overwrite=TRUE)
  TRI_reproj <- spatialEco::tri(dem_reproj, s = 3, exact = TRUE, file.name = NULL)
  writeRaster(TRI_reproj, filename = "./Shapefiles/WA DEM rasters/WPPP_TRI_reproj.tif", format="GTiff", overwrite=TRUE)
  
  #'  Calculate Topographic Position Index (TPI)
  #'  TPI: difference between the value of a cell and the mean value of its 8 
  #'  surrounding cells
  #'  Positive TPI values represent locations that are higher than the average 
  #'  of their surroundings, as defined by the neighborhood (ridges). Negative 
  #'  TPI values represent locations that are lower than their surroundings 
  #'  (valleys). TPI values near zero are either flat areas (where the slope is 
  #'  near zero) or areas of constant slope (where the slope of the point is 
  #'  significantly greater than zero). (http://www.jennessent.com/downloads/tpi-poster-tnc_18x22.pdf)
  #'  Using ~250m radius scale for TPI because this should capture ridges, drainages,
  #'  etc. within an animal's home range and is consistent with scale of %forest, 
  #'  %grass, %shrub variables used in analyses.
  # TPI <- spatialEco::tpi(dem, scale = 3, win = "rectangle", normalize = FALSE, zero.correct = FALSE) # 1 pixel radius... too fine a scale for these critters
  TPI <- spatialEco::tpi(dem, scale = 0.0025, win = "circle", zero.correct = FALSE)  # approximately 250m radius in decimal degrees
  writeRaster(TPI, filename = "./Shapefiles/WA DEM rasters/WPPP_TPI.tif", format="GTiff", overwrite=TRUE)
  # TPI_reproj <- spatialEco::tpi(dem_reproj, scale = 300, win = "circle", zero.correct = FALSE) # 250m radius based on projection
  # writeRaster(TPI_reproj, filename = "./Shapefiles/WA DEM rasters/WPPP_TPI_reproj.tif", format="GTiff", overwrite=TRUE)
  
  #'  Calculate Roughness
  #'  Roughness: the difference between the maximum and the minimum value of a 
  #'  cell and its 8 surrounding cells
  rough <- raster::terrain(dem, opt = "roughness")
  writeRaster(rough, filename = "./Shapefiles/WA DEM rasters/WPPP_roughness.tif", format="GTiff", overwrite=TRUE)
  rough_reproj <- raster::terrain(dem_reproj, opt = "roughness")
  writeRaster(rough_reproj, filename = "./Shapefiles/WA DEM rasters/WPPP_roughness_reproj.tif", format="GTiff", overwrite=TRUE)
  
  
  #' #'  Merge into a single raster stack
  #' dem <- raster("./Shapefiles/WA DEM rasters/WPPP_DEM_30m_reproj.tif")
  #' slope_aspect <- raster("./Shapefiles/WA DEM rasters/WPPP_slope_aspect_reproj.tif")
  #' tri <- raster("./Shapefiles/WA DEM rasters/WPPP_TRI_reproj.tif")
  #' tpi <- raster("./Shapefiles/WA DEM rasters/WPPP_TPI_reproj.tif")
  #' rough <- raster("./Shapefiles/WA DEM rasters/WPPP_roughness_reproj.tif")
  #' terrain_stack <- stack(dem, slope_aspect, tri, rough)
  
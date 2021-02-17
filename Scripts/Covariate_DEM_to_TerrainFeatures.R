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
  library(raster)

  #'  Calculate slope & aspect
  #'  Where slope = 0, aspect is set to 90 degrees if unit = 'degrees'
  #'  neighbor = 8 best for rough surfaces
  #'  neighbor = 4 better for smoother surfaces
  slope_aspect <- raster::terrain(dem, opt = c("slope", "aspect"), unit = "degrees", neighbors = 8)
  writeRaster(slope_aspect, filename = "./Shapefiles/WA DEM rasters/WPPP_slope_aspect.tif", format="GTiff", overwrite=TRUE)
  
  #'  Calculate Terrain Ruggedness Index (TRI)
  #'  TRI: mean of the absolute differences between the value of a cell and the 
  #'  value of its 8 surrounding cells
  #'  spatialEco tri function allows you to set scale of neighbor window around 
  #'  each cell: 3 is default, 5 (etc) expands neighbor cells included in 
  #'  calculation and generates wider range of TRI values at camera sites 
  TRI <- spatialEco::tri(dem, s = 3, exact = TRUE, file.name = NULL)
  writeRaster(TRI, filename = "./Shapefiles/WA DEM rasters/WPPP_TRI.tif", format="GTiff", overwrite=TRUE)
  
  #'  Calculate Roughness
  #'  Roughness: the difference between the maximum and the minimum value of a 
  #'  cell and its 8 surrounding cells
  rough <- raster::terrain(dem, opt = "roughness")
  writeRaster(rough, filename = "./Shapefiles/WA DEM rasters/WPPP_roughness.tif", format="GTiff", overwrite=TRUE)
  
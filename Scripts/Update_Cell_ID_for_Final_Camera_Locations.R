  ##  Updating Grid Cell ID for deployed cameras
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Oct. 2019
  ##  -------------------------------------------
  ##  Script to help extract the correct grid cell number for cameras that could
  ##  not be deployed in the original randomly selected grid cell. This often
  ##  occurred if we could not access the original cell for one reason or another.
  ##  -------------------------------------------
  
  #  Load packages
  library(rgdal)
  library(rgeos)
  library(raster)
  library(tidyverse)
  
  
  #  Read in most recent camera deployment info
  cams <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/AudioMoth_and_Camera_Deployment &Checking_2020_082420.csv")
  #cams <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/AudioMoth_and_Camera_Depoyment_and_Checking_090319.csv")
  #cams <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/AudioMoth_and_Camera_Depoyment_and_Checking_090119.csv")

  # Load grid cells and study area polygons
  grid_OK <- readOGR("./Shapefiles/Grid cells/MW_1kmgrid.shp")
  grid_NE <- readOGR("./Shapefiles/Grid cells/NE_1kmgrid.shp")
  rast_OK <- raster("./Shapefiles/Grid cells/OK_rasterized_arc.img")
  rast_NE <- raster("./Shapefiles/Grid cells/NE_rasterized_arc.img")
  
  OK <- readOGR("./Shapefiles/fwdstudyareamaps/StudyAreaPolygons/METHOW_SA.shp")
  NE <- readOGR("./Shapefiles/fwdstudyareamaps/StudyAreaPolygons/NE_SA.shp")
  
  #  Check projections and reproject where necessary
  projection(grid_OK)
  projection(rast_OK)
  projection(OK)
  
  saproj <- projection(grid_OK)
  
  OK <- spTransform(OK, saproj)
  NE <- spTransform(NE, saproj)
  
  #  Reduce cam dataframe to just location data
  cams_skinny <- cams %>%
    dplyr::select("Cell_ID", "Cam_ID", "Study_Area", "Cam_Long", "Cam_Lat") %>%
    na.omit()
  cams_OK <- cams_skinny %>%
    filter(Study_Area == "OK")
  cams_NE <- cams_skinny %>%
    filter(Study_Area == "NE")
  ams_skinny <- cams %>%
    dplyr::select("Cell_ID", "Study_Area", "AM_Long", "AM_Lat") %>%
    na.omit() 
  ams_OK <- ams_skinny %>%
    filter(Study_Area == "OK")
  ams_NE <- ams_skinny %>%
    filter(Study_Area == "NE")
  
  #  Make cam and am locations spatial
  camproj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  cams_OK_spdf <- SpatialPointsDataFrame(cams_OK[,4:5], cams_OK[,1:3], proj4string = camproj)
  cams_NE_spdf <- SpatialPointsDataFrame(cams_NE[,4:5], cams_NE[,1:3], proj4string = camproj)
  ams_OK_spdf <- SpatialPointsDataFrame(ams_OK[,3:4], ams_OK[,1:2], proj4string = camproj)
  ams_NE_spdf <- SpatialPointsDataFrame(ams_NE[,3:4], ams_NE[,1:2], proj4string = camproj)
  
  #  Reproject to match study area projection
  cams_OK_reproj <- spTransform(cams_OK_spdf, saproj)
  cams_NE_reproj <- spTransform(cams_NE_spdf, saproj)
  ams_OK_reproj <- spTransform(ams_OK_spdf, saproj)
  ams_NE_reproj <- spTransform(ams_NE_spdf, saproj)
  
  #  Plot to see how they line up
  plot(OK)
  plot(grid_OK, add = T)
  plot(cams_OK_reproj, pch = 19, col = "red", add = T)
  
  plot(NE)
  plot(grid_NE, add = T)
  plot(cams_NE_reproj, pch = 19, col = "blue", add = T)
  
  plot(ams_OK_reproj)
  plot(ams_NE_reproj)
  
  #  Extract raster cell number at location of camera or AM
  newcell_OK <- raster::extract(rast_OK, cams_OK_reproj)
  newcell_NE <- raster::extract(rast_NE, cams_NE_reproj)
  
  newcell_AMOK <- raster::extract(rast_OK, ams_OK_reproj)
  newcell_AMNE <- raster::extract(rast_NE, ams_NE_reproj)
  
  #  Merge into a single dataframe for each study area and device
  OK_cams <- cams_OK_reproj@data %>%
    cbind(newcell_OK) %>%
    mutate(
      Cell_ID_new = paste0(Study_Area, newcell_OK),
      Name = paste(Cell_ID_new, Cam_ID, sep = "_")
    ) %>%
    dplyr::select("Cell_ID", "Cam_ID", "Study_Area", "Cell_ID_new", "Name") %>%
    cbind(cams_OK_spdf@coords) %>%
    #filter(Name != "OK2145_3") %>% # remove initial deployement info (new camera at this location)
    unique()  # remove duplicate cameras where location stated the same but camera moved slightly (OK6286 & OK8420)
  
  NE_cams <- cams_NE_reproj@data %>%
    cbind(newcell_NE) %>%
    mutate(
      Cell_ID_new = paste0(Study_Area, newcell_NE),
      Name = paste(Cell_ID_new, Cam_ID, sep = "_")
    ) %>%
    dplyr::select("Cell_ID", "Cam_ID", "Study_Area", "Cell_ID_new", "Name") %>%
    cbind(cams_NE_spdf@coords) #%>%
    #filter(Cell_ID_new != "NENA") %>%  # remove row with missing Cell_ID info (NE1538- cam Lat/Long way wrong for 1st deployment)
    #filter(Cam_Lat != "48.56892")  # remove row with duplicate camera (moved camera to new location)
    #  I think Bell Meadows NE3597 cam location is mixed up (.### for lat & long swapped?)- check with Trent  
  
  OKAM_rows <- nrow(ams_OK_reproj)
  OK_AMs <- ams_OK_reproj@data %>%
    cbind(newcell_AMOK) %>%
    mutate(
      AM = rep("AM", OKAM_rows),
      Cell_ID_new = paste0(Study_Area, newcell_AMOK),
      Name = paste(Cell_ID_new, AM, sep = "_")
    ) %>%
    dplyr::select("Cell_ID", "Study_Area", "Cell_ID_new", "Name") %>%
    cbind(ams_OK_spdf@coords) %>%
    unique()    # remove duplicate AMs (OK2820 & OK8420)
  #  Had to correct Longitude of OK1404 & OK2820 AM in database!
  
  OKNE_rows <- nrow(ams_NE_reproj)
  NE_AMs <- ams_NE_reproj@data %>%
    cbind(newcell_AMNE) %>%
    mutate(
      AM = rep("AM", OKNE_rows),
      Cell_ID_new = paste0(Study_Area, newcell_AMNE),
      Name = paste(Cell_ID_new, AM, sep = "_")
    ) %>%
    dplyr::select("Cell_ID", "Study_Area", "Cell_ID_new", "Name") %>%
    cbind(ams_NE_spdf@coords) 
  #  Had to correct NE1538 & NE5627 locations in database!
  #  Be sure to double check location of NE1538 (AM & cam) on OnX
    
  
  #  Merge both study areas together
  Cam_locs <- rbind(OK_cams, NE_cams)
  AM_locs <- rbind(OK_AMs, NE_AMs)

  #  Make them spatial again
  Cam_locs_spdf <- SpatialPointsDataFrame(Cam_locs[,6:7], Cam_locs[,1:5], proj4string = camproj)
  AM_locs_spdf <- SpatialPointsDataFrame(AM_locs[,5:6], AM_locs[,1:4], proj4string = camproj)
  
  #  Make sure everything lines up the way you think it should
  #  Are there any obvious mismatches between camera and AM locations?
  #  But first reproject study area shapefiles so they match original cam & AM projection
  OKsa <- spTransform(OK, camproj)
  NEsa <- spTransform(NE, camproj)
  
  plot(OKsa)
  plot(Cam_locs_spdf, add = T, pch = 19)
  plot(AM_locs_spdf, add = T, col = "red")
  
  # Troubleshoot odd locations
  #plot(Cam_locs_spdf[Cam_locs_spdf@data$Cell_ID == "OK4751",], add = T, pch = 19, col = "blue") #no AM
  #plot(Cam_locs_spdf[Cam_locs_spdf@data$Cell_ID == "OK5331ish",], add = T, pch = 19, col = "blue")
  
  plot(NEsa)
  plot(Cam_locs_spdf, add = T, pch = 19)
  plot(AM_locs_spdf, add = T, col = "red")
  
  #  Write shapefiles
  writeOGR(Cam_locs_spdf, "./Shapefiles/Camera_Locations", "Cam_locs_spdf_090319", driver = "ESRI Shapefile")
  writeOGR(AM_locs_spdf, "./Shapefiles/Camera_Locations", "AM_locs_spdf_090319", driver = "ESRI Shapefile")
  
  #  Write KML files
  #  For this to work, need to specify a specific column to use for data ("Name" since it's grid cell and cam ID)
  # Cam_locs_spdf_WGS84 <- spTransform(Cam_locs_spdf, CRS("+proj=longlat +datum=WGS84"))
  writeOGR(Cam_locs_spdf["Name"], "./Shapefiles/Camera_Locations/Cam_locs_spdf_090319.kml", layer = "Name", driver = "KML")
  writeOGR(AM_locs_spdf["Name"], "./Shapefiles/Camera_Locations/AM_locs_spdf_090319.kml", layer = "Name", driver = "KML")
  
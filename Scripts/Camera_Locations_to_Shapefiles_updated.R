    ##  Create shapefiles of camera locations
    ##  Sarah Bassing
    ##  Washington Predator-Prey Project
    ##  April 2020
    ##  ================================================
    ##  Simple script to make spatial files of camera locations
    ##  Relies on using Camera_Station_Wrangling.R to create master files of all
    ##  deployments and checks.
    ##
    ##  Version 1: lists all locations of a camera, even if moved or pulled from
    ##  the field early; good for tracking all camera locations over time
    ##
    ##  Version 2: drops the original deployment/check location(s) if the camera 
    ##  had to be removed early or moved to a new location; good for tracking
    ##  CURRENT location of cameras in the field
    ##  ================================================
  
  #  Load packages
  library(tidyverse)

  ##  ==================================================    
  ##  Version 1: Organize ALL camera locations, even if cameras were moved during season
  
  #  Cameras deployed summer 2018
  master18_19 <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2018_updated_2021-05-28.csv") %>%  # camera_master_2018_updated.9.25.20
    dplyr::select("Status", "Date", "Study_Area", "Cell_ID", "Camera_ID", "Camera_Long", "Camera_Lat", "Distance_Focal_Point", "Height_frm_grnd", "Monitoring", "Canopy_Cov", "Land_Mgnt", "Habitat_Type") %>%
    #  Add a column merging cell & camera, format date
    mutate(
      Name = paste(Cell_ID, "_", Camera_ID),
      Name = gsub("[[:space:]]", "", Name),
      Date = as.Date(Date, format = "%Y-%m-%d") #"%m/%d/%Y"
    ) %>%
    arrange(Name, Date) %>%
    #  Remove entries when the camera was pulled
    filter(Status != "Removed") %>%
    #  Remove duplicate data (where deployment and check data match for a given camera)
    group_by(Camera_Lat, Camera_Long) %>%
    distinct(Name, .keep_all = TRUE) %>%
    ungroup() %>%
    #  Remove cameras that were deployed but never collected data (BURNED) 
    filter(Name != "OK1474_104" & Name != "OK2051_98" & Name != "OK6270_109") %>%
    #  Remove duplicate cameras with incorrect coordinates (cameras were not moved)
    filter(Cell_ID != "NE5094" | Status != "Checked") %>%    
    filter(Cell_ID != "NE6019" | Status != "Checked") %>%
    filter(Cell_ID != "NE7602" | Status != "Checked")
    
  #  Convert to dataframes
  cams_all18_19 <- as.data.frame(master18_19) %>%
    dplyr::select(-c(Status, Study_Area))
  OK_all18_19 <- as.data.frame(master18_19[master18_19$Study_Area == "OK",]) %>%
    dplyr::select(-c(Status, Study_Area))
  NE_all18_19 <- as.data.frame(master18_19[master18_19$Study_Area == "NE",]) %>%
    dplyr::select(-c(Status, Study_Area))
  #  How many camera stations total?
  nrow(OK_all18_19)
  nrow(NE_all18_19)   
  
  #  Cameras deployed summer 2019
  master19_20 <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2019_updated_2021-05-28.csv") %>% #camera_master_2019_updated.9.25.20.csv
    dplyr::select("Status", "Date", "Study_Area", "Cell_ID", "Camera_ID","Camera_Long", "Camera_Lat",  "Distance_Focal_Point", "Height_frm_grnd", "Monitoring", "Canopy_Cov", "Land_Mgnt", "Habitat_Type") %>%
    #  Add a column merging cell & camera, format date
    mutate(
      Name = paste(Cell_ID, "_", Camera_ID),
      Name = gsub("[[:space:]]", "", Name),
      Date = as.Date(Date, format = "%Y-%m-%d") #"%m/%d/%Y"
    ) %>%
    arrange(Name, Date) %>%
    #  Remove entries when the camera was pulled
    filter(Status != "Removed") %>%
    #  Remove duplicate data (where deployment and check data match for a given camera)
    group_by(Camera_Lat, Camera_Long) %>%
    distinct(Name, .keep_all = TRUE) %>%
    ungroup() %>%
    #  Remove camera station that did not change but camera # was changed due to damage
    filter(Name != "OK2145_3") %>%
    #  Remove duplicates that dplyr thinks are distinct
    filter(Cell_ID != "NE1990" | Status != "Checked") %>%    
    filter(Cell_ID != "NE2383" | Status != "Checked") 

  #  Convert to dataframes
  cams_all19_20 <- as.data.frame(master19_20) %>%
    dplyr::select(-c(Status, Study_Area))
  OK_all19_20 <- as.data.frame(master19_20[master19_20$Study_Area == "OK",]) %>%
    dplyr::select(-c(Status, Study_Area))
  NE_all19_20 <- as.data.frame(master19_20[master19_20$Study_Area != "OK",]) %>%
    dplyr::select(-c(Status, Study_Area))
  #  How many camera stations total?
  nrow(OK_all19_20)
  nrow(NE_all19_20)
  
  #  Cameras deployed summer 2020
  master20_21 <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2020_updated_2021-05-29.csv") %>% # camera_master_2020_updated.9.25.20.csv
    dplyr::select("Status", "Date", "Study_Area", "Cell_ID", "Camera_ID","Camera_Long", "Camera_Lat",  "Distance_Focal_Point", "Height_frm_grnd", "Monitoring", "Canopy_Cov", "Land_Mgnt", "Habitat_Type") %>%
    #  Add a column merging cell & camera, format date
    mutate(
      Name = paste(Cell_ID, "_", Camera_ID),
      Name = gsub("[[:space:]]", "", Name),
      Date = as.Date(Date, format = "%Y-%m-%d") #"%m/%d/%Y"
    ) %>%
    arrange(Name, Date) %>%
    #  Remove entries when the camera was pulled
    filter(Status != "Removed") %>%
    #  Remove duplicate data (where deployment and check data match for a given camera)
    group_by(Camera_Lat, Camera_Long) %>%
    distinct(Name, .keep_all = TRUE) %>%
    ungroup() %>%
    #  Remove camera station that did not change but camera # was changed due to damage
    filter(Name != "OK2145_3") %>%
    #  Remove duplicates that dplyr thinks are distinct
    filter(Cell_ID != "NE1990" | Status != "Checked") %>%    
    filter(Cell_ID != "NE2383" | Status != "Checked") 
  
  #  Convert to dataframes
  cams_all20_21 <- as.data.frame(master20_21) %>%
    dplyr::select(-c(Status, Study_Area))
  OK_all20_21 <- as.data.frame(master20_21[master20_21$Study_Area == "OK",]) %>%
    dplyr::select(-c(Status, Study_Area))
  NE_all20_21 <- as.data.frame(master20_21[master20_21$Study_Area != "OK",]) %>%
    dplyr::select(-c(Status, Study_Area))
  #  How many camera stations total?
  nrow(OK_all20_21)
  nrow(NE_all20_21)
  
  
  
  ##  ==================================================
  ##  Version 2: Only cameras that are currently deployed in their most recent location
  
  #  Cameras deployed summer 2020
  current <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2020_updated_2021-05-29.csv") %>% #camera_master_2020_updated.9.24.20.csv 
    dplyr::select("Status", "Date", "Study_Area", "Cell_ID", "Camera_ID", "Camera_Long", "Camera_Lat", "Cam_Removed") %>%
    #  Add a column merging cell & camera, format date and camera removal data
    mutate(
      Name = paste(Cell_ID, "_", Camera_ID),
      Name = gsub("[[:space:]]", "", Name),
      Date = as.Date(Date, format = "%Y-%m-%d"), #"%m/%d/%Y"
      Cam_Removed = ifelse(is.na(Cam_Removed), "1", Cam_Removed)  # 1 = N, 2 = Y
    ) %>%
    #select(-Date) %>%
    arrange(Name, Date) %>%
    #  Filter data to only have most recent check & location data
    group_by(Cell_ID) %>%  # origionally used Name 
    filter(Date == max(Date)) %>%
    ungroup() %>%
    #  Exclude camera stations that are no longer active (cameras moved or removed)
    filter(Cam_Removed != 2) %>%
    #  Remove duplicate cameras when redeployed to slightly new location
    #  Keep in mind this only works if the "Checked" row is below the "Deployed" row
    distinct(Name, .keep_all = TRUE) %>%
    dplyr::select(Name, Study_Area, Camera_Long, Camera_Lat) 
  
  #  Convert to dataframes
  cams_current <- as.data.frame(current)  
  OK_current <- as.data.frame(current[current$Study_Area == "OK",]) %>%
    dplyr::select(-Study_Area)
  NE_current <- as.data.frame(current[current$Study_Area != "OK",]) %>%
    dplyr::select(-Study_Area)
  
  #  Do I have the right number of cameras?
  nrow(OK_current)
  nrow(NE_current)
  
  
  ##  ==================================================
  ##  Bonus Version 3: Cameras and AudioMoths currently deployed
  #  Cameras deployed summer 2020
  Cams <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/AudioMoth_and_Camera_Deployment_2020_052821.csv") %>%  #_100520
    dplyr::select("Date", "Study_Area", "Cell_ID", "Cam_ID", "Cam_Long", "Cam_Lat") %>%
    #  Add a column merging cell & camera, format date and camera removal data
    mutate(
      Name = paste(Cell_ID, "_", Cam_ID),
      Name = gsub("[[:space:]]", "", Name),
      Date = as.Date(Date, format = "%m/%d/%Y")
    ) %>%
    arrange(Name) %>%
    #  Remove duplicate cameras when redeployed to slightly new location
    distinct(Name, .keep_all = TRUE) %>%
    dplyr::select(Name, Study_Area, Cam_Long, Cam_Lat) 
  #  Add NE2897 to this list (deployed in 2019 but never retrieved!)
  add <- master20_21[master20_21$Cell_ID == "NE2897",] %>%
    mutate(
      Name = paste(Cell_ID, "_", Camera_ID),
      Name = gsub("[[:space:]]", "", Name)
    ) %>%
    dplyr::select(Name, Study_Area, Camera_Long, Camera_Lat)
  colnames(add) <- c("Name", "Study_Area", "Cam_Long", "Cam_Lat")
  Cams <- rbind(Cams, add) %>%
    arrange(Name)
  
  #  Convert to dataframes
  Cams <- as.data.frame(Cams)  
  OK_Cams <- as.data.frame(Cams[Cams$Study_Area == "OK",]) %>%
    dplyr::select(-Study_Area)
  NE_Cams <- as.data.frame(Cams[Cams$Study_Area != "OK",]) %>%
    dplyr::select(-Study_Area)
  
  AMs <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/AudioMoth_and_Camera_Deployment_2020_052821.csv") %>% #_100520
    dplyr::select("AM_DeploymentDate", "Study_Area", "Cell_ID", "AM_Long", "AM_Lat") %>%
    #  Add a column merging cell & camera, format date and camera removal data
    mutate(
      Name = paste(Cell_ID, "_AM"),
      Name = gsub("[[:space:]]", "", Name),
      Date = as.Date(AM_DeploymentDate, format = "%m/%d/%Y")
    ) %>%
    arrange(Name) %>%
    filter(!is.na(AM_Long)) %>%
    dplyr::select(-c(AM_DeploymentDate)) %>%
    #  Remove duplicate cameras when redeployed to slightly new location
    distinct(Name, .keep_all = TRUE) %>%
    dplyr::select(Name, Study_Area, AM_Long, AM_Lat)
  
  #  Convert to dataframes
  AMs <- as.data.frame(AMs)  
  OK_AMs <- as.data.frame(AMs[AMs$Study_Area == "OK",]) %>%
    dplyr::select(-Study_Area)
  NE_AMs <- as.data.frame(AMs[AMs$Study_Area != "OK",]) %>%
    dplyr::select(-Study_Area)
  
  
  
  
  ##  ===============================================
  ##  Make these spatial and save as shapefiles and GPX files
  
  #  Load spatial packages  
  library(sp)
  library(rgdal)
  library(rgeos)
  library(raster)
  
  #  Data collected in WGS84 geographic coordinate system (Lat/Long)
  WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  
  #  Study area shapefiles
  OK_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "METHOW_SA") 
  NE_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "NE_SA")  
  OK <- spTransform(OK_SA, WGS84)
  NE <- spTransform(NE_SA, WGS84)
  
  #  Create SpatialPointDataFrames
  cams_master18_19_spdf <- SpatialPointsDataFrame(coords = cams_all18_19[,4:5], data = cams_all18_19, proj4string = WGS84)
  OK_master18_19_spdf <- SpatialPointsDataFrame(coords = OK_all18_19[,4:5], data = OK_all18_19, proj4string = WGS84)
  NE_master18_19_spdf <- SpatialPointsDataFrame(coords = NE_all18_19[,4:5], data = NE_all18_19, proj4string = WGS84)
  
  cams_master19_20_spdf <- SpatialPointsDataFrame(coords = cams_all19_20[,4:5], data = cams_all19_20, proj4string = WGS84)
  OK_master19_20_spdf <- SpatialPointsDataFrame(coords = OK_all19_20[,4:5], data = OK_all19_20, proj4string = WGS84)
  NE_master19_20_spdf <- SpatialPointsDataFrame(coords = NE_all19_20[,4:5], data = NE_all19_20, proj4string = WGS84)
  
  cams_master20_21_spdf <- SpatialPointsDataFrame(coords = cams_all20_21[,4:5], data = cams_all20_21, proj4string = WGS84)
  OK_master20_21_spdf <- SpatialPointsDataFrame(coords = OK_all20_21[,4:5], data = OK_all20_21, proj4string = WGS84)
  NE_master20_21_spdf <- SpatialPointsDataFrame(coords = NE_all20_21[,4:5], data = NE_all20_21, proj4string = WGS84)
  
  cams_current20_21 <- SpatialPointsDataFrame(coords = cams_current[,3:4], data = cams_current, proj4string = WGS84)
  OKcams_current20_21 <- SpatialPointsDataFrame(coords = OK_current[,2:3], data = OK_current, proj4string = WGS84)
  NEcams_current20_21 <- SpatialPointsDataFrame(coords = NE_current[,2:3], data = NE_current, proj4string = WGS84)
  
  Cams_2020 <- SpatialPointsDataFrame(coords = Cams[,3:4], data = Cams, proj4string = WGS84)
  OKCams_2020 <- SpatialPointsDataFrame(coords = OK_Cams[,2:3], data = OK_Cams, proj4string = WGS84)
  NECams_2020 <- SpatialPointsDataFrame(coords = NE_Cams[,2:3], data = NE_Cams, proj4string = WGS84)
  AMs_2020 <- SpatialPointsDataFrame(coords = AMs[,3:4], data = AMs, proj4string = WGS84)
  OKAMs_2020 <- SpatialPointsDataFrame(coords = OK_AMs[,2:3], data = OK_AMs, proj4string = WGS84)
  NEAMs_2020 <- SpatialPointsDataFrame(coords = NE_AMs[,2:3], data = NE_AMs, proj4string = WGS84)
  
  #  Does everything make sense?
  plot(cams_master18_19_spdf)
  plot(OK, add = T); plot(NE, add = T)
  
  plot(cams_master20_21_spdf)
  plot(OK, add = T); plot(NE, add = T)
  plot(cams_current20_21, add = T, col = "red", pch = 19)
  plot(cams_master20_21_spdf, add = T)
  plot(NEcams_current20_21, add = T, col = "darkgreen", pch = 19)
  plot(OKcams_current20_21, add = T, col = "darkgreen", pch = 19)

  
  plot(OK)
  plot(OKCams_2020, add = T)
  plot(OKAMs_2020, add = T, col = "darkorange", pch = 19)
  plot(NE)
  plot(NECams_2020, add = T)
  plot(NEAMs_2020, add = T, col = "darkorange", pch = 19)
  
  #  Write shapefiles
  writeOGR(cams_master18_19_spdf, dsn = "./Shapefiles/Camera_Locations", layer = "cams_master18_19_spdf_050220", driver = "ESRI Shapefile", overwrite = F )
  writeOGR(OK_master18_19_spdf, dsn = "./Shapefiles/Camera_Locations", layer = "OK_master18_19_spdf_050220", driver = "ESRI Shapefile", overwrite = F )
  writeOGR(NE_master18_19_spdf, dsn = "./Shapefiles/Camera_Locations", layer = "NE_master18_19_spdf_050220", driver = "ESRI Shapefile", overwrite = F )
  
  writeOGR(cams_master19_20_spdf, dsn = "./Shapefiles/Camera_Locations", layer = "cams_master19_20_spdf_050220", driver = "ESRI Shapefile", overwrite = F )
  writeOGR(OK_master19_20_spdf, dsn = "./Shapefiles/Camera_Locations", layer = "OK_master19_20_spdf_050220", driver = "ESRI Shapefile", overwrite = F )
  writeOGR(NE_master19_20_spdf, dsn = "./Shapefiles/Camera_Locations", layer = "NE_master19_20_spdf_050220", driver = "ESRI Shapefile", overwrite = F )
  
  writeOGR(cams_master20_21_spdf, dsn = "./Shapefiles/Camera_Locations", layer = "cams_master20_21_spdf_052921", driver = "ESRI Shapefile", overwrite = F )
  writeOGR(OK_master20_21_spdf, dsn = "./Shapefiles/Camera_Locations", layer = "OK_master20_21_spdf_052921", driver = "ESRI Shapefile", overwrite = F )
  writeOGR(NE_master20_21_spdf, dsn = "./Shapefiles/Camera_Locations", layer = "NE_master20_21_spdf_052921", driver = "ESRI Shapefile", overwrite = F )
  
  writeOGR(cams_current20_21, dsn = "./Shapefiles/Camera_Locations", layer = "Cam_currentlocs_spdf_052921", driver = "ESRI Shapefile", overwrite = F )
  writeOGR(OKcams_current20_21, dsn = "./Shapefiles/Camera_Locations", layer = "Cam_currentlocs.OK_spdf_052921", driver = "ESRI Shapefile", overwrite = F )
  writeOGR(NEcams_current20_21, dsn = "./Shapefiles/Camera_Locations", layer = "Cam_currentlocs.NE_spdf_052921", driver = "ESRI Shapefile", overwrite = F )
  
  writeOGR(Cams_2020, dsn = "./Shapefiles/Camera_Locations", layer = "Cam_2020locs_spdf_052921", driver = "ESRI Shapefile", overwrite = F )
  writeOGR(AMs_2020, dsn = "./Shapefiles/Camera_Locations", layer = "AM_2020locs_spdf_052921", driver = "ESRI Shapefile", overwrite = F )
  
  #  Write GPX files
  #  Be sure spdf only includes NAME, LONG, LAT
  writeOGR(cams_current20_21, dsn="./Shapefiles/Camera_Locations/Cam_currentlocs_spdf_052921.gpx",
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
  writeOGR(Cams_2020, dsn="./Shapefiles/Camera_Locations/Cam_2020locs_spdf_052921.gpx",
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
  writeOGR(AMs_2020, dsn="./Shapefiles/Camera_Locations/AM_2020locs_spdf_052921.gpx",
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)

  
  

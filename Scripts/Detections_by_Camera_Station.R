  #'  ============================================
  #'  Final Detection & Camera Station Data Set
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  November 2020
  #'  ============================================
  #'  Script to combine species detection data with camera station data. This 
  #'  ensures that all camera locations and species detections match up and no
  #'  camera station or detection is left out.
  #'  
  #'  Combines: 
  #'  "Bassing_AllDetections.csv" from Clean_Review_CSVs.R
  #'     -Contains ALL detections of animals, humans, & vehicles (no empties)
  #'  "camera_master_2018-2021_updated_9.25.20.csv" from Camera_Station_Wrangling.R
  #'     -Contains ALL deployment, check, and pulling data for each camera
  #'  
  #'  1. Drop camera checking and removal data from camera master file.
  #'  2. Join detection data with camera stat data via grid cell & camera number
  #'  3. Trouble shoot mismatches between two data sets
  #'  4. Simplify merged data to retain only columns that are relevant for analyses
  #'  5. Create spatial data for mapping and spatial analyses
  #'  6. Save that shit!
  #'  ============================================

  #'  Load libraries
  library(chron)  
  library(tidyverse)
  
  #'  Read in data, format, and filter
  #'  Camera station data:
  allstations <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2018-2021_updated_9.25.20.csv") %>%
    select("Status", "Year", "Date", "Study_Area", "Cell_ID", "Camera_ID", "Name", "Camera_Long", "Camera_Lat", "Distance_Focal_Point", "Height_frm_grnd", "Monitoring", "Canopy_Cov", "Land_Mgnt", "Habitat_Type") %>%
    mutate(
      Date = as.Date(Date, format = "%Y-%m-%d"),
      CameraLocation = as.factor(as.character(Name))
    ) %>%
    arrange(Name, Date) %>%
    #  Remove entries when the camera was pulled
    filter(Status != "Removed") %>%
    #  Remove duplicate data (where deployment and check data match for a given camera)
    group_by(Camera_Lat, Camera_Long) %>%
    distinct(Name, .keep_all = TRUE) %>%
    ungroup() %>%
    select(-Name) %>%
    #  Remove duplicate cameras with incorrect coordinates (cameras were not moved)
    filter(Cell_ID != "NE5094" | Status != "Checked") %>%    
    filter(Cell_ID != "NE6019" | Status != "Checked") %>%
    filter(Cell_ID != "NE7602" | Status != "Checked") %>%
    #  Remove duplicates that dplyr thinks are distinct for some reason
    filter(Cell_ID != "NE1990" | Status != "Checked") %>%    
    filter(Cell_ID != "NE2383" | Status != "Checked")
    #  Remove camera station that did not change but camera # was changed due to damage
    #filter(CameraLocation != "OK2145_3")
    
  #'  Species detection data  
  alldetections <- read.csv("./Output/Bassing_AllDetections_2020-11-24.csv") %>%
    select(-c(X, Folder, ImageQuality)) %>%
    mutate(
      DateTime = as.POSIXct(DateTime,
                            format="%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles"),
      Date = as.Date(Date, format = "%Y-%m-%d"),
      Time = chron(times = Time),
      CameraLocation = as.factor(as.character(CameraLocation)),
      Species = as.factor(as.character(Species)),
      HumanActivity = as.factor(as.character(HumanActivity)),
      Count = as.numeric(Count),
      AF = as.numeric(AF),
      AM = as.numeric(AM),
      AU = as.numeric(AU),
      OS = as.numeric(OS),
      UNK = as.numeric(UNK),
      Collars = as.numeric(Collars),
      Tags = as.character(Tags),
      Color = as.character(Color),
      SecondOp = as.factor(as.character(SecondOp))
    ) %>%
    #  Remove Moultrie cameras from detection data
    #  Moultrie cameras not included in camera station data for now
    filter(!grepl("Moultrie", CameraLocation))

  #'  Or to save myself the reformatting step...
  # source("./Scripts/Clean_Reviewed_CSVs.R")
  # alldetections <- alldetections %>% filter(!grepl("Moultrie", CameraLocation))

  #'  For now, subset to just 2018-2019 NE data for data integration project
  #'  Will need to do this on a larger scale for all data
  NEstations <- allstations[allstations$Study_Area == "NE" & allstations$Year == "Year1",]
  #'  Tossed duplicate camera stations that moved to slightly new locations
  #'  THIS IS NOT A PERMINANT FIX!!!!!
  NEstat <- NEstations[!duplicated(NEstations$CameraLocation),]
  NEdetections <- alldetections[alldetections$CameraLocation != "OK4880_94",]
  
  #'  Double check I have the same camera location information in each data set
  #'  NA's indicate that CameraLocation is missing
  cams <- as.data.frame(unique(NEstat$CameraLocation)) 
  cams <- cbind(cams, rep("stations", nrow(cams)))
  colnames(cams) <- c("CameraLocation", "Data Source A")
  dets <- as.data.frame(unique(NEdetections$CameraLocation)) 
  dets <- cbind(dets, rep("detection cameras", nrow(dets)))
  colnames(dets) <- c("CameraLocation", "Data Source B")
  diff <- full_join(cams, dets, by = "CameraLocation")
  
  #'  Join detection and camera station data into one messy massive data frame
  #'  Each version should have the same number of observations if they match
  dim(right_join(NEdetections, NEstat, by = "CameraLocation"))
  dim(left_join(NEdetections, NEstat, by = "CameraLocation"))
  dim(full_join(NEdetections, NEstat, by = "CameraLocation"))
  
  full_dat <- full_join(NEdetections, NEstat, by = "CameraLocation") %>%
    #'  Rename Date columns to make it more clear how these differ
    mutate(
      Date = as.Date(Date.x, format = "%Y-%m-%d"),
      DeployDate = as.Date(Date.y, format = "%Y-%m-%d")
    ) %>%
    #'  Drop unnecessary columns
    select(-c(Date.x, DT_Good, Service, Empty, SecondOp, Status, Date.y)) %>%
    #'  Reorganize columns to put Date data in more useful locations
    relocate("Date", .after = DateTime) %>%
    relocate("DeployDate", .after = Color)
  
  cams <- distinct(full_dat, full_dat$CameraLocation)
  
  #'  Final set of detection data with camera locations included for each observation
  animal_det <- full_dat %>%
    filter(Animal == "true" | Animal == "TRUE")
  SEFS521_camdata <- full_dat %>%
    filter(Species == "Cougar" | Species == "Elk")
  
  #'  Save for group project!
  write.csv(full_dat, "G:/My Drive/1_Repositories/WPPP_Data_Integration/full_camdata.csv")
  write.csv(SEFS521_camdata, "G:/My Drive/1_Repositories/WPPP_Data_Integration/SEFS521_camdata.csv")
  
  
  #'  =============================================
  #'  Make the species detection data spatial based on CameraLocation lat/long
  #'  Load required packages
  require(sf)
  #'  Define coordinate projection
  wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  NEdat <- st_as_sf(full_dat, coords = c("Camera_Long", "Camera_Lat"), crs = wgs84)

  
  
  

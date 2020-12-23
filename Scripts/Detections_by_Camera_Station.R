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
  allstations <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2018-2021_updated_2020-12-22_skinny.csv") %>% 
    select("Status", "Year", "Date", "Study_Area", "Cell_ID", "Camera_ID", "Name", "Camera_Lat", "Camera_Long", "Distance_Focal_Point", "Height_frm_grnd", "Monitoring", "Canopy_Cov", "Land_Mgnt", "Habitat_Type", "Pull_Status") %>%
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
    filter(Cell_ID != "NE2383" | Status != "Checked") %>%
    #  Remove duplicates where camera angle changed but location effectively did not
    filter(Cell_ID != "OK2749" | Camera_Lat != "48.64027") %>%
    filter(Cell_ID != "OK3667" | Camera_Lat != "48.54991") %>%
    filter(Cell_ID != "OK7658" | Camera_Lat != "48.17755") %>%
    #  Remove camera station that did not change but camera # was changed due to damage
    filter(Cell_ID != "OK2145" | Camera_ID != "3") # DON'T FORGET TO CHANGE OK2145_3" to "OK2145_112 in detection data!!!

    
  #'  Species detection data  
  alldetections <- read.csv("./Output/Bassing_AllDetections_2020-12-22.csv") %>%
    select(-c(X, Folder, ImageQuality)) %>%
    mutate(
      DateTime = as.POSIXct(DateTime,
                            format="%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles"),
      Date = as.Date(Date, format = "%Y-%m-%d"),
      Time = chron(times = Time),
      # #  Change camera location name to match camera station data
      # CameraLocation = ifelse(CameraLocation == "OK2145_3", "OK2145_112", CameraLocation),
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
    filter(!grepl("Moultrie", CameraLocation)) %>%
    droplevels()


  #'  Or to save myself the reformatting step...
  # source("./Scripts/Clean_Reviewed_CSVs.R")
  # alldetections <- alldetections %>% filter(!grepl("Moultrie", CameraLocation))

  #'  For now, subset to just 2018-2019 data
  #'  Will need to do this on a larger scale for all data
  stations <- allstations[allstations$Year == "Year1",] #allstations$Study_Area == "NE" & 
  #'  Tossed duplicate camera stations that moved to slightly new locations
  #'  THIS IS NOT A PERMINANT FIX!!!!!
  stations <- stations[!duplicated(stations$CameraLocation),]
  #NEdetections <- alldetections[alldetections$CameraLocation != "OK4880_94",]
  
  #'  Double check I have the same camera location information in each data set
  #'  NA's indicate that CameraLocation is missing
  cams <- as.data.frame(unique(stations$CameraLocation)) 
  cams <- cbind(cams, rep("stations", nrow(cams)))
  colnames(cams) <- c("CameraLocation", "Data Source A")
  dets <- as.data.frame(unique(alldetections$CameraLocation)) 
  dets <- cbind(dets, rep("detection cameras", nrow(dets)))
  colnames(dets) <- c("CameraLocation", "Data Source B")
  diff <- full_join(cams, dets, by = "CameraLocation")
  
  #'  Join detection and camera station data into one messy massive data frame
  #'  Each version should have the same number of observations if they match
  dim(right_join(alldetections, stations, by = "CameraLocation"))
  dim(left_join(alldetections, stations, by = "CameraLocation"))
  dim(full_join(alldetections, stations, by = "CameraLocation"))
  
  #'  Append the correct camera location data to each image. Relevant for cameras
  #'  that moved part way through the season to a different location within the
  #'  grid cell so all site-specific variables changed. 
  #'  Year 1: NE2881_9, NE2902_22, NE3903_25, & NE5094_10
  #'  Need the start and end dates for each camera station so read in All_Camera_Stations 
  #'  problems data frame.
  deployed <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/All_Camera_Stations_18-19_updated_12.22.20.csv")
    
  messy <- full_join(stations, deployed, by = c("Cell_ID")) %>% 
    #'  FYI: by = c("Camera_Lat" = "Latitude", "Camera_Long" = "Longitude")
    #'  is good for making sure all coordinates match up across databases
    #'  Drop Camera_Lat & Camera_Long since these don't account for camera locations
    #'  that moved part way through season
    select(-c(Camera_Lat, Camera_Long))
  
  #'  Pull out data for cameras that were moved to a second location ("b" is 
  #'  added to the CameraLocation name in the deployed data frame)
  moved <- messy %>%
    filter(str_detect(CameraLocation.y, "b"))
  
  #'  Retain only where CameraLocation names are consistent across stations & 
  #'  deployed data frames (this removes funky duplicates that occurred during
  #'  the full_join but also drops the moved location data)
  nomess <- distinct(messy[messy$CameraLocation.x == messy$CameraLocation.y,])
  
  #'  Merge two parts back together so all camera locations are represented by no
  #'  duplicates occur
  clean_deployed <- rbind(nomess, moved) %>%
    arrange(CameraLocation.y) %>%
    mutate(
      Setup_date = as.Date(Setup_date, format = "%m/%d/%Y"),
      Retrieval_date = as.Date(Retrieval_date, format = "%m/%d/%Y"),
      CameraLocation.x = as.factor(as.character(CameraLocation.x)),
      CameraLocation.y = as.factor(as.character(CameraLocation.y))
    )

  #'  Double check I have the same camera location information in each data set
  #'  NA's indicate that CameraLocation is missing (should only happen for "b" sites)
  cams1 <- as.data.frame(as.factor(as.character(clean_deployed$CameraLocation.y)))
  cams1 <- cbind(cams1, rep("CameraLocation.y", nrow(cams1)))
  colnames(cams1) <- c("CameraLocation", "Data Source A")
  cams2 <- as.data.frame(as.factor(as.character(clean_deployed$CameraLocation.x))) 
  cams2 <- cbind(cams2, rep("CameraLocation.x", nrow(cams2)))
  colnames(cams2) <- c("CameraLocation", "Data Source B")
  cams3 <- as.data.frame(unique(alldetections$CameraLocation)) 
  cams3 <- cbind(cams3, rep("Detection Data", nrow(cams3)))
  colnames(cams3) <- c("CameraLocation", "Data Source C")
  diff <- full_join(cams3, cams1, by = "CameraLocation")
  diff <- full_join(diff, cams2, by = "CameraLocation")
  
  #'  Now merge them all together so only the right camera location information
  #'  is attached to the detection data
  clean <- data.frame()
  for(i in 1: nrow(clean_deployed)){
    #'  Extract site-specific location & deployment data
    allcamloc <- clean_deployed$CameraLocation.y[i]
    basecamloc <- clean_deployed$CameraLocation.x[i]
    start <- clean_deployed$Setup_date[i]  
    end <- clean_deployed$Retrieval_date[i]
    
    #'  Subset detection data by individual camera sites
    site <- subset(alldetections, CameraLocation == basecamloc)
    site$FinalLocation <- allcamloc
    activecam <- subset(site, Date >= start & Date <= end) 
    
    clean <- rbind(clean, activecam)
  }
  
  #'  WARNING: Some cameras were pulled & redployed on the same day so there may
  #'  be images from both locations on same day. Leads to images on that day 
  #'  being duplicated for each camera location name. 
  #'  E.g., Camera NE2902_22 and NE2902_22b have end and start dates on 10/4/18,
  #'  respectively. All images from that day are duplicated in step above. Need 
  #'  to drop duplicates based on time the camera was moved.
  #'  Problem cameras and notes: 
  #'  NE2902_22 moved 10/4/18, no animal/human images prior to move on 10/4 so
  #'  all images from 10/4 (memory card C69) should be assigned to NE2902_22b.
  #'  NE5094_10 tree logged on 7/13/18, redeployed to new location on 7/31/18 so  
  #'  all images from 7/31 (memory card C72) should be assigned to NE5094_10b.
  
  squeeky_clean <- clean %>%
    filter(FinalLocation != "NE2902_22" | !grepl("C69", RelativePath))

  
  
  full_dat <- full_join(alldetections, stations, by = "CameraLocation") %>%
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

  
  
  

  #'  ============================================
  #'  Final Detection & Camera Station Data Set
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  November 2020
  #'  ============================================
  #'  Script to combine species detection data with camera station data (mainly,
  #'  camera coordinates and deployment covariate data). This ensures that all
  #'  camera locations and species detections match up and no camera station or
  #'  detection is left out. Also accounts for when a camera station was moved 
  #'  to a new location within the grid cell, changing camera coordinates and
  #'  covariate data for detections that occurred in a single grid cell.
  #'  
  #'  Combines: 
  #'  "Bassing_AllDetections_DATE.csv" from Clean_Review_CSVs.R
  #'     -Contains ALL detections of animals, humans, & vehicles (no empties)
  #'  "camera_master_2018-2021_updated_DATE_skinny.csv" from Camera_Station_Wrangling.R
  #'     -Contains ALL deployment, check, and pulling data for each camera
  #'  "All_Camera_Stations_18-19_updated_DATE.csv"
  #'     -Contains camera locations (including updated names/locations when a
  #'     camera was moved), deployment & pull dates, and problem dates; this is 
  #'     a reduced and hang-built database using the camera_master file above.
  #'      
  #'  
  #'  1. Drop camera checking and removal data from camera master file.
  #'  2. Join detection & camera station data via camera station name & location
  #'  3. Trouble shoot mismatches between the data sets
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

  #'  Camera deployment, retrieval, and problem dates for each station
  #'  This includes the locations and date ranges of camera stations that moved
  #'  part way through the season!
  deployed <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/All_Camera_Stations_18-19_updated_12.22.20.csv")
  
  #'  Species detection data  
  alldetections <- read.csv("./Output/Bassing_AllDetections_2020-12-24.csv") %>%
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
  # alldetections <- alldetections %>% filter(!grepl("Moultrie", CameraLocation)) %>% droplevels()

  
  #'  For now, subset to just 2018-2019 data
  #'  Will need to do this on a larger scale for all data
  stations <- allstations[allstations$Year == "Year1",]  
  #'  Toss duplicate camera stations that moved to new locations
  #'  THIS IS NOT A PERMINANT FIX!!!!!
  og_stations <- stations[!duplicated(stations$CameraLocation),]

  #'  Double check I have the same camera location information in each database
  #'  NA's indicate that CameraLocation is missing (there should be none!)
  cams <- as.data.frame(unique(og_stations$CameraLocation)) 
  cams <- cbind(cams, rep("stations", nrow(cams)))
  colnames(cams) <- c("CameraLocation", "Data Source A")
  dets <- as.data.frame(unique(alldetections$CameraLocation)) 
  dets <- cbind(dets, rep("detection cameras", nrow(dets)))
  colnames(dets) <- c("CameraLocation", "Data Source B")
  diff <- full_join(cams, dets, by = "CameraLocation")
  
  #'  Join detection and camera station data into one messy massive data frame
  #'  Each version should have the same number of observations if they match
  dim(right_join(alldetections, og_stations, by = "CameraLocation"))
  dim(left_join(alldetections, og_stations, by = "CameraLocation"))
  dim(full_join(alldetections, og_stations, by = "CameraLocation"))
  
  #'  Append correct camera location data to each image. Important for cameras
  #'  that moved part way through the season to a different location within the
  #'  grid cell so all site-specific variables & coordinates changed. 
  #'  Year 1: NE2881_9, NE2902_22, NE3903_25, & NE5094_10
  
  #'  First double check that all coordinates match up between databases!
  #'  The only NAs should occur with start/end dates for cameras that moved to 
  #'  slightly different locations but are monitoring the exact same spot
  #'  (e.g., moved to a neighboring tree). All cameras that were moved to a truly
  #'  new location (i.e., CameraLocations ending in "b") should have deployment
  #'  data and no NAs.
  #'  Note: will end up with extra rows in my final data set (full_dat) as a 
  #'  result and need to drop them at the very end.
  #'  Yr1 cams with OK NAs: NE3149_8, NE5740_15, NE5853_20, NE6491_34... should be more
  match_coord <- full_join(stations, deployed, by = c("Camera_Lat" = "Latitude", "Camera_Long" = "Longitude"))
  
  #'  Join original stations (don't account for moves) with start/end data for all stations
  messy <- full_join(og_stations, deployed, by = c("Cell_ID")) %>% 
    #'  Drop Camera_Lat & Camera_Long since these don't account for camera locations
    #'  that moved part way through season
    select(-c(Camera_Lat, Camera_Long))
  
  #'  Pull out data for cameras that were moved to a second location ("b" was 
  #'  added to the CameraLocation name in the deployed data frame)
  moved <- messy %>%
    filter(str_detect(CameraLocation.y, "b"))
  
  #'  Retain only where CameraLocation names are consistent across stations & 
  #'  deployed data frames (this removes funky duplicates that occurred during
  #'  the full_join but also drops the location data for cameras that moved)
  nomess <- distinct(messy[messy$CameraLocation.x == messy$CameraLocation.y,])
  
  #'  Merge two parts back together so all camera locations are represented but
  #'  no duplicates occur
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
  #'  Extract site-specific location & deployment data
  for(i in 1: nrow(clean_deployed)){
    #'  CameraLoction names & coordinates, including cameras that were moved
    allcamloc <- clean_deployed$CameraLocation.y[i]
    allcamlat <- clean_deployed$Latitude[i]
    allcamlong <- clean_deployed$Longitude[i]
    #'  Original CameraLocation names (these will match the detection data)
    basecamloc <- clean_deployed$CameraLocation.x[i]
    #'  Identify the day each camera was deployed and pulled/moved
    start <- clean_deployed$Setup_date[i]  
    end <- clean_deployed$Retrieval_date[i]
    
    #'  Subset detection data by individual camera site
    site <- subset(alldetections, CameraLocation == basecamloc)
    #'  Append the updated CameraLocation name, including cameras that moved
    site$FinalLocation <- allcamloc
    site$Camera_Lat <- allcamlat
    site$Camera_Long <- allcamlong
    #'  Only retain detections that fall within the start & end dates for each
    #'  camera. This is what ensures observations are correctly assigned to 
    #'  camera locations when the camera was moved.
    activecam <- subset(site, Date >= start & Date <= end) 
    #'  Combine data for each camera
    clean <- rbind(clean, activecam)
  }
  
  #'  WARNING: Some cameras were pulled & redployed on the same day so there may
  #'  be images from both locations on the same day. Leads to images from that  
  #'  day being duplicated with each camera location name. 
  #'  E.g., Camera NE2902_22 and NE2902_22b have end and start dates on 10/4/18,
  #'  respectively. All images from that day are duplicated in step above. Need 
  #'  to drop duplicates based on time the camera was moved or camera's new 
  #'  memory card.
  #'  Problem cameras and notes: 
  #'  NE2902_22 moved 10/4/18, no animal/human images prior to move on 10/4 so
  #'  all images from 10/4 (memory card C69) should be assigned to NE2902_22b.
  squeeky_clean <- clean %>%
    filter(FinalLocation != "NE2902_22" | !grepl("C69", RelativePath))

  #'  Add camera station covariate data to detection data
  full_dat <- full_join(squeeky_clean, stations, by = c("Camera_Lat", "Camera_Long")) %>%
    #'  Rename Date columns to make it clearer how these differ; rename location name
    mutate(
      Date = as.Date(Date.x, format = "%Y-%m-%d"),
      DeployDate = as.Date(Date.y, format = "%Y-%m-%d"),
      CameraLocation = FinalLocation
    ) %>%
    #'  Drop unnecessary columns
    select(-c(Date.x, Date.y, DT_Good, Service, Empty, SecondOp, Status, FinalLocation,
              CameraLocation.x, CameraLocation.y, Cell_ID, Camera_ID, Pull_Status)) %>%
    #'  Reorganize columns to put data in more useful locations
    relocate("Date", .after = DateTime) %>%
    relocate("DeployDate", .after = Color) %>%
    relocate(c("CameraLocation", "Camera_Lat", "Camera_Long"), .after = Time) %>%
  #'  Remove rows with NAs that were created during full_join. 
  #'  This happens when a camera station has no detection data. Specifically, 
  #'  when camera sites have a second record in the "stations" data frame 
  #'  because the camera was moved slightly but is still monitoring the same
  #'  spot (coordinates changed by a few meters).
  filter(!is.na(File))
  
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

  
  
  

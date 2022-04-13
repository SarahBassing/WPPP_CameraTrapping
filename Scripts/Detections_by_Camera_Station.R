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
  #'     Sourced here through Camera_Station_Covariate_Wrangling.R
  #'     -Contains ALL deployment, check, and pulling data for each camera with
  #'     cleaned camera station covariate data
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

  #'  Clean workspace & load libraries
  rm(list = ls())

  library(chron)  
  library(tidyverse)
  
  #'  Read in data, format, and filter
  #'  Camera station data:
  source("./Scripts/Camera_Station_Covariate_Wrangling.R") 
  allstations <- allstations %>%
    dplyr::select("Status", "Year", "Date", "Study_Area", "Cell_ID", "Camera_ID", "Name", 
           "Camera_Lat", "Camera_Long", "Distance_Focal_Point", "Height_frm_grnd", 
           "Monitoring", "Canopy_Cov", "Land_Mgnt", "Land_Owner", "Habitat_Type", 
           "Pull_Status") %>%
    mutate(
      Date = as.Date(Date, format = "%Y-%m-%d"),
      Name = ifelse(Name == "OK5332_73b", "OK5332_73", Name),
      CameraLocation = as.factor(as.character(Name))
    ) %>%
    arrange(Name, Date) %>%
    #  Remove entries when the camera was pulled
    filter(Status != "Removed") %>%
    #  Remove duplicate data (where deployment and check data match for a given camera)
    group_by(Camera_Lat, Camera_Long) %>%
    distinct(Name, .keep_all = TRUE) %>%
    ungroup() %>%
    dplyr::select(-Name) %>%
    #  Remove duplicate cameras with incorrect coordinates (cameras were not moved)
    filter(Cell_ID != "NE5094" | Status != "Checked") %>%    
    filter(Cell_ID != "NE6019" | Status != "Checked") %>%
    filter(Cell_ID != "NE7602" | Status != "Checked") %>%
    #  Remove duplicates that dplyr thinks are distinct for some reason
    filter(Cell_ID != "NE1990" | Status != "Checked") %>%    
    filter(Cell_ID != "NE2383" | Status != "Checked") %>%
    #  Remove duplicates where camera angle changed but location effectively did not
    #  Actually save this step for later down (match_coord step below)
    # filter(Cell_ID != "OK2749" | Camera_Lat != "48.64027") %>%
    # filter(Cell_ID != "OK3667" | Camera_Lat != "48.54991") %>%
    # filter(Cell_ID != "OK7658" | Camera_Lat != "48.17755") %>%
    # filter(Cell_ID != "NE3081" | Camera_Lat != "48.42142") %>%
    #  Remove camera station that did not change but camera # was changed due to damage
    filter(Cell_ID != "OK2145" | Camera_ID != "3") %>%
    #  Remove camera station that was stolen and no data recorded
    filter(Cell_ID != "NE4299" | Camera_ID != "50") %>%
    filter(Cell_ID != "NE4299" | Status != "Checked") %>%
    filter(Cell_ID != "NE5314" | Camera_ID != "25") %>%
    filter(Cell_ID != "OK5332" | Camera_ID != "73") %>%
    #  Change Camera 73b to 73 since no detections at original Camera 73 location
    mutate(Camera_ID = ifelse(Camera_ID == "73b", "73", Camera_ID))
    

  #'  Camera deployment, retrieval, and problem dates for each station
  #'  This includes the locations and date ranges of camera stations that moved
  #'  part way through the season!
  deployedYr1 <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/All_Camera_Stations_18-19_updated_1.21.21.csv", header = TRUE)
  deployedYr2 <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/All_Camera_Stations_19-20.csv", header = TRUE)
  deployedYr3 <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/All_Camera_Stations_20-21.csv", header = TRUE)
  deployed <- rbind(deployedYr1, deployedYr2, deployedYr3)
  
  #'  Species detection data  
  alldetections <- read.csv("./Output/Bassing_AllDetections18-21_2022-04-13.csv") %>% #Bassing_AllDetections18-20_2022-03-16
    dplyr::select(-c(X, Folder, ImageQuality)) %>%
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
    filter(!grepl("Moultrie", CameraLocation)) %>%
    filter(!grepl("Moultire", CameraLocation)) %>%
    droplevels()

  #'  Or to save myself the reformatting step...
  # source("./Scripts/Clean_Reviewed_CSVs.R")
  # alldetections <- alldetections %>% filter(!grepl("Moultrie", CameraLocation)) %>% droplevels()

  
  #' #'  For now, subset to just 2018-2019 data
  #' #'  Will need to do this on a larger scale for all data
  #' # stationsYr1 <- allstations[allstations$Year == "Year1",]  
  #' # stationsYr2 <- allstations[allstations$Year == "Year2",]
  #' stations <- allstations[allstations$Year != "Year3",]
  
  #'  Toss duplicate camera stations that moved to new locations
  #'  THIS IS NOT A PERMINANT FIX!!!!!
  # og_stations <- stationsYr1[!duplicated(stationsYr1$CameraLocation),]
  # og_stations <- stationsYr2[!duplicated(stationsYr2$CameraLocation),]
  og_stations <- allstations[!duplicated(allstations$CameraLocation),]

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
  #'  Year 1: NE2881_9, NE2902_22, NE3903_25, NE5094_10, OK2749, OK3667, OK7658
  #'  Year 2: NE1380_106, NE1538_6, OK2145_112, OK6286_52, OK8420_71
  
  #'  First double check that all coordinates match up between databases!
  #'  The only NAs should occur with start/end dates for cameras that moved to 
  #'  slightly different locations but are monitoring the exact same spot
  #'  (e.g., moved to a neighboring tree). All cameras that were moved to a truly
  #'  new location (i.e., CameraLocations ending in "b") should have deployment
  #'  data and no NAs.
  #'  Note: will end up with extra rows in my final data set (full_dat) as a 
  #'  result and need to drop them at the very end.
  #'  Yr1 cams with NAs that are ok: NE3149_8, NE5740_15, NE5853_20, NE6491_34, 
  #'  OK2749_59, OK3667_92, & OK7858_43
  #'  Yr2 cams with NAs that are ok: NE1380_106, NE1538_6, OK8420_71
  match_coord <- full_join(allstations, deployed, by = c("Camera_Lat" = "Latitude", "Camera_Long" = "Longitude"))
  # match_coord <- full_join(stationsYr2, deployedYr2, by = c("Camera_Lat" = "Latitude", "Camera_Long" = "Longitude"))
  
  #'  Join original stations (don't account for moves) with start/end data for all stations
  # messy <- full_join(og_stations, deployedYr1, by = c("Cell_ID")) %>% 
  messy <- full_join(og_stations, deployed, by = c("Cell_ID")) %>%
    #'  Drop Camera_Lat & Camera_Long since these don't account for camera locations
    #'  that moved part way through season
    dplyr::select(-c(Camera_Lat, Camera_Long))
  
  #'  Pull out data for cameras that were moved to a second location ("b" was 
  #'  added to the CameraLocation name in the deployed data frame)
  #'  Only applies to Year 1 cameras
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
  
  #'  WARNING: Some cameras were pulled & redeployed on the same day so there may
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
  full_dat <- full_join(squeeky_clean, allstations, by = c("Camera_Lat", "Camera_Long")) %>%
    #'  Rename Date columns to make it clearer how these differ; rename location name
    mutate(
      Date = as.Date(Date.x, format = "%Y-%m-%d"),
      DeployDate = as.Date(Date.y, format = "%Y-%m-%d"),
      CameraLocation = FinalLocation
    ) %>%
    #'  Drop unnecessary columns
    dplyr::select(-c(Date.x, Date.y, DT_Good, Service, Empty, SecondOp, Status, FinalLocation,
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

  
  ####  Save for projects!  ####
  #'  --------------------------
  write.csv(full_dat, paste0("./Output/full_camdata18-21_", Sys.Date(), ".csv"))
  

  #'  Camera station covariates with updated CameraLocation names (b cameras)
  stations <- clean_deployed %>%
    dplyr::select("Year", "Study_Area", "Cell_ID", "Camera_ID.y",   
                  "CameraLocation.y", "Latitude", "Longitude", "Distance_Focal_Point", 
                  "Height_frm_grnd", "Monitoring", "Canopy_Cov", "Land_Mgnt", 
                  "Land_Owner", "Habitat_Type")
  colnames(stations) <- c("Year", "Study_Area", "Cell_ID", "Camera_ID", 
                          "CameraLocation", "Latitude", "Longitude", "Distance_Focal_Point", 
                          "Height_frm_grnd", "Monitoring", "Canopy_Cov", "Land_Mgnt", 
                          "Land_Owner", "Habitat_Type")
  # write.csv(stations, paste0("./Output/Camera_Station18-21_Covariates_", Sys.Date(), ".csv")) 
  
  #'  Final set of detection data with camera locations included for each observation
  animal_det <- full_dat %>%
    filter(Animal == "true" | Animal == "TRUE")
  SEFS521_camdata <- full_dat %>%
    filter(Species == "Cougar" | Species == "Elk")
  # write.csv(SEFS521_camdata, "G:/My Drive/1_Repositories/WPPP_Data_Integration/SEFS521_camdata.csv")
  
  #'  All moose detections
  OKmoose <- full_dat %>%
    filter(Species == "Moose") %>%
    filter(str_detect(CameraLocation, paste("NE"), negate = TRUE))
  # write.csv(OKmoose, paste0('./Output/OKMoose_allimgs_', Sys.Date(), '.csv'))
  NEmoose <- full_dat %>%
    filter(Species == "Moose") %>%
    filter(str_detect(CameraLocation, paste("OK"), negate = TRUE))
  # write.csv(NEmoose, paste0('./Output/NEMoose_allimgs_', Sys.Date(), '.csv'))
  
  #'  All wolf detections
  wolves <- full_dat %>%
    filter(Species == "Wolf") %>%
    filter(Year == "Year1")
  # write.csv(wolves, paste0('./Output/Wolf_allimgsYr1_', Sys.Date(), '.csv'))
  
  #'  All cougar detections
  cougars <- full_dat %>%
    filter(Species == "Cougar") #%>%
    #filter(AU > 0 | UNK > 0)
  collared_coug <- cougars %>%
    filter(Collars > 0)
  # write.csv(cougars, paste0('./Output/Cougars_allimgs_', Sys.Date(), '.csv'))
  
  
  #  =============================
  #### CAPSTONE PROJECT DATA  ####
  #  =============================
  
  #'  Coyote, bobcat & human detections for Alyssa
  meso <- full_dat %>%
    filter(Species == "Bobcat" | Species == "Coyote") %>%
    filter(!grepl("Moultrie", CameraLocation))
  #'  Human detections (on foot and vehicle)
  humans <- full_dat %>%
    filter(Human == "TRUE" | Vehicle == "TRUE") %>%
    filter(!grepl("Moultrie", CameraLocation))
  Alyssa_data <- rbind(meso, humans)
  # write.csv(Alyssa_data, paste0('G:/My Drive/1 Volunteers/Capstone Projects/Alyssa/Alyssa_dataYr2_', Sys.Date(), '.csv'))
  
  #'  Cattle & bear detections for Angela
  moo <- full_dat %>%
    filter(Species == "Cattle") %>%
    dplyr::select(-c(Distance_Focal_Point, Height_frm_grnd, Monitoring, Canopy_Cov, Land_Mgnt, Land_Owner, Habitat_Type))
  # write.csv(moo, paste0('G:/My Drive/1 Volunteers/Capstone Projects/2021-2022/Angela/Cattle_detections_', Sys.Date(), '.csv'))
  bears <- full_dat %>%
    filter(Species == "Black Bear")   
  # write.csv(bears, paste0('G:/My Drive/1 Volunteers/Capstone Projects/2021-2022/Angela/Bear_detections_', Sys.Date(), '.csv'))
  
  #'  Cougar detections for Donovan
  cougars <- full_dat %>%
    filter(Species == "Cougar")
  # write.csv(cougars, paste0('G:/My Drive/1 Volunteers/Capstone Projects/2021-2022/Donovan/Cougar_detections_', Sys.Date(), '.csv'))
  
  #'  Deer detections for Sophia (need to narrow down the time period)
  Deer <- full_dat %>%
    filter(Species == "White-tailed Deer" | Species == "Mule Deer") %>%
    filter(!grepl("Moultrie", CameraLocation))
  # write.csv(Deer, paste0('G:/My Drive/1 Volunteers/Capstone Projects/2021-2022/Sophia/Deer_detections_', Sys.Date(), '.csv'))
  #'  Human detections for Sophia (Aug 1 - Jan 31)
  FallHumans <- full_dat %>%
    filter(Human == "TRUE" | Vehicle == "TRUE") %>%
    filter(!grepl("Moultrie", CameraLocation)) %>%
    mutate(
      Month = strftime(Date, "%m")
    ) %>%
    filter(Month == "01" | Month == "08" | Month == "09" | Month == "10" | Month == "11" | Month == "12") %>%
    dplyr::select(-c(Distance_Focal_Point, Height_frm_grnd, Monitoring, Canopy_Cov, Land_Mgnt, Land_Owner, Habitat_Type, Month))
  # write.csv(FallHumans, paste0('G:/My Drive/1 Volunteers/Capstone Projects/2021-2022/Sophia/FallHuman_detections_', Sys.Date(), '.csv'))
  
  #'  Human detections for Cameron (Aug 1 - Jan 31)
  FallHumans <- full_dat %>%
    filter(Human == "TRUE" | Vehicle == "TRUE") %>%
    filter(!grepl("Moultrie", CameraLocation)) %>%
    mutate(
      Month = strftime(Date, "%m")
    ) %>%
    filter(Month == "01" | Month == "08" | Month == "09" | Month == "10" | Month == "11" | Month == "12")
  # write.csv(FallHumans, paste0('G:/My Drive/1 Volunteers/Side projects for interns/Hunter-Cattle-Activity/FallHuman_detections_', Sys.Date(), '.csv'))
  
  #'  Multi-species detections for Celine (need to pick species and narrow time period)
  MultiSppDetections <- full_dat %>%
    filter(Animal == "true" | Animal == "TRUE")
  # write.csv(MultiSppDetections, paste0('G:/My Drive/1 Volunteers/Capstone Projects/2021-2022/Celine/MultiSpp_detections_', Sys.Date(), '.csv'))
  
  #'  Deer detections for Hunter (Aug 1 - Jan 31)
  FallDeer <- full_dat %>%
    filter(Species == "White-tailed Deer" | Species == "Mule Deer") %>%
    filter(!grepl("Moultrie", CameraLocation)) %>%
    mutate(
      Month = strftime(Date, "%m")
    ) %>%
    filter(Month == "01" | Month == "08" | Month == "09" | Month == "10" | Month == "11" | Month == "12") %>%
    dplyr::select(-c(Distance_Focal_Point, Height_frm_grnd, Monitoring, Canopy_Cov, Land_Mgnt, Land_Owner, Habitat_Type, Month))
  #'  Human detections for Hunter (Aug 1 - Jan 31)
  FallHumans <- full_dat %>%
    filter(Human == "TRUE" | Vehicle == "TRUE") %>%
    filter(!grepl("Moultrie", CameraLocation)) %>%
    mutate(
      Month = strftime(Date, "%m")
    ) %>%
    filter(Month == "01" | Month == "08" | Month == "09" | Month == "10" | Month == "11" | Month == "12") %>%
    dplyr::select(-c(Distance_Focal_Point, Height_frm_grnd, Monitoring, Canopy_Cov, Land_Mgnt, Land_Owner, Habitat_Type, Month))
  Hunter_data <- rbind(FallDeer, FallHumans)
  # write.csv(Hunter_data, paste0('G:/My Drive/1 Volunteers/Capstone Projects/2021-2022/Hunter/Hunter_DetectionData_', Sys.Date(), '.csv'))
  

  #'  Extract independent detections
  #'  Create a column identifying whether each image is an "independent" event
  #'  If camera site is diff from previous row then give unique value. If not then...
  #'  If species detected is diff from previous row at same site then give unique value. If not then...
  #'  If DateTime is >30 min from previous DateTime at same site for same species then give unique value. If not then...
  #'  Capture value is the same as that in the previous row.
  dat <- full_dat %>%
    #'  Remove all Service and Empty images for this to work
    filter(!is.na(Species)) %>%
    arrange(CameraLocation, DateTime)
  caps <- c()
  caps[1] <- 1
  for (i in 2:nrow(dat)){
    if (dat$CameraLocation[i-1] != dat$CameraLocation[i]) caps[i] = i
    else (if (dat$Species[i-1] != dat$Species[i]) caps[i] = i
          else (if (difftime(dat$DateTime[i], dat$DateTime[i-1], units = c("mins")) > 30) caps[i] = i
                else caps[i] = caps[i-1]))
  }
  
  caps <- as.factor(caps)
  
  #'  Add new column to larger data set
  capdata <- cbind(as.data.frame(dat), caps)
  
  #'  Retain only the first image from each unique detection event  
  detections <- capdata %>% 
    group_by(caps) %>% 
    slice(1L) %>%
    ungroup()
  
  #'  Moose detections for WDFW
  NEmoose <- detections %>%
    filter(Species == "Moose") %>%
    filter(str_detect(CameraLocation, paste("OK"), negate = TRUE)) %>%
    dplyr::select(-caps)
  # write.csv(NEmoose, paste0('./Output/NEMoose_indcaps_', Sys.Date(), '.csv'))
  OKmoose <- detections %>%
    filter(Species == "Moose") %>%
    filter(str_detect(CameraLocation, paste("NE"), negate = TRUE)) %>%
    dplyr::select(-caps)
  # write.csv(NEmoose, paste0('./Output/OKMoose_indcaps_', Sys.Date(), '.csv'))
  
  #'  Cougar detections for WDFW
  Cougs <- detections %>%
    filter(Species == "Cougar") %>%
    dplyr::select(-caps)
  # write.csv(Cougs, paste0('./Output/Cougar_inddet_', Sys.Date(), '.csv'))
  
  #'  Wolf detections for Lisanne
  wolf <- detections %>%
    filter(Species == "Wolf") %>%
    filter(Year == "Year2") %>%
    # filter(str_detect(CameraLocation, paste("NE"), negate = TRUE)) %>%
    dplyr::select(-caps)
  # write.csv(wolf, paste0('./Output/wolf_inddetYr2_', Sys.Date(), '.csv'))
  
  #'  =============================================
  #'  Make the species detection data spatial based on CameraLocation lat/long
  #'  Load required packages
  require(sf)
  require(ggplot2)
  #'  Define coordinate projection
  wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  #'  Read in and transform study area shapefiles
  OK_SA <- st_read("./Shapefiles/fwdstudyareamaps", layer = "METHOW_SA")
  NE_SA <- st_read("./Shapefiles/fwdstudyareamaps", layer = "NE_SA")
  OK_wgs84 <- st_transform(OK_SA, wgs84)
  NE_wgs84 <- st_transform(NE_SA, wgs84)
  
  #'  Create spatial objects from detection data
  cams <- st_as_sf(deployed[,1:5], coords = c("Longitude", "Latitude"), crs = wgs84)
  NEcams <- cams[grepl("NE", cams$CameraLocation),]
  OKcams <- cams[grepl("OK", cams$CameraLocation),]
  
  Yr1dat <- st_as_sf(full_dat, coords = c("Camera_Long", "Camera_Lat"), crs = wgs84)
  Yr1spp <- group_split(Yr1dat, Yr1dat$Species)
  Yr1wolf <- Yr1dat[Yr1dat$Species == "Wolf",]
  Yr1bear <- Yr1dat[Yr1dat$Species == "Black Bear",]
  Yr1coug <- Yr1dat[Yr1dat$Species == "Cougar",]
  Yr1cougcoll <- Yr1coug[Yr1coug$Collar > 0,]
  Yr1elk <- Yr1dat[Yr1dat$Species == "Elk",]
  Yr1moose <- Yr1dat[Yr1dat$Species == "Moose",]
  Yr1md <- Yr1dat[Yr1dat$Species == "Mule Deer",]
  Yr1wtd <- Yr1dat[Yr1dat$Species == "White-tailed Deer",]
  Yr1cow <- Yr1dat[Yr1dat$Species == "Cattle",]
  NEYr1moose <- st_as_sf(NEmoose, coords = c("Camera_Long", "Camera_Lat"), crs = wgs84)
  OKYr1moose <- st_as_sf(OKmoose, coords = c("Camera_Long", "Camera_Lat"), crs = wgs84)
  Yr1wolf <- st_as_sf(wolf, coords = c("Camera_Long", "Camera_Lat"), crs = wgs84)
  Yr1coug <- st_as_sf(Cougs, coords = c("Camera_Long", "Camera_Lat"), crs = wgs84)
  
  
  ggplot() +
    geom_sf(data = NE_wgs84, fill = NA) +
    geom_sf(data = OK_wgs84, fill = NA) +
    geom_sf(data = Yr1spp[[1]])
  ggplot() +
    geom_sf(data = NE_wgs84, fill = NA) +
    geom_sf(data = OK_wgs84, fill = NA) +
    geom_sf(data = Yr1elk)
  ggplot() +
    geom_sf(data = NE_wgs84, fill = NA) +
    geom_sf(data = OK_wgs84, fill = NA) +
    geom_sf(data = Yr1coug)
  ggplot() +
    geom_sf(data = NE_wgs84, fill = NA) +
    geom_sf(data = OK_wgs84, fill = NA) +
    geom_sf(data = Yr1coug, col = "blue") +
    geom_sf(data = Yr1cougcoll, col = "orange") + 
    ggtitle("Cams with cougar and collared cougar detections") +
    labs(x = "Longitude", y = "Latitude")
  ggplot() +
    geom_sf(data = NE_wgs84, fill = NA) +
    geom_sf(data = OK_wgs84, fill = NA) +
    geom_sf(data = Yr1cow, col = "blue") +
    ggtitle("Cams with Cattle Detections") +
    labs(x = "Longitude", y = "Latitude")
  
  pdf(file = "./Output/wolf_camera_detection_18-19.pdf")
  ggplot() +
    geom_sf(data = NE_wgs84, fill = NA) +
    geom_sf(data = OK_wgs84, fill = NA) +
    geom_sf(data = Yr1wolf)
  dev.off()
  
  
  #'  Plot mule deer vs white-taile deer detections
  ggplot() +
    geom_sf(data = OK_wgs84, fill = NA) +
    geom_sf(data = NE_wgs84, fill = NA) +
    geom_sf(data = Yr1wtd, shape  = 23, size = 3, fill = "darkred") +
    geom_sf(data = Yr1md, shape = 8, size = 2) 

  #'  Plot NE moose detections
  #'  Count the number of independent detections per camera station
  NEYr1moose <- NEYr1moose %>%
    group_by(CameraLocation) %>%
    summarise(count = n()) %>%
    ungroup()
  OKYr1moose <- OKYr1moose %>%
    group_by(CameraLocation) %>%
    summarise(count = n()) %>%
    ungroup()
  Yr1wolf <- Yr1wolf %>%
    group_by(CameraLocation) %>%
    summarise(count = n()) %>%
    ungroup()
  Yr1coug <- Yr1coug %>%
    group_by(CameraLocation) %>%
    summarise(count = n()) %>%
    ungroup()
  #'  Plot moose detections based on the number of independent detections/camera
  pdf(file = "./Output/NE_moose_camera_detection.pdf")
  ggplot() +
    geom_sf(data = NE_wgs84, fill = NA) +
    geom_sf(data = NEcams, shape = 1, aes(fill = "A"), show.legend = "point") + 
    geom_sf(data = NEYr1moose, aes(size = count), shape  = 21, fill = "darkred") + 
    scale_fill_manual(values = c("A" = "transparent"),
                      labels = c("Camera traps"), name = "Legend") +
    labs(size = "Independent \ndetections") +
    labs(x = "Longitude", y = "Latitude") +
    ggtitle("WPPP Camera Trap Moose Detections \n(2018 - 2019)") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  dev.off()
  
  pdf(file = "./Output/wolf_camera_detections_18-19.pdf")
  ggplot() +
    geom_sf(data = OK_wgs84, fill = NA) +
    geom_sf(data = NE_wgs84, fill = NA) +
    geom_sf(data = OKcams, shape = 1, aes(fill = "A"), show.legend = "point") + 
    geom_sf(data = NEcams, shape = 1, aes(fill = "A"), show.legend = "point") + 
    # geom_sf(data = OKYr1moose, aes(size = count), shape  = 21, fill = "darkred") + 
    # geom_sf(data = NEYr1moose, aes(size = count), shape  = 21, fill = "darkred") +
    geom_sf(data = Yr1wolf, aes(size = count), shape = 21, fill = "orange") +
    scale_fill_manual(values = c("A" = "transparent"),
                      labels = c("Camera traps"), name = "") +
    labs(size = "Independent \ndetections") +
    labs(x = "Longitude", y = "Latitude") +
    ggtitle("WPPP Camera Trap Wolf Detections \n(2018 - 2019)") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  dev.off()
  
  pdf(file = "./Output/cougar_camera_detections_18-19.pdf")
  ggplot() +
    geom_sf(data = OK_wgs84, fill = NA) +
    geom_sf(data = NE_wgs84, fill = NA) +
    geom_sf(data = OKcams, shape = 1, aes(fill = "A"), show.legend = "point") + 
    geom_sf(data = NEcams, shape = 1, aes(fill = "A"), show.legend = "point") + 
    geom_sf(data = Yr1coug, aes(size = count), shape = 21, fill = "orange") +
    scale_fill_manual(values = c("A" = "transparent"),
                      labels = c("Camera traps"), name = "") +
    labs(size = "Independent \ndetections") +
    labs(x = "Longitude", y = "Latitude") +
    ggtitle("WPPP Camera Trap Cougar Detections \n(2018 - 2019)") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  dev.off()
    
  #' #'  More basic plot
  #' ggplot() +
  #'   geom_sf(data = NE_wgs84, fill = NA) +
  #'   geom_sf(data = NEcams, aes(fill = "A"), size = 3, shape = 1, show.legend = "point") + 
  #'   geom_sf(data = NEYr1moose, aes(fill = "B"), size = 3, shape  = 21, show.legend = "point") + 
  #'   scale_fill_manual(values = c("A" = "transparent", "B" = "darkred"),
  #'                     labels = c("Camera Traps", "Moose Detections"), name = "") +
  #'   labs(x = "Longitude", y = "Latitude") +
  #'   ggtitle("WPPP Camera Trap Moose Detections \n(2018 - 2019)") +
  #'   theme_classic() +
  #'   theme(legend.position = c(0.875, 0.9)) +
  #'   theme(plot.title = element_text(hjust = 0.5)) 
    

  
  
  

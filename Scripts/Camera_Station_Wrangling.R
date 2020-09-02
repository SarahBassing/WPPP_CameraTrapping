  ##  Clean camera deployment & checking data from the field
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2019
  ##  ================================
  ##  Organize camera deployment and checking data. Must have created separate csv
  ##  files of the camera deployment and check information and added a column for
  ##  which year the camera was deployed in.
  
  #  Load packages
  library(tidyverse)
  
  #  Read in & select relevant data
  #  2018-2019 data
  cameras18 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/Camera_Deployment_2018.csv")) %>%
    dplyr::select("Date", "Study_area", "Year",
                "Cell_No", "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", "Make",
                "Dist_to_road_m", "Height_from_ground_m", "Feature_monitored",
                "Azimuth", "Slope", "Aspect",
                "Canopy_Cov_percent", "Land_mgnt", "Habitat_type") %>%
    dplyr::filter(Make != "Moultrie") %>%  # excluding Moultrie data b/c its crap # == "Reconyx"
    dplyr::filter(Cell_No != "OKbonus")   # not an official study camera
  colnames(cameras18) <- c("Date", "Study_Area", "Year", "Cell_ID", 
                         "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", "Make",
                         "Distance_Focal_Point", "Height_frm_grnd", "Monitoring",
                         "Azimuth", "Slope", "Aspect", "Canopy_Cov", "Land_Mgnt", "Habitat_Type")
  
  ncams18 <- nrow(cameras18) 
  deployed18 <- as.data.frame(rep("Deployed", ncams18))
  colnames(deployed18) <- "Status"
  cams18 <- cbind(deployed18, cameras18)
  
  #  2019-2020 data
  cameras19 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/AudioMoth_and_Camera_Depoyment_2019.csv")) %>%
    dplyr::select("Date", "Study_Area",  "Cell_ID", "Cam_ID", "Memory_Card", 
                  "Cam_Lat", "Cam_Long", "Cam_Type", 
                  "Cam_Distance_Focal_Point", "Cam_Distance_Ground", "Monitoring", 
                  "Cam_Azimuth", "Cam_Slope", "Cam_Aspect", 
                  "Cam_Canopy_Cover", "Land_Type", "Habitat_Type")
    colnames(cameras19) <- c("Date", "Study_Area", "Cell_ID", 
                           "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", "Make",
                           "Distance_Focal_Point", "Height_frm_grnd", "Monitoring",
                           "Azimuth", "Slope", "Aspect", "Canopy_Cov", "Land_Mgnt", "Habitat_Type")
                           
    ncams19 <- nrow(cameras19) 
    Year <- rep("Year2", ncams19)
    deployed19 <- as.data.frame(rep("Deployed", ncams19))
    colnames(deployed19) <- "Status"
    cams19 <- cbind(deployed19, Year, cameras19)
    
  
  #  Checking 2018 cameras in summer 2018  
  checks18 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/Camera_Checking_2018.csv")) %>%
    dplyr::select("Date", "Study_area", "Year", "Cell_No",
                  "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", "Make",
                  "Condition", "How_damaged", "Cam_replaced", "No_Images",
                  "Adj_to_placement", "Why1",
                  "New_location", "New_Cam_Lat", "New_Cam_Long",
                  "Cam_removed", "Why2") %>%
    dplyr::filter(Make != "Moultrie") %>% # most Moultrie pulled by 6/3 but two were pulled 6/30 #== "Reconyx"
    dplyr::filter(Cell_No != "OKbonus")
  colnames(checks18) <- c("Date", "Study_Area", "Year", "Cell_ID",
                          "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", "Make",
                          "Condition", "Explain1", "Cam_Replaced", "Num_Images",
                          "Adjustments", "Explain2", 
                          "New_Location", "New_Lat", "New_Long",
                          "Cam_Removed", "Explain3")
  nchks18 <- nrow(checks18) 
  checked18 <- as.data.frame(rep("Checked", nchks18))
  colnames(checked18) <- "Status"
  chks18 <- cbind(checked18, checks18)
  
  #  Pulling summer 2018 cameras in summer 2019
  checks18_19 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/Camera_Checking_2019.csv")) %>%
    dplyr::select("Date", "Study_Area", "Year", "Cell_ID", 
                  "Cam_ID", "Cam_Card", "Cam_Lat", "Cam_Long", 
                  "Cam_Condition", "Explain1", "Cam_Replaced", "Num_Images",
                  "Adjustments", "Explain2", 
                  "New_Location", "New_Lat", "New_Long",
                  "Cam_Removed", "Explain3", 
                  "Cam_Azimuth", "Cam_Slope", "Cam_Aspect") %>%
    dplyr::filter(Year == "Year1")
  colnames(checks18_19) <- c("Date", "Study_Area", "Year", "Cell_ID",
                          "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long",
                          "Condition", "Explain1", "Cam_Replaced", "Num_Images",
                          "Adjustments", "Explain2",
                          "New_Location", "New_Lat", "New_Long",
                          "Cam_Removed", "Explain3",
                          "Azimuth", "Slope", "Aspect")
  nchks18_19 <- nrow(checks18_19) 
  checked18_19 <- as.data.frame(rep("Removed", nchks18_19))
  colnames(checked18_19) <- "Status"
  chks18_19 <- cbind(checked18_19, checks18_19)
  
  #  Checking 2019 cameras in summer 2019
  checks19 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/Camera_Checking_2019.csv")) %>%
    dplyr::select("Date", "Study_Area", "Year", "Cell_ID", 
                  "Cam_ID", "Cam_Card", "Cam_Lat", "Cam_Long", 
                  "Cam_Condition", "Explain1", "Cam_Replaced", "Num_Images",
                  "Adjustments", "Explain2", 
                  "New_Location", "New_Lat", "New_Long",
                  "Cam_Removed", "Explain3", 
                  "Cam_Azimuth", "Cam_Slope", "Cam_Aspect") %>%
    dplyr::filter(Year == "Year2")
  colnames(checks19) <- c("Date", "Study_Area", "Year", "Cell_ID",
                             "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long",
                             "Condition", "Explain1", "Cam_Replaced", "Num_Images",
                             "Adjustments", "Explain2",
                             "New_Location", "New_Lat", "New_Long",
                             "Cam_Removed", "Explain3",
                             "Azimuth", "Slope", "Aspect")
  nchks19 <- nrow(checks19) 
  checked19 <- as.data.frame(rep("Checked", nchks19))
  colnames(checked19) <- "Status"
  chks19 <- cbind(checked19, checks19)
  
  
  
  #  Slim this down for now
  cameras18_slim <- dplyr::select(cams18, c("Status", "Date", "Study_Area", "Cell_ID", 
                                   "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long",
                                   "Distance_Focal_Point", "Height_frm_grnd", "Monitoring",
                                   "Canopy_Cov", "Land_Mgnt", "Habitat_Type"))
  cameras19_slim <- dplyr::select(cams19, c("Status", "Date", "Study_Area", "Cell_ID", 
                                        "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long",
                                        "Distance_Focal_Point", "Height_frm_grnd", "Monitoring",
                                        "Canopy_Cov", "Land_Mgnt", "Habitat_Type"))
  checks18_slim <- dplyr::select(chks18, c("Status", "Date", "Study_Area", "Cell_ID", 
                                      "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", 
                                      "Condition", "Explain1", "Num_Images", 
                                      "Adjustments", "Explain2", "Cam_Removed"))
  checks18_19_slim <- dplyr::select(chks18_19, c("Status", "Date", "Study_Area", "Cell_ID", 
                                           "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", 
                                           "Condition", "Explain1", "Num_Images", 
                                           "Adjustments", "Explain2", "Cam_Removed"))
  checks19_slim <- dplyr::select(chks19, c("Status", "Date", "Study_Area", "Cell_ID",  
                                      "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", 
                                      "Condition", "Explain1", "Num_Images",
                                      "Adjustments", "Explain2","Cam_Removed"))
  
  #  Bind deploy and check dataframes together into single file
  camera_master_2018 <- bind_rows(cameras18_slim, checks18_slim, checks18_19_slim)
  camera_master_2019 <- bind_rows(cameras19_slim, checks19_slim) # add more data once cameras are pulled
  
  ## ========================================
  #  Save these master files
  # write.csv(camera_master_2018, file = "G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2018_updated.5.1.20.csv")
  # write.csv(camera_master_2019, file = "G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2019_updated.5.1.20.csv")
  ## ========================================
  
  
  #  Combine auxiliary camera station data from deployments, checks, removals
  #  to be used as covariates for future analyses
  
  master18_19 <- camera_master_2018 %>%
    #  Add a column merging cell & camera, format date
    mutate(
      Name = paste(Cell_ID, "_", Camera_ID),
      Date = as.Date(Date, format = "%m/%d/%Y")
    ) %>%
    #  Remove cameras that were deployed but never collected data (BURNED) 
    filter(Name != "OK1474 _ 104" & Name != "OK2051 _ 98" & Name != "OK6270 _ 109")
  
  master19_20 <- camera_master_2019 %>% 
    #  Add a column merging cell & camera, format date
    mutate(
      Name = paste(Cell_ID, "_", Camera_ID),
      Date = as.Date(Date, format = "%m/%d/%Y")
    )
    
  #  Merge & rearrange so ordered by camera location, then deployment-check-removal date
  all_cams <- rbind(master18_19, master19_20) %>%
    arrange(Name, Date)
  
  #  Save to create "problems" csv and camera deployment covariates csv
  #  Problem dates will have to be extracted from image data (last image taken)
  #  and csv organized by hand in excel
  #  Camera deployment covariates will be propogated to relevant rows by hand in excel
  write.csv(all_cams, file = "G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2018-2020.csv")
  
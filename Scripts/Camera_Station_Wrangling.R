  ##  Clean camera deployment & checking data from the field
  ##  Sarah Bassing
  ##  Washington Predator-Prey Project
  ##  Sept. 2019
  ##  ======================================================
  ##  Organize camera deployment and checking data. Must have created separate csv
  ##  files of the camera deployment and check information and added a column for
  ##  which year the camera was deployed in. Use to gather all site-specific
  ##  location and deployment data. Final exports are used to generate "problems"
  ##  data file for camtrapR to use when making encounter histories.
  ##  ======================================================
  
  #  Load packages
  library(tidyverse)
  
  #  Read in & select relevant data               
  #  2018-2019 data
  cameras18 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/Camera_Deployment_2018.csv")) %>%
    dplyr::select("Date", "Study_area", "Year",
                "Cell_No", "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", "Make",
                "Dist_to_road_m", "Height_from_ground_m", "Feature_monitored",
                "Azimuth", "Slope", "Aspect",
                "Canopy_Cov_percent", "Land_mgnt", "Name", "Habitat_type") %>%
    dplyr::filter(Make != "Moultrie") %>%     # excluding Moultrie data b/c its crap # == "Reconyx"
    dplyr::filter(Cell_No != "OKbonus") %>%   # not an official study camera
    mutate(
      Study_area = as.factor(as.character((Study_area))),
      Camera_ID = as.factor(as.character((Camera_ID))))
  colnames(cameras18) <- c("Date", "Study_Area", "Year", "Cell_ID", 
                         "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", "Make",
                         "Distance_Focal_Point", "Height_frm_grnd", "Monitoring",
                         "Azimuth", "Slope", "Aspect", "Canopy_Cov", "Land_Mgnt", 
                         "Land_Owner", "Habitat_Type")
  deployed18 <- as.data.frame(rep("Deployed", nrow(cameras18)))
  colnames(deployed18) <- "Status"
  cams18 <- cbind(deployed18, cameras18)
  
  #  2019-2020 data
  cameras19 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/AudioMoth_and_Camera_Depoyment_2019.csv")) %>%
    dplyr::select("Date", "Study_Area",  "Cell_ID", "Cam_ID", "Memory_Card", 
                  "Cam_Lat", "Cam_Long", "Cam_Type", 
                  "Cam_Distance_Focal_Point", "Cam_Distance_Ground", "Monitoring", 
                  "Cam_Azimuth", "Cam_Slope", "Cam_Aspect", 
                  "Cam_Canopy_Cover", "Land_Type", "Land_Management", "Habitat_Type") %>%
    mutate(
      Study_Area = as.factor(as.character((Study_Area))),
      Cam_ID = as.factor(as.character((Cam_ID))))
    colnames(cameras19) <- c("Date", "Study_Area", "Cell_ID", 
                           "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", "Make",
                           "Distance_Focal_Point", "Height_frm_grnd", "Monitoring",
                           "Azimuth", "Slope", "Aspect", "Canopy_Cov", "Land_Mgnt", 
                           "Land_Owner", "Habitat_Type")
    Year <- rep("Year2", nrow(cameras19))
    deployed19 <- as.data.frame(rep("Deployed", nrow(cameras19)))
    colnames(deployed19) <- "Status"
    cams19 <- cbind(deployed19, Year, cameras19)
    
    #  2020-2021 data
    cameras20 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/AudioMoth_and_Camera_Deployment_2020_100520.csv")) %>%
      dplyr::select("Date", "Study_Area",  "Cell_ID", "Cam_ID", "Memory_Card", 
                    "Cam_Lat", "Cam_Long",  
                    "Cam_Distance_Focal_Point", "Cam_Distance_Ground", 
                    "Cam_Width_of_feature", "Monitoring", 
                    "Cam_Azimuth", "Cam_Slope", "Cam_Aspect", 
                    "Cam_Canopy_Cover", "Land_Type", "Land_Management", "Habitat_Type") %>%
      mutate(
        Study_Area = as.factor(as.character((Study_Area))),
        Cam_ID = as.factor(as.character((Cam_ID))))
    colnames(cameras20) <- c("Date", "Study_Area", "Cell_ID", 
                             "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", 
                             "Distance_Focal_Point", "Height_frm_grnd", 
                             "Trail_Width", "Monitoring",
                             "Azimuth", "Slope", "Aspect", "Canopy_Cov", "Land_Mgnt",
                             "Land_Owner", "Habitat_Type")
    Year <- rep("Year3", nrow(cameras20))
    deployed20 <- as.data.frame(rep("Deployed", nrow(cameras20)))
    colnames(deployed20) <- "Status"
    cams20 <- cbind(deployed20, Year, cameras20)
    
    
  
  #  Checking Year 1 (summer 2018) cameras in summer 2018  
  checks18 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/Camera_Checking_2018.csv")) %>%
    dplyr::select("Date", "Study_area", "Year", "Cell_No",
                  "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", "Make",
                  "Condition", "How_damaged", "Cam_replaced", "No_Images",
                  "Adj_to_placement", "Why1",
                  "New_location", "New_Cam_Lat", "New_Cam_Long",
                  "Cam_removed", "Why2") %>%
    dplyr::filter(Make != "Moultrie") %>% # most Moultrie pulled by 6/3 but two were pulled 6/30 #== "Reconyx"
    dplyr::filter(Cell_No != "OKbonus") %>%
    mutate(Camera_ID = as.factor(as.character((Camera_ID))))
  colnames(checks18) <- c("Date", "Study_Area", "Year", "Cell_ID",
                          "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", "Make",
                          "Condition", "Explain1", "Cam_Replaced", "Num_Images",
                          "Adjustments", "Explain2", 
                          "New_Location", "New_Lat", "New_Long",
                          "Cam_Removed", "Explain3")
  checked18 <- as.data.frame(rep("Checked", nrow(checks18)))
  colnames(checked18) <- "Status"
  chks18 <- cbind(checked18, checks18)
  
  #  Pulling Year 1 (summer 2018) cameras in summer 2019
  summchecks19 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/Camera_Checking_2019.csv")) %>%
    dplyr::select("Date", "Study_Area", "Year", "Cell_ID", 
                  "Cam_ID", "Cam_Card", "Cam_Lat", "Cam_Long", 
                  "Cam_Condition", "Explain1", "Cam_Replaced", "Num_Images",
                  "Adjustments", "Explain2", 
                  "New_Location", "New_Lat", "New_Long",
                  "Cam_Removed", "Explain3", 
                  "Cam_Azimuth", "Cam_Slope", "Cam_Aspect") %>%
    mutate(Cam_ID = as.factor(as.character((Cam_ID))))
  colnames(summchecks19) <- c("Date", "Study_Area", "Year", "Cell_ID",
                          "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long",
                          "Condition", "Explain1", "Cam_Replaced", "Num_Images",
                          "Adjustments", "Explain2",
                          "New_Location", "New_Lat", "New_Long",
                          "Cam_Removed", "Explain3",
                          "Azimuth", "Slope", "Aspect")
  checks18_19 <- dplyr::filter(summchecks19, Year == "Year1")
  checked18_19 <- as.data.frame(rep("Removed", nrow(checks18_19)))
  colnames(checked18_19) <- "Status"
  chks18_19 <- cbind(checked18_19, checks18_19)
  
  #  Checking Year 2 (summer 2019) cameras in summer 2019
  checks19 <- dplyr::filter(summchecks19, Year == "Year2")
  checked19 <- as.data.frame(rep("Checked", nrow(checks19)))
  colnames(checked19) <- "Status"
  chks19 <- cbind(checked19, checks19)
  
  #  Pulling Year 2 (summer 2019) cameras in summer 2020
  summchecks20 <- as.data.frame(read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/AudioMoth_and_Camera_Checking_2020_100520.csv")) %>%
    dplyr::select("Date", "Study_Area", "Cell_ID",  
                  "Cam_ID", "Cam_Card", "Cam_Lat", "Cam_Long", 
                  "Cam_Condition", "Explain1", "Cam_Replaced", "Num_Images",
                  "Adjustments", "Explain2", 
                  "New_Location", "New_Lat", "New_Long",
                  "Cam_Removed", "Explain3", "Cam_Width_of_feature") %>%
    mutate(Year = ifelse(Cam_Removed == "Y", "Year2", "Year3"),
           Cam_ID = as.factor(as.character((Cam_ID))))
  colnames(summchecks20) <- c("Date", "Study_Area", "Cell_ID", # "Year",
                          "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long",
                          "Condition", "Explain1", "Cam_Replaced", "Num_Images",
                          "Adjustments", "Explain2",
                          "New_Location", "New_Lat", "New_Long",
                          "Cam_Removed", "Explain3", "Trail_Width", "Year")
  #  Identify & change Year designation for OK3940_47 
  #  Trent went to pull but didn't have the right key
  #  Cam_Removed makes it look like a Year3 camera with the above code
  dim(summchecks20)
  which(summchecks20$Cell_ID == "OK3940" & summchecks20$Cam_Removed == "N")
  summchecks20$Year[summchecks20$Cell_ID == "OK3940"] <- "Year2"
  #  Reorder so Year column is in correct spot
  summchecks20 <- dplyr::select(summchecks20, "Date", "Study_Area", "Year", "Cell_ID",
                            "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long",
                            "Condition", "Explain1", "Cam_Replaced", "Num_Images",
                            "Adjustments", "Explain2",
                            "New_Location", "New_Lat", "New_Long",
                            "Cam_Removed", "Explain3", "Trail_Width")
  checks19_20 <- dplyr::filter(summchecks20, Year == "Year2")
  checked19_20 <- as.data.frame(rep("Removed", nrow(checks19_20)))
  colnames(checked19_20) <- "Status"
  chks19_20 <- cbind(checked19_20, checks19_20)
  
  #  Checking Year 3 (summer 2020) cameras in summer 2020
  checks20 <- dplyr::filter(summchecks20, Year == "Year3")
  checked20 <- as.data.frame(rep("Checked", nrow(checks20)))
  colnames(checked20) <- "Status"
  chks20 <- cbind(checked20, checks20)
  
  
  
  #  Slim this down for now
  cameras18_slim <- dplyr::select(cams18, c("Status", "Year", "Date", "Study_Area", "Cell_ID", 
                                        "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long",
                                        "Distance_Focal_Point", "Height_frm_grnd", "Monitoring",
                                        "Canopy_Cov", "Land_Mgnt", "Land_Owner", "Habitat_Type"))
  cameras19_slim <- dplyr::select(cams19, c("Status", "Year", "Date", "Study_Area", "Cell_ID", 
                                        "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long",
                                        "Distance_Focal_Point", "Height_frm_grnd", "Monitoring",
                                        "Canopy_Cov", "Land_Mgnt", "Land_Owner", "Habitat_Type"))
  cameras20_slim <- dplyr::select(cams20, c("Status", "Year", "Date", "Study_Area", "Cell_ID", 
                                            "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long",
                                            "Distance_Focal_Point", "Height_frm_grnd", "Trail_Width", 
                                            "Monitoring","Canopy_Cov", "Land_Mgnt", "Land_Owner", 
                                            "Habitat_Type"))
  checks18_slim <- dplyr::select(chks18, c("Status", "Year", "Date", "Study_Area", "Cell_ID", 
                                      "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", 
                                      "Condition", "Explain1", "Num_Images", 
                                      "Adjustments", "Explain2", "Explain3", "Cam_Removed"))
  checks18_19_slim <- dplyr::select(chks18_19, c("Status", "Year", "Date", "Study_Area", "Cell_ID", 
                                           "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", 
                                           "Condition", "Explain1", "Num_Images", 
                                           "Adjustments", "Explain2", "Explain3", "Cam_Removed"))
  checks19_slim <- dplyr::select(chks19, c("Status", "Year", "Date", "Study_Area", "Cell_ID",  
                                      "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", 
                                      "Condition", "Explain1", "Num_Images",
                                      "Adjustments", "Explain2", "Explain3", "Cam_Removed"))
  checks19_20_slim <- dplyr::select(chks19_20, c("Status", "Year", "Date", "Study_Area", "Cell_ID", 
                                                 "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", 
                                                 "Condition", "Explain1", "Num_Images", 
                                                 "Adjustments", "Explain2", "Explain3", 
                                                 "Cam_Removed", "Trail_Width"))
  checks20_slim <- dplyr::select(chks20, c("Status", "Year", "Date", "Study_Area", "Cell_ID",  
                                           "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long", 
                                           "Condition", "Explain1", "Num_Images",
                                           "Adjustments", "Explain2", "Explain3", "Cam_Removed"))
  
  
  
  #  Bind deploy and check dataframes together into single file
  camera_master_2018 <- bind_rows(cameras18_slim, checks18_slim, checks18_19_slim) %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    arrange(Cell_ID, Date)
  camera_master_2019 <- bind_rows(cameras19_slim, checks19_slim, checks19_20_slim) %>%
    #  Reogranize columns so trail width is with other measurement data
    dplyr::select("Status", "Year", "Date", "Study_Area", "Cell_ID", 
                  "Camera_ID", "Card_No", "Camera_Lat", "Camera_Long",
                  "Distance_Focal_Point", "Height_frm_grnd", "Trail_Width", 
                  "Monitoring","Canopy_Cov", "Land_Mgnt", "Land_Owner", 
                  "Habitat_Type", "Condition", "Explain1", "Num_Images",
                  "Adjustments", "Explain2", "Explain3", "Cam_Removed") %>%
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    arrange(Cell_ID, Date)
  camera_master_2020 <- bind_rows(cameras20_slim, checks20_slim) %>%  #  eventually create and add checks20_21_slim
    mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    arrange(Cell_ID, Date)
    
  ## ========================================
  #  Save these master files
  # write.csv(camera_master_2018, file = "G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2018_updated.9.25.20.csv")
  # write.csv(camera_master_2019, file = "G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2019_updated.9.25.20.csv")
  # write.csv(camera_master_2020, file = "G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2020_updated.9.25.20.csv")
  ## ========================================
  
  
  #  Combine auxiliary camera station data from deployments, checks, removals
  #  to be used as covariates for future analyses
  
  master18_19 <- camera_master_2018 %>%
    #  Concatenates cell & camera IDs and remove spaces during concatenate process
    mutate(
      Name = paste(Cell_ID, "_", Camera_ID),
      Name = gsub("[[:space:]]", "", Name),
      #  Create date format
      Date = as.Date(Date, format = "%m/%d/%Y"),
      #  Need to add this column to it matches with later years
      Trail_Width = "NA"
    ) %>%
    #  Remove cameras that were deployed but never collected data (BURNED) 
    filter(Name != "OK1474_104" & Name != "OK2051_98" & Name != "OK6270_109") %>%
    dplyr::select("Status", "Year", "Date", "Study_Area", "Cell_ID", "Camera_ID",
                  "Card_No", "Camera_Lat", "Camera_Long", "Distance_Focal_Point",
                  "Height_frm_grnd", "Trail_Width", "Monitoring", "Canopy_Cov", 
                  "Land_Mgnt", "Land_Owner", "Habitat_Type", "Condition", "Explain1",
                  "Num_Images", "Adjustments", "Explain2", "Explain3", "Cam_Removed",
                  "Name")
  
  master19_20 <- camera_master_2019 %>% 
    mutate(
      Name = paste(Cell_ID, "_", Camera_ID),
      Name = gsub("[[:space:]]", "", Name),
      Date = as.Date(Date, format = "%m/%d/%Y")
    )
  
  master20_21 <- camera_master_2020 %>%
    mutate(
      Name = paste(Cell_ID, "_", Camera_ID),
      Name = gsub("[[:space:]]", "", Name),
      Date = as.Date(Date, format = "%m/%d/%Y")
    )
    
  #  Merge & rearrange so ordered by camera location, then deployment-check-removal date
  all_cams <- rbind(master18_19, master19_20, master20_21) %>%
    arrange(Year, Name, Date)
  
  ## ==========================================
  #  Save to create "problems" csv and camera deployment covariates csv
  #  Problem dates will have to be extracted from image data (last image taken)
  #  and csv organized by hand in excel
  #  Camera deployment covariates will be propogated to relevant rows by hand in excel
  # write.csv(all_cams, file = "G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2018-2021_updated_9.25.20.csv")
  ## =========================================
  
  
  #  Create final camera deployment covariate data set, including deployment & pull
  #  dates, dates when camera had potential issues
  #  Identify cameras that were damaged or went missing and hold out their checks
  ugh <- filter(all_cams, Condition == "Disturbed" | Condition == "Damaged" | 
                  Condition == "Disturbed/Damaged" | Condition == "Missing")
  prob_cams <- all_cams[all_cams$Cell_ID %in% ugh$Cell_ID,] %>%
    mutate(dbl_check = 1)
  #  Last date damaged/missing camera was checked or removed
  prob_last <- prob_cams %>%
    group_by(Cell_ID) %>%
    filter(Date == max(Date), na.rm = TRUE) %>%
    ungroup()
  #  Double check I didn't drop any grid cells
  length(unique(ugh$Cell_ID)); length(unique(prob_cams$Cell_ID)); length(unique(prob_last$Cell_ID))
  
  #  Subset list of cameras to only ones that were not damaged or missing
  good_cams <- all_cams[!(all_cams$Cell_ID %in% prob_cams$Cell_ID),] %>%
    mutate(dbl_check = 0)
  #  Deployment data
  first <- filter(good_cams, Status == "Deployed")
  #  Data from camera removal
  pull <- filter(good_cams, Status == "Removed")
  #  Last date camera was checked before removal 
  #  Only cameras that were not damaged or missing
  check <- good_cams %>%
    group_by(Cell_ID) %>%
    filter(Status == "Checked") %>%
    filter(Date == max(Date), na.rm = TRUE) %>%
    ungroup()
  #  Keep data from last check if no official removal data was recorded
  last <- rbind(check, pull) %>%
    arrange(Cell_ID) %>%
    group_by(Cell_ID) %>%
    filter(Date == max(Date), na.rm = TRUE) %>%
    ungroup()
  #  Merge deployment and removal data, plus all check data for problem cameras
  full <- rbind(first, last, prob_cams) %>%
    arrange(Cell_ID)
  #  Gather date of last check or removal for all cameras
  end_prob <- select(prob_last, c(Cell_ID, Camera_ID, Status, Date))
  end_good <- select(last, c(Cell_ID, Camera_ID, Status, Date))
  end <- rbind(end_prob, end_good)
  final_sites <- left_join(full, end, by = (c("Cell_ID", "Camera_ID"))) %>%
    filter(Status.x != "Removed" | dbl_check == 1)
  colnames(final_sites) <- c("Status", "Year", "Date", "Study_Area", "Cell_ID", 
                             "Camera_ID", "Card_ID", "Camera_Lat", "Camera_Long", 
                             "Distance_Focal_Point", "Height_frm_grnd", "Trail_Width",
                             "Monitoring", "Canopy_Cov", "Land_Mgnt", "Land_Owner", 
                             "Habitat_Type", "Condition", "Explain1", "Num_Images",
                             "Adjustments", "Explain2", "Explain3", "Cam_Removed",
                             "Name", "dbl_check", "Pull_Status", "Last_Date")
  #  Keep an eye out for: 
  #  1. Cameras that were removed but no pull data were recorded,
  #  2. Cameras that were redeployed part way through season,
  #  3. Memory cards that filled up before camera was checked,
  #  4. Cameras that were removed by landowner and collected by us later.
  #  Keep in mind Year 3 data are not complete
  
  #  Save
  #  Use this to eventually create "problems" All_Camera_Stations files for camtrapR
  # write.csv(final_sites, file = "G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2018-2021_updated_12.22.20_skinny.csv")
  
  
  
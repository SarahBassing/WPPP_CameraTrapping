  #'  ============================================
  #'  NARR data summaries
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  April 2021
  #'  ============================================
  #'  NARR Daily Max Precipitation (mm) and Mean Temperature (K) (32 km res)
  #'  Extracted from North American Regional Reanalysis, data produced by the 
  #'  National Centers for Environmental Prediction. Data kindly extracted by 
  #'  O.Sanderfoot. 
  #'  
  #'  Script filters data to desired data ranges and calculates weekly averages 
  #'  to match detection histories for occupancy models.
  #'  ============================================
  
  #'  Load libraries
  library(tidyverse)
  
  #'  Read in & format NARR data extracted by O.Sanderfoot
  narr <- read.csv("./Output/WPPP_weather_data_all.csv") %>%
    mutate(
      Observation_Date_Formatted = as.Date(Observation_Date_Formatted)
    ) %>%
    dplyr::select(-X)
  
  #'  Filter by date and summarize weather data by sampling occasion
  #'  Each sampling occasion is 7 days long
  narr_smr18 <- narr %>%
    filter(Observation_Date_Formatted > "2018-06-30" & Observation_Date_Formatted < "2018-09-30") %>%
    mutate(
      Week = 1 + as.numeric(Observation_Date_Formatted - as.Date("2018-07-01")) %/% 7
    ) %>%
    group_by(Camera_Location, Latitude, Longitude, Week) %>%
    summarise_at(c("Air_Temperature_K", "Precipitation_mm"), mean) %>%
    ungroup() %>%
    mutate(
      Year = "Year1",
      Season = "Summer18"
    )
  colnames(narr_smr18) <- c("CameraLocation", "Latitude", "Longitude", "Occasion", 
                            "MeanDTemp_K", "MeanDPrecip_mm", "Year", "Season")

  narr_wtr1819 <- narr %>%
    filter(Observation_Date_Formatted > "2018-11-30" & Observation_Date_Formatted < "2019-03-02") %>%
    mutate(
      Week = 1 + as.numeric(Observation_Date_Formatted - as.Date("2018-12-01")) %/% 7
    ) %>%
    group_by(Camera_Location, Latitude, Longitude, Week) %>%
    summarise_at(c("Air_Temperature_K", "Precipitation_mm"), mean) %>%
    ungroup() %>%
    mutate(
      Year = "Year1",
      Season = "Winter1819"
    )
  colnames(narr_wtr1819) <- c("CameraLocation", "Latitude", "Longitude", "Occasion", 
                            "MeanDTemp_K", "MeanDPrecip_mm", "Year", "Season")
  
  narr_smr19 <- narr %>%
    filter(Observation_Date_Formatted > "2019-06-30" & Observation_Date_Formatted < "2019-09-30") %>%
    mutate(
      Week = 1 + as.numeric(Observation_Date_Formatted - as.Date("2019-07-01")) %/% 7
    ) %>%
    group_by(Camera_Location, Latitude, Longitude, Week) %>%
    summarise_at(c("Air_Temperature_K", "Precipitation_mm"), mean) %>%
    ungroup() %>%
    mutate(
      Year = "Year2",
      Season = "Summer19"
    )
  colnames(narr_smr19) <- c("CameraLocation", "Latitude", "Longitude", "Occasion", 
                            "MeanDTemp_K", "MeanDPrecip_mm", "Year", "Season")
  
  narr_wtr1920 <- narr %>%
    filter(Observation_Date_Formatted > "2019-11-30" & Observation_Date_Formatted < "2020-03-01") %>%
    mutate(
      Week = 1 + as.numeric(Observation_Date_Formatted - as.Date("2019-12-01")) %/% 7
    ) %>%
    group_by(Camera_Location, Latitude, Longitude, Week) %>%
    summarise_at(c("Air_Temperature_K", "Precipitation_mm"), mean) %>%
    ungroup() %>%
    mutate(
      Year = "Year2",
      Season = "Winter1920"
    )
  colnames(narr_wtr1920) <- c("CameraLocation", "Latitude", "Longitude", "Occasion", 
                            "MeanDTemp_K", "MeanDPrecip_mm", "Year", "Season")

  
  #'  Combine data across seasons
  mean_narr <- rbind(narr_smr18, narr_wtr1819, narr_smr19, narr_wtr1920)

  #'  Save for future analyses
  write.csv(mean_narr, "./Output/NARR_weeklymeans_smr18-wtr20.csv")
  
    
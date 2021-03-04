  #'  ============================================
  #'  Format camera station data
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  February 2021
  #'  ============================================
  #'  Script to format camera station data, particularly the categorical
  #'  site-specific covariate data. This is super tedious but note taking in the
  #'  field was very inconsistent and verbose. Need to reduce habitat/site
  #'  descriptions down to simple categorical variables for further analyses.
  #'  
  #'  Uses:
  #'  "camera_master_2018-2021_updated_DATE_skinny.csv" from Camera_Station_Wrangling.R
  #'       This script is sourced into Detections_by_Camera_Station.R & others


  #'  Load libraries
  library(chron)  
  library(tidyverse)
  
  #'  Read in data, format, and filter
  allstations <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Data Entry/camera_master_2018-2021_updated_2020-12-22_skinny.csv") %>% 
    dplyr::select("Status", "Year", "Date", "Study_Area", "Cell_ID", "Camera_ID", "Name", 
           "Camera_Lat", "Camera_Long", "Distance_Focal_Point", "Height_frm_grnd", 
           "Monitoring", "Canopy_Cov", "Land_Mgnt", "Land_Owner", "Habitat_Type", "Pull_Status") %>%
    mutate(
      Date = as.Date(Date, format = "%m/%d/%Y"),  #"%Y-%m-%d"
      CameraLocation = as.factor(as.character(Name))
    ) %>%
    # arrange(Name, Date) %>%
    # #  Remove entries when the camera was pulled
    # filter(Status != "Removed") %>%
    # #  Remove duplicate data (where deployment and check data match for a given camera)
    # group_by(Camera_Lat, Camera_Long) %>%
    # distinct(Name, .keep_all = TRUE) %>%
    # ungroup() %>%
    # select(-Name) %>%
    # #  Remove duplicate cameras with incorrect coordinates (cameras were not moved)
    # filter(Cell_ID != "NE5094" | Status != "Checked") %>%
    # filter(Cell_ID != "NE6019" | Status != "Checked") %>%
    # filter(Cell_ID != "NE7602" | Status != "Checked") %>%
    # #  Remove duplicates that dplyr thinks are distinct for some reason
    # filter(Cell_ID != "NE1990" | Status != "Checked") %>%
    # filter(Cell_ID != "NE2383" | Status != "Checked") %>%
    # #  Remove camera station that did not change but camera # was changed due to damage
    # filter(Cell_ID != "OK2145" | Camera_ID != "3") %>%
    mutate(
      #  Adjust name of feature being monitored by camera
      Monitoring = ifelse(Monitoring == "Decommissioned Road, Game Trail", "Decommissioned road", Monitoring), 
      Monitoring = ifelse(Monitoring == "old, overgrown road", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "game trail (decommissioned road)", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "game trail (old overgrown road)", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "game trail (decommissioned road)", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "Decommissioned Road, game trail", "Decommissioned road", Monitoring), 
      Monitoring = ifelse(Monitoring == "Decommissioned road, game trail", "Decommissioned road", Monitoring), 
      Monitoring = ifelse(Monitoring == "old dirt road, game trail", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "Decomissioned Road (Game Trail)", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "game trail (old road)", "Decommissioned road", Monitoring), 
      Monitoring = ifelse(Monitoring == "trail (super old decommissioned road)", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "Decomissioned Road", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "Decommissioned Road", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "Deconmissioned Road", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "decommissioned road", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "Decommisioned Road", "Decommissioned road", Monitoring),	 
      Monitoring = ifelse(Monitoring == "Decomnissioned Road", "Decommissioned road", Monitoring),
      Monitoring = ifelse(Monitoring == "dirt road, game trail", "Dirt Road", Monitoring),
      Monitoring = ifelse(Monitoring == "Dirt Road, Game Trail", "Dirt road", Monitoring), 
      Monitoring = ifelse(Monitoring == "dirt road (logging)", "Dirt road", Monitoring),
      Monitoring = ifelse(Monitoring == "Dirt Road (might be a closed road)", "Dirt road", Monitoring),
      Monitoring = ifelse(Monitoring == "Dirt Road", "Dirt road", Monitoring),
      Monitoring = ifelse(Monitoring == "dirt road", "Dirt road", Monitoring), 
      Monitoring = ifelse(Monitoring == "dirt road ", "Dirt road", Monitoring),
      Monitoring = ifelse(Monitoring == "Dirt Road, Game trail", "Dirt road", Monitoring), 
      Monitoring = ifelse(Monitoring == "Closed Road, Game trail", "Closed road", Monitoring),
      Monitoring = ifelse(Monitoring == "Closed Road", "Closed road", Monitoring), 
      Monitoring = ifelse(Monitoring == "closed road", "Closed road", Monitoring), 
      Monitoring = ifelse(Monitoring == "Close Road", "Closed road", Monitoring), 
      Monitoring = ifelse(Monitoring == "gated dirt road", "Closed road", Monitoring),
      Monitoring = ifelse(Monitoring == "gated road", "Closed road", Monitoring),
      Monitoring = ifelse(Monitoring == "game trail, fenceline", "Game trail", Monitoring), 
      Monitoring = ifelse(Monitoring == "game trail?", "Game trail", Monitoring), 
      Monitoring = ifelse(Monitoring == "game trail", "Game trail", Monitoring),
      Monitoring = ifelse(Monitoring == "Game Trail", "Game trail", Monitoring),
      Monitoring = ifelse(Monitoring == "field", "Game trail", Monitoring),
      Monitoring = ifelse(Monitoring == "game Trail", "Game trail", Monitoring), 
      Monitoring = ifelse(Monitoring == "game trail ", "Game trail", Monitoring),
      Monitoring = ifelse(Monitoring == "trail", "Trail", Monitoring),
      #  Adjust name of land management type
      Land_Mgnt = ifelse(Land_Mgnt == "federal", "Federal", Land_Mgnt),
      Land_Mgnt = ifelse(Land_Mgnt == "federal, access through private", "Federal", Land_Mgnt),
      Land_Mgnt = ifelse(Land_Mgnt == "private", "Private", Land_Mgnt),
      Land_Mgnt = ifelse(Land_Mgnt == "Private ", "Private", Land_Mgnt),
      Land_Mgnt = ifelse(Land_Mgnt == "state", "State", Land_Mgnt),
      #  Adjust name of managing agency or owner type
      Land_Owner2 = ifelse(grepl("NF", Land_Owner), "USFS", Land_Owner),
      Land_Owner2 = ifelse(grepl("USFS", Land_Owner), "USFS", Land_Owner2),
      Land_Owner2 = ifelse(grepl("LPO", Land_Owner), "LPO", Land_Owner2),
      Land_Owner2 = ifelse(grepl("Cline", Land_Owner), "LPO", Land_Owner2),
      Land_Owner2 = ifelse(grepl("BLM", Land_Owner), "BLM", Land_Owner2),
      Land_Owner2 = ifelse(grepl("National Forest", Land_Owner), "USFS", Land_Owner2),
      Land_Owner2 = ifelse(grepl("NF", Land_Owner), "USFS", Land_Owner2),
      Land_Owner2 = ifelse(grepl("WDFW", Land_Owner), "WDFW", Land_Owner2),
      Land_Owner2 = ifelse(grepl("Wildlife", Land_Owner), "WDFW", Land_Owner2),
      Land_Owner2 = ifelse(grepl("wildlife", Land_Owner), "WDFW", Land_Owner2),
      Land_Owner2 = ifelse(grepl("DNR", Land_Owner), "WA DNR", Land_Owner2),
      Land_Owner2 = ifelse(grepl("WADNR", Land_Owner), "WA DNR", Land_Owner2),
      Land_Owner2 = ifelse(grepl("State Forest", Land_Owner), "WA DNR", Land_Owner2),
      Land_Owner2 = ifelse(grepl("Gamble", Land_Owner), "Private", Land_Owner2),
      Land_Owner2 = ifelse(grepl("Timber", Land_Owner2), "Private timber", Land_Owner2), # note: looking to new column here
      Land_Owner2 = ifelse(grepl("timber", Land_Owner2), "Private timber", Land_Owner2), # otherwise end up overwriting
      Land_Owner2 = ifelse(grepl("Lumber", Land_Owner2), "Private timber", Land_Owner2), # some DNR and private ownership
      Land_Owner2 = ifelse(grepl("Arden", Land_Owner), "Private timber", Land_Owner2),
      Land_Owner2 = ifelse(grepl("Stimpson", Land_Owner), "Private timber", Land_Owner2),
      Land_Owner2 = ifelse(grepl("Stimson", Land_Owner), "Private timber", Land_Owner2),
      Land_Owner2 = ifelse(grepl("Hancock", Land_Owner), "Private timber", Land_Owner2),
      Land_Owner2 = ifelse(grepl("Handcock", Land_Owner), "Private timber", Land_Owner2),
      Land_Owner2 = ifelse(grepl("system", Land_Owner), "Private timber", Land_Owner2),
      Land_Owner2 = ifelse(grepl("Inland", Land_Owner), "Private timber", Land_Owner2),
      Land_Owner2 = ifelse(Land_Mgnt == "Private" & Land_Owner2 != "Private timber", "Private", Land_Owner2),
      #  Adjust habitat type
      Habitat_Type2 = ifelse(grepl("Riparian", Habitat_Type), "Riparian", Habitat_Type),
      Habitat_Type2 = ifelse(grepl("riparian", Habitat_Type), "Riparian", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("Riparian, Sagebrush", Habitat_Type), "Riparian", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("riparian, sagebrush steppe", Habitat_Type), "Riparian", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("Sagebrush", Habitat_Type), "Shrub-steppe", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("sagebrush", Habitat_Type), "Shrub-steppe", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("bitterbrush", Habitat_Type), "Shrub-steppe", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("Grassland", Habitat_Type2), "Grassland", Habitat_Type2),      # note: looking to new column here
      Habitat_Type2 = ifelse(grepl("Grassand", Habitat_Type2), "Grassland", Habitat_Type2),       # otherwise end up overwriting
      Habitat_Type2 = ifelse(grepl("grassland", Habitat_Type2), "Grassland", Habitat_Type2),      # some habitat types
      Habitat_Type2 = ifelse(grepl("Agricultre", Habitat_Type2), "Agriculture", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("agriculture", Habitat_Type2), "Agriculture", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("Burn", Habitat_Type2), "Burned", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("burned", Habitat_Type2), "Burned", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("barely burned", Habitat_Type), "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("Low", Habitat_Type2), "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("low", Habitat_Type2), "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("LOW", Habitat_Type2), "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("High", Habitat_Type2), "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("high", Habitat_Type2), "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("hgih", Habitat_Type2), "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("mid", Habitat_Type2), "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("timber cut", Habitat_Type2), "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(grepl("Timber cut", Habitat_Type2), "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(Habitat_Type == "burned (15-20 years?), high elevation mixed conifer", "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(Habitat_Type == "burned (old), low elevation mixed conifer", "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(Habitat_Type == "Timber cut, burned probably a prescribed brun after thinng", "Mixed conifer", Habitat_Type2),
      Habitat_Type2 = ifelse(Habitat_Type == "agriculture, riparian, sagebrush", "Riparian", Habitat_Type2)
    ) %>%
    relocate(Land_Owner2, .after = Land_Owner) %>%
    relocate(Habitat_Type2, .after = Habitat_Type)
  
  #'  Rename these manipulated columns
  colnames(allstations)[colnames(allstations) == 'Land_Owner'] <- 'Land_Owner_original'
  colnames(allstations)[colnames(allstations) == 'Land_Owner2'] <- 'Land_Owner'
  colnames(allstations)[colnames(allstations) == 'Habitat_Type'] <- 'Habitat_Type_original'
  colnames(allstations)[colnames(allstations) == 'Habitat_Type2'] <- 'Habitat_Type'
  
  
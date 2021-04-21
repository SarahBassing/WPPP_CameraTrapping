    ##  Wrangling Those Camera Locations
    ##  August 2018
################################################################################
  #  This script pulls in and cuts out relevant spatial data (e.g., USFS ranger
  #  districts) and extracts camera locations specific to those areas.
  #  Then saves this information for to be shared with relevant parties.
################################################################################
  #### Initial set up ####

  #  Load packages
  library(rgdal)
  library(rgeos)
  library(raster)
  library(tidyr)
  library(dplyr)

  #  Read in shapefiles
  OK_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "METHOW_SA") #Okanogan
  NE_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "NE_SA")  #NE
  # GMUS <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "GMU_Generalized") #gmus
  USFS <- readOGR("./Shapefiles/S_USA.RangerDistrict", layer = "S_USA.RangerDistrict") #USFS Ranger Districts
  DNR <- readOGR("./Shapefiles/WADNR shapefiles/WA_DNR_Managed_Land_Parcels", layer = "WA_DNR_Managed_Land_Parcels") #WA DNR parcels
  # Reservations <- readOGR("./Shapefiles/cb_2017_us_aiannh_500k", layer = "cb_2017_us_aiannh_500k") #2017 Census American Indian Homelands
  # proads <- readOGR("./Shapefiles/WADNR shapefiles", layer = "DNR_Proprietary_Roads_Statewide")
  # oroads <- readOGR("./Shapefiles/WADNR shapefiles", layer = "Forest_Practices_Orphaned_and_Abandoned_Roads")
  # aroads <- readOGR("./Shapefiles/WADNR shapefiles", layer = "WADNR_Active_Roads")
  
  
  #  Reproject shapefiles to be in meters, not feet
  #  +proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 
  #  +lon_0=-120.5 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs 
  new_proj <- CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  
  OK <- spTransform(OK_SA, new_proj)  # note the study area name change here!
  NE <- spTransform(NE_SA, new_proj)
  # gmus <- spTransform(GMUS, new_proj)
  usfs <- spTransform(USFS, new_proj)
  # dnr <- spTransform(DNR, new_proj)
  # tribes <- spTransform(Reservations, new_proj)
  
  #  Read in shapefiles that are already in the correct projection, specific to study areas
  OKNF <- readOGR("./Shapefiles/S_USA.RangerDistrict", layer = "Okanogan_NF")  # Okanogran NF
  CONF <- readOGR("./Shapefiles/S_USA.RangerDistrict", layer = "Colville_NF")  # Colville NF
  OK_DNR <- readOGR("./Shapefiles/WADNR shapefiles/WA_DNR_Managed_Land_Parcels", layer = "Okanogan_DNR")
  NE_DNR <- readOGR("./Shapefiles/WADNR shapefiles/WA_DNR_Managed_Land_Parcels", layer = "Northeast_DNR")
  
################################################################################
  ####  Read in Randomly Selected Camera locations  ####
  
  #  Read in cell centroids and final camera locations
  cams1819 <- readOGR("./Shapefiles/Camera_Locations", layer = "cams_master18_19_spdf_050220")
  cams1920 <- readOGR("./Shapefiles/Camera_Locations", layer = "cams_master19_20_spdf_050220")
  cams2021 <- readOGR("./Shapefiles/Camera_Locations", layer = "Cam_2020locs_spdf_100520")
  # OK_cell_centroids <- readOGR("./Shapefiles/Camera_Locations", layer = "OK_cam_points_WGS84_summer20")
  # NE_cell_centroids <- readOGR("./Shapefiles/Camera_Locations", layer = "NE_cam_points_WGS84_summer20")
  # # OK_cell_centroids <- readOGR("./Shapefiles/Camera_Locations", layer = "OK_cam_points_summer19")
  # # NE_cell_centroids <- readOGR("./Shapefiles/Camera_Locations", layer = "NE_cam_points_summer19")
  # # OK_cell_centroids <- readOGR("G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018", layer = "OK_Centroids_Final_2018")
  # # NE_cell_centroids <- readOGR("G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018", layer = "NE_Centoids_Final_2018") #typo in layer name has to stay
  # OK_cell_centroids <- readOGR("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Camera_Locations", layer = "OK_cam_points_summer19")
  # NE_cell_centroids <- readOGR("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Camera_Locations", layer = "NE_cam_points_summer19")
  #   
  # 
  # Aug_Cameras <- readOGR("./Shapefiles/Camera_Locations", layer = "Cam_Locations_080519")
  # # OK_cameras <- readOGR("G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018", layer = "OK_Camera_Locations_2018")
  # # NE_cameras <- readOGR("G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018", layer = "NE_Camera_Locations_2018")

  #  Reproject
  cams1819 <- spTransform(cams1819, new_proj)
  cams1920 <- spTransform(cams1920, new_proj)
  cams2021 <- spTransform(cams2021, new_proj)
  # OK_cent <- spTransform(OK_cell_centroids, new_proj)
  # NE_cent <- spTransform(NE_cell_centroids, new_proj)
  # OK_Cams <- spTransform(OK_cameras, new_proj)
  # NE_Cams <- spTransform(NE_cameras, new_proj)
  # 
  # Aug_Cams <- spTransform(Aug_Cameras, new_proj)
  
    
################################################################################
  ####  Create Land Ownership Shapefiles Specific to Study Areas  ####
  
  ####  USFS Ranger Districts  ####
  
  #  Pull study area specific US National Forests out
  #  Okanogan-Wenatchee National Forest
  Okanogan_NF <- usfs[usfs@data$FORESTNAME == "Okanogan-Wenatchee National Forest",]
  # OKNF <- Okanogan_NF[OK_cent,]
  OKNF <- Okanogan_NF[cams1819,]
  #  Colville National Forest
  Colville_NF <- usfs[usfs@data$FORESTNAME == "Colville National Forest",]
  # CONF <- Colville_NF[NE_cent,]
  CONF <- Colville_NF[cams1819,]
  
  #  Write shapefiles for National Forests relevant to study areas
  # writeOGR(OKNF, dsn = "./Shapefiles/S_USA.RangerDistrict", layer = "Okanogan_NF", driver = "ESRI Shapefile", overwrite = T)
  # writeOGR(CONF, dsn = "./Shapefiles/S_USA.RangerDistrict", layer = "Colville_NF", driver = "ESRI Shapefile", overwrite = T)
  
  
  ####  Washington Department of Natural Resources  ####
  
  #  DNR lands within each study area
  OK_DNR <- OK_DNR[OK,]
  NE_DNR <- NE_DNR[NE,]
  
  #  Write shapefiles for study area-specific DNR parcels
  # writeOGR(OK_DNR, dsn = "./Shapefiles/WA_DNR_Managed_Land_Parcels", layer = "Okanogan_DNR", driver = "ESRI Shapefile", overwrite = T)
  # writeOGR(NE_DNR, dsn = "./Shapefiles/WA_DNR_Managed_Land_Parcels", layer = "Northeast_DNR", driver = "ESRI Shapefile", overwrite = T)
  
  ####  American Indian Reservations  ####
  
  #  Colville Tribe
  Col_Tribe <- tribes[tribes@data$AIANNHCE == "0760",]
  #  Spokane Tribe
  Spoke_Tribe <- tribes[tribes@data$AIANNHCE == "3940",]
  
  #  Write shapefiles of these specific reservations
  # writeOGR(Col_Tribe, dsn = "./Shapefiles", layer = "Col_Tribe", driver = "ESRI Shapefile", overwrite = T)
  # writeOGR(Spoke_Tribe, dsn = "./Shapefiles", layer = "Spoke_Tribe", driver = "ESRI Shapefile", overwrite = T)
    
################################################################################
  ####  Filter Camera Locations by Land Ownership  ####
  
  ##  USFS  ##
  #  Pull only cameras that fall on USFS land
  # oknf_cents <- OK_cent[OKNF,]
  # conf_cents <- NE_cent[CONF,]
  # oknf_cams <- OK_Cams[OKNF,]
  # conf_cams <- NE_Cams[CONF,]
  
  oknf_cams18 <- cams1819[OKNF,]
  oknf_cams19 <- cams1920[OKNF,]
  oknf_cams20 <- cams2021[OKNF,]
  conf_cams18 <- cams1819[CONF,]
  conf_cams19 <- cams1920[CONF,]
  conf_cams20 <- cams2021[CONF,]
  
  #  How many cameras on USFS total
  cams18 <- length(oknf_cams18@data$Cell_ID) + length(conf_cams18@data$Cell_ID)
  cams19 <- length(oknf_cams19@data$Cell_ID) + length(conf_cams19@data$Cell_ID)
  cams20 <- length(oknf_cams20@data$Name) + length(conf_cams20@data$Name)
  cams18+cams19+cams20
  length(oknf_cams18@data$Cell_ID) + length(oknf_cams19@data$Cell_ID) + length(oknf_cams20@data$Name)
  length(conf_cams18@data$Cell_ID) + length(conf_cams19@data$Cell_ID) + length(conf_cams20@data$Name)
  
  #  Create and save dataframe & shapefiles of camera locations to be shared with USFS
  Camera_Centroids_2020_OKNF <- as.data.frame(oknf_cents)
  # write.csv(Camera_Locations_2018_OKNF, file = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018/For Sharing/Camera_Locations_2018_OKNF.csv")
  writeOGR(oknf_cents, dsn = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2020/For Sharing", layer = "UW Cameras 2020 Okanogan NF", driver = "ESRI Shapefile", overwrite = T)
  writeOGR(oknf_cents, dsn="G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2020/For Sharing/UWCameras_2020_OkanoganNF.gpx",
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
  Camera_Locations_2020_CONF <- as.data.frame(conf_cents)
  # write.csv(Camera_Locations_2018_CONF, file = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018/For Sharing/Camera_Locations_2018_CONF.csv")
  writeOGR(conf_cents, dsn = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2020/For Sharing", layer = "UW Cameras 2020 Colville NF", driver = "ESRI Shapefile", overwrite = T)
  
    
  ##  WA DNR  ##
  #  Pull only cameras that fall on WS DNR land in the two study areas
  okdnr_cents <- OK_cent[OK_DNR,]
  nednr_cents <- NE_cent[NE_DNR,]
  okdnr_cams <- OK_Cams[OK_DNR,]
  nednr_cams <- NE_Cams[NE_DNR,]

  #  Create and save dataframe & shapefiles of camera locations to be shared with USFS
  Camera_Locations_OK_2020_DNR <- as.data.frame(okdnr_cents) #cams
  # write.csv(Camera_Locations_OK_2018_DNR, file = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018/For Sharing/Camera_Locations_OK_2018_DNR.csv")
  writeOGR(okdnr_cents, dsn = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2020/For Sharing", layer = "UW Proposed Cameras 2020 Okanogan DNR", driver = "ESRI Shapefile", overwrite = T)
  Camera_Locations_NE_2020_DNR <- as.data.frame(nednr_cents) #cams
  # write.csv(Camera_Locations_NE_2018_DNR, file = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018/For Sharing/Camera_Locations_NE_2018_DNR.csv")
  writeOGR(nednr_cents, dsn = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2020/For Sharing", layer = "UW Proposed Cameras 2020 Northeast DNR", driver = "ESRI Shapefile", overwrite = T)
  
  
################################################################################
  ####  Camera locations specific to Gebbers Properties  ####
  
  #  Which camera centroids fall on Gebbers Properties?
  #  2020: OK5641 (DNR), OK7073, OK8127 (Buckhorn Mtn), OK8234
  OK_cam_points_WGS84 <- readOGR("./Shapefiles/Camera_Locations", layer = "OK_cam_points_WGS84_summer20")
  #OK_cam_points <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/OK_cents_summer20.csv")
  UWCams_Gebbers2020_Proposed <- OK_cam_points_WGS84[OK_cam_points_WGS84@data$Name == "OK5641" |
                                                 OK_cam_points_WGS84@data$Name == "OK7073" |
                                                 OK_cam_points_WGS84@data$Name == "OK8127" |
                                                 OK_cam_points_WGS84@data$Name == "OK8234",]
  colnames(UWCams_Gebbers2020_Proposed) <- c("X", "Y", "Elevation_Strata", "Name")
  # #  2019: OK6597, OK7365, OK7545; DNR land possibly leased by Gebbers: OK7350
  # UWCams_Gebbers2019_Proposed <- OK_cent[OK_cent@data$Cell_ID == "OK6597" |
  #                                       OK_cent@data$Cell_ID == "OK7350" |
  #                                       OK_cent@data$Cell_ID == "OK7365" |
  #                                       OK_cent@data$Cell_ID == "OK7545",]
  
  #  Save for sharing with Gebbers
  writeOGR(UWCams_Gebbers2020_Proposed, dsn = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2020/For Sharing", layer = "UWCams_Gebbers2020_Proposed", driver = "ESRI Shapefile", overwrite = T)
  writeOGR(UWCams_Gebbers2020_Proposed, dsn="G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2020/For Sharing/UWCams_Gebbers2020_Proposed.gpx",
           dataset_options="GPX_USE_EXTENSIONS=yes",layer="waypoints",driver="GPX", overwrite_layer = T)
  
  #writeOGR(UWCams_Gebbers2019_Proposed, dsn = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2019/For Sharing", layer = "UWCams_Gebbers2019_Proposed", driver = "ESRI Shapefile", overwrite = T)
  
  
  #  Once cameras are deployed and they need to final camera coordinates
  #  Read in csv of camera locations that fall on Gebbers properties
  gebs <- read.csv("G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018/For Sharing/Gebbers_Cameras_Deployed_Summer2018.csv")

  #  Define coordinates in a new dataframe
  xy <- gebs[,3:4]
  
  #  Create a spatialpointsdataframe
  #  Data collected in WGS84
  Gebbers_2018 <- SpatialPointsDataFrame(coords = xy, data = gebs, 
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  #  Alternative version of this projection apparently...
  # "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
  
  #  Write the shapefile to give to landowner
  writeOGR(Gebbers_2018, dsn = "G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018/For Sharing", layer = "Gebbers_2018", driver = "ESRI Shapefile", overwrite = T)
  
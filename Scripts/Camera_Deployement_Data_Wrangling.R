    ##  Wrangling Camera Deployment Data
    ##  August 2018
################################################################################
  #  This script is used to put together basic summary information regarding 
  #  camera traps to be used for reports, presentations, etc.
################################################################################
  ####  Initial Set Up  ####

  #  Load packages
  library(tidyr)
  library(dplyr)

  #  Read in data
  deployed_18 <- read.csv("./Input Data/Deployed_Cameras_Summer2018.csv")
  detected_18 <- read.csv("./Input Data/Species_Detected_Summer_2018.csv", header = T)
  # detected_18 <- read.csv("./Input Data/Species_Detected_Summer_2018_Spp.csv", 
  #                         header = T) 
  # detected_18 <- read.table("./Input Data/Species_Detected_Summer_2018_Spp.txt", 
  #                           header = T, sep = "")
################################################################################
  ####  Basic summary info of deployed cameras  ####
  
  #  Number of cameras deployed
  ncam <- nrow(deployed_18)  # as of 8.31.18 keep in mind I'm missing the locations of some deployed cameras (waiting on Trent)

  #  Number of cameras deployed per study area
  NE_deployed <- sum(deployed_18$Study_area == "NE")  # 8.31.18 this seems to be a little off
  OK_deployed <- sum(deployed_18$Study_area == "OK")  # 8.31.18 this count OK, still waiting on data from Trent
    
  ###  Cameras on private vs public land  ####
  
  #  Which cameras are on private land?
  private <- which(deployed_18$Land_mgnt == "Private")  # across both study areas
  NE_private <- which(deployed_18$Study_area == "NE" & deployed_18$Land_mgnt == "Private")
  OK_private <- which(deployed_18$Study_area == "OK" & deployed_18$Land_mgnt == "Private")

  #  Cameras on private land = 1, public land = 0
  yes_private <- ifelse(deployed_18$Land_mgnt == "Private", 1, 0)  # across both study areas
  yes_NEprivate <- ifelse(deployed_18$Study_area == "NE" & deployed_18$Land_mgnt == "Private", 1, 0)
  yes_OKprivate <- ifelse(deployed_18$Study_area == "OK" & deployed_18$Land_mgnt == "Private", 1, 0)
  
  #  Count number of cameras on private vs public land
  nprivate <- sum(yes_private)  # across both study areas
  npublic <- ncam - nprivate  # across both study areas
  NE_nprivate <- sum(yes_NEprivate); NE_npublic <- NE_deployed - NE_nprivate
  OK_nprivate <- sum(yes_OKprivate); OK_npublic <- OK_deployed - OK_nprivate
  
  #  Percent public vs private
  percent_private <- nprivate/ncam  # across both study areas
  NE_percent_private <- NE_nprivate/NE_deployed
  OK_percent_private <- OK_nprivate/OK_deployed
  
################################################################################
  ####  Species Detected Summaries  ####
  
  #  All species detected on each camera were recorded with a unique 3-letter code
  #  For each camera the species were listed in separate columns but these columns
  #  are NOT species specific and are a total disorganized mess
  #  I need to sumamrize these data by # of cameras that detected each species, 
  #  species by study area, etc.  Later I will also summarize where collars were
  #  detected and of which species, but this will not be done in R.
  
  #  Check class type of species columns
  class(detected_18$Species_1)
  
  #  Count number of cameras that detected a species of interest
  #  Not the most efficient method for doing this
  #  Keep in mind some cameras are double counted in this dataset because they
  #  were checked twice... will need to fix that eventually
  
  # counts <- detected_18[,c(3,10:39)]  # save cell_ID and species counts per camera
  # counts <- as.data.frame(counts)
  # str(counts)  #everything comes up as integers... but I need them to be numeric to make a histogram
  # counts <- as.numeric(counts[,2:30])  #doesn't work b/c each column is a list.  WHY are they lists?!
  # hist(counts)

  
  WTD <- sum(detected_18[,10:24] == "WTD", na.rm = TRUE)
  MLD <- sum(detected_18[,10:24] == "MLD", na.rm = TRUE)
  ELK <- sum(detected_18[,10:24] == "ELK", na.rm = TRUE)
  MOS <- sum(detected_18[,10:24] == "MOS", na.rm = TRUE)
  UND <- sum(detected_18[,10:24] == "UND", na.rm = TRUE)
  CGR <- sum(detected_18[,10:24] == "CGR", na.rm = TRUE)
  WLF <- sum(detected_18[,10:24] == "WLF", na.rm = TRUE)
  BCT <- sum(detected_18[,10:24] == "BCT", na.rm = TRUE)
  BKB <- sum(detected_18[,10:24] == "BKB", na.rm = TRUE)
  COY <- sum(detected_18[,10:24] == "COY", na.rm = TRUE)
  HUM <- sum(detected_18[,10:24] == "HUM", na.rm = TRUE)
  VCL <- sum(detected_18[,10:24] == "VCL", na.rm = TRUE)
  ATV <- sum(detected_18[,10:24] == "ATV", na.rm = TRUE)
  DOG <- sum(detected_18[,10:24] == "DOG", na.rm = TRUE)
  CAT <- sum(detected_18[,10:24] == "CAT", na.rm = TRUE)
  CTL <- sum(detected_18[,10:24] == "CTL", na.rm = TRUE)
  SKN <- sum(detected_18[,10:24] == "SKN", na.rm = TRUE)
  TRK <- sum(detected_18[,10:24] == "TRK", na.rm = TRUE)
  GRS <- sum(detected_18[,10:24] == "GRS", na.rm = TRUE)
  RBT <- sum(detected_18[,10:24] == "RBT", na.rm = TRUE)
  RCN <- sum(detected_18[,10:24] == "RCN", na.rm = TRUE)
  BGR <- sum(detected_18[,10:24] == "BGR", na.rm = TRUE)
  SQR <- sum(detected_18[,10:24] == "SQR", na.rm = TRUE)


  #  Keep in mind some of these cameras were checked twice so I'll need to
  #  remove a few camera counts from these
  #  Subtract: 10WTD, 1MLD, 2MOS, 1ELK, 3CGR, 2WLF, 2BKB, 2BCT, 5COY, 2TRK, 
  #  1SKN, 1SQR, 1HUM
  
  WTD <- WTD-10; MLD <- MLD-1; MOS <- MOS-2; ELK <- ELK-1; CGR <- CGR-3
  WLF <- WLF-2; BKB <- BKB-2; BCT <- BCT-2; COY <- COY-5; TRK <- TRK-2
  SKN <- SKN-1; SQR <- SQR-1; HUM <- HUM-1

  Cam_detect_totals <- as.numeric(c(WTD, MLD, ELK, MOS, UND, CGR, WLF, BKB, COY,
                                    BCT, HUM, VCL, ATV, DOG, CAT, CTL, SKN, TRK,
                                    GRS, RBT, RCN, BGR, SQR))
  Cam_detect_counts <- as.data.frame(Cam_detect_totals)
  # colnames(Cam_detect_counts) <- c("WTD", "MLD", "ELK", "MOS", "UND", "CGR", "WLF", "BKB", "COY",
  #                                "BCT", "HUM", "VCL", "ATV", "DOG", "CAT", "CTL", "SKN", "TRK",
  #                                "GRS", "RBT", "RCN", "BGR", "SQR")
  # wtd <- rep("WTD", WTD)
  # mld <- rep("MLD", MLD)
  
  Cam_detect_spp <- as.factor(c("WTD", "MLD", "ELK", "MOS", "UND", "CGR", "WLF", "BKB", "COY",
                                "BCT", "HUM", "VCL", "ATV", "DOG", "CAT", "CTL", "SKN", "TRK",
                                "GRS", "RBT", "RCN", "BGR", "SQR"))
  
  Detected_spp <- cbind(Cam_detect_spp, Cam_detect_counts)
  colnames(Detected_spp) <- c("Species", "Count")
  plot(Detected_spp)
  
  tst <- t(Detected_spp)
  counts <- table(Detected_spp)
  
  #  blurg why isn't this working?!  I'm trying to compile these into a single 
  #  dataframe so I can make a couple plots!
  # Target_spp <- c("WTD", "MLD", "ELK", "MOS", "UND", "CGR", "WLF", "BKB", "COY", "BCT")
  # Target_count <- as.numeric(c(WTD, MLD, ELK, MOS, UND, CGR, WLF, BKB, COY, BCT))
  # #Targets <- as.data.frame(rbind(Target_spp, Target_count))
  # colnames(Targets) <- c("WTD", "MLD", "ELK", "MOS", "UND", "CGR", "WLF", "BKB", "COY", "BCT")
  # Targets <- Targets[-1,]
  # Targets <- as.numeric(Targets)
  # 
  # ugh <- detected_18[,10:24]
  # Humph <- ugh %>%
  #   mutate(
  #     WTD <- ifelse(ugh == "WTD", 1, detected_18),
  #     MLD <- ifelse(ugh == "MLD", 2, detected_18)
  #   )
  # tst <- ifelse(detected_18[,10:24] == "WTD",1,0)
  
  
  #  For just the NE
  NE_detections <- detected_18 %>%
    filter(Study_area == "NE")
  WTD <- sum(NE_detections[,10:24] == "WTD", na.rm = TRUE)
  MLD <- sum(NE_detections[,10:24] == "MLD", na.rm = TRUE)
  ELK <- sum(NE_detections[,10:24] == "ELK", na.rm = TRUE)
  MOS <- sum(NE_detections[,10:24] == "MOS", na.rm = TRUE)
  UND <- sum(NE_detections[,10:24] == "UND", na.rm = TRUE)
  CGR <- sum(NE_detections[,10:24] == "CGR", na.rm = TRUE)
  WLF <- sum(NE_detections[,10:24] == "WLF", na.rm = TRUE)
  BCT <- sum(NE_detections[,10:24] == "BCT", na.rm = TRUE)
  BKB <- sum(NE_detections[,10:24] == "BKB", na.rm = TRUE)
  COY <- sum(NE_detections[,10:24] == "COY", na.rm = TRUE)
  HUM <- sum(NE_detections[,10:24] == "HUM", na.rm = TRUE)
  VCL <- sum(NE_detections[,10:24] == "VCL", na.rm = TRUE)
  ATV <- sum(NE_detections[,10:24] == "ATV", na.rm = TRUE)
  DOG <- sum(NE_detections[,10:24] == "DOG", na.rm = TRUE)
  CAT <- sum(NE_detections[,10:24] == "CAT", na.rm = TRUE)
  CTL <- sum(NE_detections[,10:24] == "CTL", na.rm = TRUE)
  SKN <- sum(NE_detections[,10:24] == "SKN", na.rm = TRUE)
  TRK <- sum(NE_detections[,10:24] == "TRK", na.rm = TRUE)
  GRS <- sum(NE_detections[,10:24] == "GRS", na.rm = TRUE)
  RBT <- sum(NE_detections[,10:24] == "RBT", na.rm = TRUE)
  RCN <- sum(NE_detections[,10:24] == "RCN", na.rm = TRUE)
  BGR <- sum(NE_detections[,10:24] == "BGR", na.rm = TRUE)
  SQR <- sum(NE_detections[,10:24] == "SQR", na.rm = TRUE)
  
  NECam_detect_totals <- as.numeric(c(WTD, MLD, ELK, MOS, UND, CGR, WLF, BKB, COY,
                                    BCT, HUM, VCL, ATV, DOG, CAT, CTL, SKN, TRK,
                                    GRS, RBT, RCN, BGR, SQR))
  NECam_detect_counts <- as.data.frame(NECam_detect_totals)
  
  #  For just the OK
  OK_detections <- detected_18 %>%
    filter(Study_area == "OK")
  WTD <- sum(OK_detections[,10:24] == "WTD", na.rm = TRUE)
  MLD <- sum(OK_detections[,10:24] == "MLD", na.rm = TRUE)
  ELK <- sum(OK_detections[,10:24] == "ELK", na.rm = TRUE)
  MOS <- sum(OK_detections[,10:24] == "MOS", na.rm = TRUE)
  UND <- sum(OK_detections[,10:24] == "UND", na.rm = TRUE)
  CGR <- sum(OK_detections[,10:24] == "CGR", na.rm = TRUE)
  WLF <- sum(OK_detections[,10:24] == "WLF", na.rm = TRUE)
  BCT <- sum(OK_detections[,10:24] == "BCT", na.rm = TRUE)
  BKB <- sum(OK_detections[,10:24] == "BKB", na.rm = TRUE)
  COY <- sum(OK_detections[,10:24] == "COY", na.rm = TRUE)
  HUM <- sum(OK_detections[,10:24] == "HUM", na.rm = TRUE)
  VCL <- sum(OK_detections[,10:24] == "VCL", na.rm = TRUE)
  ATV <- sum(OK_detections[,10:24] == "ATV", na.rm = TRUE)
  DOG <- sum(OK_detections[,10:24] == "DOG", na.rm = TRUE)
  CAT <- sum(OK_detections[,10:24] == "CAT", na.rm = TRUE)
  CTL <- sum(OK_detections[,10:24] == "CTL", na.rm = TRUE)
  SKN <- sum(OK_detections[,10:24] == "SKN", na.rm = TRUE)
  TRK <- sum(OK_detections[,10:24] == "TRK", na.rm = TRUE)
  GRS <- sum(OK_detections[,10:24] == "GRS", na.rm = TRUE)
  RBT <- sum(OK_detections[,10:24] == "RBT", na.rm = TRUE)
  RCN <- sum(OK_detections[,10:24] == "RCN", na.rm = TRUE)
  BGR <- sum(OK_detections[,10:24] == "BGR", na.rm = TRUE)
  SQR <- sum(OK_detections[,10:24] == "SQR", na.rm = TRUE)
  
  OKCam_detect_totals <- as.numeric(c(WTD, MLD, ELK, MOS, UND, CGR, WLF, BKB, COY,
                                      BCT, HUM, VCL, ATV, DOG, CAT, CTL, SKN, TRK,
                                      GRS, RBT, RCN, BGR, SQR))
  OKCam_detect_counts <- as.data.frame(OKCam_detect_totals)
  
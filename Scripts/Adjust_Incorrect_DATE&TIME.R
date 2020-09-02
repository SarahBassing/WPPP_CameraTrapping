  ##  Adjust Incorrect Camera Trap Date & Time
  ##  Sarah Bassing, WPPP
  ##  The World is on Fire 2020
  ##  ==========================================================
  ##  This script reads in processed image sets where the date and/or time is
  ##  incorrect as a result of camera malfuction or inappropriate programming
  ##  during deployment. Once corrected, these image sets can be added to the 
  ##  larger database of processed images.
  ##  ==========================================================
    
  ##  Libraries and data
  library(camtrapR)
  library(exifr)
  library(tidyverse)
  
  dat <- read.csv("G:/My Drive/1 Data/Image Processing/REVIEWING Processed Images/Proofed csvs & ddb/NE3000_C18_S3_CH_REVIEWED_DATETIMEWRONG.csv")
  dat2 <- read.csv("G:/My Drive/1 Data/Image Processing/REVIEWING Processed Images/Proofed csvs & ddb/OK4880_94_C216_CH_REVIEWED.csv")
  timeShift <- read.csv("G:/My Drive/1 Data/Image Processing/timeShiftTable.csv")
  
  ##  Attempting to use timeShiftTable to adjust the date and time of camera data
  dat <- rbind(dat,dat2)
  tst <- timeShiftImages(dat, 
                        timeShiftTable = timeShift,
                        stationCol = "Station",
                        cameraCol = "camera",
                        hasCameraFolders = FALSE,
                        timeShiftColumn = "timeshift",
                        timeShiftSignColumn = "sign",
                        undo = FALSE)
  
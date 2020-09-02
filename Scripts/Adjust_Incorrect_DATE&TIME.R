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
  #library(exifr)
  library(tidyverse)
  
  #  Make sure R can find ExifTool
  #  (Necessary for timeShiftImages funciton to work)
  #  Not sure how this all works but make sure the path leads to the exiftool.exe 
  #  file (remove (-k) if included in the file name). 
  #  More here: https://exiftool.org/#shift & https://groups.google.com/g/camtrapr/c/kAoUZuBUt8o
  system("exiftool")
  exiftool_dir <- "C:/exiftool-12.05"      
  exiftoolPath(exiftoolDir = exiftool_dir)
  grepl(exiftool_dir,  Sys.getenv("PATH"))
  
  #  Read in data
  NE3815 <- read.csv("NE3815_28_C125_MSW_datetimeweird-cleaned.csv")
  OK4880 <- read.csv("OK4880_C175_TT_DATEOFF1DAY.csv")
  timeShift <- read.csv("G:/My Drive/1 Data/Image Processing/timeShiftTable.csv")
  
  ####  DOES NOT WORK  ####
  #  The timeShiftImages function apparently ONLY works on raw images, not on the
  #  already processed image data recorded in a csv  :(
  
  #  Attempting to use timeShiftTable to adjust the date and time of camera data
  #dat <- rbind(NE3815, OK4880)
  tst <- timeShiftImages(inDir = , 
                        timeShiftTable = timeShift,
                        stationCol = "Station",
                        cameraCol = "camera",
                        hasCameraFolders = FALSE,
                        timeShiftColumn = "timeshift",
                        timeShiftSignColumn = "sign",
                        undo = FALSE)
  
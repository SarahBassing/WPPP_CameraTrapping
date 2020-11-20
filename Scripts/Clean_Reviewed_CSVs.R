  ##  Compile processed camera csv files
  ##  Washington Predator-Prey Project
  ##  Sarah Bassing
  ##  Sept 2020
  ##  ------------------------------------------------------------
  ##  This script reads in fully reviewed camera trap data from csv files and 
  ##  compiles them into a single MEGA data table. Note that camera data were 
  ##  processed by 2 independent individuals & reviewed by a third individual.
  ##  Script sources "Adjust_Incorrect_DATE&TIME.R" script, which adjusts the
  ##  Date and Time on cameras with known issues, & merges with MEGA data table.
  ##  Script then identifies which cameras had date & time errors and if any
  ##  images are still marked "Second Opinion". Script then filters service
  ##  and empty images out.
  ##  ------------------------------------------------------------
  
  ## Load packages
  
  #library(data.table)
  library(chron)
  library(plyr)
  library(readr)
  library(tidyverse)

  
  ####  READ IN REVIEWED CAMERA DATA  ####
  ##  --------------------------------------------------------  
  ##  Function to read in all csv files at once, and organize columns so they
  ##  are in a usable format.
  ##
  ##  SCREW YOU POSIXct
  ##  %d = day of month (decimal number)
  ##  %m = month (decimal number); %b month (abreviated); %B = month (full name)
  ##  %y = year (2 digit); %Y = year (4 digit)
  
  #  Reads in data processed by 3 independent reviewers
  #mydir <- "G:/My Drive/1 Data/Image Processing/REVIEWING Processed Images/Proofed csvs & ddb" # update later
  mydir <- "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Reviewed Image Data/Year 1"
  myfiles <- list.files(path = mydir, pattern = "*.csv", full.names = TRUE)
  full_csv <- ldply(myfiles, read_csv, col_names = TRUE) %>%
    transmute(
      File = as.character(File),
      RelativePath = as.character(RelativePath),
      Folder = as.character(Folder),
      DateTime = as.POSIXct(paste(Date, Time),
                            format="%d-%b-%Y %H:%M:%S",tz="America/Los_Angeles"),
      Date = as.Date(Date, format = "%d-%b-%Y"),
      Time = chron(times = Time),
      ImageQuality = as.factor(ImageQuality),
      CameraLocation = as.factor(as.character(CameraLocation)),
      DT_Good = as.factor(as.character(DT_Good)),
      Service = as.factor(as.character(Service)),
      Empty = as.factor(as.character(Empty)),
      Animal = as.factor(as.character(Animal)),
      Human = as.factor(as.character(Human)),
      Vehicle = as.factor(as.character(Vehicle)),
      Species = as.factor(as.character(Species)),
      HumanActivity = as.factor(as.character(HumanActivity)),
      Count = as.numeric(Count),
      AF = as.numeric(AdultFemale),
      AM = as.numeric(AdultMale),
      AU = as.numeric(AdultUnknown),
      OS = as.numeric(Offspring),
      UNK = as.numeric(UNK),
      Collars = as.numeric(Collars),
      Tags = as.character(Tags),
      Color = as.character(NaturalMarks),
      SecondOp = as.factor(as.character(SecondOpinion))
    )
  
  str(full_csv)
  
  
  #  Source "Adjust_Incorrect_DATE&TIME.R" script to bring in cameras with known
  #  and corrected date/time issues.
  source("./Scripts/Adjust_Incorrect_DATE&TIME.R")
  
  #  Append sourced data to MEGA data file
  full_csv <- rbind(full_csv, NE3000_S3_C18_DTGood, NE3109_S4_C31_C96_C131_DTGood,
                    NE3815_C26_C61_DTGood, NE3815_C125_DTGood, NE5511_C168_C186_DTGood,
                    OK4880_C175_DTGood)
  
  #  UH OH, dimensions don't match
  
  
  ####  FINAL CLEANING  ####
  ##  ---------------------------------------------------
  ##  Last chance to clean up data before moving onto real analyses
  ##  Make sure date & times are correct and if not, make adjustments
  ##  Make sure all Second Opinion images have been reviewed
  
  #  Identify which cameras MAY have date & time issues (DT_Good marked FALSE)
  unique(full_csv$CameraLocation[which(full_csv$DT_Good == "FALSE")])
  
  ##  FULL PAUSE  ##
  
  #  Use Adjust_Incorrect_DATE&TIME script to correct errors where needed
  #  WARNING: TIME is incorrect and should not be used for further analyses
  #  WARNING: DATE is incorrect for some detections close to daylight savings shift
  source("./Scripts/Adjust_Incorrect_Date&TIME.R")
  
  #  Remove image data with incorrect times and append corrected data
  #  figure out what to do about wrong date & times... split DateTime out? Is it possible to just pull out times?
  
  
  
  
  
  
  
  
  #  Identify which images still need a second review
  unique(full_csv$CameraLocation[which(full_csv$SecondOp == "TRUE")])
  #  BRB, gotta go check these

  
  ####  FILTER DATA & SAVE  ####
  ##  ---------------------------------------------
  
  #  Filter service and empty images out
  alldetections <- full_csv %>%
    filter(Empty != "TRUE") %>%
    filter(Service != "TRUE")

  #  Save for later analyses
  write.csv(alldetections, "./Output/Bassing_AllDetections.csv")


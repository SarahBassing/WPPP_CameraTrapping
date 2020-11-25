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
  
  #str(full_csv)
  
  #  Read in un-reviewed files
  mydir2 <- "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Processed Image Data/Year 1/Format 1"
  myfiles2 <- list.files(path = mydir2, pattern = "*.csv", full.names = TRUE)
  partial_csv2 <- ldply(myfiles2, read_csv, col_names = TRUE) %>%
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
  #str(partial_csv2)
  
  mydir3 <- "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Processed Image Data/Year 1/Format 2"
  myfiles3 <- list.files(path = mydir3, pattern = "*.csv", full.names = TRUE)
  partial_csv3 <- ldply(myfiles3, read_csv, col_names = TRUE) %>%
    transmute(
      File = as.character(File),
      RelativePath = as.character(RelativePath),
      Folder = as.character(Folder),
      DateTime = as.POSIXct(paste(Date, Time),
                            format="%d-%b-%y %H:%M:%S",tz="America/Los_Angeles"), 
      Date = as.Date(Date, format = "%d-%b-%y"), 
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
  #str(partial_csv3)
  
  #  For some reason all Species classification data are changed to "NA"
  #  Read in separately and the problem doesn't occur
  prob1 <- read.csv("G:/My Drive/1_Repositories/WPPP_CameraTrapping/Reviewed Image Data/Year 1/NE5921_26_C84, 101, 174_CH_REVIEWED.csv")
  prob2 <- read.csv("G:/My Drive/1_Repositories/WPPP_CameraTrapping/Reviewed Image Data/Year 1/NE6491_34_C104&C126_CH_REVIEWED.csv")
  probs <- rbind(prob1, prob2) %>%
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
  
  prob3 <- read.csv("G:/My Drive/1_Repositories/WPPP_CameraTrapping/Processed Image Data/Year 1/Why_are_you_weird/NE3558_69_C88_C105_DM.csv") %>%
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
  
  #  Source "Adjust_Incorrect_DATE&TIME.R" script to bring in cameras with known
  #  and corrected date/time issues.
  source("./Scripts/Adjust_Incorrect_DATE&TIME.R")
  
  #  Append sourced data to MEGA data file
  full_csv <- rbind(full_csv, partial_csv2, partial_csv3, probs, prob3, 
                    NE3000_S3_C18_DTGood, NE3109_S4_C31_C96_C131_DTGood,
                    NE3815_C26_C61_DTGood, NE3815_C125_DTGood, 
                    NE5511_C168_C186_DTGood, OK4880_C175_DTGood)
  
  #  Am I missing any cameras?
  cams <- as.data.frame(unique(full_csv$CameraLocation))
  
  
  ####  FINAL CLEANING  ####
  ##  ---------------------------------------------------
  ##  Last chance to clean up data before moving onto real analyses
  ##  Make sure date & times are correct and if not, make adjustments
  ##  Make sure all Second Opinion images have been reviewed

  ##  FULL STOP  ##

  #  Identify which cameras & memory cards MAY have date & time issues
  #  Print cameras that have already been adjusted
  print(adjusted)
  #  DT_Good marked FALSE
  droplevels(unique(full_csv$CameraLocation[which(full_csv$DT_Good == "FALSE" | full_csv$DT_Good == "false")]))
  unique(full_csv$RelativePath[which(full_csv$DT_Good == "FALSE" | full_csv$DT_Good == "false")])

  #  Flagged as having incorrect DateTime but I have confirmed they are correct
  #  NE3000_48_C231
  #  NE4498_21_C6
  #  NE5345_3_C16

  ##  Mmmk we're good  ##

  #  Double check if any cameras are dropping Species classifications after formatting
  #  Just because it lists these cameras doesn't mean there's a problem
  droplevels(unique(full_csv$CameraLocation[is.na(full_csv$Species) & full_csv$Animal == "TRUE"]))
  droplevels(unique(full_csv$CameraLocation[is.na(full_csv$Species) & full_csv$Animal == "true"]))
  #  Why are there duplicate observations but one as NA for species?!
  tst <- alldetections[alldetections$CameraLocation == "NE5921_26" & alldetections$File == "RCNX1616.JPG",]
  tst <- alldetections[alldetections$CameraLocation == "NE5921_26" & alldetections$File == "RCNX0366.JPG",]
  #  Not sure what to do about this but at least the species is retained in one version
  
  
  ####  Eventually need to deal with these duplicate observations....  ####
  
  
  #  Identify which cameras have images that were never reviewed or where info
  #  was incorrectly propogated across empty images
  droplevels(unique(full_csv$CameraLocation[full_csv$Animal == "FALSE" & full_csv$Human == "FALSE" & full_csv$Vehicle == "FALSE"]))
  droplevels(unique(full_csv$CameraLocation[full_csv$Animal == "false" & full_csv$Human == "false" & full_csv$Vehicle == "false"]))
  trouble <- full_csv[full_csv$Animal == "FALSE" & full_csv$Human == "FALSE" & full_csv$Vehicle == "FALSE",]
  #  BRB, gotta fix these

  #  Which ones are reading in as lower-cased "true"/"false"? This is causing problems
  droplevels(unique(full_csv$CameraLocation[full_csv$Animal == "true"]))

  #  Identify which images still need a second review
  unique(full_csv$CameraLocation[which(full_csv$SecondOp == "TRUE" | full_csv$SecondOp == "true")])
  #  BRB, gotta go check these

  
  ####  FILTER DATA & SAVE  ####
  ##  ---------------------------------------------
  
  #  Filter service and empty images out
  alldetections <- full_csv %>%
    filter(Empty != "TRUE" & Empty != "true") %>%
    filter(Service != "TRUE" & Service != "true")

  #  Save for later analyses
  write.csv(alldetections, paste0('./Output/Bassing_AllDetections_', Sys.Date(), '.csv')) # "./Output/Bassing_AllDetections.csv"

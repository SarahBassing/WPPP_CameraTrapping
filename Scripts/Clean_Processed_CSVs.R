  ##  Compile processed camera csv files
  ##  Washington Predator-Prey Project
  ##  Sarah Bassing
  ##  May 2020
  ##  ------------------------------------------------------------
  ##  This script reads in processed camera trap data from csv files and compiles
  ##  them into a single MEGA data table. Note that camera data were processed
  ##  at different times so early data csv files were manipulated in excel outside
  ##  of Timelapse. This means those data have to be read in separately from 
  ##  camera data that were processed later.
  ##  ------------------------------------------------------------
  
  ## Load packages
  
  library(data.table)
  library(chron)
  library(tidyverse)
  
  #  SCREW YOU POSIXct
  #  %d = day of month (decimal number)
  #  %m = month (decimal number); %b month (abreviated); %B = month (full name)
  #  %y = year (2 digit); %Y = year (4 digit)
  
  ##  --------------------------------------------------------  
  #  NORTHEAST CAMS
  
  #  First round reads in data processed with older templates and csv files needed
  #  to be adjusted so they had correct number of columns
  setwd("G:/My Drive/1 Volunteers/Capstone Projects/2019-2020/Kate/Raw Image Data Reformatted CSVs")
  
  #  Read in all files in the abover folder and format
  mega.tblNE <- list.files(pattern = "*.csv") %>%
    map_dfr(~fread(., stringsAsFactors = FALSE)) %>%
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
      Color = as.character(NaturalMarks)
    )
  str(mega.tblNE)
  
  #  Remove bad Moultrie camera data
  mega.tblNE <- mega.tblNE %>%
    filter(Date > "2012-02-09")
  
  #  Second round reads in all processed data using current template that has the 
  #  correct number of columns- did not require additional formating in excel
  #  Note the date structure is different
  setwd("G:/My Drive/1 Volunteers/Capstone Projects/2019-2020/Kate/Raw Image Data Timelapse Output")
  
  #  Read in data from above folder and format
  mega.tblNE2 <- list.files(pattern = "*.csv") %>%
    map_dfr(~fread(., stringsAsFactors = FALSE)) %>%  
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
      Color = as.character(NaturalMarks)
    )
  
  
  #  OKANOGAN CAMS
  #  First round
  setwd("G:/My Drive/1 Volunteers/Capstone Projects/2019-2020/Kate/Raw Image Data Reformatted CSVs OK")

  #  Read in all files in the abover folder and format
  mega.tblOK <- list.files(pattern = "*.csv") %>%
    map_dfr(~fread(., stringsAsFactors = FALSE)) %>%
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
      Color = as.character(NaturalMarks)
    )
  str(mega.tblOK)
  
  #  Second round, note the date structure is different
  setwd("G:/My Drive/1 Volunteers/Capstone Projects/2019-2020/Kate/Raw Image Data Timelapse Output OK")

  #  Read in data from above folder and format
  mega.tblOK2 <- list.files(pattern = "*.csv") %>%
    map_dfr(~fread(., stringsAsFactors = FALSE)) %>%  
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
      Color = as.character(NaturalMarks)
    )

  
  #  Merge NE tables
  full.tblNE <- rbind(mega.tblNE, mega.tblNE2)
  #  Merge OK tables
  full.tblOK <- rbind(mega.tblOK, mega.tblOK2)
  #  Merge NE & OK tables
  full.tbl <- rbind(full.tblNE, full.tblOK)
  
  #  Subset image data
  alldetections <- full.tbl %>%
    filter(Empty != "TRUE") %>%
    filter(Service != "TRUE")
  animals <- full.tbl %>%
    filter(Animal == "TRUE")
  # humans <- full.tbl %>%
  #   filter(Human == "TRUE")
  # cars <- full.tbl %>%
  #   filter(Vehicle == "TRUE")
  cougs <- full.tbl %>%
    filter(Species == "Cougar")
  # coug <- animals %>%
  #   filter(Species == "Cougar")
  deer <- full.tbl %>%
    filter(Species == "White-tailed Deer" | Species == "Mule Deer")
  
  #  Filter data to only contain wtd, human, and vehicle detections
  Drew.data <- full.tblNE %>%
    filter(Species == "White-tailed Deer" | Human == "TRUE" | Vehicle == "TRUE")
  #  Filter data to only contain cougar detections
  cougar.data <- full.tbl %>%
    filter(Species == "Cougar")
  #  Double check that we noted all the collars
  cougar.collars <- cougar.data %>%
    filter(Collars == "1") %>%
    select(CameraLocation, DateTime, Date, Time, Species, Count, Collars, Tags, AF, AM, AU, OS, UNK, File, RelativePath)

 
  write.csv(alldetections, "G:/My Drive/1 Volunteers/Capstone Projects/Bassing_AllDetections.csv")
  #write.csv(Drew.data, "G:/My Drive/1 Volunteers/Capstone Projects/Drew/Drew4data2.csv")
  write.csv(Drew.data, "G:/My Drive/1 Volunteers/Capstone Projects/Drew/DrewData_Updated051320.csv")
  write.csv(cougar.data, "G:/My Drive/1 Volunteers/Capstone Projects/Kate/CougarData.csv")
  #write.csv(cougar.collars, "G:/My Drive/1 Volunteers/Capstone Projects/Kate/CougarCollars.csv")
  write.csv(deer, "G:/Shared drives/Doe Fawn Ratio/Data/DeerData.csv")
  
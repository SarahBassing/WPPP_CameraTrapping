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
  library(data.table)
  library(chron)
  library(tidyverse)
  #library(camtrapR)
  #library(exifr)
  
  #  Read in data
  NE3815_C125 <- read.csv("./Processed Image Data/NE3815_28_C125_MSW_datetimeweird-cleaned.csv")
  OK4880_C175 <- read.csv("./Processed Image Data/OK4880_C175_TT_DATEOFF1DAY.csv")
  
  #  Remove data that does not need to be adjusted 
  #  DON'T FORGET TO ADD THESE BACK IN ONCE DATE & TIME ARE FIXED!!!
  NE5511_C186 <- read.csv("./Processed Image Data/NE5511_54_C168_C186_JM-DATETIME_WRONG-cleaned.csv") %>%
    filter(str_detect(RelativePath, paste("C168"), negate = TRUE)) 
  # another way to code it:
  # remove_if <- "C168"
  # filter(str_detect(RelativePath, paste(remove_if, collapse="|"), negate = TRUE)

  #  Example with a single csv
  NE3815 <- NE3815_C125 %>%
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
  
  str(NE3815)
  min(NE3815$Date); max(NE3815$Date)
  min(NE3815$DateTime); max(NE3815$DateTime)
  
  
  #  Use base R to change date and time
  #  Check out https://www.gormanalysis.com/blog/dates-and-times-in-r-without-losing-your-sanity/
  
  #  Move the date up 21 days
  #  Example:
  # ymd <- NE3815$Date
  # newymd <- ymd + 21
  # min(newymd); max(newymd)
  NE3815 <- NE3815 %>%
    mutate(RgtDate = Date + 21,
           WrgDate = Date) %>%
    select(-Date)
  
  #  Change date & time (using DateTime column)
  #  This is done by adding or subtracting n seconds to the DateTime
  #  Doesn't appear that I can do this with Time column with current format
  #  Examples:
  # dt <- NE3815$DateTime
  # dtday <- dt + 24*60*60 # add 1 day
  # dthr <- dt + 60*60  # add 1 hour
  # dtmin <- dt + 60  # add 1 minute
  # dtdayhr <- dt + 24*60*60 + 60*60 #  add 1 day & 1 hour
  # dtbckday <- dt - 24*60*60  # subtract 1 day
  NE3815 <- NE3815 %>%
    mutate(RgtDateTime = DateTime + 21*24*60*60 + 7*60*60 + 3*60, # plus 21 days, 7 hours, 2 minutes
           WrgDateTime = DateTime) %>%
    select(-DateTime)

  #  Reorganize entire dataframe to match other camera dataframes
  #  REMEMBER that the Time column is incorrect and should not be used for further analyses
  NE3815_C125_tst <- NE3815 %>%
    transmute(
      File = as.character(File),
      RelativePath = as.character(RelativePath),
      Folder = as.character(Folder),
      DateTime = RgtDateTime, 
      Date = RgtDate, 
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
      AF = as.numeric(AF),
      AM = as.numeric(AM),
      AU = as.numeric(AU),
      OS = as.numeric(OS),
      UNK = as.numeric(UNK),
      Collars = as.numeric(Collars),
      Tags = as.character(Tags),
      Color = as.character(Color)
    )
  
  str(NE3815_C125_tst)
  
  
  
  
  
  
  
  
  ####  DOES NOT WORK  ####
  #  The timeShiftImages function apparently ONLY works on raw images, not on the
  #  already processed image data recorded in a csv  :(
  
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
  # no workie
  
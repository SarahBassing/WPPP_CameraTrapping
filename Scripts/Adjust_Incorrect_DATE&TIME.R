  ##  Correct Camera Trap Dates & Times
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
  
  #  Read in data where date & time are incorrect
  NE3000_S3_C18 <- read.csv("./Processed Image Data/NE3000_C18_S3_CH_REVIEWED_DATETIMEWRONG.csv")
  NE3109_S4_C31_C96_C131 <- read.csv("./Processed Image Data/NE3109_113, Moultrie3_C31, C96, C131, S4_SBB_REVIEWED.csv") 
  NE3815_C125 <- read.csv("./Processed Image Data/NE3815_28_C125_CH_REVIEWED_datetimeweird.csv")
  NE3815_C26_C61 <- read.csv("./Processed Image Data/NE3815_28_C26_61_CH_REVIEWED.csv") 
  NE5511_C168_C186 <- read.csv("./Processed Image Data/NE5511_54_C168_C186_JM-DATETIME_WRONG-cleaned.csv")
  OK4880_C175 <- read.csv("./Processed Image Data/OK4880_C175_TT_DATEOFF1DAY.csv")

  #  Step 1
  #  Function to format raw csv data so date, time, and other values are in a 
  #  consistent format that can be manipulated further
  #  Important time zones: 
  #  "America/Los_Angeles" = "PST8PDT" accounts for daylight savings, UTC/GMT -7 offset
  #  "America/Alaska" = "AKST9AKDT" accounts for daylight savings time, UTC/GMT -8 offset
  format_csv <- function(x) {
    format_raw <- x %>%
      transmute(
        File = as.character(File),
        RelativePath = as.character(RelativePath),
        Folder = as.character(Folder),
        DateTime = as.POSIXct(paste(Date, Time),  
                              format="%d-%b-%y %H:%M:%S",tz="America/Los_Angeles"), # update to %d-%b-%Y once all reviewed
        Date = as.Date(Date, format = "%d-%b-%y"), # update to %d-%b-%Y once all reviewed
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
    return(format_raw)
  }
  
  #  Bundle data that need to be reformatted in a list
  # raw_list <- list(NE3000_S3_C18, NE3109_S4_C31_C96_C131, NE3815_C26_C61,
  #                  NE3815_C125) #, NE5511_C168_C186, OK4880_C175
  raw_list <- list(NE5511_C168_C186)
  #  Run formatting function
  format_raw <- lapply(raw_list, format_csv)


  ## ============================================================  
  #  Step 2
  #  Use base R to change date and time
  #  This is done by adding or subtracting n days to Date and n seconds to DateTime
  #  Check out https://www.gormanalysis.com/blog/dates-and-times-in-r-without-losing-your-sanity/

  #  Separate each memory card out so individual date & time issues can be fixed
  #  Filter out memory cards with correct date & times
  #  Shift date by adding or subtracting days
  #  Shift date and time but adding or subtracting seconds (24*60*60 = 1 full day)
  #  Keep in mind that dates in Date vs DateTime may change when times is close to midnight
  #  Adjusting for incorrect shifts btwn PST & PDT are tricky
  
  NE3000_S3_C18 <- format_raw[[1]]
  NE3000_C18 <- NE3000_S3_C18 %>%
    filter(str_detect(RelativePath, paste("S3"), negate = TRUE)) %>%
  # Plus 1 day, munus 1 hour, 24 minutes
  # No need to worry about PDT vs PST here
    mutate(RgtDate = Date + 1,
           WrgDate = Date,
           RgtDateTime = DateTime + 24*60*60 - 60*60 - 24*60, 
           WrgDateTime = DateTime)
  NE3109_S4_C31_C96_C131 <- format_raw[[2]]
  
  NE3109_S4 <- NE3109_S4_C31_C96_C131 %>%
    filter(str_detect(RelativePath, paste("C31"), negate = TRUE)) %>%
    filter(str_detect(RelativePath, paste("C96"), negate = TRUE)) %>%
    filter(str_detect(RelativePath, paste("C131"), negate = TRUE)) %>%
  # Plus 6 years, 4 months, 20 days (1/1/12 to 5/21/18)
    mutate(RgtDate = Date + 2323,  
           WrgDate = Date,
           RgtDateTime = DateTime + 6*365*24*60*60 + 133*24*60*60,
           WrgDateTime = DateTime) %>%
  # Remove 2 servicing images with way different dates- too much work to correct & no need
    filter(File != "MFDC0001.JPG" & File != "MFDC0023.JPG" & File != "MFDC0024.JPG")
  
  NE3815_C26_C61 <- format_raw[[3]]
  NE3815_C61 <- NE3815_C26_C61 %>%
    filter(str_detect(RelativePath, paste("C26"), negate = TRUE)) %>%
  # Plus 21 days, 7 hours, 3 minutes    
    mutate(RgtDate = Date + 21,
           WrgDate = Date,
           RgtDateTime = DateTime + 21*24*60*60 + 7*60*60 + 3*60, 
           WrgDateTime = DateTime)
  
  # NE3815_C125 <- format_raw[[1]] %>% # update this number
  #     mutate(RgtDate = Date + 21,
  #            WrgDate = Date,
  #            RgtDateTime = DateTime + 21*24*60*60 + 7*60*60 + 3*60, # plus 21 days, 7 hours, 3 minutes
  #            WrgDateTime = DateTime)
  
  NE3815_C125 <- format_raw[[4]]
  # Extract section that shifted to PST based on incorrect dates
  NE3815_C125_PDT <- NE3815_C125[NE3815_C125$Date > "2018-11-03",] %>% 
  # Move forward 1 hour to put this section back into PDT
    mutate(DateTime = DateTime + 60*60)
  #  Combine so all data are on PDT now
  NE3815_C125 <- rbind(NE3815_C125[NE3815_C125$Date < "2018-11-04",], NE3815_C125_PDT) %>% 
  # Shift all data to correct date and time based on placard info
  # Plus 21 days, 7 hours, 3 minutes
    mutate(RgtDate = Date + 21,                                   
           WrgDate = Date,
           RgtDateTime = DateTime + 21*24*60*60 + 7*60*60 + 3*60, 
           WrgDateTime = DateTime)
  # NOTE: shift btwn PDT & PST is accounted for during the addition above
  # (Only 6 hr difference during time period btwn WrgDateTime 11/4/18 & RgtDateTime 11/14/18)
  # # Extract new section that needs to shift to PST based on now correct dates
  # NE3815_C125_PST <- NE3815_C125[NE3815_C125$RgtDate > "2018-11-03",] %>%
  # # Minus 1 hour to put back on PST after real 11/4/18
  #   mutate(RgtDateTime = RgtDateTime - 60*60, 
  #          WrgDateTime = DateTime)
  # # Combine so data have correct dates & times while appropriately accounting for PDT
  # NE3815_C125 <- rbind(NE3815_C125[NE3815_C125$RgtDate < "2018-11-04",], NE3815_C125_PST)
  
  #  THIS IS ACCOUNTING FOR DIFFERENCES IN PST & PDT ON BOTH SIDES while doing the subtraction... need to address this somehow
  NE5511_C168_C186 <- format_raw[[1]] # update this number
  NE5511_C186 <- NE5511_C168_C186 %>%
    filter(str_detect(RelativePath, paste("C168"), negate = TRUE)) %>%
    mutate(RgtDate = Date + 163,  # Date and DateTime are off when shifted times are close to midnight
           WrgDate = Date,
           RgtDateTime = DateTime + 163*24*60*60 - 4*60*60 - 37*60, # plus 163 days, minus 4 hours, 37 minutes (7/3/18 to 12/13/18)
           WrgDateTime = DateTime)
  #  Currently time of pull is 1 hour off, also C168 is 1 hour off of placard check time
  #  Things to keep in mind about NE5511 C186: b/c dates are wrong the time zone is
  #  also wrong (1st half PDT instead of PST) so need to shift everything to PST, 
  #  then correct date & time, then shift 2nd half to PDT once date is corrected
  #  But when I do this the new times are hours off from the camera check data TR provided
  NE5511_C168_C186 <- format_raw[[1]] # update this number
  NE5511_C186 <- NE5511_C168_C186 %>%
    filter(str_detect(RelativePath, paste("C168"), negate = TRUE))
  NE5511_PST <- NE5511_C186[NE5511_C186$Date < "2018-11-04",] %>%   # wrong 11/4/18; File = "RCNX2761.JPG"
    mutate(DateTime = DateTime - 60*60)                             # move back 1 hr to PST
  NE5511_C186_PST <- rbind(NE5511_PST, NE5511_C186[NE5511_C186$Date > "2018-11-03",]) %>% # all PST now; File = "RCNX2760.JPG"
    mutate(RgtDate = Date + 163,
           WrgDate = Date,
           RgtDateTime = DateTime + 163*24*60*60 - 3*60*60 - 37*60, # plus 163 days, minus 3 hours, 37 minutes (7/3/18 to 12/13/18)
           WrgDateTime = DateTime)
  NE5511_PDT <- NE5511_C186_PST[NE5511_C186_PST$RgtDate > "2019-03-09",] %>%  # real daylight savings time starts
    mutate(RgtDateTime = RgtDateTime + 60*60)  # plus 1 hr to PDT
  NE5511_C186 <- rbind(NE5511_C186_PST[NE5511_C186_PST$RgtDate < "2019-03-10",], NE5511_PDT)
  
  # OK4880_C175 <- format_raw[[6]] %>%
  #   mutate(RgtDate = Date + 1,
  #          WrgDate = Date,
  #          RgtDateTime = DateTime + 24*60*60, # plus 1 day
  #          WrgDateTime = DateTime)
  

  ## ==========================================================
  #  Step 3
  #  Function to reorganize shifted data to match other camera data
  #  REMEMBER that the Time column is incorrect and should not be used for further analyses
  reformat_csv <- function(x) {
    format_shiftdat <- x %>%
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
    return(format_shiftdat)
  }

  #  List dataframes that had date & time data shifted
  #shift_list <- list(NE3000_C18, NE3109_S4, NE3815_C61, NE3815_C125, NE5511_C186, OK4880_C175)
  shift_list <- list(NE3815_C125, NE5511_C186)
  #  Run reformatting function
  reformat_shiftdat <- lapply(shift_list, reformat_csv)
  #  Separate out individual cameras
  NE3815_C125shift <- reformat_shiftdat[[1]] # update this number
  NE5511_C186shift <- reformat_shiftdat[[2]] # update this number
  
  #  Add memory cards back in where the data & time were correct
  NE5511_C168 <- NE5511_C168_C186 %>%
    filter(str_detect(RelativePath, paste("C186"), negate = TRUE)) 
  NE5511_C168_C186shift <- rbind(NE5511_C168, NE5511_C186shift)   #NOT QUITE RIGHT- I THINK DAYLIGHT SAVINGS TIME WAS NOT ADJUSTED FOR AT SOME POINT- NEED TO FIX

  
  #  From here, source this script to merge these corrected data sets in with 
  #  other processed & formatted image data
  
  
  ## ====================================================
  # ####  Example with a single csv  ####
  # NE3815 <- NE3815_C125 %>% 
  #   transmute(
  #     File = as.character(File),
  #     RelativePath = as.character(RelativePath),
  #     Folder = as.character(Folder),
  #     DateTime = as.POSIXct(paste(Date, Time),
  #                           format="%d-%b-%y %H:%M:%S",tz="America/Los_Angeles"), 
  #     Date = as.Date(Date, format = "%d-%b-%y"), 
  #     Time = chron(times = Time),
  #     ImageQuality = as.factor(ImageQuality),
  #     CameraLocation = as.factor(as.character(CameraLocation)),
  #     DT_Good = as.factor(as.character(DT_Good)),
  #     Service = as.factor(as.character(Service)),
  #     Empty = as.factor(as.character(Empty)),
  #     Animal = as.factor(as.character(Animal)),
  #     Human = as.factor(as.character(Human)),
  #     Vehicle = as.factor(as.character(Vehicle)),
  #     Species = as.factor(as.character(Species)),
  #     HumanActivity = as.factor(as.character(HumanActivity)),
  #     Count = as.numeric(Count),
  #     AF = as.numeric(AdultFemale),
  #     AM = as.numeric(AdultMale),
  #     AU = as.numeric(AdultUnknown),
  #     OS = as.numeric(Offspring),
  #     UNK = as.numeric(UNK),
  #     Collars = as.numeric(Collars),
  #     Tags = as.character(Tags),
  #     Color = as.character(NaturalMarks)
  #   )
  # 
  # str(NE3815)
  # min(NE3815$Date); max(NE3815$Date)
  # min(NE3815$DateTime); max(NE3815$DateTime)
  # 
  # 
  # #  Use base R to change date and time
  # #  Check out https://www.gormanalysis.com/blog/dates-and-times-in-r-without-losing-your-sanity/
  # 
  # #  Move the date up 21 days
  # #  Example:
  # # ymd <- NE3815$Date
  # # newymd <- ymd + 21
  # # min(newymd); max(newymd)
  # NE3815 <- NE3815 %>%
  #   mutate(RgtDate = Date + 21,
  #          WrgDate = Date) %>%
  #   select(-Date)
  # 
  # #  Change date & time (using DateTime column)
  # #  This is done by adding or subtracting n seconds to the DateTime
  # #  Doesn't appear that I can do this with Time column with current format
  # #  Examples:
  # # dt <- NE3815$DateTime
  # # dtday <- dt + 24*60*60 # add 1 day
  # # dthr <- dt + 60*60  # add 1 hour
  # # dtmin <- dt + 60  # add 1 minute
  # # dtdayhr <- dt + 24*60*60 + 60*60 #  add 1 day & 1 hour
  # # dtbckday <- dt - 24*60*60  # subtract 1 day
  # NE3815 <- NE3815 %>%
  #   mutate(RgtDateTime = DateTime + 21*24*60*60 + 7*60*60 + 3*60, # plus 21 days, 7 hours, 2 minutes
  #          WrgDateTime = DateTime) %>%
  #   select(-DateTime)
  # 
  # #  Reorganize entire dataframe to match other camera dataframes
  # #  REMEMBER that the Time column is incorrect and should not be used for further analyses
  # NE3815_C125_tst <- NE3815 %>%
  #   transmute(
  #     File = as.character(File),
  #     RelativePath = as.character(RelativePath),
  #     Folder = as.character(Folder),
  #     DateTime = RgtDateTime, 
  #     Date = RgtDate, 
  #     Time = chron(times = Time),
  #     ImageQuality = as.factor(ImageQuality),
  #     CameraLocation = as.factor(as.character(CameraLocation)),
  #     DT_Good = as.factor(as.character(DT_Good)),
  #     Service = as.factor(as.character(Service)),
  #     Empty = as.factor(as.character(Empty)),
  #     Animal = as.factor(as.character(Animal)),
  #     Human = as.factor(as.character(Human)),
  #     Vehicle = as.factor(as.character(Vehicle)),
  #     Species = as.factor(as.character(Species)),
  #     HumanActivity = as.factor(as.character(HumanActivity)),
  #     Count = as.numeric(Count),
  #     AF = as.numeric(AF),
  #     AM = as.numeric(AM),
  #     AU = as.numeric(AU),
  #     OS = as.numeric(OS),
  #     UNK = as.numeric(UNK),
  #     Collars = as.numeric(Collars),
  #     Tags = as.character(Tags),
  #     Color = as.character(Color)
  #   )
  # 
  # str(NE3815_C125_tst)
  # 
  # #### DON'T FORGET  ####
  # #  Add memory cards back to cameras that had data from multiple cards where 
  # #  date & time were correct
  # #  NE3000 S3; NE3109 C31, C96, C131; NE3815 C26; NE5511 C168
  
  
  
  
  ## ==========================================================
  ####  DOES NOT WORK  ####
  # #  The timeShiftImages function apparently ONLY works on raw images, not on the
  # #  already processed image data recorded in a csv  :(
  # 
  # #library(camtrapR)
  # #library(exifr)
  # #  Make sure R can find ExifTool
  # #  (Necessary for timeShiftImages funciton to work)
  # #  Not sure how this all works but make sure the path leads to the exiftool.exe 
  # #  file (remove (-k) if included in the file name). 
  # #  More here: https://exiftool.org/#shift & https://groups.google.com/g/camtrapr/c/kAoUZuBUt8o
  # system("exiftool")
  # exiftool_dir <- "C:/exiftool-12.05"      
  # exiftoolPath(exiftoolDir = exiftool_dir)
  # grepl(exiftool_dir,  Sys.getenv("PATH"))
  # 
  # #  Read in data
  # NE3815 <- read.csv("NE3815_28_C125_MSW_datetimeweird-cleaned.csv")
  # OK4880 <- read.csv("OK4880_C175_TT_DATEOFF1DAY.csv")
  # timeShift <- read.csv("G:/My Drive/1 Data/Image Processing/timeShiftTable.csv")
  # 
  # #  Attempting to use timeShiftTable to adjust the date and time of camera data
  # #dat <- rbind(NE3815, OK4880)
  # tst <- timeShiftImages(inDir = , 
  #                       timeShiftTable = timeShift,
  #                       stationCol = "Station",
  #                       cameraCol = "camera",
  #                       hasCameraFolders = FALSE,
  #                       timeShiftColumn = "timeshift",
  #                       timeShiftSignColumn = "sign",
  #                       undo = FALSE)
  # # no workie
  
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
  library(lubridate)
  library(tidyverse)

  
  #  Read in data where date & time are incorrect
  NE3000_S3_C18 <- read.csv("./Reviewed Image Data/DATETIMEWRONG/NE3000_C18_S3_CH_REVIEWED_DATETIMEWRONG.csv")
  NE3109_S4_C31_C96_C131 <- read.csv("./Reviewed Image Data/DATETIMEWRONG/NE3109_113, Moultrie3_C31, C96, C131, S4_SBB_REVIEWED.csv") 
  NE3815_C125 <- read.csv("./Reviewed Image Data/DATETIMEWRONG/NE3815_28_C125_CH_REVIEWED_datetimeweird.csv")
  NE3815_C26_C61 <- read.csv("./Reviewed Image Data/DATETIMEWRONG/NE3815_28_C26_61_CH_REVIEWED_datetimewrongC61.csv") 
  NE5511_C168_C186 <- read.csv("./Reviewed Image Data/DATETIMEWRONG/NE5511_54_C168_C186_CH_REVIEWED-DATETIMEWRONG.csv")
  OK4880_C175 <- read.csv("./Reviewed Image Data/DATETIMEWRONG/OK4880_C175_CH_REVIEWED_DATEOFF1DAY.csv")

  # OK5712 & OK4944 need to be adjusted for Yr 2 data
  
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
                              format="%d-%b-%Y %H:%M:%S",tz="America/Los_Angeles"), # %d-%b-%y depending on date structure 
        Date = as.Date(Date, format = "%d-%b-%Y"), # %d-%b-%y same here
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
    return(format_raw)
  }
  
  #  Bundle data that need to be reformatted in a list
  raw_list <- list(NE3000_S3_C18, NE3109_S4_C31_C96_C131, NE3815_C26_C61,
                   NE3815_C125, NE5511_C168_C186, OK4880_C175)
  #raw_list <- list(NE5511_C168_C186)
  #  Run formatting function
  format_raw <- lapply(raw_list, format_csv)


  ## ============================================================  
  #  Step 2
  #  Use base R to change date and time
  #  This is done by adding or subtracting n days to Date and n seconds to DateTime
  #  Check out https://www.gormanalysis.com/blog/dates-and-times-in-r-without-losing-your-sanity/

  #  Separate each memory card out so individual date & time issues can be fixed
  #  Filter out memory cards with correct date & times
  #  Shift date and time but adding or subtracting seconds (24*60*60 = 1 full day)
  #  Separate updated DateTime into new Date and Time columns
  #  FYI, adjusting for incorrect shifts btwn PST & PDT is tricky
  
  NE3000_S3_C18 <- format_raw[[1]]
  NE3000_C18 <- NE3000_S3_C18 %>%
    filter(str_detect(RelativePath, paste("S3"), negate = TRUE)) %>%
  # Plus 1 day, munus 1 hour, 24 minutes
  # No need to worry about PDT vs PST here
    mutate(#RgtDate = Date + 1,
           #WrgDate = Date,
           WrgDateTime = DateTime,
           RgtDateTime = DateTime + 24*60*60 - 60*60 - 24*60, 
           RgtDate = date(RgtDateTime),
           RgtTime = format(as.POSIXct(RgtDateTime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"))
  
  NE3109_S4_C31_C96_C131 <- format_raw[[2]]
  NE3109_S4 <- NE3109_S4_C31_C96_C131 %>%
    filter(str_detect(RelativePath, paste("C31"), negate = TRUE)) %>%
    filter(str_detect(RelativePath, paste("C96"), negate = TRUE)) %>%
    filter(str_detect(RelativePath, paste("C131"), negate = TRUE)) %>%
  # Plus 6 years, 4 months, 20 days (1/1/12 to 5/21/18)
    mutate(#RgtDate = Date + 2323,  
           #WrgDate = Date,
           WrgDateTime = DateTime,
           RgtDateTime = DateTime + 6*365*24*60*60 + 133*24*60*60,
           RgtDate = date(RgtDateTime),
           RgtTime = format(as.POSIXct(RgtDateTime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
           ) %>%
  # Remove 2 servicing images with way different dates- too much work to correct & no need
    filter(File != "MFDC0001.JPG" & File != "MFDC0023.JPG" & File != "MFDC0024.JPG")
  
  NE3815_C26_C61 <- format_raw[[3]]
  NE3815_C61 <- NE3815_C26_C61 %>%
    filter(str_detect(RelativePath, paste("C26"), negate = TRUE)) %>%
  # Plus 21 days, 7 hours, 3 minutes    
    mutate(#RgtDate = Date + 21,
           #WrgDate = Date,
           WrgDateTime = DateTime,
           RgtDateTime = DateTime + 21*24*60*60 + 7*60*60 + 3*60, 
           RgtDate = date(RgtDateTime),
           RgtTime = format(as.POSIXct(RgtDateTime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"))
  
  NE3815_C125 <- format_raw[[4]]
  # Extract section that shifted to PST based on incorrect dates
  NE3815_C125_PDT <- NE3815_C125[NE3815_C125$Date > "2018-11-03",] %>% 
  # Move forward 1 hour to put this section back into PDT
    mutate(DateTime = DateTime + 60*60)
  #  Combine so all data are on PDT now
  NE3815_C125 <- rbind(NE3815_C125[NE3815_C125$Date < "2018-11-04",], NE3815_C125_PDT) %>% 
  # Shift all data to correct date and time based on placard info
  # Plus 21 days, 7 hours, 3 minutes
    mutate(#RgtDate = Date + 21,                                   
           #WrgDate = Date,
           WrgDateTime = DateTime,
           RgtDateTime = DateTime + 21*24*60*60 + 7*60*60 + 3*60,
           RgtDate = date(RgtDateTime),
           RgtTime = format(as.POSIXct(RgtDateTime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"))
  # NOTE: shift btwn PDT & PST is accounted for during the addition above
  # (Only 6 hr difference during time period btwn WrgDateTime 11/4/18 & RgtDateTime 11/14/18)

  NE5511_C168_C186 <- format_raw[[5]] 
  #  Card C186 is WAY off on date and time so need to adjust for that but also...
  #  It looks like camera did not adjust for daylight savings time
  NE5511_C186 <- NE5511_C168_C186 %>%
    filter(str_detect(RelativePath, paste("C168"), negate = TRUE))
  #  First pull out dates that correspond with when camera SHOULD have shifted its
  #  time from PDT to PST (clock should have fell back 11/4/18 02:00:00)
  #  And force the time back by one hour to adjust for that shift
  NE5511_PST <- NE5511_C186[NE5511_C186$Date > "2018-11-03",] %>%
    mutate(DateTime = DateTime - 60*60)
  #  Add images that have been adjusted to PST back to images that are currently in PDT
  #  in PDT and shift ALL images by the appropriate amount of time
  NE5511_C186_adj <- rbind(NE5511_C186[NE5511_C186$Date < "2018-11-04",], NE5511_PST) %>% 
  #  Plus 163 days, minus 3 hours, 37 minutes (7/3/18 to 12/13/18)
    mutate(#RgtDate = Date + 163,
           #WrgDate = Date,
           WrgDateTime = DateTime,
           RgtDateTime = DateTime + 163*24*60*60 - 4*60*60 - 37*60, 
           RgtDate = date(RgtDateTime),
           RgtTime = format(as.POSIXct(RgtDateTime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"))
  #  Note: this should match up with the dates and times recorded in the placards
  #  and datasheets. C186 deployed 12/13/18 10:11am, and pulled 7/15/19 08:25am
  #  Card C168 is also off by an hour- assuming the camera did not adjust for 
  #  daylight savings time during this deployment either
  NE5511_C168 <- NE5511_C168_C186 %>%
    filter(str_detect(RelativePath, paste("186"), negate = TRUE))
  NE5511_PST <- NE5511_C168[NE5511_C168$Date > "2018-11-03",] %>%
    mutate(DateTime = DateTime - 60*60)  
  NE5511_C168_adj <- rbind(NE5511_C168[NE5511_C168$Date < "2018-11-04",], NE5511_PST) %>%
    #  Add these columns in for consistency
    mutate(#RgtDate = Date,
           #WrgDate = Date,          
           WrgDateTime = DateTime,  
           RgtDateTime = DateTime,
           RgtDate = date(RgtDateTime),  # Date is actually fine
           RgtTime = format(as.POSIXct(RgtDateTime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"))  
  
  #  Merge both adjusted NE5511 memory cards back together
  NE5511_C168_C186 <- rbind(NE5511_C168_adj, NE5511_C186_adj) 
  
  OK4880_C175 <- format_raw[[6]] %>%
  #  Plus 1 day
    mutate(#RgtDate = Date + 1,
           #WrgDate = Date,
           WrgDateTime = DateTime,
           RgtDateTime = DateTime + 24*60*60,
           RgtDate = date(RgtDateTime),  
           RgtTime = format(as.POSIXct(RgtDateTime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S"))
  

  ## ==========================================================
  #  Step 3
  #  Function to reorganize shifted data to match other camera data
  reformat_csv <- function(x) {
    format_shiftdat <- x %>%
      transmute(
        File = as.character(File),
        RelativePath = as.character(RelativePath),
        Folder = as.character(Folder),
        DateTime = RgtDateTime,
        #DateTime = as.POSIXct(RgtDateTime, format="%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles"),
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
        Color = as.character(Color),
        SecondOp = as.factor(as.character(SecondOp))
      )
    return(format_shiftdat)
  }

  #  List data frames that had date & time data shifted
  shift_list <- list(NE3000_C18, NE3109_S4, NE3815_C61, NE3815_C125, NE5511_C168_C186, OK4880_C175)
  #shift_list <- list(NE5511_C168_C186)
  #shift_list <- list(NE3000_C18, NE3109_S4, NE3815_C61, NE3815_C125, OK4880_C175)
  #  Run reformatting function
  reformat_shiftdat <- lapply(shift_list, reformat_csv)
  #  Separate out individual cameras
  NE3000_C18shift <- reformat_shiftdat[[1]]
  NE3109_S4shift <- reformat_shiftdat[[2]]
  NE3815_C61shift <- reformat_shiftdat[[3]]
  NE3815_C125shift <- reformat_shiftdat[[4]]
  NE5511_C168_C186shift <- reformat_shiftdat[[5]]
  OK4880_C175shift <- reformat_shiftdat[[6]]
  
  #  Add additional memory cards (with good date/times) back to adjusted cameras
  NE3000_S3 <- NE3000_S3_C18 %>%
    filter(str_detect(RelativePath, paste("C18"), negate = TRUE))
  NE3000_S3_C18_DTGood <- rbind(NE3000_S3, NE3000_C18shift)
  
  NE3109_C31_C96_C131 <- NE3109_S4_C31_C96_C131 %>%
    filter(str_detect(RelativePath, paste("S4"), negate = TRUE))
  NE3109_S4_C31_C96_C131_DTGood <- rbind(NE3109_S4shift, NE3109_C31_C96_C131)
  
  NE3815_C26 <- NE3815_C26_C61 %>%
    filter(str_detect(RelativePath, paste("C61"), negate = TRUE))
  NE3815_C26_C61_DTGood <- rbind(NE3815_C26, NE3815_C61shift)
  
  #  Rename the rest of the cameras so they are consistent for later analyses
  NE3815_C125_DTGood <- NE3815_C125shift
  NE5511_C168_C186_DTGood <- NE5511_C168_C186shift
  OK4880_C175_DTGood <- OK4880_C175shift
  
  #  From here, source this script to merge these corrected data sets in with 
  #  other processed & formatted image data
  #  Print the names of the cameras included here
  print(adjusted <- c("NE3000_S3_C18_DTGood", "NE3109_S4_C31_C96_C131_DTGood", "NE3815_C26_C61_DTGood", 
          "NE3815_C125_DTGood", "NE5511_C168_C186_DTGood", "OK4880_C175_DTGood"))

  
  
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
  # #  Separate out Date and Time based on shifted DateTime
  # require(lubridate)
  # tstdate <- OK4880_C175 %>%
  #   mutate(
  #     NewDate = date(RgtDateTime),
  #     NewTime = format(as.POSIXct(RgtDateTime, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S") 
  #   )
  # 
  # #  Reorganize entire dataframe to match other camera dataframes
  # #  REMEMBER that the Time column is incorrect and should not be used for further analyses
  #NE3815_C125_tst <- NE3815 %>%
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
  

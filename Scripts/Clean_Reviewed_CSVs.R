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
  
  ## Clean workspace & Load packages
  
  rm(list = ls())
  
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
  ##  %m = month (decimal number); %b month (abbreviated); %B = month (full name)
  ##  %y = year (2 digit); %Y = year (4 digit)
  
  
  #  Read in all csv files together, force all columns to characters, reformat 
  #  columns so all are consistent & of desired format
  mydir <- "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Test"
  csv_files <- list.files(path = mydir, pattern = "*.csv", full.names = TRUE) %>% 
    #  col_types forces all columns to be characters
    #  Forcing to character addresses issue w/ inconsistent typecasting of columns
    map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
    mutate(
      #  Reformat dates based on the structure of the character string
      DateNew = ifelse(nchar(Date) == 11, 
                       format(as.Date(parse_date_time(Date,"dbY")), "%Y-%m-%d"), Date),
      DateNew = ifelse(nchar(Date) <= 9, 
                       format(as.Date(parse_date_time(Date,"dby")), "%Y-%m-%d"), DateNew),
      #  Add extra columns to identify potential errors in date conversion
      #  Correct date format should have 10 characters (YYYY-MM-DD)
      Date_10char = ifelse(nchar(DateNew) != 10, "Fail", "Good"),
      #  Correct date format should be in the 2000's (not 0018, etc.)
      Date_2000 = ifelse(year(DateNew) == 0018 | year(DateNew) == 0019 | year(DateNew) == 0020, 
                         "Fail", "Good"),
      CameraLocation = as.factor(as.character(CameraLocation)),
      #  Reformat columns to the desired format
      DT_Good = ifelse(DT_Good == "true", "TRUE", DT_Good),
      DT_Good = ifelse(DT_Good == "false", "FALSE", DT_Good),
      DT_Good = as.factor(as.character(DT_Good)),
      Service = ifelse(Service == "true", "TRUE", Service),
      Service = ifelse(Service == "false", "FALSE", Service),
      Service = as.factor(as.character(Service)),
      Empty = ifelse(Empty == "true", "TRUE", Empty),
      Empty = ifelse(Empty == "false", "FALSE", Empty),
      Empty = as.factor(as.character(Empty)),
      Animal = ifelse(Animal == "true", "TRUE", Animal),
      Animal = ifelse(Animal == "false", "FALSE", Animal),
      Animal = as.factor(as.character(Animal)),
      Human = ifelse(Human == "true", "TRUE", Human),
      Human = ifelse(Human == "false", "FALSE", Human),
      Human = as.factor(as.character(Human)),
      Vehicle = ifelse(Vehicle == "true", "TRUE", Vehicle),
      Vehicle = ifelse(Vehicle == "false", "FALSE", Vehicle),
      Vehicle = as.factor(as.character(Vehicle)),
      Species = as.character(Species),
      HumanActivity = as.character(HumanActivity),
      Count = as.numeric(Count),
      AdultFemale = as.numeric(AdultFemale),
      AdultMale = as.numeric(AdultMale), 
      AdultUnknown = as.numeric(AdultUnknown),
      Offspring = as.numeric(Offspring),
      UNK = as.numeric(UNK),
      Collars = as.numeric(Collars),
      Tags = as.character(Tags),
      NaturalMarks = as.character(NaturalMarks),
      SecondOpinion = ifelse(SecondOpinion == "true", "TRUE", SecondOpinion),
      SecondOpinion = ifelse(SecondOpinion == "false", "FALSE", SecondOpinion),
      SecondOpinion = as.factor(as.character(SecondOpinion)),
      # SecondOpinion = as.numeric(SecondOpinion),
      Comments = as.character(Comments)
    )
  
  #  Check for rows where the date format is incorrect
  fail_10char <- csv_files[csv_files$Date_10char == "Fail",]
  head(fail_10char)
  fail_2000 <- csv_files[csv_files$Date_2000 == "Fail",]
  head(fail_2000)
  
  #  Final structuring and formatting of detection data
  megadata <- as.data.frame(csv_files) %>%
      transmute(
        File = as.character(File),
        RelativePath = as.character(RelativePath),
        Folder = as.character(Folder),
        DateTime = as.POSIXct(paste(DateNew, Time),
                              format="%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles"),
        Date = as.Date(DateNew, format = "%Y-%m-%d"),
        # Date = as.Date(Date, format = "%d-%b-%Y"),
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
  
  #  Does it all add up?
  table(megadata$Species, useNA = "ifany")
  sum(table(megadata$Species, useNA = "ifany"))
  nrow(megadata)
  sum(table(megadata$HumanActivity, useNA = "ifany"))
  table(megadata$HumanActivity, useNA = "ifany")
  
  
  
  
  
  # NE2947 <- read.csv("G:/My Drive/1_Repositories/WPPP_CameraTrapping/Test/NE2947_12_C24_C81_CH_REVIEWED.csv")
  # hm <- file_list[[6]]
  # 
  # 
  # tst <- file_list[[9]] %>%
  #   mutate(
  #     #DateNew = ifelse(nchar(Date) == 11, as.Date(Date, format = "%d-%b-%Y"), Date)
  #     #DateNew = ifelse(nchar(Date) <= 9, as.Date(Date, format = "%d-%b-%y"), Date)
  #     #DateNew = ifelse(nchar(Date) == 11, format(as.Date(Date, format = "%d-%b-%Y"), "%Y-%m-%d"), Date)
  #     #DateNew = ifelse(nchar(Date) <= 9, format(as.Date(Date, format = "%d-%b-%y"), "%Y-%m-%d"), Date)
  #     DateNew = ifelse(nchar(Date) == 11, format(as.Date(parse_date_time(Date,"dbY")), "%Y-%m-%d"), Date),
  #     DateNew = ifelse(nchar(Date) <= 9, format(as.Date(parse_date_time(Date,"dby")), "%Y-%m-%d"), DateNew)
  #     # DateNew = format(as.Date(Date, format = "%d-%b-%Y"), "%Y-%m-%d"),
  #     # DateNew = as.Date(DateNew, format = "%Y-%m-%d")
  #   )
  # 
  # 
  # for(i in 1:nrow(file_list[[9]])){
  #   if (nchar(file_list[[9]]$Date) == 11) tst = format(as.Date(file_list[[9]]$Date, format = "%d-%b-%Y"), "%Y-%m-%d")
  #   else(if (nchar(file_list[[9]]$Date) <= 9) tst = format(as.Date(file_list[[9]]$Date, format = "%d-%b-%y"), "%Y-%m-%d")
  #        else tst = file_list[[9]]$Date)
  # }
  # 
  # format_date <- function(dat) {
  #   tst <- c()
  #   for(i in 1:nrow(dat)){
  #     if (nchar(dat$Date) == 11) tst = format(as.Date(dat$Date, format = "%d-%b-%Y"), "%Y-%m-%d")
  #       else(if (nchar(dat$Date) <= 9) tst = format(as.Date(dat$Date, format = "%d-%b-%y"), "%Y-%m-%d")
  #            else tst = dat$Date)
  #   }
  #   GoodDate <- as.data.frame(tst)  
  #   datfinal <- cbind(dat, GoodDate)
  #   return(datfinal)
  # }
  # 
  # format_date <- function(dat) {
  #   tst <- c()
  #   for(i in 1:nrow(dat)){
  #     if (nchar(dat$Date) == 11) tst = format(as.Date(parse_date_time(dat$Date,"dbY")), "%Y-%m-%d")
  #     else(if (nchar(dat$Date) <= 9) tst = format(as.Date(parse_date_time(dat$Date,"dby")), "%Y-%m-%d")
  #          else tst = dat$Date)
  #   }
  #   GoodDate <- as.data.frame(tst)  
  #   datfinal <- cbind(dat, GoodDate)
  #   return(datfinal)
  # }
  # 
  # format_date <- function(dat) {
  #     tst <- dat %>%
  #       mutate(
  #         #  Reformat dates based on the structure of the character string
  #         DateNew = ifelse(nchar(Date) == 11, 
  #                          format(as.Date(parse_date_time(Date,"dbY")), "%Y-%m-%d"), Date),
  #         DateNew = ifelse(nchar(Date) <= 9, 
  #                          format(as.Date(parse_date_time(Date,"dby")), "%Y-%m-%d"), DateNew),
  #         #  Correct date format should have 10 characters (YYYY-MM-DD)
  #         Date_10char = ifelse(nchar(DateNew) != 10, "Fail", "Good"),
  #         #  Correct date format should be in the 2000's (not 0018, etc.)
  #         Date_2000 = ifelse(year(DateNew) == 0018 | year(DateNew) == 0019 | year(DateNew) == 0020, 
  #                            "Fail", "Good")
  #       )
  #   return(tst)
  # }
  # 
  # format_TF <- function(dat) {
  #   tst <- dat %>%
  #     mutate(
  #       Service = ifelse(Service == "true", "TRUE", Service),
  #       Service = ifelse(Service == "false", "FALSE", Service),
  #       Service = as.factor(as.character(Service)),
  #       Empty = ifelse(Empty == "true", "TRUE", Empty),
  #       Empty = ifelse(Empty == "false", "FALSE", Empty),
  #       Empty = as.factor(as.character(Empty)),
  #       # Animal = ifelse(Animal == "true", "TRUE", Animal),
  #       # Animal = ifelse(Animal == "false", "FALSE", Animal),
  #       # Animal = as.factor(as.character(Animal)),
  #       # Human = ifelse(Human == "true", "TRUE", Human),
  #       # Human = ifelse(Human == "false", "FALSE", Human),
  #       # Human = as.factor(as.character(Human)),
  #       # Vehicle = ifelse(Vehicle == "true", "TRUE", Vehicle),
  #       # Vehicle = ifelse(Vehicle == "false", "FALSE", Vehicle),
  #       # Vehicle = as.factor(as.character(Vehicle)),
  #       Species = as.character(Species),
  #       HumanActivity = as.character(HumanActivity)
  #     )
  # }
  # 
  # 
  # # mylist <- list(file_list[[1]], file_list[[2]], file_list[[3]], file_list[[9]]) #file_list[[1]], file_list[[2]], file_list[[3]], file_list[[8]], file_list[[9]]
  # wah <- lapply(file_list, format_date)
  # wah <- lapply(file_list, format_TF)
  # wah1 <- wah[[1]]
  # wah2 <- wah[[2]]
  # wah3 <- wah[[3]]
  # wah4 <- wah[[4]]
  # wah5 <- wah[[5]]
  # wah6 <- wah[[6]]
  # wah7 <- wah[[7]]
  # wah8 <- wah[[8]]
  # wah9 <- wah[[9]]
  # wah10 <- wah[[10]]
  #   
  #   mutate(
  #     #DateNew = ifelse(nchar(Date) == 11, as.Date(Date, format = "%d-%b-%Y"), Date)
  #     #DateNew = ifelse(nchar(Date) <= 9, as.Date(Date, format = "%d-%b-%y"), Date)
  #     tst = ifelse(nchar(Date) == 11, format(as.Date(Date, format = "%d-%b-%Y"), "%Y-%m-%d"), Date)
  #     #tst = ifelse(nchar(Date) <= 9, format(as.Date(Date, format = "%d-%b-%y"), "%Y-%m-%d"), Date)
  #     # DateNew = format(as.Date(Date, format = "%d-%b-%Y"), "%Y-%m-%d"),
  #     # DateNew = as.Date(DateNew, format = "%Y-%m-%d")
  #   ) 
  
  
    
  
  
  
  
  #  Reads in data processed by 3 independent reviewers
  mydir <- "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Reviewed Image Data/Year 1"
  myfiles <- list.files(path = mydir, pattern = "*.csv", full.names = TRUE)
  partial_csv1 <- ldply(myfiles, read_csv, col_names = TRUE) %>%
    transmute(
      File = as.character(File),
      RelativePath = as.character(RelativePath),
      Folder = as.character(Folder),
      Date = as.Date(Date, format = "%d-%b-%Y"),
      Time = chron(times = Time),
      DateTime = as.POSIXct(paste(Date, Time),
                            format="%d-%b-%Y %H:%M:%S",tz="America/Los_Angeles"),
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
  
  #  For some reason all classification data are changed to "NA" in these image 
  #  sets. Likely to do with some data being recognized as logical vs characters.
  #  Not sure why this is happening... very annoying.
  #  Read in separately and the problem doesn't occur
  mydir4 <- "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Reviewed Image Data/Why_are_you_weird"
  partial_weird1 <- list.files(path = mydir4, pattern = "*.csv", full.names = TRUE) %>% 
    lapply(read.csv, stringsAsFactors = F) %>% 
    bind_rows %>%
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
  
  #  Same deal with un-reviewed files
  mydir5 <- "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Processed Image Data/Year 1/Why_are_you_weird"
  partial_weird2 <- list.files(path = mydir5, pattern = "*.csv", full.names = TRUE) %>% 
    lapply(read.csv, stringsAsFactors = F) %>% 
    bind_rows %>%
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
  
  #  This one really doesn't want to get with the program so reading in separately
  prob1 <- read.csv("G:/My Drive/1_Repositories/WPPP_CameraTrapping/Reviewed Image Data/Problems/NE1487_30_C178_SBB_REVIEWED.csv")
  #  Date format of 1st row is weird, need to drop it (service image so no big loss)
  #  And drop extra empty column at end of data frame
  prob1 <- prob1[-1,-34]
  prob2 <- read.csv("G:/My Drive/1_Repositories/WPPP_CameraTrapping/Processed Image Data/Year 1/Problems/NE4771_95_C95&C24_AMR-vehicles.csv")
  prob3 <- read.csv("G:/My Drive/1_Repositories/WPPP_CameraTrapping/Reviewed Image Data/Problems/NE5022_56_C115_KB-vehicles_REVIEWED.csv")
  prob3 <- prob3[,-34]
  probs <- rbind(prob1, prob2, prob3) %>%  # 
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
  
  
  #  Source "Adjust_Incorrect_DATE&TIME.R" script to bring in cameras with known
  #  and corrected date/time issues.
  source("./Scripts/Adjust_Incorrect_DATE&TIME.R")
  
  #  Append sourced data to MEGA data file
  full_csv <- rbind(megadata,
                    NE3000_S3_C18_DTGood, NE3109_S4_C31_C96_C131_DTGood,
                    NE3815_C26_C61_DTGood, NE3815_C125_DTGood, 
                    NE5511_C168_C186_DTGood, OK4880_C175_DTGood) 
    # full_csv <- rbind(partial_csv1, partial_csv2, partial_csv3, partial_weird1, 
    #                   partial_weird2, probs,
    #                   NE3000_S3_C18_DTGood, NE3109_S4_C31_C96_C131_DTGood,
    #                   NE3815_C26_C61_DTGood, NE3815_C125_DTGood, 
    #                   NE5511_C168_C186_DTGood, OK4880_C175_DTGood) %>%
    #  Drop service image with incorrect data/time before camera was fully set
    filter(CameraLocation != "NE5853_Moultrie5" | File != "MFDC0001.JPG")
  
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
  #  Has to do with mystery formatting of original csv files. New method for 
  #  reading in raw csv files should have taken care of this.
  
  #  Which ones are reading in as lower-cased "true"/"false"? This is causing problems
  droplevels(unique(full_csv$CameraLocation[full_csv$Animal == "true"]))
  #  Again, new method of reading in raw csv files should have fixed this
  
  
  #  Identify which cameras have images that were never reviewed or where info
  #  was incorrectly propagated across empty images
  droplevels(unique(full_csv$CameraLocation[full_csv$Service == "FALSE" & 
                                              full_csv$Empty == "FALSE" & 
                                              full_csv$Animal == "FALSE" & 
                                              full_csv$Human == "FALSE" & 
                                              full_csv$Vehicle == "FALSE"]))
  #  BRB, gotta fix these

  #  Identify which images still need a second review
  unique(full_csv$CameraLocation[which(full_csv$SecondOp == "TRUE" | full_csv$SecondOp == "true")])
  #  BRB, gotta go check these

  
  ####  FILTER DATA & SAVE  ####
  ##  ---------------------------------------------
  
  #  Filter service and empty images out (shouldn't have any lower-case T/F but just in case)
  alldetections <- full_csv %>%
    filter(Empty != "TRUE" & Empty != "true") %>%
    filter(Service != "TRUE" & Service != "true") 
  
  #  Consider adding this to alldetections to make for a cleaner data set
  #  Every detection would have something listed under the Species column
  #  But need to make sure it's not hiding errors in the data
  #  And would need to fill these in for Service & Empty images as well
  #  Need to have something in the Species column for each detection
  # mutate(
  #   Species = ifelse(Human == "TRUE" | Human == "true", "Human", Species),
  #   Species = ifelse(Vehicle == "TRUE" | Vehicle == "true", "Vehicle", Species),
  #   Species = ifelse(Species == "", "NA", Species),
  #   HumanActivity = ifelse(HumanActivity == "", "NA", HumanActivity)
  # )

  #  Save for later analyses
  write.csv(alldetections, paste0('./Output/Bassing_AllDetections_', Sys.Date(), '.csv'))

  
  #  Filter for capstone students
  #  Coyote, bobcat & human detections for Alyssa
  meso <- alldetections %>%
    filter(Species == "Bobcat" | Species == "Coyote") %>%
    filter(!grepl("Moultrie", CameraLocation))
  #  Human detections (on foot and vehicle)
  humans <- alldetections %>%
    filter(Human == "TRUE" | Human == "true" | Vehicle == "TRUE" | Vehicle == "true") %>%
    filter(!grepl("Moultrie", CameraLocation)) %>%
    mutate(
      Species = ifelse(Species == "Human", NA, as.character(Species))
    )
  Alyssa_data <- rbind(meso, humans)
  write.csv(Alyssa_data, paste0('G:/My Drive/1 Volunteers/Capstone Projects/Alyssa/Alyssa_data_', Sys.Date(), '.csv'))

  #  All study species detections for Jessalyn
  allstudyspp <- alldetections %>%
    filter(Species == "Elk" | Species == "Moose" | Species == "Mule Deer" | 
           Species == "White-tailed Deer" | Species == "Cougar" | Species == "Wolf" | 
           Species == "Black Bear" | Species == "Bobcat" | Species == "Coyote" | 
           Species == "Turkey" | Species == "Snowshoe Hare" | Species == "Rabbit Spp") %>%
    filter(!grepl("Moultrie", CameraLocation))
  # write.csv(allstudyspp, paste0('G:/My Drive/1 Volunteers/Capstone Projects/Jessalyn/Bassing_AllStudySpecies_', Sys.Date(), '.csv'))
  
  #  Cougar detections
  cougar.data <- full.tbl %>%
    filter(Species == "Cougar")
  #  Double check that we noted all the collars
  cougar.collars <- cougar.data %>%
    filter(Collars == "1") %>%
    select(CameraLocation, DateTime, Date, Time, Species, Count, Collars, Tags, AF, AM, AU, OS, UNK, File, RelativePath)
  
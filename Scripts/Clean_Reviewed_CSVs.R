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
  ##  Script then identifies if cameras have any date & time errors and if any
  ##  images are still marked "Second Opinion". Script finally filters service
  ##  and empty images out and saves for further prep and analysis.
  ##  ------------------------------------------------------------
  
  ## Clean work space & load packages
  
  rm(list = ls())
  
  library(chron)
  library(plyr)
  library(readr)
  library(tidyverse)
  library(lubridate)

  
  ####  READ IN REVIEWED CAMERA DATA  ####
  #'  --------------------------------------------------------  
  #'  SCREW YOU POSIXct
  #'  %d = day of month (decimal number)
  #'  %m = month (decimal number); %b month (abbreviated); %B = month (full name)
  #'  %y = year (2 digit); %Y = year (4 digit)
  
  #'  Read in all csv files together, force all columns to characters, reformat
  #'  columns so all are consistent & of desired format
  #'  REVIEWED DATA
  # mydir <- "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Reviewed Image Data/Year 1"  #Year 1
  # mydir <- "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Test"
  read_files <- function(mydir) {
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
        Date_2000 = ifelse(year(DateNew) == 0018 | year(DateNew) == 0019 | 
                           year(DateNew) == 0020 | year(DateNew) == 0021, 
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
        Comments = as.character(Comments)
      )
    return(csv_files)
  }
  
  #'  Run data from each year through function (processed and REVIEWED data)
  #'  Note Year 1 data has an extra empty column that makes merging with Year 2 data tough
  mydir <- list("G:/My Drive/1_Repositories/WPPP_CameraTrapping/Reviewed Image Data/Year 1",
                "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Reviewed Image Data/Year 2",
                "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Reviewed Image Data/Year 3")
  
  read_dat <- lapply(mydir, read_files)
  #'  Warnings are due to an extra empty column at end of csv files- ignore
  
  #' #'  Check for rows where the date format is incorrect
  #' fail_10char <- csv_files[csv_files$Date_10char == "Fail",]
  #' head(fail_10char)
  #' fail_2000 <- csv_files[csv_files$Date_2000 == "Fail",]
  #' head(fail_2000)
  #' #'  And rows where there's no species or human activity but count data
  #' missing_data <- csv_files[is.na(csv_files$Species) & is.na(csv_files$HumanActivity) & csv_files$Count > 0,]
  #' print(droplevels(unique(missing_data$CameraLocation)))
  #' #'  BRB gotta go fix these!
  #' 
  #' #'  PROCESSED DATA BUT NOT REVIEWED YET
  #' mydir <- "G:/My Drive/1_Repositories/WPPP_CameraTrapping/Processed Image Data/Year 2" 
  #' csv_files2 <- list.files(path = mydir, pattern = "*.csv", full.names = TRUE) %>%
  #'   #  col_types forces all columns to be characters
  #'   #  Forcing to character addresses issue w/ inconsistent typecasting of columns
  #'   map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
  #'   mutate(
  #'     #  Reformat dates based on the structure of the character string
  #'     DateNew = ifelse(nchar(Date) == 11,
  #'                      format(as.Date(parse_date_time(Date,"dbY")), "%Y-%m-%d"), Date),
  #'     DateNew = ifelse(nchar(Date) <= 9,
  #'                      format(as.Date(parse_date_time(Date,"dby")), "%Y-%m-%d"), DateNew),
  #'     #  Add extra columns to identify potential errors in date conversion
  #'     #  Correct date format should have 10 characters (YYYY-MM-DD)
  #'     Date_10char = ifelse(nchar(DateNew) != 10, "Fail", "Good"),
  #'     #  Correct date format should be in the 2000's (not 0018, etc.)
  #'     Date_2000 = ifelse(year(DateNew) == 0018 | year(DateNew) == 0019 |
  #'                        year(DateNew) == 0020 | year(DateNew) == 0021,
  #'                        "Fail", "Good"),
  #'     CameraLocation = as.factor(as.character(CameraLocation)),
  #'     #  Reformat columns to the desired format
  #'     DT_Good = ifelse(DT_Good == "true", "TRUE", DT_Good),
  #'     DT_Good = ifelse(DT_Good == "false", "FALSE", DT_Good),
  #'     DT_Good = as.factor(as.character(DT_Good)),
  #'     Service = ifelse(Service == "true", "TRUE", Service),
  #'     Service = ifelse(Service == "false", "FALSE", Service),
  #'     Service = as.factor(as.character(Service)),
  #'     Empty = ifelse(Empty == "true", "TRUE", Empty),
  #'     Empty = ifelse(Empty == "false", "FALSE", Empty),
  #'     Empty = as.factor(as.character(Empty)),
  #'     Animal = ifelse(Animal == "true", "TRUE", Animal),
  #'     Animal = ifelse(Animal == "false", "FALSE", Animal),
  #'     Animal = as.factor(as.character(Animal)),
  #'     Human = ifelse(Human == "true", "TRUE", Human),
  #'     Human = ifelse(Human == "false", "FALSE", Human),
  #'     Human = as.factor(as.character(Human)),
  #'     Vehicle = ifelse(Vehicle == "true", "TRUE", Vehicle),
  #'     Vehicle = ifelse(Vehicle == "false", "FALSE", Vehicle),
  #'     Vehicle = as.factor(as.character(Vehicle)),
  #'     Species = as.character(Species),
  #'     HumanActivity = as.character(HumanActivity),
  #'     Count = as.numeric(Count),
  #'     AdultFemale = as.numeric(AdultFemale),
  #'     AdultMale = as.numeric(AdultMale),
  #'     AdultUnknown = as.numeric(AdultUnknown),
  #'     Offspring = as.numeric(Offspring),
  #'     UNK = as.numeric(UNK),
  #'     Collars = as.numeric(Collars),
  #'     Tags = as.character(Tags),
  #'     NaturalMarks = as.character(NaturalMarks),
  #'     SecondOpinion = ifelse(SecondOpinion == "true", "TRUE", SecondOpinion),
  #'     SecondOpinion = ifelse(SecondOpinion == "false", "FALSE", SecondOpinion),
  #'     SecondOpinion = as.factor(as.character(SecondOpinion)),
  #'     Comments = as.character(Comments)
  #'   )
  #' #'  Warnings are due to an extra empty column at end of csv files- ignore
  #' 
  #' 
  #' #'  Check for rows where the date format is incorrect
  #' fail2_10char <- csv_files2[csv_files2$Date_10char == "Fail",]
  #' head(fail_10char)
  #' fail2_2000 <- csv_files2[csv_files2$Date_2000 == "Fail",]
  #' head(fail_2000)
  #' #'  And rows where there's no species or human activity but count data
  #' missing_data2 <- csv_files2[is.na(csv_files2$Species) & is.na(csv_files2$HumanActivity) & csv_files2$Count > 0,]
  #' print(droplevels(unique(missing_data2$CameraLocation)))
  
  
  #'  Final re-formatting of detection data
  #'  Merge reviewed data and processed data and re-format together
  format_dat <- function(read_dat){
    megadata <- as.data.frame(read_dat) %>%  #rbind(csv_files, csv_files2)
        transmute(
          File = as.character(File),
          RelativePath = as.character(RelativePath),
          Folder = as.character(Folder),
          DateTime = as.POSIXct(paste(DateNew, Time),
                                format="%Y-%m-%d %H:%M:%S",tz="America/Los_Angeles"),
          Date = as.Date(DateNew, format = "%Y-%m-%d"),
          Time = chron(times = Time),
          ImageQuality = as.factor(ImageQuality),
          #' Change camera number where camera station stayed the same but camera number
          #' was changed part way through season due to damage- only applies to YR2
          CameraLocation = ifelse(CameraLocation == "OK2145_3", "OK2145_112", as.character(CameraLocation)),
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
    return(megadata)
  }
  
  #'  Run each year's worth of camera data through function
  cleaned_dat <- lapply(read_dat, format_dat)
  megadata <- rbind(cleaned_dat[[1]], cleaned_dat[[2]], cleaned_dat[[3]]) 
  
  #'  Keep data split out by year if needed
  megadata_yr1 <- cleaned_dat[[1]] 
  megadata_yr2 <- cleaned_dat[[2]]
  megadata_yr3 <- cleaned_dat[[3]]
  
  #'  Check for rows where the date format is incorrect
  fail_10char <- megadata[megadata$Date_10char == "Fail",]
  head(fail_10char)
  fail_2000 <- megadata[megadata$Date_2000 == "Fail",]
  head(fail_2000)
  #'  And rows where there's no species or human activity but count data
  missing_data <- megadata[is.na(megadata$Species) & is.na(megadata$HumanActivity) & megadata$Count > 0,]
  print(droplevels(unique(missing_data$CameraLocation)))
  #'  BRB gotta go fix these!
  
  #' #'  Double check it still looks good
  #' #'  And rows where there's no species or human activity but count data
  #' missing_data3 <- megadata[is.na(megadata$Species) & is.na(megadata$HumanActivity) & megadata$Count > 0,]
  #' print(droplevels(unique(missing_data3$CameraLocation)))
  
  #'  Does it all add up?
  table(megadata$Species, useNA = "ifany")
  sum(table(megadata$Species, useNA = "ifany"))
  nrow(megadata)
  sum(table(megadata$HumanActivity, useNA = "ifany"))
  table(megadata$HumanActivity, useNA = "ifany")

  
  #'  Source "Adjust_Incorrect_DATE&TIME.R" script to bring in cameras with known
  #'  and corrected date/time issues.
  source("./Scripts/Adjust_Incorrect_DATE&TIME.R")
  
  
  #'  Append sourced data to MEGA data file
  full_csv <- rbind(megadata,
                    #  Year 1 data
                    NE3000_S3_C18_DTGood, NE3109_S4_C31_C96_C131_DTGood,
                    NE3815_C26_C61_DTGood, NE3815_C125_DTGood,
                    NE5511_C168_C186_DTGood, OK4880_C175_DTGood,
                    OK7237_C159_C241_DTGood,
                    #  Year 2 data
                    OK4306_C23_DTGood, OK4489_C104_C132_DTGood,
                    OK4944_C97_DTGood, OK5719_C116_DTGood, 
                    OK7545_C110_DTGood, OK8226_C206_MSD2001_DTGood,
                    #  Year 3 data
                    OK8302_C197_DTGood) %>%
    filter(CameraLocation != "NE5853_Moultrie5" | File != "MFDC0001.JPG") %>%
    #  Drop this one cow image w/ bizarre incorrect date/time- camera malfunction
    #  (plenty of cow pix before & after it so no real loss of data)
    #  FYI, this filtering also drops an empty image with the same file number
    filter(CameraLocation != "NE7394_117" | File != "RCNX2117.JPG")
  

  #'  Am I missing any cameras?
  cams <- as.data.frame(unique(full_csv$CameraLocation))
  
  
  ####  FINAL CLEANING  ####
  #'  ---------------------------------------------------
  #'  Last chance to clean up data before moving onto real analyses
  #'  Make sure date & times are correct and if not, make adjustments
  #'  Make sure weird formatting isn't dropping species/human activity data
  #'  Make sure all Second Opinion images have been reviewed

  ##  FULL STOP  ##

  #'  Identify which cameras & memory cards MAY have date & time issues
  #'  Print cameras that have already been adjusted
  print(adjusted)
  #'  DT_Good marked FALSE
  droplevels(unique(full_csv$CameraLocation[which(full_csv$DT_Good == "FALSE" | full_csv$DT_Good == "false")]))
  #'  Hang tight, gotta double check any I haven't already fixed...

  ##  Mmmk we're good  ##

  #'  Double check if cameras are dropping Species classifications after formatting
  #'  Just because it lists these cameras doesn't mean there's a problem
  droplevels(unique(full_csv$CameraLocation[is.na(full_csv$Species) & full_csv$Animal == "TRUE"]))
  #'  Missing species data is due to confusion re: classifying humans with dog/horse
  
  #'  Which ones are reading in as lower-cased "true"/"false"? This is causing problems
  droplevels(unique(full_csv$CameraLocation[full_csv$Animal == "true"]))
  #'  New method of reading in raw csv files should have fixed this
  
  #'  Identify which cameras have images that were never reviewed or where info
  #'  was incorrectly propagated across empty images
  droplevels(unique(full_csv$CameraLocation[full_csv$Service == "FALSE" &
                                              full_csv$Empty == "FALSE" &
                                              full_csv$Animal == "FALSE" &
                                              full_csv$Human == "FALSE" &
                                              full_csv$Vehicle == "FALSE"]))
  ##  BRB, gotta fix these  ##

  #'  Identify which images still need a second review
  unique(full_csv$CameraLocation[which(full_csv$SecondOp == "TRUE" | full_csv$SecondOp == "true")])
  ##  BRB, gotta go check these  ##

  
  #'  Clean up some obvious mistakes with processed data
  allimgs <- full_csv %>%
    mutate(
      #  Change Empty to FALSE for Service images
      Empty = ifelse(Service == "TRUE", "FALSE", as.character(Empty)),
      #  Change Empty to FALSE when Species is labeled
      Empty = ifelse(Empty == "TRUE" & !is.na(Species), "FALSE", as.character(Empty)),
      #  Change Animal to FALSE when HumanActivity is labeled (human w/ dog doesn't count as animal)
      Animal = ifelse(Animal == "TRUE" & !is.na(HumanActivity), "FALSE", as.character(Animal)),
      #  Change Human and Vehicle to FALSE when Species is labeled
      Human = ifelse(Human == "TRUE" & !is.na(Species), "FALSE", as.character(Human)),
      Vehicle = ifelse(Vehicle == "TRUE" & !is.na(Species), "FALSE", as.character(Vehicle)),
      #  Change Human to FALSE when HumanActivity is a vehicle
      Human = ifelse(Human == "TRUE" & HumanActivity == "Vehicle Truck Car", "FALSE", as.character(Human)),
      Human = ifelse(Human == "TRUE" & HumanActivity == "Vehicle ATV", "FALSE", as.character(Human)),
      Human = ifelse(Human == "TRUE" & HumanActivity == "Dirt Bike", "FALSE", as.character(Human)),
      Human = ifelse(Human == "TRUE" & HumanActivity == "Snowmobile", "FALSE", as.character(Human)),
      #  Change Vehicle to FALSE when HumanActivity is not a vehicle
      Vehicle = ifelse(Vehicle == "TRUE" & HumanActivity == "Hiker", "FALSE", as.character(Vehicle)),
      Vehicle = ifelse(Vehicle == "TRUE" & HumanActivity == "Horseback Rider", "FALSE", as.character(Vehicle)),
      Vehicle = ifelse(Vehicle == "TRUE" & HumanActivity == "Human w Dog", "FALSE", as.character(Vehicle)),
      Vehicle = ifelse(Vehicle == "TRUE" & HumanActivity == "Hunter Bow", "FALSE", as.character(Vehicle)),
      Vehicle = ifelse(Vehicle == "TRUE" & HumanActivity == "Hunter Rifle", "FALSE", as.character(Vehicle)),
      Vehicle = ifelse(Vehicle == "TRUE" & HumanActivity == "Skier", "FALSE", as.character(Vehicle)),
      Vehicle = ifelse(Vehicle == "TRUE" & HumanActivity == "Bicycle", "FALSE", as.character(Vehicle)),
      #  Remove "--" in Species column
      Species = ifelse(Species == "--", NA, as.character(Species)),
      #  Drop hiker and dog detections in Service images
      Species = ifelse(Service == "TRUE", NA, as.character(Species)),
      #  Fill in Species column with Human or Vehicle when HumanActivity is labeled 
      #  Important for generating detection histories with camtrapR
      Species = ifelse(Human == "TRUE", "Human", Species),
      Species = ifelse(Vehicle == "TRUE", "Vehicle", Species),
      #  Reformat all the columns I messed with
      Service = as.factor(as.character(Service)),
      Empty = as.factor(as.character(Empty)),
      Animal = as.factor(as.character(Animal)),
      Human = as.factor(as.character(Human)),
      Vehicle = as.factor(as.character(Vehicle)),
      Species = as.character(Species),
      HumanActivity = as.character(HumanActivity)
    )
  
  #'  Double check only one column for Animal, Human, or Vehicle is TRUE
  nrow(allimgs[allimgs$Animal == "TRUE" & allimgs$Human == "TRUE",])
  nrow(allimgs[allimgs$Animal == "TRUE" & allimgs$Vehicle == "TRUE",])
  nrow(allimgs[allimgs$Human == "TRUE" & allimgs$Vehicle == "TRUE",])

  #'  Check for rows where the date format is incorrect
  #'  The correct date format should have 10 characters (YYYY-MM-DD)
  date_ok <- allimgs[nchar(as.character(allimgs$Date)) != 10,]
  #  The correct date format should be in the 2000's (not 0018, etc.)
  cent_ok <- allimgs[year(allimgs$Date) == 0018 | year(allimgs$Date) == 0019 | 
                       year(allimgs$Date) == 0020 | year(allimgs$Date) == 0021,]
  
  
  #'  Make sure there are no rows with no species or human activity but count data
  missing_obs <- allimgs[is.na(allimgs$Species) & is.na(allimgs$HumanActivity) & allimgs$Count > 0,]
  print(droplevels(unique(missing_obs$CameraLocation)))
  
  
  
  ####  FILTER DATA & SAVE  ####
  #'  ---------------------------------------------
  
  #'  Filter service and empty images out 
  alldetections <- allimgs %>%
    filter(Empty != "TRUE") %>% 
    filter(Service != "TRUE")  
  
  
  #'  Save for later analyses
  write.csv(alldetections, paste0('./Output/Bassing_AllDetections18-21_', Sys.Date(), '.csv'))

  
  #'  Filter for capstone students
  #'  All study species detections for Jessalyn
  allstudyspp <- alldetections %>%
    filter(Species == "Elk" | Species == "Moose" | Species == "Mule Deer" | 
             Species == "White-tailed Deer" | Species == "Cougar" | Species == "Wolf" | 
             Species == "Black Bear" | Species == "Bobcat" | Species == "Coyote" | 
             Species == "Turkey" | Species == "Snowshoe Hare" | Species == "Rabbit Spp") %>%
    filter(!grepl("Moultrie", CameraLocation))
  # write.csv(allstudyspp, paste0('G:/My Drive/1 Volunteers/Capstone Projects/2020-2021/Jessalyn- Carlton Extern/Bassing_AllStudySpecies_', Sys.Date(), '.csv'))
  
  #'  Coyote, bobcat & human detections for Alyssa
  meso <- alldetections %>%
    filter(Species == "Bobcat" | Species == "Coyote") %>%
    filter(!grepl("Moultrie", CameraLocation))
  #'  Human detections (on foot and vehicle)
  humans <- alldetections %>%
    filter(Human == "TRUE" | Vehicle == "TRUE") %>%
    filter(!grepl("Moultrie", CameraLocation))
  Alyssa_data <- rbind(meso, humans)
  # write.csv(Alyssa_data, paste0('G:/My Drive/1 Volunteers/Capstone Projects/Alyssa/Alyssa_data_', Sys.Date(), '.csv'))

  #'  Cougar detections
  cougar.data <- alldetections %>%
    filter(Species == "Cougar")
  #  Double check that we noted all the collars
  cougar.collars <- cougar.data %>%
    filter(Collars == "1") %>%
    select(CameraLocation, DateTime, Date, Time, Species, Count, Collars, Tags, AF, AM, AU, OS, UNK, File, RelativePath)
 
  #'  Moose detections for WDFW
  NEmoose <- alldetections %>%
    filter(Species == "Moose") %>%
    filter(str_detect(CameraLocation, paste("OK"), negate = TRUE))
  # write.csv(NEmoose, paste0('./Output/NEMoose_allimgs_', Sys.Date(), '.csv'))
  
  #'  Wolf detections for Trent
  wolves <- alldetections %>%
    filter(Species == "Wolf")
  # write.csv(wolves, paste0('./Output/Wolf_allimgs_', Sys.Date(), '.csv'))
  
  #'  Bird detections for Microsoft 
  bird_detections <- alldetections %>%
    filter(Species == "Bird Spp" | Species == "Common Raven" | Species == "Grouse Spp" | 
           Species == "Raptor Spp" | Species == "Turkey") %>%
    mutate(
      Animal = ifelse(Animal == "FALSE", "TRUE", as.character(Animal))
    )
  # write.csv(bird_detections, paste0('./Output/Bassing_BirdImages_', Sys.Date(), '.csv'))
  
  
  
  
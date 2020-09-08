    ##  Camera Check Cleaning
    ##  Feb. 2019
################################################################################
  #  This script cleans the oh so messy camera check data so I can summarize 
  #  basic information about the number of species detected, etc.
################################################################################
  #### Set up  ####

  #  Load packages
  library(stringr)
  library(DataCombine)
  library(ggplot2)
  library(tidyr)
  library(dplyr)

  #  Read in data
  checks_yr1 <- read.csv("G:/My Drive/1 Data/Summary Stats for Reporting/Camera_Checking_2018.csv", header = TRUE)
  checks_yr2 <- read.csv("G:/My Drive/1 Data/Summary Stats for Reporting/AudioMoth_and_Camera_Checking_2019.csv", header = TRUE)
  checks_yr3 <- read.csv("G:/My Drive/1 Data/Summary Stats for Reporting/AudioMoth_and_Camera_Checking_2020_090820.csv", header = TRUE)
  #checks_ <- read.csv("G:/My Drive/1 Data/Summary Stats for Reporting/AudioMoth_and_Camera_Deployment &Checking_2020_082420.csv", header = TRUE)

    
  #  Past files used:
  # Camera_Checks_Thru_Nov2018.csv
  # Camera_Deployment_and_Checking_071219.csv
  
  colnames(checks_yr1)
  colnames(checks_yr2)
  colnames(checks_yr3)
  #colnames(checks)
  
  #  Reduce the number of fields I'm working with & make consistent format
  slim_checks1 <- checks_yr1[,c(1, 4, 6:9, 14:15, 31)] %>%
    transmute(
      Date = Date,
      Study_area = Study_area,
      Cell_ID = Cell_No,
      Cam_ID = Camera_ID, 
      Cam_Long = Camera_Long,
      Cam_Lat = Camera_Lat,
      Cam_Card = Card_No,
      Num_Images = No_Images,
      Species = Species
    )
  slim_checks2 <- checks_yr2[,c(1, 4:8, 12:13, 29)]
  slim_checks3 <- checks_yr3[,c(1, 4:8, 12:13, 30)]
  #slim_checks <- checks[,c(1, 4:8, 12:13, 30)]
  #slim_checks <- slim_checks[!(slim_checks$Cell_ID == "OKbonus"),]
  #  Check Date, Study Area, Cell Number, Camera ID, Camera Long, Camera Lat, 
  #  Card Number, Number of Images, Species Observed
  
  spp_checks1 <- slim_checks1[,c(1:6, 9)]
  spp_checks2 <- slim_checks2[,c(1:6, 9)]
  spp_checks3 <- slim_checks3[,c(1:6, 9)]
  #spp_checks <- checks[,c(1, 4:6, 12, 30)]
  #spp_checks <- spp_checks[!(spp_checks$Cell_ID == "OKbonus"),]
  #  Check Date, Study Area, Cell Number, Camera ID, Card Number, Species Observed
  
################################################################################
  ####  Fix up species data  ####

  #  Because of how strings work, I need to duplicate rows of camera check data 
  #  for each species observed at a check --> put dataset into long format. Then 
  #  sort by camera location and species, filter out repeats, and count the 
  #  number of cameras where each species was observed by study area.
  
  
  #  Testing out this stringr package
  
  # #  Create a vector of the string you want to split up
  # x <- slim_checks[1, 9]
  # #  Split the string by where commas fall
  # tst <- str_split(x, ",")
  # #  Grab the row of data that this corresponds to
  # xfull <- slim_checks[1,]
  # #  Bind them together to create 2 entries for the camera check, 1 per species
  # back <- cbind(xfull, tst)
  
  #  Works for 1 iteration
  
  # x <- c()
  # spp <- c()
  # for(i in 1:nrow(spp_checks[1,])) {
  #   x[i] <- as.character(spp_checks[i,6])
  #   spp[i] <- str_split(x[i], ",")
  #   ind_spp <- cbind(spp_checks[i,], spp[i])
  # }
  
  #  Update the spp_checks for each year
  #spp_checks <- spp_checks1
  #spp_checks <- spp_checks2
  spp_checks <- spp_checks3
  
  #  Use the stringr package to manipulate strings of species observed at each camera
  # #  Create some empty vectors and lists to hold data with each iteration
  x <- c()
  spp <- c()
  #trimmed <- c()
  ind_spp <- as.list(NA)
  
  #  Loop through each row (camera check) to pull individual species apart from
  #  the long string of listed species observed during each camera check
  for(i in 1:nrow(spp_checks)) {    #[1:2,]
    #  Creates a vector of the string you want to split up
    x[i] <- as.character(spp_checks[i,7]) #[i,6]
    # #  Trim extra white spaces around individual character strings
    # trimmed[i] <- str_trim(x[i], "both")
    #  Splits the string by commas in the string
    spp[i] <- str_split(x[i], ",")
    #  Creates list of dfs, each row in the df is a duplicate of camera location data 
    #  w/ a single species that was observed during that check listed in final column
    ind_spp[[i]] <- cbind(spp_checks[i,], spp[i])
  }
  
  #  Rename the df columns in each list so they can easily be merged together
  newnames <- c("Date", "Study_area", "Cell_ID", "Camera_ID", "Long", "Lat", "Species", "Spp_Obs")
  spp_list <- lapply(ind_spp, setNames, newnames)
  
  #  Merge lists of dfs into one giant df
  long_check <- bind_rows(spp_list, .id = "column_label")
  
  #  Need to make species names consistent throughout... ugh.
  #  Replace abbreviations with a single name for each species
  #  Oh my god be more standardized with your data sheets in the future!
  #head(long_check)
  replaces <- data.frame(from = c("atc", "atv", "ATVs", "riding lawn mower",
                                  "american badger", "badger",
                                  "black bears","black bear", "bear",
                                  "Black Black bear", 
                                  "bikes with dog", "bicycle", "bicycles", "bikes",  
                                  "mountain bike", "biker", "bike",
                                  "bird spp.", "mountain blue bird",
                                  "bluebird", "California quail", "quail", "doves",  
                                  "magpie", "red-tailed hawk", "robins", "robin", "woodpecker", 
                                  "short-eared owl","song bird", "bird species", 
                                  "bird spp", "bird", "bird spp?", "Bird spp. spp.", 
                                  "Bird spp.s", "blue Bird spp.", "hawk", 
                                  "great horned owl","owl", "flicker", 
                                  "bobcate", "bobcats", "babcat", "bobcat", "bobcat?",  
                                  "bunny", "cottontail rabbit", "rabit", "rabbit",
                                  "bushy tailed wood rat", "busy tailed woodrat", "bushtailed woodrat",
                                  "bushy-tailed wood rat", "woodrat", "mice", 
                                  "mouse", "rodent",
                                  "cars", "car", "trucks", "truck", "Jeep", "bulldozer",  
                                  "plus vehicles", "vehicles", "vehicle", "mower",
                                  "domestic cats", "domestic cat", "house cats", "house cat", 
                                  "cat", "House House cat", "BobHouse cat", 
                                  "chipmunk", "Douglas squirrel", "ground squirrel", "western gray squirrel",
                                  "squirrel (red?)", "squirrel spp.", "squirrel species", 
                                  "red squirrel", "flying squirrel", "squirrel", 
                                  "collared cougar", "cougar", "mountain lion", "Mountain Lion",
                                  "collared coyote", "coyote?", "coyotes", "coyote", "cayote",
                                  "and dirt bikes", "dirt Bicycle", "and Dirt Bicycle",
                                  "Dirt bikes", "dirt bikes", "dirt bike", "dirtbike",
                                  "and Dirt bike", "dirtBicycle", 
                                  "cows", "cow", "livestock", "House cattle",
                                  "crows", "crow", "ravens", "raven",
                                  "domestic dog", "domestic Dog", "dogs", "Dogs", 
                                  "dog", 
                                  "collared elk", "elk",
                                  "grouse", "Ruffed Grouse",
                                  "mule deer", "mulie",
                                  "hikers", "hikers with Dog", "hiker", "HIKERS!", "hunters",  
                                  "hunter", "Vision Questers!", "loggers", "person",
                                  "hunters",
                                  "horse packers", "horses and humans", "mule", 
                                  "horses", "horse riders", "horseback riders", "horse",
                                  "pedestrian", "people with chainsaws", 
                                  "people riding Horse", "horseback rider",
                                  "human with Horse", "people", "humans", 
                                  "human", "Human and Dog", "Human with Dog", "humer with Dog",
                                  "lynx",
                                  "moose",
                                  "racoon", "raccoon",
                                  "striped skunk", "skunk",
                                  "snowmobile",
                                  "snowshoe hare", "snowshoe", "snow shoe", "snowhare hare",
                                  "snowshore hare", "showshoe hare", "snowshow hare",
                                  "wild turkey", "turky", "turkeys", "turkey", "tukey",
                                  "unkwn deer spp.",
                                  "unkwn night creature", "unknown",
                                  "mystery animal that messed up camera", "mystery animal",
                                  "white-tailed deer", "white tail deer", "white-tail", "white tailed deer",   
                                  "White tailed deer", "white-tail deer", "White-tailed deer deer", 
                                  "wt deer", "wtd", "wdt", "whitetail",
                                  "one black wolf", "one wolf", "gray wolf", 
                                  "wolves", "fat wolf","wolf", 
                                  "yellow-bellied marmot", "hoary marmot", "marmot", "marten", 
                                  "mountain goat", "fisher", "mountain beaver",
                                  "otter", "wolverine", "weasel", "porcupine", "pig"), 
                         to = c("ATV", "ATV", "ATV", "ATV",
                                "Badger", "Badger",
                                "Black bear", "Black bear", "Black bear", "Black bear",
                                "Bicycle", "Bicycle", "Bicycle", "Bicycle", "Bicycle",
                                "Bicycle", "Bicycle",
                                "bluebird",
                                "Bird spp.", "Bird spp.", "Bird spp.", "Bird spp.",
                                "Bird spp.", "Bird spp.", "Bird spp.", "Bird spp.", 
                                "Bird spp.", "Bird spp.", "Bird spp.", "Bird spp.",
                                "Bird spp.", "Bird spp.", "Bird spp.", "Bird spp.",
                                "Bird spp.", "Bird spp.", "Bird spp.", "Bird spp.",
                                "Bird spp.", "Bird spp.", "Bird spp.",
                                "Bobcat", "Bobcat", "Bobcat", "Bobcat", "Bobcat",
                                "Rabbit spp.", "Rabbit spp.", "Rabbit spp.",
                                "Rabbit spp.",
                                "Rodent spp.", "Rodent spp.", "Rodent spp.", "Rodent spp.",
                                "Rodent spp.", "Rodent spp.", "Rodent spp.",
                                "Rodent spp.",
                                "Vehicle", "Vehicle", "Vehicle", "Vehicle", "Vehicle", 
                                "Vehicle", "Vehicle", "Vehicle", "Vehicle", "Vehicle",
                                "House cat", "House cat", "House cat", "House cat",
                                "House cat", "House cat", "Bobcat",
                                "Squirrel spp.", "Squirrel spp.", "Squirrel spp.", 
                                "Squirrel spp.", "Squirrel spp.", "Squirrel spp.",
                                "Squirrel spp.", "Squirrel spp.", "Squirrel spp.",
                                "Squirrel spp.",
                                "Cougar", "Cougar", "Cougar", "Cougar",
                                "Coyote", "Coyote", "Coyote", "Coyote", "Coyote",
                                "Dirt bike", "Dirt bike", "Dirt bike", "Dirt bike",
                                "Dirt bike", "Dirt bike", "Dirt bike", "Dirt bike",
                                "Dirt bike",
                                "Cattle", "Cattle", "Cattle", "Cattle",
                                "Corvid spp.", "Corvid spp.", "Corvid spp.", "Corvid spp.",
                                "Dog", "Dog", "Dog", "Dog", "Dog",
                                "Elk", "Elk",
                                "Grouse spp.", "Grouse spp.",
                                "Mule deer", "Mule deer",
                                "Human", "Human", "Human", "Human", "Human",
                                "Human", "Human", "Human", "Human", "Human",
                                "Horse", "Horse", "Horse", "Horse", "Horse", "Horse", "Horse",
                                "Human", "Human", "Human", "Human", "Human",
                                "Human", "Human", "Human", "Human", "Human", "Human",
                                "Lynx",
                                "Moose",
                                "Raccoon", "Raccoon",
                                "Skunk", "Skunk",
                                "Snowmobile",
                                "Snowshoe hare", "Snowshoe hare", "Snowshoe hare",
                                "Snowshoe hare", "Snowshoe hare", "Snowshoe hare",
                                "Snowshoe hare",
                                "Turkey", "Turkey", "Turkey", "Turkey", "Turkey",
                                "Unknown deer spp.",
                                "Unknown spp.", "Unknown spp.", "Unknown spp.", 
                                "Unknown spp.",
                                "White-tailed deer", "White-tailed deer", "White-tailed deer", 
                                "White-tailed deer", "White-tailed deer", "White-tailed deer",
                                "White-tailed deer", "White-tailed deer", "White-tailed deer",
                                "White-tailed deer", "White-tailed deer",
                                "Wolf", "Wolf", "Wolf", "Wolf", "Wolf", "Wolf",
                                "Marmot", "Marmot", "Marmot", "Marten", "Mountain goat", "Fisher",
                                "Mountain beaver", "Otter", "Wolverine", "Weasel", "Porcupine", "Pig"))
  fixed_check <- FindReplace(data = long_check, Var = "Spp_Obs", replaceData = replaces,
                             from = "from", to = "to", exact = FALSE)

  

  
  #  Remove white space around character strings
  #  White space is a residual of having species names followed by a comma then space
  nix_ws <- str_trim(fixed_check$Spp_Obs)
  nix_ws <- as.data.frame(nix_ws)
  colnames(nix_ws) <- "Spp_Obs"

  
  #  Double check that I didn't screw up the find and replace function
  #  Compare species from long_check to fixed_check to make sure it all looks good
  tst <- as.data.frame(cbind(long_check$Spp_Obs, nix_ws)) #fixed_check$Spp_Obs
  colnames(tst) <- c("original", "corrected")
  ord <- tst %>%
    arrange(corrected)
  
  #  You are a magnificent beast. This looks amazing.
  animals <- as.data.frame(cbind(fixed_check[,2:7] , nix_ws))


  
  #  Extra cleaning to simplify summary stats!
  
  #  Detections of humans are currently separated by type of activity
  #  Merge all human activity (ATV, Human, etc.) into a single human category
  #  Can use either version (animals or hums_and_animals) for summary stats
  hums <- data.frame(from = c("ATV", "Bicycle", "Dirt bike", "Vehicle", "Horseback rider",
                              "Snowmobile", "Human", "Human activitys", "Human activity activity"),
                     to = c("Human activity", "Human activity", "Human activity", 
                            "Human activity", "Human activity", "Human activity",
                            "Human activity", "Human activity", "Human activity"))
  hums_and_animals <- FindReplace(data = animals, Var = "Spp_Obs", 
                                  replaceData = hums, from = "from", to = "to", 
                                  exact = FALSE)
  
  #  Consolidate detections of rare species and ones of no interest to the project
  #  Also group some species together (like all the birds except turkeys)
  other <- data.frame(from = c("Corvid spp.", "Grouse spp.","Lynx", "Marmot",
                               "Rabbit spp.", "Rodent spp.", "Squirrel spp.",
                               "Unknown spp.", "Badger", "Wolverine", "Otter", 
                               "Porcupine", "Marten", "Weasel", "Raccoon", "Pig", 
                               "Fisher", "Mountain goat", "Mountain beaver"),
                      to = c("Bird spp.", "Bird spp.", "Other", "Other", "Other",
                             "Other", "Other", "Other", "Other", "Other", "Other",
                             "Other", "Other", "Other", "Other", "Other", "Other",
                             "Other", "Other"))
  focal_species <- FindReplace(data = hums_and_animals, Var = "Spp_Obs", 
                               replaceData = other, from = "from", to = "to", 
                               exact = FALSE)
  
################################################################################  
  #### Consolidate observations  ####
  
  #  Filter out repeat camera checks when the same species was detected multiple times
  #  Arrange by grid cell number
  #  Count the number of times each species was observed across both study areas
  #  Arrange by species alphabetically
  skinny_animals <- animals %>%
    na.omit() %>%
    group_by(Cell_ID) %>%
    distinct(Spp_Obs) %>%
    ungroup() %>%
    #arrange(Cell_Nmb) %>%
    count(Spp_Obs) %>%
    arrange(Spp_Obs)
  
  #  Split it up by study area now
  OK_animals <- animals %>%
    na.omit() %>%
    filter(Study_area == "OK") %>%
    group_by(Cell_ID) %>%
    distinct(Spp_Obs) %>%
    ungroup() %>%
    count(Spp_Obs) %>%
    arrange(Spp_Obs)
    
  
  NE_animals <- animals %>%
    na.omit() %>%
    filter(Study_area == "NE") %>%
    group_by(Cell_ID) %>%
    distinct(Spp_Obs) %>%
    ungroup() %>%
    count(Spp_Obs) %>%
    arrange(Spp_Obs)
  
  #  Put them all together to double check we're still good
  All_animals <- full_join(OK_animals, NE_animals, by = "Spp_Obs") %>%
    na.omit() %>%
    arrange(Spp_Obs)
  colnames(All_animals) <- c("Spp_Obs", "OK_count", "NE_count")
  All_animals <- All_animals[!(All_animals$Spp_Obs == "etc" |All_animals$Spp_Obs == "etc."|All_animals$Spp_Obs == ""),]


  last_test <- full_join(All_animals, skinny_animals, by = "Spp_Obs") %>%
    na.omit() %>%
    arrange(Spp_Obs) %>%
    mutate(Sum = OK_count + NE_count)
  colnames(last_test) <- c("Spp_Obs", "OK_count", "NE_count", "Total_count", "Sum")
  last_test <- last_test[!(last_test$Spp_Obs == "etc" |last_test$Spp_Obs == "etc."|last_test$Spp_Obs == ""),]
  last_test <- last_test[,c("Spp_Obs", "OK_count", "NE_count", "Sum")]
  #  Keep in mind NAs in the study area-specific columns will mess up the SUM 
  #  function within the mutate function
  

  #  Repeat with the condensed human activity version
  skinny_hum_and_ans <- hums_and_animals %>%
    na.omit() %>%
    group_by(Cell_ID) %>%
    distinct(Spp_Obs) %>%
    ungroup() %>%
    count(Spp_Obs) %>%
    arrange(Spp_Obs)

  
  #  Split it up by study area now
  OK_hum_and_ans <- hums_and_animals %>%
    na.omit() %>%
    filter(Study_area == "OK") %>%
    group_by(Cell_ID) %>%
    distinct(Spp_Obs) %>%
    ungroup() %>%
    count(Spp_Obs) %>%
    arrange(Spp_Obs)
  
  
  NE_hum_and_ans <- hums_and_animals %>%
    na.omit() %>%
    filter(Study_area == "NE") %>%
    group_by(Cell_ID) %>%
    distinct(Spp_Obs) %>%
    ungroup() %>%
    count(Spp_Obs) %>%
    arrange(Spp_Obs)
  
  
  #  And repeat with the condensed non-target species version
  skinny_focal_spp <- focal_species %>%
    na.omit() %>%
    group_by(Cell_ID) %>%
    distinct(Spp_Obs) %>%
    ungroup() %>%
    count(Spp_Obs) %>%
    arrange(Spp_Obs)
  # Get rid of etc. and such... hard coding is bad, I know
  skinny_focal_spp <- skinny_focal_spp[!(skinny_focal_spp$Spp_Obs == "etc" |skinny_focal_spp$Spp_Obs == "etc."|skinny_focal_spp$Spp_Obs == "" |skinny_focal_spp$Spp_Obs == "none"),]
  #skinny_focal_spp <- skinny_focal_spp[-c(1,5,11:12,22),] 
  
  
  #  Split it up by study area now
  OK_spp <- focal_species %>%
    na.omit() %>%
    filter(Study_area == "OK") %>%
    group_by(Cell_ID) %>%
    distinct(Spp_Obs) %>%
    ungroup() %>%
    count(Spp_Obs) %>%
    arrange(Spp_Obs)
  #OK_spp <- OK_spp[-c(4, 10:11, 21),]
  OK_spp <- OK_spp[!(OK_spp$Spp_Obs == "etc" |OK_spp$Spp_Obs == "etc."|OK_spp$Spp_Obs == ""),]
  
  
  NE_spp <- focal_species %>%
    na.omit() %>%
    filter(Study_area == "NE") %>%
    group_by(Cell_ID) %>%
    distinct(Spp_Obs) %>%
    ungroup() %>%
    count(Spp_Obs) %>%
    arrange(Spp_Obs)
  #NE_spp <- NE_spp[-c(1, 10:11),]
  NE_spp <- NE_spp[!(NE_spp$Spp_Obs == "etc" |NE_spp$Spp_Obs == "etc."|NE_spp$Spp_Obs == ""),]
  
################################################################################
  ####  Visualize!  ####
  
  #  Base plotting
  barplot(skinny_focal_spp$n, names.arg = skinny_focal_spp$Spp_Obs, 
          ylim = c(0, 60), las = 2, xlab = "Species", ylab = "Number of Cameras", 
          main = "Number of Cameras where a Species was Detected \n Eastern Washington")
  barplot(OK_spp$n, names.arg = OK_spp$Spp_Obs, ylim = c(0, 70), 
          las = 2, xlab = "Species", ylab = "Number of Cameras", 
          main = "Number of Cameras where a Species was Detected \n Okanogan Study Area")
  barplot(NE_spp$n, names.arg = NE_spp$Spp_Obs, ylim = c(0, 50), 
          las = 2, xlab = "Species", ylab = "Number of Cameras", 
          main = "Number of Cameras where a Species was Detected \n Northeast Study Area")
  
  #  Let's make it look better
  df <- as.data.frame(skinny_focal_spp)
  ggplot(data = df, aes(x = Spp_Obs, y = n)) +
    geom_bar(stat="identity") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 80)) +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = "black",  size = 14),
          axis.title.x = element_text(colour = "black", size = 16)) +
          xlab("Species Detected") +
    theme(axis.text.y = element_text(colour = "black",  size = 14),
          axis.title.y = element_text(colour = "black", size = 16)) +
          ylab("Number of Cameras") +
    theme(plot.title = element_text(hjust = 0.5, size = 18)) +
      ggtitle("Number of Cameras where a Species was Detected \n in Eastern Washington") 
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


  
  #  Plot OK and NE detections together
  #  Need to combine OK and NE counts into one dataframe first
  NE_spp1 <- as.data.frame(NE_spp) # detections summer 2018 checks
  NE_spp2 <- as.data.frame(NE_spp) # detections summer 2019 checks
  NE_spp3 <- as.data.frame(NE_spp) # detections summer 2020 checks
  OK_spp1 <- as.data.frame(OK_spp) # detections summer 2018 checks
  OK_spp2 <- as.data.frame(OK_spp) # detections summer 2019 checks
  OK_spp3 <- as.data.frame(OK_spp) # detections summer 2020 checks
  
  temp <- NE_spp %>%
    full_join(OK_spp, by = "Spp_Obs") 
  NE_OK_join <- mutate(temp,
      rnd = as.numeric(rep(1), nrow(temp))
    )
  colnames(NE_OK_join) <- c("Spp_Obs", "NE", "OK", "rnd")
  df2 <- gather(NE_OK_join, "rnd", "n", 2:3) %>%
    arrange(Spp_Obs) %>%
    mutate(
      Spp_Obs = Spp_Obs,
      rnd = rnd,
      n = ifelse(is.na(n) == TRUE, 0, n)
    )
  colnames(df2) <- c("Spp_Obs", "SA", "n")
  df2 <- df2[!(df2$Spp_Obs == "none"),]

  
  #  Plot OK and NE detections together
  ggplot(df2, aes(x=Spp_Obs, y=n, fill=SA)) + 
    geom_bar (stat="identity", position = position_dodge(width = 1)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 60)) +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = "black",  size = 14),
          axis.title.x = element_text(colour = "black", size = 16)) +
          xlab("Species Detected") +
    theme(axis.text.y = element_text(colour = "black",  size = 14),
          axis.title.y = element_text(colour = "black", size = 16)) +
          ylab("Number of Cameras") +
    scale_fill_discrete(name = "Study Area", labels = c("Northeast", "Okanogan")) +
    theme(legend.title = element_text(size = 14),
          legend.text = element_text(size = 14)) +
    theme(plot.title = element_text(hjust = 0.5, size = 18)) +
    ggtitle("Number of Cameras where a Species was Detected \n in Eastern Washington 2019 - 2020") 
  
  
  # Just the NE
  df3 <- df2[df2$SA == "NE",]
  ggplot(df3, aes(x = Spp_Obs, y = n)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 50)) +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = "black",  size = 14),
          axis.title.x = element_text(colour = "black", size = 16)) +
    xlab("Species Detected") +
    theme(axis.text.y = element_text(colour = "black",  size = 14),
          axis.title.y = element_text(colour = "black", size = 16)) +
    ylab("Number of Cameras") +
    theme(plot.title = element_text(hjust = 0.5, size = 18)) +
    ggtitle("Number of Cameras where a Species was Detected \n in Northeastern Study Area, WA") 
  
  
  # Just the OK
  df4 <- df2[df2$SA == "OK",]
  ggplot(df4, aes(x = Spp_Obs, y = n)) +
    geom_bar(stat = "identity", fill = "red") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 60)) +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = "black",  size = 14),
          axis.title.x = element_text(colour = "black", size = 16)) +
    xlab("Species Detected") +
    theme(axis.text.y = element_text(colour = "black",  size = 14),
          axis.title.y = element_text(colour = "black", size = 16)) +
    ylab("Number of Cameras") +
    theme(plot.title = element_text(hjust = 0.5, size = 18)) +
    ggtitle("Number of Cameras where a Species was Detected \n in the Okanogan Study Area, WA") 
  
  
  
  # Super slimmed down version of the NE data
  df5 <- df2[df2$SA == "NE",]
  #df5 <- df5[c(2:3,5:6,8,11:19),]
  df5 <- df5[c(1:5, 7:8, 14),]
  ggplot(df5, aes(x = Spp_Obs, y = n)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    scale_y_continuous(expand = c(0,0), limits = c(0, 60)) +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = "black",  size = 16),
          axis.title.x = element_text(colour = "black", size = 16)) +
    xlab("Species Detected") +
    theme(axis.text.y = element_text(colour = "black",  size = 16),
          axis.title.y = element_text(colour = "black", size = 16)) +
    ylab("Number of Cameras") +
    theme(plot.title = element_text(hjust = 0.5, size = 18)) +
    ggtitle("Number of Cameras where a Species was Detected \n in the Northeast Study Area, WA") 
  
  
  
  # Super slimmed down version of all the data
  df6 <- df2
  # df6 <- df6[c(3:6,9:12,15:16,21:38),]
  # ggplot(df6, aes(x=Spp_Obs, y=n, fill=SA)) + 
  #   geom_bar (stat="identity", position = position_dodge(width = 1)) +
  #   scale_y_continuous(expand = c(0,0), limits = c(0, 60)) +
  #   theme_bw() +
  #   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  #   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, colour = "black",  size = 16),
  #         axis.title.x = element_text(colour = "black", size = 16)) +
  #   xlab("Species Detected") +
  #   theme(axis.text.y = element_text(colour = "black",  size = 16),
  #         axis.title.y = element_text(colour = "black", size = 16)) +
  #   ylab("Number of Cameras") +
  #   scale_fill_discrete(name = "Study Area", labels = c("Northeast", "Okanogan")) +
  #   theme(legend.title = element_text(size = 16),
  #         legend.text = element_text(size = 16)) +
  #   theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  #   ggtitle("Number of Cameras where a Species was Detected \n in Eastern Washington, 2018-2019") 
  
  
  ####  Mapping detections!  ####
  
  library(rgdal)
  library(raster)
  
  #  Read in spatial data
  WGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  OK_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "METHOW_SA") #Okanogan
  NE_SA <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "NE_SA")  #NE
  OK <- spTransform(OK_SA, WGS84)
  NE <- spTransform(NE_SA, WGS84)
  
  cams_yr1 <- readOGR("./Shapefiles/Camera_Locations", layer = "cams_master18_19_spdf_050220")
  cams_yr2 <- readOGR("./Shapefiles/Camera_Locations", layer = "cams_master19_20_spdf_050220")
  cams_yr3 <- readOGR("./Shapefiles/Camera_Locations", layer = "Cam_locs_spdf_090820")
  
  #  Snag location data from camera shapefiles
  yr1_locs <- cams_yr1@data
  colnames(yr1_locs) <- c("Date", "Cell_ID", "Camera_ID", "Long", "Lat", "Dist", 
                          "Height", "Monitoring", "Canopy", "Land_Mgnt", "Habitat", 
                          "Cell_Loc")
  yr2_locs <- cams_yr2@data
  colnames(yr2_locs) <- c("Date", "Cell_ID", "Camera_ID", "Long", "Lat", "Dist", 
                          "Height", "Monitoring", "Canopy", "Land_Mgnt", "Habitat", 
                          "Cell_Loc")
  
  yr3_cams <- cams_yr3@data  # does not include deployment data right now
  yr3_coord <- cams_yr3@coords
  yr3_locs <- cbind(yr3_cams, yr3_coord)
  colnames(yr3_locs) <- c("old_Cell_ID", "Camera_ID", "Study_area", "Cell_ID", "Name", "Long", "Lat")
  yr3_locs <- yr3_locs %>%
    dplyr::select("Study_area", "Cell_ID", "Camera_ID", "Long", "Lat")

  
  
  #  Extract camera ID for specific species
  colnames(focal_species) <- c("Date", "Study_area", "Cell_ID", "Camera_ID", "Card_Nmb", "Spp_Obs")
  wolf <- focal_species[which(focal_species$Spp_Obs == "Wolf"),] 
  wolf_det <- wolf %>%
    left_join(yr2_locs, by = "Cell_ID")
  mulies <- focal_species[which(focal_species$Spp_Obs == "Mule deer"),] %>%
    left_join(yr2_locs, by = "Cell_ID")
  wtd <- focal_species[which(focal_species$Spp_Obs == "White-tailed deer"),] %>%
    left_join(yr2_locs, by = "Cell_ID")
  elk <- focal_species[which(focal_species$Spp_Obs == "Elk"),] %>%
    left_join(yr2_locs, by = "Cell_ID")
  moose <- focal_species[which(focal_species$Spp_Obs == "Moose"),] %>%
    left_join(yr2_locs, by = "Cell_ID")
  cougar <- focal_species[which(focal_species$Spp_Obs == "Cougar"),] %>%
    left_join(yr2_locs, by = "Cell_ID")
  coy <- focal_species[which(focal_species$Spp_Obs == "Coyote"),] %>%
    left_join(yr2_locs, by = "Cell_ID")
  bob <- focal_species[which(focal_species$Spp_Obs == "Bobcat"),] %>%
    left_join(yr2_locs, by = "Cell_ID")
  bear <-   bob <- focal_species[which(focal_species$Spp_Obs == "Black bear"),] %>%
    left_join(yr2_locs, by = "Cell_ID")
  
  
  
  
  
  
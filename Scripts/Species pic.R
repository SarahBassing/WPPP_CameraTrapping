  #  --------------------------------------  
  #  Cameron Ho September 2021
  #  Sarah Bassing Quantitative Ecology Lab
  #  --------------------------------------
  #  Function to locate specific images and copy them to a new folder. 
  #  Requires a CSV file containing image number, file directory, and year (Year1, 
  #  Year2, Year3 of deployment) for specific images of interest.  
  #
  #  Create CSVs containing desired data using Detections_by_Camera_Station.R 
  #  in the WPPP_CameraTrapping R project.
  #  --------------------------------------
  
  #  Load libraries
  library(filesstrings) # use of file.move
  library(lubridate)    # to pull year
  library(stringr)      # use of str_split
  library(progress)     # progress bar
  
  #  Set the working directory where the raw images are saved
  #setwd("F:/WA PPP")
  setwd("H:/WPPP image data")
  
  #  Function to find and save specific images from master image set
  pull.spp.images <- function(folder, file.name){
    
    # creating a folder to put all of the folders in with the appropriate name
    dir.create(folder, showWarnings=F) 
    
    # reading in the csv with the desired images of interest (should be placed in the working directory)
    ioi <- read.csv(file.name)
    ioi$sd <- sapply(str_split(sapply(str_split(ioi$RelativePath, "_"), "[", 2),"\\\\"), "[", 2)  # don't question the 4 backslashes, just trust
    
    # loop that goes through each image of interest, adds the cell#_sd# to the name, and copies the images into the new folder
    pb <- txtProgressBar(0, nrow(ioi), style = 3) # progress bar
    for(i in 1:nrow(ioi)){
      
      
      # which image to pull
      pull <- ioi[i,]
      
      # pulling all the necessary information about image i
      pic.path <- pull$RelativePath
      pic.name <- pull$File
      if(pull$Year == "Year1"){    deploy.year <- 2018}
      if(pull$Year == "Year2"){    deploy.year <- 2019}
      if(pull$Year == "Year3"){    deploy.year <- 2020}
      pull.year <- deploy.year + 1
      cell <- substr(pull$CameraLocation, 1,6)
      sd <- pull$sd
      CameraLocation <- pull$CameraLocation
      RXfile <- str_sub(pull$RelativePath,-8,-1)
      
      
      # for Northeast study area
      if(substr(pull$CameraLocation, 1, 2) == "NE"){
        
        # moving image and adding camera cell and sd card to name
        file.copy(from=paste0("F:/WA PPP/Camera Trap Data Northeast/Cameras_", deploy.year, "-", pull.year, "/", cell,"_complete/", pic.path, "/", pic.name),  #putting images in a temp folder
                  to=paste0(getwd(), "/", folder, "/", CameraLocation, "_", sd, "_", RXfile, "_", pic.name))
       }
  
      
      # for Okanogan study area
      if(substr(pull$CameraLocation, 1, 2) == "OK"){
        
        # moving image and adding camera cell and sd card to name
        file.copy(from=paste0("F:/WA PPP/Camera Trap Data Okanogan/Cameras_", deploy.year, "-", pull.year, "/", cell,"_complete/", pic.path, "/", pic.name),  #putting images in a temp folder
                  to=paste0(getwd(), "/", folder, "/", CameraLocation, "_", sd, "_", RXfile, "_", pic.name))
      }
      
      setTxtProgressBar(pb, i)
    }
  }
  
  
  #### Run function
  ## folder = "new folder name that will contain the images"
  ## file = "name of the csv with the images that you want to pull"
  pull.spp.images(folder="WPPP_Turkey_Detections1a", file.name= "F:/WA PPP/turkey1.csv")
  pull.spp.images(folder="WPPP_Turkey_Detections2a", file.name= "F:/WA PPP/turkey2.csv")
  pull.spp.images(folder="WPPP_Turkey_Detections3a", file.name= "F:/WA PPP/turkey3.csv")
  pull.spp.images(folder="WPPP_Turkey_Detections4a", file.name= "F:/WA PPP/turkey4.csv")
  pull.spp.images(folder="WPPP_Turkey_Detections5a", file.name= "F:/WA PPP/turkey5.csv")
  pull.spp.images(folder="WPPP_Grouse_Detections", file.name= "F:/WA PPP/Grouse_allimgs_2022-07-01.csv")
  pull.spp.images(folder="WPPP_Turkey_Detections", file.name="F:/WA PPP/Turkey_allimgs_2022-07-01.csv")

  
  #  next step is to mess around with moving where the new folder goes- want to
  #  save it somewhere other than my external hard drives. Updated the file.copy 
  #  code so it's not looking to the current working directory (no getwd()) OR
  #  don't set the working directory above since the from directory is hard coded 
  #  anyway. 
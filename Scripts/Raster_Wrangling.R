    ##  Raster Cropping & Reprojection Function
    ##  Sarah Bassing
    ##  Washington Predator-Prey Project
    ##  May 2019
################################################################################

  #  Function to crop and reproject various raster files to so extent and 
  #  projection are consistent with study areas. This is not a quick function 
  #  because it has to reproject each raster multiple times and they are of 
  #  relatively fine resolution (generally 30 x 30 meters). Final output is a
  #  list of rasters (out) that have been cropped to the extent of each study area.

  #  Requires rgdal and raster packages

################################################################################

  ####  Initial set up  ####

  #  Load packages
  library(rgdal)
  library(raster)

  #  Read in study area shapefiles
  OK <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "METHOW_SA")
  NE <- readOGR("./Shapefiles/fwdstudyareamaps", layer = "NE_SA") 
  
  
  #  Read in rasters
  dem <- raster("./Shapefiles/WA_DEM_files/WA_DEM/wa_dem1.img")
  nlcd_landcov <- raster("./Shapefiles/Land_cover/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img")
    
  #  Check projections
  projection(OK)
  projection(dem)
  projection(walandcov)
  projection(uslandcov)
  #  They're all different. Shocker.
  
################################################################################
  
  ####  Function  ####
  
  #  Reproject study area polygons to match the rasters
  #  Crop the rasters to the study area bounding boxes
  #  Reproject cropped rasters back to study area projection
  
  crop_raster <- function(rast, sa) {

    #  Define projections
    #sa_proj <- projection(sa[[1]])
    new_proj <- projection(rast)
    
    #  Reproject polygons to match raster
    ok_newproj <- spTransform(sa[[1]], new_proj)
    ne_newproj <- spTransform(sa[[2]], new_proj)
    
    #  Extract bounding boxes of polygons
    ok_box <- bbox(ok_newproj)
    ne_box <- bbox(ne_newproj)
    
    #  Crop raster to match bounding boxes of polygons
    ok_rast <- crop(rast, ok_box)
    ne_rast <- crop(rast, ne_box)
    
    #  Reproject cropped rasters to WGS84 lat/long because for some reason this
    #  is the only way I can get it to reproject to other projections...
    #  Then reproject that version to match the original polygon projection
    ok_rast_latlong <- projectRaster(ok_rast, crs = ('+proj=longlat'))
    ok_rast_reproj <- projectRaster(ok_rast_latlong, crs = crs(sa[[1]]))
    ne_rast_latlong <- projectRaster(ne_rast, crs = ('+proj=longlat'))
    ne_rast_reproj <- projectRaster(ne_rast_latlong, crs = crs(sa[[2]]))
    
    #  Return cropped and reprojected rasters as a list
    return(list(ok_rast_reproj, ne_rast_reproj))
    # return(list(ok_rast_reproj, ne_rast_reproj))
    
  } 
  
  
  #  Apply function to a specific raster
  out <- crop_raster(rast = nlcd_landcov, sa = list(OK, NE)) #okpoly = OK, nepoly = NE
  OK_landcov <- unlist(out[[1]])
  NE_landcov <- unlist(out[[2]])

  
  #  Haven't figured out how to apply function to a group of rasters yet...
  #  This doesn't work
  #  How do I provide the function a list and get a list of lists back???
  # rast <- c(uslandcov, dem)
  # 
  # for(i in 1:lenght(rast)) {
  #     out[[i]] <- crop_raster(rast[[i]], sa = list(OK, NE)) 
  # }
  
################################################################################
  
  ####  Plot and Save  ####
  
  #  How's it look?
  plot(OK_landcov)
  plot(OK, add = T)
  
  plot(NE_landcov)
  plot(NE, add = T)
  
  
  #  Save cropped raster layers for mapping and data extraction
  #  HFA is Erdas Imagine Images w/ file extension .img
  # writeRaster(OK_landcov, filename ="./Shapefiles/Land_cover/OK_landcov", "HFA")
  # writeRaster(NE_landcov, filename = "./Shapefiles/Land_cover/NE_landcov", "HFA")

  
  
  
  
  
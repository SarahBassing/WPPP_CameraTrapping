  #'  ============================================
  #'  Percent Canopy Cover- Global Forest Change
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  February 2021
  #'  ============================================
  #'  Calculate percent canopy cover across WPPP extended boundary. 
  #'  Uses data downloaded from University of Maryland's Global Forest Change database
  #'  http://earthenginepartners.appspot.com/science-2013-global-forest
  #'  Based on Hansen et al. 2013: https://science.sciencemag.org/content/342/6160/850
  
  #'  Load libraries
  library(sf)
  library(rgeos)
  library(raster)
  
  #'  Read in spatial data
  wppp_bound <- st_read("G:/My Drive/1_Repositories/WPPP_Data_Integration/Shapefiles/WPPP_CovariateBoundary", layer = "WPPP_CovariateBoundary")
  
  #'  Identify projections & resolutions of relevant features
  sa_proj <- projection("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  projection(wppp_bound)
  
  ####  GLOBAL FOREST CHANGE  ####
  #'  Read in Global Forest Change Data (WPPP boundary covers two 10x10 degrees tiles)
  #'  Tree canopy cover for year 2000: percent canopy closure for all veg >5m
  #'  Treecover2000: 0-100 % canopy cover at pixel level
  # gfc_treecovW <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2019-v1.7_treecover2000_50N_130W.tif")
  # gfc_treecovE <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2019-v1.7_treecover2000_50N_120W.tif")
  gfc_treecovW <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2020-v1.8_treecover2000_50N_130W.tif")
  gfc_treecovE <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2020-v1.8_treecover2000_50N_120W.tif")
  #'  Forest cover gain: forest change from non-forest to forest (1 = gain, 0 = no gain)
  #'  Gains: 1 = 100% gain at pixel level at any point between 2001 & 2019
  # gfc_gainW <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2019-v1.7_gain_50N_130W.tif")
  # gfc_gainE <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2019-v1.7_gain_50N_120W.tif")
  gfc_gainW <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2020-v1.8_gain_50N_130W.tif")
  gfc_gainE <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2020-v1.8_gain_50N_120W.tif")
  #'  Gross forest cover loss (lossyear): stand-replacing disturbance, forest
  #'  changed from forest to non-forest at pixel level (0 = no loss, 1-19 = loss 
  #'  detected primarily in year 2001-2019, respectively)
  #'  Loss: 1 through 19 = 100% loss at pixel level during year 2001-2019 
  # gfc_lossW <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2019-v1.7_lossyear_50N_130W.tif")
  # gfc_lossE <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2019-v1.7_lossyear_50N_120W.tif")
  gfc_lossW <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2020-v1.8_lossyear_50N_130W.tif")
  gfc_lossE <- raster("G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/Hansen_GFC-2020-v1.8_lossyear_50N_120W.tif")
  projection(gfc_treecovW)
  res(gfc_treecovW) #  Keep projection in mind here! (should equate to ~30m res)
  #'  Merge west and east GFC rasters to cover full extent of WPPP boundary
  gfc_treecov <- raster::merge(gfc_treecovW, gfc_treecovE)
  gfc_gain <- raster::merge(gfc_gainW, gfc_gainE)
  gfc_loss <- raster::merge(gfc_lossW, gfc_lossE)
  
  #'  Check it out
  projection(wppp_bound)
  projection(gfc_treecov)
  
  plot(gfc_treecov)
  plot(wppp_bound, add = T)
  plot(gfc_gain)
  plot(gfc_loss)
  
  #'  Crop rasters to WPPP extended boundary
  bb <- extent(wppp_bound)
  gfc_treecov_crop <- raster::crop(gfc_treecov, extent(bb))
  gfc_gain_crop <- raster::crop(gfc_gain, extent(bb))
  gfc_loss_crop <- raster::crop(gfc_loss, extent(bb))
  
  plot(gfc_treecov_crop)
  plot(wppp_bound, add = T)
  
  #'  Calculate tree cover gains & losses from base year (2000)
  #'  Convert all gain values that = 1 to be 100, else 0
  gain <- gfc_gain_crop
  gain[gain$layer == 1,] <- 100
  summary(gain)
  
  #'  Add gain pixels to treecov pixels 
  treecov_gain <- gfc_treecov_crop + gain
  #'  Values >100 occur b/c adding 100 to areas that area already 1 - 99% forested
  #'  Change all values >100 to 100
  treecov_gain[treecov_gain$layer > 100] <- 100
  #'  Double check it worked
  summary(treecov_gain)
  # plot(gfc_treecov_crop)
  # plot(treecov_gain)
  # plot(gain)
  
  #'  For 2017 tree cover layer, convert all loss values 1 - 17 to be 100
  #'  Values after 2017 are converted to 0
  loss17 <- gfc_loss_crop
  loss17[loss17$layer == 18,] <- 0
  loss17[loss17$layer == 19,] <- 0
  loss17[loss17$layer == 20,] <- 0
  loss17[loss17$layer >= 1,] <- 100
  #'  For 2018 tree cover layer, convert all loss values 1 - 18 to be 100
  #'  Values after 2018 are converted to 0
  loss18 <- gfc_loss_crop
  loss18[loss18$layer == 19,] <- 0
  loss18[loss18$layer == 20,] <- 0
  loss18[loss18$layer >= 1,] <- 100
  #'  For 2019 tree cover layer, convert all loss values 1 - 19 to be 100
  #'  Values after 2019 are converted to 0
  loss19 <- gfc_loss_crop
  loss19[loss19$layer == 20,] <- 0
  loss19[loss19$layer >= 1,] <- 100
  #'  For 2020 tree cover layer, convert all loss values >0 to be 100, else 0
  loss20 <- gfc_loss_crop
  loss20[loss20$layer >= 1,] <- 100
  #'  Double check it worked
  summary(loss17)
  summary(loss18)
  summary(loss19)
  summary(loss20)
  # plot(gfc_loss_crop)
  # plot(loss)
  
  #'  Subtract loss pixels from treecover pixels
  treecov_loss17 <- treecov_gain - loss17
  treecov_loss18 <- treecov_gain - loss18
  treecov_loss19 <- treecov_gain - loss19
  treecov_loss20 <- treecov_gain - loss20
  #'  Double check it worked
  summary(treecov_loss17)
  summary(treecov_loss18)
  summary(treecov_loss20)
  # plot(gfc_treecov_crop)
  # plot(treecov_loss)
  # plot(loss)
  #'  Negative numbers generated by subtracting treecov values from 100
  #'  Change all negative numbers to 0
  treecov_loss17[treecov_loss17$layer <= 0] <- 0
  treecov_loss18[treecov_loss18$layer <= 0] <- 0
  treecov_loss19[treecov_loss19$layer <= 0] <- 0
  treecov_loss20[treecov_loss20$layer <= 0] <- 0
  summary(treecov_loss19)
  plot(gfc_treecov_crop)
  plot(loss17)
  plot(treecov_loss17)
  plot(loss18)
  plot(treecov_loss18)
  plot(loss19)
  plot(treecov_loss19)
  plot(loss20)
  plot(treecov_loss20)
  
  #'  Project to match prefered WPPP projection
  # treecov_2018 <- projectRaster(treecov_loss18, crs = crs(sa_proj))
  # treecov_2019 <- projectRaster(treecov_loss19, crs = crs(sa_proj))
  # projection(treecov_2019)
  # res(treecov_2019)
  # plot(treecov_2019)
  #'  Weird things happen here- lots of NAs introduced. Why does that happen?
  
  #'  Save annual tree cover rasters
  #'  Tree cover = canopy closure for all vegetation taller than 5m in height
  treecov_2017 <- treecov_loss17
  treecov_2018 <- treecov_loss18
  treecov_2019 <- treecov_loss19
  treecov_2020 <- treecov_loss20
  writeRaster(treecov_2017,
              filename = "G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/treecov_2017.tif",
              format="GTiff", overwrite=TRUE)
  writeRaster(treecov_2018,
              filename = "G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/treecov_2018.tif",
              format="GTiff", overwrite=TRUE)
  writeRaster(treecov_2019,
              filename = "G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/treecov_2019.tif",
              format="GTiff", overwrite=TRUE)
  writeRaster(treecov_2020,
              filename = "G:/My Drive/1 Dissertation/Analyses/Shapefiles/Global_Forest_Change/treecov_2020.tif",
              format="GTiff", overwrite=TRUE)
  
  pdf(file = "./Output/TreeCover_2000v2020.pdf")
  plot(gfc_treecov_crop, main = "Tree Canopy Cover 2000")
  plot(treecov_loss20, main = "Tree Canopy Cover 2020")
  dev.off()
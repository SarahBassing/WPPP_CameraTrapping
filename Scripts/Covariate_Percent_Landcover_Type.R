  #'  ============================================
  #'  Calculate Percent Landcover Type
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  May 2021
  #'  ============================================
  #'  # Cascadia Landcover Classifications
  #'  101 Water; 111 Glacier; 121 Barren; 201 Emergent Wetland; 202 Woody Wetland; 
  #'  Marine Wetland; 211 Mesic Grass; 212 Xeric Grass; 221 Mesic Shrub
  #'  222 Xeric Shrub; 230 Forest; 310 Agriculture; 331 Commercial/Industrial; 332 Residential	


  #'  Load libraries
  library(sf)
  library(stars)
  library(rgeos)
  library(raster)
  library(tidyverse)
  
  #'  Read in and inspect landcover data
  landcov18 <- raster("./Shapefiles/Cascadia_layers/landcover_2018.tif")
  landcov19 <- raster("./Shapefiles/Cascadia_layers/landcover_2019.tif")
  # nlcd <- raster("./Shapefiles/Land_cover/NLCD_2016_Land_Cover/NLCD_2016_Land_Cover_L48_20190424.img")
  projection(landcov18)
  res(landcov18)
  
  #'  Read in raster to match landcover rasters to (based on study area projection)
  dem <- raster("./Shapefiles/WA DEM rasters/WPPP_DEM_30m_reproj.tif")
  
  #'  Identify projections & resolutions of relevant features
  sa_proj <- projection("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")

  #'  Reproject landcover rasters to match dem 30x30m resolution and projection
  #'  Use nearest neighbor method (ngb) to compute values for new raster
  #'  ngb is appropriate for categorical variables (bilinear for continuous)
  landcov18_reproj <- projectRaster(landcov18, crs = crs(sa_proj), res = res(dem), method = "ngb")
  landcov19_reproj <- projectRaster(landcov19, crs = crs(sa_proj), res = res(dem), method = "ngb")
  
  #'  Check it out
  projection(landcov18_reproj)
  projection(dem)
  res(landcov18_reproj)
  res(dem)
  extent(landcov18_reproj)
  extent(dem)
  
  #'  Save
  writeRaster(landcov18_reproj, filename = "./Shapefiles/Cascadia_layers/landcov18_reproj.tif", format="GTiff", overwrite=TRUE)
  writeRaster(landcov19_reproj, filename = "./Shapefiles/Cascadia_layers/landcov19_reproj.tif", format="GTiff", overwrite=TRUE)
  

  #'  Reclassify landcover classifications (code provided by L. Satterfield)
  #'  ======================================================================
  #'  E.g., Forest is labeled 230, so everything from = 229 (non-inclusive), 
  #'  to = 230, (including 230) gets assigned value of 230. 
  #'  Anything from 230 (non-inclusive) through 333 gets a value of 0.
  #'  
  #'  "Forest" class that includes: Forest (230), Woody Wetland (202), and Emergent Wetland (201)
  forestm <- matrix(c(0,200,0,
                      200,202,1,
                      202,229,0,
                      229,230,1,
                      230,332,0),ncol=3,byrow=TRUE)
  
  #'  Check to make sure it looks right
  forestm
  #'  Reclassify the raster based on a matrix
  forest18 <- reclassify(landcov18_reproj, forestm)
  forest19 <- reclassify(landcov19_reproj, forestm)
  #'  Plot to see how it looks - only forest areas (binary format)
  plot(forest18, main = "Forest")
  
  
  #'  MesicGrass: Mesic Grass (211) + Barren (121)
  mgrassm <- matrix(c(0,120,0,
                      120,121,1,
                      121,210,0,
                      210,211,1,
                      211,332,0),ncol=3,byrow=TRUE)
  mgrassm
  mgrass18 <- reclassify(landcov18_reproj, mgrassm)
  mgrass19 <- reclassify(landcov19_reproj, mgrassm)
  plot(mgrass18, main = "Mesic Grass")
  
  #'  Developed: Agriculture (310), Commercial/Industrial (331), Residential (332)
  dvlpm <- matrix(c(0,309,0,
                    309,310,1,
                    310,330,0,
                    330,332,1),ncol=3,byrow=TRUE)
  dvlp18 <- reclassify(landcov18_reproj, dvlpm)
  dvlp19 <- reclassify(landcov19_reproj, dvlpm)
  plot(dvlp18, main = "Developed")
  
  #'  MesixMix: Mesic Shrub (221) + Mesic Grass (211)
  mesicmixm <- matrix(c(0,210,0,
                        210,211,1,
                        211,220,0,
                        220,221,1,
                        221,332,0),ncol=3,byrow=TRUE)
  mesicmix18 <- reclassify(landcov18_reproj, mesicmixm)
  mesicmix19 <- reclassify(landcov19_reproj, mesicmixm)
  plot(mesicmix18, main = "Mesic Mix")
  
  #'  ForestMix: Forest (230) + Woody Wetland (202) + Emergent Wetland (201) + Mesic Shrub (221) + Mesic Grass (211)
  forestmixm <- matrix(c(0,200,0,
                         200,202,1,
                         201,210,0,
                         210,211,1,
                         211,220,0,
                         220,221,1,
                         221,229,0,
                         229,230,1,
                         221,332,0),ncol=3,byrow=TRUE)
  forestmix18 <- reclassify(landcov18_reproj, forestmixm)
  forestmix19 <- reclassify(landcov19_reproj, forestmixm)
  plot(forestmix18, main = "Forest Mix")
  
  #'  ForestMix2: Forest (230) + Woody Wetland (202) + Emergent Wetland (201) + Mesic Shrub (221) 
  forestmix2m <- matrix(c(0,200,0,
                          200,202,1,
                          201,220,0,
                          220,221,1,
                          221,229,0,
                          229,230,1,
                          221,332,0),ncol=3,byrow=TRUE)
  forestmix218 <- reclassify(landcov18_reproj, forestmix2m)
  forestmix219 <- reclassify(landcov19_reproj, forestmix2m)
  plot(forestmix218, main = "Forest Mix 2")
  
  #'  XericGrass: Xeric Grass (212)
  xgrassm <- matrix(c(0,211,0,
                      211,212,1,
                      212,332,0),ncol=3,byrow=TRUE)
  xgrass18 <- reclassify(landcov18_reproj, xgrassm)
  xgrass19 <- reclassify(landcov19_reproj, xgrassm)
  plot(xgrass18)
  
  #'  XericShrub: Xeric Shrub (222)
  xshrubm <- matrix(c(0,221,0,
                      221,222,1,
                      222,332,0),ncol=3,byrow=TRUE)
  xshrub18 <- reclassify(landcov18_reproj, xshrubm)
  xshrub19 <- reclassify(landcov19_reproj, xshrubm)
  plot(xshrub19)
  
  
  #'  Create a moving window buffer with a 250 m radius
  #'  It's based on the resolution/projection of the input raster
  buffer <- raster::focalWeight(landcov18_reproj, 250, "circle")
  
  #'  Create proportional cover rasters: 
  #'  multiples the binary raster by the focal weight and then sums within the buffer
  forestprop_18 <- focal(forest18, buffer)
  forestprop_19 <- focal(forest19, buffer)
  mgrassprop_18 <- focal(mgrass18, buffer)
  mgrassprop_19 <- focal(mgrass19, buffer)
  dvlpprop_18 <- focal(dvlp18, buffer)
  dvlpprop_19 <- focal(dvlp19, buffer)
  mesicmixprop_18 <- focal(mesicmix18, buffer)
  mesicmixprop_19 <- focal(mesicmix19, buffer)
  forestmixprop_18 <- focal(forestmix18, buffer)
  forestmixprop_19 <- focal(forestmix19, buffer)
  forestmix2prop_18 <- focal(forestmix218, buffer)
  forestmix2prop_19 <- focal(forestmix219, buffer)
  xgrassprop_18 <- focal(xgrass18, buffer)
  xgrassprop_19 <- focal(xgrass19, buffer)
  xshrubprop_18 <- focal(xshrub18, buffer)
  xshrubprop_19 <- focal(xshrub19, buffer)
  
  # plot the output
  plot(forestprop_18, main = "Forest Prop")
  plot(mgrassprop_18, main = "Mesic Grass Prop")
  plot(dvlpprop_18, main = "Developed Prop")
  plot(mesicmixprop_18, main = "Mesic Mix Prop")
  plot(forestmixprop_18, main = "Forest Mix Prop")
  plot(forestmix2prop_18, main = "Forest Mix 2 Prop")
  plot(xgrassprop_18, main = "Xeric Grass Prop")
  plot(xshrubprop_18, main = "Xeric Shrub Prop")
  
  #write individual output rasters
  writeRaster(forestprop_18,"./Shapefiles/Cascadia_layers/forestprop_18.tif")
  writeRaster(forestprop_19,"./Shapefiles/Cascadia_layers/forestprop_19.tif")
  writeRaster(mgrassprop_18,"./Shapefiles/Cascadia_layers/mgrassprop_18.tif")
  writeRaster(mgrassprop_19,"./Shapefiles/Cascadia_layers/mgrassprop_19.tif")
  writeRaster(dvlpprop_18,"./Shapefiles/Cascadia_layers/dvlpprop_18.tif")
  writeRaster(dvlpprop_19,"./Shapefiles/Cascadia_layers/dvlpprop_19.tif")
  writeRaster(mesicmixprop_18,"./Shapefiles/Cascadia_layers/mesicmixprop_18.tif")
  writeRaster(mesicmixprop_19,"./Shapefiles/Cascadia_layers/mesicmixprop_19.tif")
  writeRaster(forestmixprop_18,"./Shapefiles/Cascadia_layers/forestmixprop_18.tif")
  writeRaster(forestmixprop_19,"./Shapefiles/Cascadia_layers/forestmixprop_19.tif")
  writeRaster(forestmix2prop_18,"./Shapefiles/Cascadia_layers/forestmix2prop_18.tif")
  writeRaster(forestmix2prop_19,"./Shapefiles/Cascadia_layers/forestmix2prop_19.tif")
  writeRaster(xgrassprop_18,"./Shapefiles/Cascadia_layers/xgrassprop_18.tif")
  writeRaster(xgrassprop_19,"./Shapefiles/Cascadia_layers/xgrassprop_19.tif")
  writeRaster(xshrubprop_18,"./Shapefiles/Cascadia_layers/xshrubprop_18.tif")
  writeRaster(xshrubprop_19,"./Shapefiles/Cascadia_layers/xshrubprop_19.tif")
  
  
  
  
  
  
  
  
  
  
  
  
  
  

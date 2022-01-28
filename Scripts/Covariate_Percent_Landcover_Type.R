  #'  ===================================================
  #'  Calculate Percent Landcover Type & Distance to Edge
  #'  Washington Predator-Prey Project
  #'  Sarah Bassing
  #'  May 2021
  #'  ===================================================
  #'  Script to reproject Cascadia landcover rasters to study area projection
  #'  (takes FOREVER and needs a lot of memory), reclassify landcover types to 
  #'  a smaller set of habitat types, and create rasters representing the percent
  #'  of lancover type within 250m of each pixel using a moving window analysis.
  #'  Reclassification and moving window code provided by L. Satterfield. Script
  #'  also reclassifies rasters to 1 and NA for habitat of interest and everything
  #'  else, which is used to calculate distance to edge in ArcGIS.
  #'
  #'  Initial Cascadia Landcover Classifications:
  #'  101 Water; 111 Glacier; 121 Barren; 201 Emergent Wetland; 202 Woody Wetland; 
  #'  Marine Wetland; 211 Mesic Grass; 212 Xeric Grass; 221 Mesic Shrub
  #'  222 Xeric Shrub; 230 Forest; 310 Agriculture; 331 Commercial/Industrial; 
  #'  332 Residential	
  #'  ============================================


  #'  Load libraries
  library(sf)
  library(stars)
  library(rgeos)
  library(raster)
  library(tidyverse)
  
  #'  Read in and inspect landcover data
  landcov18 <- raster("./Shapefiles/Cascadia_layers/landcover_2018.tif")
  landcov19 <- raster("./Shapefiles/Cascadia_layers/landcover_2019.tif")
  landcov20 <- raster("./Shapefiles/Cascadia_layers/landcover_2020.tif")
  # nlcd <- raster("./Shapefiles/Land_cover/NLCD_2016_Land_Cover/NLCD_2016_Land_Cover_L48_20190424.img")
  projection(landcov18)
  res(landcov18)
  
  #'  Read in raster to match landcover rasters to (based on study area projection)
  dem <- raster("./Shapefiles/WA DEM rasters/WPPP_DEM_30m_reproj.tif")
  
  #'  Identify projections & resolutions of relevant features
  sa_proj <- projection("+proj=lcc +lat_1=48.73333333333333 +lat_2=47.5 +lat_0=47 +lon_0=-120.8333333333333 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")
  wgs84 <- projection("+proj=longlat +datum=WGS84 +no_defs")
  
  #' #'  Reproject landcover rasters to match dem 30x30m resolution and projection
  #' #'  Use nearest neighbor method (ngb) to compute values for new raster
  #' #'  ngb is appropriate for categorical variables (bilinear for continuous)
  #' landcov18_reproj <- projectRaster(landcov18, crs = crs(sa_proj), res = res(dem), method = "ngb")
  #' landcov19_reproj <- projectRaster(landcov19, crs = crs(sa_proj), res = res(dem), method = "ngb")
  #' landcov20_reproj <- projectRaster(landcov20, crs = crs(sa_proj), res = res(dem), method = "ngb")

  #'  Check it out
  projection(landcov20_reproj)
  projection(dem)
  res(landcov20_reproj)
  res(dem)
  extent(landcov20_reproj)
  extent(dem)

  #' #'  Save
  #' writeRaster(landcov18_reproj, filename = "./Shapefiles/Cascadia_layers/Percent_Lancover_reproj/landcov18_reproj.tif", format="GTiff", overwrite=TRUE)
  #' writeRaster(landcov19_reproj, filename = "./Shapefiles/Cascadia_layers/Percent_Lancover_reproj/landcov19_reproj.tif", format="GTiff", overwrite=TRUE)
  #' writeRaster(landcov20_reproj, filename = "./Shapefiles/Cascadia_layers/Percent_Lancover_reproj/landcov20_reproj.tif", format="GTiff", overwrite=TRUE)
  
  
  #'  Load interpolated data
  landcov18_reproj <- raster("./Shapefiles/Cascadia_layers/Percent_Lancover_reproj/landcov18_reproj.tif")
  landcov19_reproj <- raster("./Shapefiles/Cascadia_layers/Percent_Lancover_reproj/landcov19_reproj.tif")
  landcov20_reproj <- raster("./Shapefiles/Cascadia_layers/Percent_Lancover_reproj/landcov20_reproj.tif")
  

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
  forest20 <- reclassify(landcov20_reproj, forestm)
  #'  Plot to see how it looks - only forest areas (binary format)
  plot(forest18, main = "Forest")
  
  #' #'  MesicGrass: Mesic Grass (211) + Barren (121)
  #' mgrassm <- matrix(c(0,120,0,
  #'                     120,121,1,
  #'                     121,210,0,
  #'                     210,211,1,
  #'                     211,332,0),ncol=3,byrow=TRUE)
  #' mgrassm
  #' mgrass18 <- reclassify(landcov18, mgrassm)
  #' mgrass19 <- reclassify(landcov19, mgrassm)
  #' plot(mgrass18, main = "Mesic Grass")
  
  #'  MesicGrass: Mesic Grass (211)
  mgrassm <- matrix(c(0,210,0,
                      210,211,1,
                      211,332,0),ncol=3,byrow=TRUE)
  mgrassm
  mgrass18 <- reclassify(landcov18_reproj, mgrassm)
  mgrass19 <- reclassify(landcov19_reproj, mgrassm)
  mgrass20 <- reclassify(landcov20_reproj, mgrassm)
  plot(mgrass18, main = "Mesic Grass")
  
  #'  Developed: Agriculture (310), Commercial/Industrial (331), Residential (332)
  dvlpm <- matrix(c(0,309,0,
                    309,310,1,
                    310,330,0,
                    330,332,1),ncol=3,byrow=TRUE)
  dvlp18 <- reclassify(landcov18_reproj, dvlpm)
  dvlp19 <- reclassify(landcov19_reproj, dvlpm)
  dvlp20 <- reclassify(landcov20_reproj, dvlpm)
  plot(dvlp18, main = "Developed")
  
  #'  MesixMix: Mesic Shrub (221) + Mesic Grass (211)
  mesicmixm <- matrix(c(0,210,0,
                        210,211,1,
                        211,220,0,
                        220,221,1,
                        221,332,0),ncol=3,byrow=TRUE)
  mesicmix18 <- reclassify(landcov18_reproj, mesicmixm)
  mesicmix19 <- reclassify(landcov19_reproj, mesicmixm)
  mesicmix20 <- reclassify(landcov20_reproj, mesicmixm)
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
  forestmix20 <- reclassify(landcov20_reproj, forestmixm)
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
  forestmix220 <- reclassify(landcov20_reproj, forestmix2m)
  plot(forestmix218, main = "Forest Mix 2")
  
  #'  XericGrass: Xeric Grass (212)
  xgrassm <- matrix(c(0,211,0,
                      211,212,1,
                      212,332,0),ncol=3,byrow=TRUE)
  xgrass18 <- reclassify(landcov18_reproj, xgrassm)
  xgrass19 <- reclassify(landcov19_reproj, xgrassm)
  xgrass20 <- reclassify(landcov20_reproj, xgrassm)
  plot(xgrass18, main = "Xeric Grass")
  
  #'  XericShrub: Xeric Shrub (222)
  xshrubm <- matrix(c(0,221,0,
                      221,222,1,
                      222,332,0),ncol=3,byrow=TRUE)
  xshrub18 <- reclassify(landcov18_reproj, xshrubm)
  xshrub19 <- reclassify(landcov19_reproj, xshrubm)
  xshrub20 <- reclassify(landcov20_reproj, xshrubm)
  plot(xshrub19, main = "Xeric Shrub")
  
  
  #'  Create a moving window buffer with a 250 m radius
  #'  It's based on the resolution/projection of the input raster
  #'  If in UTMs then 250m, if in lat/long then 0.00025 degrees is approx. 250m
  # buffer <- raster::focalWeight(landcov18, 0.00025, "circle") 
  buffer <- raster::focalWeight(landcov18_reproj, 250, "circle") # when projected
  
  
  
  
  #'  Create proportional cover rasters: 
  #'  multiples the binary raster by the focal weight and then sums within the buffer
  forestprop_18 <- focal(forest18, buffer)
  forestprop_19 <- focal(forest19, buffer)
  forestprop_20 <- focal(forest20, buffer)
  mgrassprop_18 <- focal(mgrass18, buffer)
  mgrassprop_19 <- focal(mgrass19, buffer)
  mgrassprop_20 <- focal(mgrass20, buffer)
  dvlpprop_18 <- focal(dvlp18, buffer)
  dvlpprop_19 <- focal(dvlp19, buffer)
  dvlpprop_20 <- focal(dvlp20, buffer)
  mesicmixprop_18 <- focal(mesicmix18, buffer)
  mesicmixprop_19 <- focal(mesicmix19, buffer)
  mesicmixprop_20 <- focal(mesicmix20, buffer)
  forestmixprop_18 <- focal(forestmix18, buffer)
  forestmixprop_19 <- focal(forestmix19, buffer)
  forestmixprop_20 <- focal(forestmix20, buffer)
  forestmix2prop_18 <- focal(forestmix218, buffer)
  forestmix2prop_19 <- focal(forestmix219, buffer)
  forestmix2prop_20 <- focal(forestmix220, buffer)
  xgrassprop_18 <- focal(xgrass18, buffer)
  xgrassprop_19 <- focal(xgrass19, buffer)
  xgrassprop_20 <- focal(xgrass20, buffer)
  xshrubprop_18 <- focal(xshrub18, buffer)
  xshrubprop_19 <- focal(xshrub19, buffer)
  xshrubprop_20 <- focal(xshrub20, buffer)
  
  #'  Plot the output
  plot(forestprop_18, main = "Forest Prop")
  plot(mgrassprop_18, main = "Mesic Grass Prop")
  plot(dvlpprop_18, main = "Developed Prop")
  plot(mesicmixprop_18, main = "Mesic Mix Prop")
  plot(forestmixprop_18, main = "Forest Mix Prop")
  plot(forestmix2prop_18, main = "Forest Mix 2 Prop")
  plot(xgrassprop_18, main = "Xeric Grass Prop")
  plot(xshrubprop_18, main = "Xeric Shrub Prop")
  
  #'  Write individual output rasters- make sure to adjust name with _wgs84 if needed
  writeRaster(forestprop_18,"./Shapefiles/Cascadia_layers/forestprop_18.tif")
  writeRaster(forestprop_19,"./Shapefiles/Cascadia_layers/forestprop_19.tif")
  writeRaster(forestprop_20,"./Shapefiles/Cascadia_layers/forestprop_20.tif")
  writeRaster(mgrassprop_18,"./Shapefiles/Cascadia_layers/mgrassprop_18.tif")
  writeRaster(mgrassprop_19,"./Shapefiles/Cascadia_layers/mgrassprop_19.tif")
  writeRaster(mgrassprop_20,"./Shapefiles/Cascadia_layers/mgrassprop_20.tif")
  writeRaster(dvlpprop_18,"./Shapefiles/Cascadia_layers/dvlpprop_18.tif")
  writeRaster(dvlpprop_19,"./Shapefiles/Cascadia_layers/dvlpprop_19.tif")
  writeRaster(dvlpprop_20,"./Shapefiles/Cascadia_layers/dvlpprop_20.tif")
  writeRaster(mesicmixprop_18,"./Shapefiles/Cascadia_layers/mesicmixprop_18.tif")
  writeRaster(mesicmixprop_19,"./Shapefiles/Cascadia_layers/mesicmixprop_19.tif")
  writeRaster(mesicmixprop_20,"./Shapefiles/Cascadia_layers/mesicmixprop_20.tif")
  writeRaster(forestmixprop_18,"./Shapefiles/Cascadia_layers/forestmixprop_18.tif")
  writeRaster(forestmixprop_19,"./Shapefiles/Cascadia_layers/forestmixprop_19.tif")
  writeRaster(forestmixprop_20,"./Shapefiles/Cascadia_layers/forestmixprop_20.tif")
  writeRaster(forestmix2prop_18,"./Shapefiles/Cascadia_layers/forestmix2prop_18.tif")
  writeRaster(forestmix2prop_19,"./Shapefiles/Cascadia_layers/forestmix2prop_19_.tif")
  writeRaster(forestmix2prop_20,"./Shapefiles/Cascadia_layers/forestmix2prop_20.tif")
  writeRaster(xgrassprop_18,"./Shapefiles/Cascadia_layers/xgrassprop_18.tif")
  writeRaster(xgrassprop_19,"./Shapefiles/Cascadia_layers/xgrassprop_19.tif")
  writeRaster(xgrassprop_20,"./Shapefiles/Cascadia_layers/xgrassprop_20.tif")
  writeRaster(xshrubprop_18,"./Shapefiles/Cascadia_layers/xshrubprop_18.tif")
  writeRaster(xshrubprop_19,"./Shapefiles/Cascadia_layers/xshrubprop_19.tif")
  writeRaster(xshrubprop_20,"./Shapefiles/Cascadia_layers/xshrubprop_20.tif")
  

  #'  Calculate distance to edge using the forest raster
  #'  Distance to forest edge
  #'  Convert 0s to NAs
  forest18na <- forest18
  forest19na <- forest19
  forest20na <- forest20
  forest18na[forest18na == 0] <- NA
  forest19na[forest19na == 0] <- NA
  forest20na[forest20na == 0] <- NA
  plot(forest18na)
  #' #'  Calculate distance to edge --- NOT DOING WHAT I WANT
  #' dist18 <- raster::distance(forest18na)
  #' dist18 <- raster::distance(forest18na, filename="forestedge18")
  #' head(dist18)
  #' summary(dist18)
  #' plot(dist18)
  #' dist19 <- raster::distance(forest19)
  #' dist20 <- raster::distance(forest20)
  #'  Distance to open edge
  #'  Reformat forest rasters so forested habitat is NA and open habitat is 1
  open18 <- forest18
  open18[open18 == 1] <- NA
  open18[open18 == 0] <- 1
  plot(open18)
  open19 <- forest19
  open19[open19 == 1] <- NA
  open19[open19 == 0] <- 1
  plot(open19)
  open20 <- forest20
  open20[open20 == 1] <- NA
  open20[open20 == 0] <- 1
  plot(open20)
  
  #'  Save- NOTE in the file name whether these are projected or in wgs84!
  writeRaster(forest18, "./Shapefiles/Cascadia_layers/closed18.tif", overwrite = TRUE)
  writeRaster(forest19, "./Shapefiles/Cascadia_layers/closed19.tif", overwrite = TRUE)
  writeRaster(forest20, "./Shapefiles/Cascadia_layers/closed20.tif", overwrite = TRUE)
  writeRaster(forest18na, "./Shapefiles/Cascadia_layers/closed18na.tif", overwrite = TRUE)
  writeRaster(forest19na, "./Shapefiles/Cascadia_layers/closed19na.tif", overwrite = TRUE)
  writeRaster(forest20na, "./Shapefiles/Cascadia_layers/closed20na.tif", overwrite = TRUE)
  writeRaster(open18, "./Shapefiles/Cascadia_layers/open18na.tif", overwrite = TRUE)
  writeRaster(open19, "./Shapefiles/Cascadia_layers/open19na.tif", overwrite = TRUE)
  writeRaster(open20, "./Shapefiles/Cascadia_layers/open20na.tif", overwrite = TRUE)
  
  
  #'  Calculate percent open habitat for HMM analyses
  #'  Open landcover classes include: Barren (121), wetland (201), wetland woody (202),   
  #'  mesic grass (211), xeric grass (212), xeric shrub (222), agriculture (301)
  #'  
  #'  Currently including mesic shrub (221) in closed habitat classification  
  #'  (along with forest) b/c most of it is shrubby forest mix that grows after 
  #'  logging in the NE and regen in the OK after fire- dense veg makes for low
  #'  visibility and limited room to chase.
  
  #'  Load study area shapefiles
  OK.SA <- st_read("./Shapefiles/fwdstudyareamaps", layer = "METHOW_SA") %>%
    st_transform(crs = wgs84)
  OK.SA <- as(OK.SA, "Spatial")
  NE.SA <- st_read("./Shapefiles/fwdstudyareamaps", layer = "NE_SA") %>%
    st_transform(crs = wgs84)
  NE.SA <- as(NE.SA, "Spatial")
  
  #'  Create matrix of open (1) and non-open (0) values
  openhab <- matrix(c(0,120,0,
                      120,121,1,
                      121,200,0,
                      200,202,1,
                      202,210,0,
                      210,212,1,
                      212,221,0,
                      221,222,1,
                      222,300,0,
                      300,301,1,
                      301,332,0),ncol=3,byrow=TRUE)
  #'  Reclassify landcover rasters based on open habitat matrix
  openhab18 <- reclassify(landcov18, openhab)
  openhab19 <- reclassify(landcov19, openhab)
  openhab20 <- reclassify(landcov20, openhab)
  
  #'  Visualize
  plot(openhab18)
  plot(openhab19)
  plot(openhab20)
  
  #'  Save example plots with study areas mapped across them
  pdf(file = "./Open_Habitat_Map.pdf")
  plot(OK.SA, main = "Open Habitat 2018")
  plot(openhab18, add = TRUE, legend = FALSE)
  plot(OK.SA, add = TRUE, lwd = 3.0)
  plot(NE.SA, main = "Open Habitat 2018")
  plot(openhab18, add = TRUE, legend = FALSE)
  plot(NE.SA, add = TRUE, lwd = 3.0)
  dev.off()
  
  #'  Calculate percent open habitat with 250m of each pixel using buffer command
  #'  defined above (make sure to use correct version based on landcov projection)
  openprop_18 <- focal(openhab18, buffer)
  openprop_19 <- focal(openhab19, buffer)
  openprop_20 <- focal(openhab20, buffer)
  
  #'  Save!
  writeRaster(openprop_18, "./Shapefiles/Cascadia_layers/PercOpen18.tif", overwrite = TRUE)
  writeRaster(openprop_19, "./Shapefiles/Cascadia_layers/PercOpen19.tif", overwrite = TRUE)
  writeRaster(openprop_20, "./Shapefiles/Cascadia_layers/PercOpen20.tif", overwrite = TRUE)
  writeRaster(openhab18, "./Shapefiles/Cascadia_layers/OpenHabitat18.tif", overwrite = TRUE)
  writeRaster(openhab19, "./Shapefiles/Cascadia_layers/OpenHabitat19.tif", overwrite = TRUE)
  writeRaster(openhab20, "./Shapefiles/Cascadia_layers/OpenHabitat20.tif", overwrite = TRUE)
  
  #' #'  Review shrub landcover to determine if should be lumped w/ open vs closed habitat
  #' #'  MesixShrub: Mesic Shrub (221)
  #' mesicshrub <- matrix(c(0,220,0,
  #'                       220,221,1,
  #'                       221,332,0),ncol=3,byrow=TRUE)
  #' mesicshrub18 <- reclassify(landcov18, mesicshrub)
  #' mesicshrub19 <- reclassify(landcov19, mesicshrub)
  #' mesicshrub20 <- reclassify(landcov20, mesicshrub)
  #' 
  #' pdf(file = "./NE_Mesic_Shrub_map.pdf")
  #' plot(OK.SA, main = "Mesic Shrub 2018")
  #' plot(mesicshrub18, add = TRUE, legend = FALSE)
  #' plot(OK.SA, add = TRUE, lwd = 3.0)
  #' dev.off()
  #' 
  #' #'  XericShrub: Xeric Shrub (222)
  #' xshrubm <- matrix(c(0,221,0,
  #'                     221,222,1,
  #'                     222,332,0),ncol=3,byrow=TRUE)
  #' xshrub18 <- reclassify(landcov18, xshrubm)
  #' xshrub19 <- reclassify(landcov19, xshrubm)
  #' xshrub20 <- reclassify(landcov20, xshrubm)
  #' 
  #' pdf(file = "./NE_Xeric_Shrub_map.pdf")
  #' plot(OK.SA, main = "Zeric Shrub 2018")
  #' plot(xshrub18, add = TRUE, legend = FALSE)
  #' plot(OK.SA, add = TRUE, lwd = 3.0)
  #' dev.off()
  #' 
  #' #'  WetWood: wetland woody (202)
  #' wetwoody <- matrix(c(0,201,0,
  #'                     201,202,1,
  #'                     202,332,0),ncol=3,byrow=TRUE)
  #' wetwood18 <- reclassify(landcov18, wetwoody)
  #' wetwood19 <- reclassify(landcov19, wetwoody)
  #' wetwood20 <- reclassify(landcov20, wetwoody)
  #' 
  #' pdf(file = "./NE_Wet_Woody_map.pdf")
  #' plot(NE.SA, main = "Wetland Wood 2018")
  #' plot(wetwood18, add = TRUE, legend = FALSE)
  #' plot(NE.SA, add = TRUE, lwd = 3.0)
  #' dev.off()
  
  
  

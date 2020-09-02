new_proj <- CRS("+proj=lcc +lat_1=45.83333333333334 +lat_2=47.33333333333334 +lat_0=45.33333333333334 +lon_0=-120.5 +x_0=500000 +y_0=0 +ellps=GRS80 +units=m +no_defs ")

OK_Centroids_Final_2018 <- readOGR("G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018", layer = "OK_Centroids_Final_2018")
OK_Cent_2018 <- spTransform(OK_Centroids_Final_2018, new_proj)

plot(MW)
plot(Q4_MW, add = T, col = "lightblue")
plot(Q3_MW, add = T, col = "darkgreen")
plot(Q2_MW, add = T, col = "green")
plot(Q1_MW, add = T, col = "yellow")
plot(MW_cam_points_32519, pch = 19, col ="black", add = T)
#plot(OK_Cent_2018, pch = 19, col = "black", add = T)

NE_Centroids_Final_2018 <- readOGR("G:/My Drive/1 Predator Prey Project/Field Work/Camera Locations/Summer 2018", layer = "NE_Centoids_Final_2018")
NE_Cent_2018 <- spTransform(NE_Centroids_Final_2018, new_proj)

plot(NE)
plot(Q1_NE, add = T, col = "yellow") 
plot(Q2_NE, add = T, col = "green")
plot(Q3_NE, add = T, col = "darkgreen")
plot(Q4_NE, add = T, col = "lightblue")
plot(NE_cam_points_32519, pch = 19, col ="black", add = T)
#plot(NE_Cent_2018, pch = 19, col = "black", add = T)



plot(MW)
plot(BQ1_MW, add = T, col = "yellow") 
plot(BQ2_MW, add = T, col = "green")
plot(BQ3_MW, add = T, col = "darkgreen")
plot(BQ4_MW, add = T, col = "lightblue")
plot(OK_Cent_2018, pch = 19, col = "black", add = T)
#plot(MW_cam_points_32519, pch = 19, col ="black", add = T)

plot(NE)
plot(BQ1_NE, add = T, col = "yellow") 
plot(BQ2_NE, add = T, col = "green")
plot(BQ3_NE, add = T, col = "darkgreen")
plot(BQ4_NE, add = T, col = "lightblue")
plot(NE_Cent_2018, pch = 19, col = "black", add = T)
#plot(NE_cam_points_32519, pch = 19, col ="black", add = T)
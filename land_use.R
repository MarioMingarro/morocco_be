library(raster)
library(rgdal)
library(tidyverse)

shp <- readOGR("D:/MARRUECOS/watershed.shp")

forest <- c("PFT1", "PFT2", "PFT3", "PFT4", "PFT5", "PFT6", "PFT7", "PFT8")
non_forest <- c("PFT9", "PFT10", "PFT11", "PFT12", "PFT13", "PFT14")

natural <- c("PFT1", "PFT2", "PFT3", "PFT4", "PFT5", "PFT6", "PFT7", "PFT8", "PFT9", "PFT10", "PFT11", "PFT12", "PFT13", "PFT14")
irrigated_crop <- c("PFT16", "PFT18", "PFT20", "PFT22", "PFT24", "PFT26", "PFT28", "PFT30")
non_irrigated_crop <- c("PFT15", "PFT17", "PFT19", "PFT21", "PFT23", "PFT25", "PFT27", "PFT29")

years <- c("2015", "2040")
years <- c("2015", "2020", "2025", "2030", "2035", "2040")

## Forest ----
raster_natural <- raster::stack()

for (i in natural){
  for (j in years){
    raster <- raster(paste0("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                     varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    raster_natural <- stack(raster_natural, raster)
    assign(paste0("raster_natural_", j), calc(raster_natural, max))
  }
}

natural <- raster::extract(raster_natural_2015, shp, fun = mean, df= T)


natural <- left_join(natural, raster::extract(raster_natural_2040, shp, fun = mean, df= T), by = "ID")

natural2 <- raster::extract(raster_natural_2040, shp, fun = mean, df= T)

writeRaster(raster_natural_2015, "D:/MARRUECOS/TEST_MAPS/raster_natural_2015.tif")
writeRaster(raster_natural_2020, "D:/MARRUECOS/TEST_MAPS/raster_natural_2020.tif")
writeRaster(raster_natural_2025, "D:/MARRUECOS/TEST_MAPS/raster_natural_2025.tif")
writeRaster(raster_natural_2030, "D:/MARRUECOS/TEST_MAPS/raster_natural_2030.tif")
writeRaster(raster_natural_2035, "D:/MARRUECOS/TEST_MAPS/raster_natural_2035.tif")
writeRaster(raster_natural_2040, "D:/MARRUECOS/TEST_MAPS/raster_natural_2040.tif")


plot(raster_forest_2015)
plot(raster_forest_2050)



## Non irrigated crop ----

raster_non_irrigated_crop <- raster::stack()

for (i in non_irrigated_crop){
  for (j in years){
    raster <- raster(paste0("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                     varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    raster_non_irrigated_crop <- stack(raster_non_irrigated_crop, raster)
    assign(paste0("raster_non_irrigated_crop_", j), calc(raster_non_irrigated_crop, max))
  }
}
raster::extract(raster_non_irrigated_crop_2015, shp, fun = mean, df= T)

writeRaster(raster_non_irrigated_crop_2015, "D:/MARRUECOS/TEST_MAPS/raster_non_irrigated_crop_2015.tif")
writeRaster(raster_non_irrigated_crop_2050, "D:/MARRUECOS/TEST_MAPS/raster_non_irrigated_crop_2050.tif")

plot(raster_non_irrigated_crop_2015)
plot(raster_non_irrigated_crop_2050)
plot(shp)

####
kk <- raster::extract(raster_non_irrigated_crop_2015, shp, fun = mean, df= T)

kk2 <- left_join(kk, shp_data, by = c(kk$ID, shp_data$X1pol_d))
shp_data <- shp@data

plot(raster_non_irrigated_crop_2015, add=T)
plot(shp[224,])
plot(shp[224,], add = T)
head(shp)
plot(shp@polygons)

raster_forest_2020 <- calc(raster_forest, max)
raster_forest_2025 <- calc(raster_forest, max)
raster_forest_2030 <- calc(raster_forest, max)
raster_forest_2040 <- calc(raster_forest, max)

raster_irrigated_crop <- raster::stack()

for (i in irrigated_crop){
  raster <- raster("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_2040.nc",
                   varname = paste0(i))
  raster <- t(flip(raster, direction = 'y'))
  raster <- mask(crop(raster, shp), shp)
  raster_irrigated_crop <- stack(raster_irrigated_crop, raster)
}

raster_irrigated_crop_2015 <- calc(raster_irrigated_crop, max)
raster_irrigated_crop_2025 <- calc(raster_irrigated_crop, max)
raster_irrigated_crop_2030 <- calc(raster_irrigated_crop, max)
raster_irrigated_crop_2040 <- calc(raster_irrigated_crop, max)

plot(raster_irrigated_crop_2015)
plot(raster_irrigated_crop_2040)

for (i in 1:length(forest)){
  for (j in 1:length(years)){
    raster <- raster(paste0("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc", 
                            varname=paste0(i))
                     raster <- t(flip(PFT1, direction='y'))
                     raster <- mask(crop(raster, shp), shp)
                     PFT2015 <- stack(PFT1,PFT2,PFT3,PFT4,PFT5,PFT6,PFT7,PFT8)
                     PFT2015 <- calc(PFT2015, max)
  }
}


PFT1 <- raster("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_2015.nc", 
               varname="PFT1")
PFT1 <- t(flip(PFT1, direction='y'))

PFT1 <- mask(crop(PFT1, shp), shp)

PFT2 <- raster("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_gfdl_2050.nc", 
               varname="PFT2")
PFT2 <- t(flip(PFT2, direction='y'))

PFT2 <- mask(crop(PFT2, shp), shp)

PFT3 <- raster("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_gfdl_2050.nc", 
               varname="PFT3")
PFT3 <- t(flip(PFT3, direction='y'))

PFT3 <- mask(crop(PFT3, shp), shp)

PFT4 <- raster("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_gfdl_2050.nc", 
               varname="PFT4")
PFT4 <- t(flip(PFT4, direction='y'))

PFT4 <- mask(crop(PFT4, shp), shp)

PFT5 <- raster("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_gfdl_2050.nc", 
               varname="PFT5")
PFT5 <- t(flip(PFT5, direction='y'))

PFT5 <- mask(crop(PFT5, shp), shp)
PFT6 <- raster("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_gfdl_2050.nc", 
               varname="PFT6")
PFT6 <- t(flip(PFT6, direction='y'))

PFT6 <- mask(crop(PFT6, shp), shp)
PFT7 <- raster("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_gfdl_2050.nc", 
               varname="PFT7")
PFT7 <- t(flip(PFT7, direction='y'))

PFT7 <- mask(crop(PFT7, shp), shp)
PFT8 <- raster("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_gfdl_2050.nc", 
               varname="PFT8")
PFT8 <- t(flip(PFT8, direction='y'))

PFT8 <- mask(crop(PFT8, shp), shp)




PFT2015 <- stack(PFT1,PFT2,PFT3,PFT4,PFT5,PFT6,PFT7,PFT8)
PFT2015 <- calc(PFT2015, max)
plot(PFT2015)

PFT2050 <- stack(PFT1,PFT2,PFT3,PFT4,PFT5,PFT6,PFT7,PFT8)
PFT2050 <- calc(PFT2050, max)
plot(PFT2050)






plot(PFT7)
plot(kk)
library(geocmeans)
data(LyonIris)
AnalysisFields <-c("Lden","NO2","PM25","VegHautPrt","Pct0_14",
                   "Pct_65","Pct_Img","TxChom1564","Pct_brevet","NivVieMed")
Data <- LyonIris@data[AnalysisFields]
for (Col in names(Data)){
  Data[[Col]] <- scale(Data[[Col]])
}

LyonIris$OID <- as.character(1:nrow(LyonIris))
FortiData <- ggplot2::fortify(LyonIris,region="OID")
GcmeansMaps<- mapClusters(LyonIris,GCmean$Belongings,undecided = 0.45)
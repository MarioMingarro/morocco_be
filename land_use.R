library(raster)
library(rgdal)
library(tidyverse)

shp <- readOGR("C:/GITHUB_REP/morocco_be/Basins/Basins_level8_ed.shp")

forest <- c("PFT1", "PFT2", "PFT3", "PFT4", "PFT5", "PFT6", "PFT7", "PFT8")
non_forest <- c("PFT9", "PFT10", "PFT11", "PFT12", "PFT13", "PFT14")

natural <- c("PFT1", "PFT2", "PFT3", "PFT4", "PFT5", "PFT6", "PFT7", "PFT8", "PFT9", "PFT10", "PFT11", "PFT12", "PFT13", "PFT14")
irrigated_crop <- c("PFT16", "PFT18", "PFT20", "PFT22", "PFT24", "PFT26", "PFT28", "PFT30")
non_irrigated_crop <- c("PFT15", "PFT17", "PFT19", "PFT21", "PFT23", "PFT25", "PFT27", "PFT29")


years <- c("2015", "2020", "2025", "2030", "2035", "2040")

## Natural ----
raster_natural <- raster::stack()

for (i in natural){
  for (j in years){
    raster <- raster(paste0("B:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                     varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    raster_natural <- stack(raster_natural, raster)
    assign(paste0("raster_natural_", j), calc(raster_natural, max))
  }
}
plot(raster)
## Irrigated crop ------
raster_irrigated <- raster::stack()

for (i in irrigated_crop){
  for (j in years){
    raster <- raster(paste0("B:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                     varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    raster_irrigated <- stack(raster_irrigated, raster)
    assign(paste0("raster_irrigated_", j), calc(raster_irrigated, max))
  }
}

raster_irrigated_crop <- raster::stack()
for (i in irrigated_crop){
    raster <- raster(paste0("B:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                     varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    raster_irrigated_crop <- stack(raster_irrigated_crop, raster)
    assign(paste0("raster_irrigated_crop_2040"), calc(raster_irrigated_crop, max))
}

irrigated_crop <- raster::extract(raster_irrigated_crop_2015, shp, fun = mean, df= T)

irrigated_crop <- left_join(irrigated_crop, raster::extract(raster_irrigated_crop_2040, shp, fun = mean, df= T), by = "ID")

## Non irrigated crop ----
raster_non_irrigated <- raster::stack()

for (i in non_irrigated_crop){
  for (j in years){
    raster <- raster(paste0("B:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_", j, ".nc"),
                     varname = paste0(i))
    raster <- t(flip(raster, direction = 'y'))
    raster <- mask(crop(raster, shp), shp)
    raster_non_irrigated <- stack(raster_non_irrigated, raster)
    assign(paste0("raster_non_irrigated_", j), calc(raster_non_irrigated, max))
  }
}

raster_non_irrigated_crop <- raster::stack()
for (i in non_irrigated_crop){
  raster <- raster(paste0("D:/DATA/land_use/Project ID 68344/GCAM-Demeter-LU/GCAM_Demeter_LU_ssp1_rcp26_modelmean_2040.nc"),
                   varname = paste0(i))
  raster <- t(flip(raster, direction = 'y'))
  raster <- mask(crop(raster, shp), shp)
  raster_non_irrigated_crop <- stack(raster_non_irrigated_crop, raster)
  assign(paste0("raster_non_irrigated_crop_2040"), calc(raster_non_irrigated_crop, max))
}

non_irrigated_crop <- raster::extract(raster_non_irrigated_crop_2015, shp, fun = mean, df= T)

non_irrigated_crop <- left_join(non_irrigated_crop, raster::extract(raster_non_irrigated_crop_2040, shp, fun = mean, df= T), by = "ID")

#####################################
writeRaster(raster_natural_2015, "A:/MARRUECOS/LAND_USE/raster_natural_2015.tif")
writeRaster(raster_natural_2020, "A:/MARRUECOS/LAND_USE/raster_natural_2020.tif")
writeRaster(raster_natural_2025, "A:/MARRUECOS/LAND_USE/raster_natural_2025.tif")
writeRaster(raster_natural_2030, "A:/MARRUECOS/LAND_USE/raster_natural_2030.tif")
writeRaster(raster_natural_2035, "A:/MARRUECOS/LAND_USE/raster_natural_2035.tif")
writeRaster(raster_natural_2040, "A:/MARRUECOS/LAND_USE/raster_natural_2040.tif")
writeRaster(raster_irrigated_2015, "A:/MARRUECOS/LAND_USE/raster_irrigated_2015.tif")
writeRaster(raster_irrigated_2020, "A:/MARRUECOS/LAND_USE/raster_irrigated_2020.tif")
writeRaster(raster_irrigated_2025, "A:/MARRUECOS/LAND_USE/raster_irrigated_2025.tif")
writeRaster(raster_irrigated_2030, "A:/MARRUECOS/LAND_USE/raster_irrigated_2030.tif")
writeRaster(raster_irrigated_2035, "A:/MARRUECOS/LAND_USE/raster_irrigated_2035.tif")
writeRaster(raster_irrigated_2040, "A:/MARRUECOS/LAND_USE/raster_irrigated_2040.tif")
writeRaster(raster_non_irrigated_2015, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2015.tif")
writeRaster(raster_non_irrigated_2020, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2020.tif")
writeRaster(raster_non_irrigated_2025, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2025.tif")
writeRaster(raster_non_irrigated_2030, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2030.tif")
writeRaster(raster_non_irrigated_2035, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2035.tif")
writeRaster(raster_non_irrigated_2040, "A:/MARRUECOS/LAND_USE/raster_non_irrigated_2040.tif")

library("writexl")

write_xlsx(non_irrigated_crop, "D:/MARRUECOS/Results/non_irrigated_crop.xlsx")

colnames(natural) <- c("ID", "y_2015", "y_2020", "y_2025", "y_2030", "y_2035", "y_2040")

natural2 <- reshape2::melt(natural[,2:3])

ggplot(natural2, aes(x=variable, y=value)) + 
  geom_boxplot()

###########################################################

library(readxl)
basins <- read_excel("D:/MARRUECOS/Results/All_basins_results.xlsx", 
                     sheet = "Hoja2")
library(PupillometryR)

kk <- filter(basins, `14prior_rf_max_pres_20` == 3 & `26prior_rf_max_fut_20`== 3)


# Natural plot
kk <- kk[,c(1,8:13)]
colnames(kk) <- c("ID", "2015", "2020", "2025", "2030", "2035", "2040")
kk <- reshape2::melt(kk, id.vars='ID')

ggplot(kk, aes(x=variable, y=value)) + 
  geom_flat_violin(
    trim = FALSE,
    alpha = 0.5
  ) +
  geom_point(position = position_jitter(width = .2),
             size = 2,
             shape = 20, col = "ID")+
  geom_line(aes(group =  ID),
            alpha = 0.5, linetype=2, col = "gray")+
  ggtitle("Natural") +
  xlab("Year") +
  ylab("Land use probability") +
  theme_minimal()


geom_flat_violin(
  position = position_nudge(x = .1, y = 0),
  trim = FALSE,
  alpha = 0.5,
  colour = NA
) +
  geom_point(position = position_jitter(width = .2),
             size = 2,
             shape = 20) +
  geom_boxplot(
    outlier.shape = NA,
    alpha = .5,
    width = .1,
    colour = "black"
  )

# Irrigated plot
kk <- kk[,15:20]
colnames(kk) <- c("ID", "2015", "2020", "2025", "2030", "2035", "2040")
kk <- reshape2::melt(kk, id.vars='ID')

ggplot(kk, aes(x=variable, y=value)) + 
  geom_boxplot() +
  geom_line(aes(group =  ID),
            alpha = 0.5, linetype=2, col = "gray")+
  ggtitle("Irrigated crop") +
  xlab("Year") +
  ylab("Land use probability")+
  theme_minimal()

# Non Irrigated plot
kk <- kk[,22:27]
colnames(kk) <- c("ID", "2015", "2020", "2025", "2030", "2035", "2040")
kk <- reshape2::melt(kk, id.vars='ID')

ggplot(kk, aes(x=variable, y=value)) + 
  geom_boxplot() +
  geom_line(aes(group =  ID),
            alpha = 0.5, linetype=2, col = "gray")+
  ggtitle("Non irrigated crop") +
  xlab("Year") +
  ylab("Land use probability")+
  theme_minimal()

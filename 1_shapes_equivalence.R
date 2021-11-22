###### Created by : David Bernal
##### Last modification : 06/09/2021
##### Modified by: 


#### I have SIT zones with different ids in 2017 and 2012, this code compares them and find the equivalence


## Required libraries

library(rgdal)
library(dplyr)




## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

## -----------------------------------------------------
#### Loading maps

sit_zones_2017 = readOGR("Data/shapessits/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017

## There are some small zones that are duplicated, those small zone are just random lines and not real sits, I delete them

sit_zones_2017 = sit_zones_2017[order(sit_zones_2017@data$Name, -sit_zones_2017@data$Shape_Area),] #Keeping the largest
sit_zones_2017 = sit_zones_2017[!duplicated(sit_zones_2017@data[c("Name")]),] # from 544 we kept 533

sit_zones_2012 <- readOGR("Data/shapes2012/Zonas_SIT2012.shp", layer = "Zonas_SIT2012") # Maps for 2012

## I do the same thing with the duplicates in 2012
sit_zones_2012 = sit_zones_2012[order(sit_zones_2012@data$C_MUN_EOD, sit_zones_2012@data$ZIT_2012, -sit_zones_2012@data$Shape_Area),]
sit_zones_2012 = sit_zones_2012[!duplicated(sit_zones_2012@data[c("C_MUN_EOD","ZIT_2012")]),] # from 409 we kept 404


## -----------------------------------------------------
#### Getting centroids

cent2017 <- as.data.frame(coordinates(sit_zones_2017)) # For 2017
colnames(cent2017) <- c('lon','lat')

cent2012 <- as.data.frame(coordinates(sit_zones_2012)) # For 2012
colnames(cent2012) <- c('lon','lat')


## -----------------------------------------------------
# Telling R that 2012 coordinates are in the same lat/lon reference system
# as the 2017 data
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
sit_zones_2012 <- spTransform(sit_zones_2012, CRS(crs))

coordinates(cent2017) <- c("lon", "lat")
proj4string(cent2017) <- proj4string(sit_zones_2012)

sit_2017 <- data.frame(sit_zones_2017@data$Name) ## zone's names in 2017


## -----------------------------------------------------
#### Equivalence

inside.sit <- over(cent2017, sit_zones_2012)
sit_2017$IdSitd <- inside.sit$Z_MODELO
sit_2017$IdMunicipiod <- inside.sit$C_MUN_EOD
sit_2017$N_MUNICIPI <- inside.sit$N_MUNICIPI
sit_2017$C_COMUNA <- inside.sit$C_COMUNA
colnames(sit_2017) <- c("SIT_D", "IdSitd", "IdMunicipiod", "N_MUNICIPI", "C_COMUNA")

write.csv(sit_2017, file = "Base/homol_sit_v1.csv")









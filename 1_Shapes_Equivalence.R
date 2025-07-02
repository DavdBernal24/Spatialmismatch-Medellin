### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medell?n, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Modified by: David Bernal

# we have SIT zones with different ids in 2017 and 2012, 
# this code compares them using spatial analysis and find the equivalence 
# for the ids.


#Required libraries


library(rgdal)

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#                     1.Loading Maps                          #  
#                                                             #
#-------------------------------------------------------------#

sit_zones_2017 = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017

## There are some small zones that are duplicated, those small zone are just random lines and not real sits, I delete them
# by using the size of them

sit_zones_2017 = sit_zones_2017[order(sit_zones_2017@data$Name, -sit_zones_2017@data$Shape_Area),] #Keeping the largest
sit_zones_2017 = sit_zones_2017[!duplicated(sit_zones_2017@data[c("Name")]),] # from 544 we kept 533

sit_zones_2012 = readOGR("Data/Shapefiles/SITs_2012/Zonas_SIT2012.shp", layer = "Zonas_SIT2012") # Maps for 2012

## I do the same thing with the duplicates in 2012
sit_zones_2012 = sit_zones_2012[order(sit_zones_2012@data$C_MUN_EOD, sit_zones_2012@data$ZIT_2012, -sit_zones_2012@data$Shape_Area),]
sit_zones_2012 = sit_zones_2012[!duplicated(sit_zones_2012@data[c("C_MUN_EOD","ZIT_2012")]),] # from 409 we kept 404


#-------------------------------------------------------------#
#                     2.Getting Centroids                     #  
#                                                             #
#-------------------------------------------------------------#

cent2017 = as.data.frame(coordinates(sit_zones_2017)) # For 2017
colnames(cent2017) = c('lon','lat')

cent2012 = as.data.frame(coordinates(sit_zones_2012)) # For 2012
colnames(cent2012) = c('lon','lat')


#-------------------------------------------------------------#
#                     3.Projecting coordinates                #  
#                                                             #
#-------------------------------------------------------------#

crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # System I am using
sit_zones_2012 = spTransform(sit_zones_2012, CRS(crs))

coordinates(cent2017) = c("lon", "lat")
proj4string(cent2017) = proj4string(sit_zones_2012)

sit_2017 = data.frame(sit_zones_2017@data$Name) ## zone's names in 2017



#-------------------------------------------------------------#
#                     4. building the data                    #  
#                                                             #
#-------------------------------------------------------------#

inside.sit = over(cent2017, sit_zones_2012) # overlaying 2017 centroids in 2012 zones
sit_2017$IdSitd = inside.sit$Z_MODELO # Z_Modelo is the column with the ids for 2012
sit_2017$IdMunicipiod = inside.sit$C_MUN_EOD
sit_2017$N_MUNICIPI = inside.sit$N_MUNICIPI
sit_2017$C_COMUNA = inside.sit$C_COMUNA
colnames(sit_2017) = c("SIT_D", "IdSitd", "IdMunicipiod", "N_MUNICIPI", "C_COMUNA")

## Saving the homologation of the zones
write.csv(sit_2017, row.names = FALSE, file = "Base/homol_sit_v1.csv")


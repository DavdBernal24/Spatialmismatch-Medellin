### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


### This code just cuts shapefiles


# These libraries help you to read and transform data
library(readr)

# Library to work with spatial data
library(rgdal) # Main library to read spatial data
library(spdep) # To be able to intersect shapefiles

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################


#-------------------------------------------------------------#
#               1.SIT Zones                                   #  
#                                                             #
#-------------------------------------------------------------#


## Loading 2017 SITs map and deleting duplicates
sit_zones = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017
sit_zones$matrix_order = c(1:nrow(sit_zones))
sit_zones = sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),] #Keeping the largest
sit_zones = sit_zones[!duplicated(sit_zones@data[c("Name")]),] # from 544 we kept 533




## IDs from Medellin(EOD)
ids = read_csv("Base/times_eod_2017_filter.csv")
ids = ids[,c("SIT_D","ID_MUNICIPIO_D")] ## 001 for medellin
ids = ids[!duplicated(ids),]


## Merging the IDS and the sit_zones
sits = merge(sit_zones, ids, by.x="Name", by.y="SIT_D", all.x=TRUE)

## Keeping Medellin
sits = sits[!is.na(sits$ID_MUNICIPIO_D),] ## 306 remaining zones

## Saving it
writeOGR(sits, dsn = "Data/Shapefiles/SITs_2017_cut/SITs_2017.shp", "SITs_2017", 
         driver= "ESRI Shapefile", overwrite_layer= TRUE)


#-------------------------------------------------------------#
#               2.Other shapes                                #  
#                                                             #
#-------------------------------------------------------------#

## Loading metro lines
lines = readOGR("Data/Shapefiles/Metro_Lines/lineas_Sistema_de_Transporte.shp", 
                layer = "lineas_Sistema_de_Transporte",use_iconv = TRUE,  encoding ="utf-8")
lines = lines[lines$OBJECTID != 337,] ## deleting palmitas
lines = lines[lines$OBJECTID != 342,] ## deleting arvi
lines = lines[lines$OBJECTID != 348,] ## deleting line p

# A shapefile with the lines that were in the system during 2012

lines_2012 = lines[lines$OBJECTID != 338, ] ## Line 2 Metroplus
lines_2012 = lines[lines$OBJECTID != 345, ] ## Line M
lines_2012 = lines[lines$OBJECTID != 346, ] ## Line H
lines_2012 = lines[lines$OBJECTID != 347, ] ## Tram line

# A shapefile with the lines that were added in 2017
lines_2017 = lines[lines$OBJECTID %in% c(338, 345, 346, 347) , ]




## Loading metro stations, I keep the ones that are also in the shapefile of lines
stations = readOGR("Data/Shapefiles/Stations/Estaciones.shp", layer = "Estaciones"
                   , use_iconv = TRUE, encoding = "UTF-8")
stations = stations[stations$LINEA %in% unique(as.character(lines$LINEA))| stations$LINEA == "Línea T-A", ]

stations_2012 = stations[stations$LINEA %in% unique(as.character(lines_2012$LINEA)), ]
stations_2017 = stations[stations$LINEA %in% unique(as.character(lines_2017$LINEA)), ]


## Loading roads
roads  = readOGR("Data/Shapefiles/Roads/Vias_UrbanoRural.shp", layer = "Vias_UrbanoRural",
                 use_iconv = TRUE, encoding = "UTF-8")

roads = roads[roads$JERARQUIZA == "Vía arteria" & roads$ESTADO == "Existente" & roads$CLASIFICAC != "Rural", ] 


## Projecting and intersecting all the shapefiles

crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" ## System I am using (takes from the SIT zones)


lines = spTransform(lines, CRS(crs))
lines = raster::intersect(lines, sits)

lines_2012 = spTransform(lines_2012, CRS(crs))
lines_2012 = raster::intersect(lines_2012, sits)

lines_2017 = spTransform(lines_2017, CRS(crs))
lines_2017 = raster::intersect(lines_2017, sits)

stations = spTransform(stations, CRS(crs))
stations = raster::intersect(stations, sits)

stations_2012 = spTransform(stations_2012, CRS(crs))
stations_2012 = raster::intersect(stations_2012, sits)

stations_2017 = spTransform(stations_2017, CRS(crs))
stations_2017 = raster::intersect(stations_2017, sits)

roads = spTransform(roads, CRS(crs))
roads = raster::intersect(roads, sits)


## Saving the files

writeOGR(lines, dsn = "Data/Shapefiles/Metro_Lines_cut/Lines_cut.shp", "Lines_cut", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE) 

writeOGR(lines_2012, dsn = "Data/Shapefiles/Metro_Lines_cut/Lines_2012.shp", "Lines_cut", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE) 

writeOGR(lines_2017, dsn = "Data/Shapefiles/Metro_Lines_cut/Lines_2017.shp", "Lines_cut", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE) 


writeOGR(stations, dsn = "Data/Shapefiles/Stations_cut/Stations_cut.shp", "Stations_cut", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE) 

writeOGR(stations_2012, dsn = "Data/Shapefiles/Stations_cut/Stations_2012.shp", "Stations_cut", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE) 

writeOGR(stations_2017, dsn = "Data/Shapefiles/Stations_cut/Stations_2017.shp", "Stations_cut", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE) 

writeOGR(roads, dsn = "Data/Shapefiles/Roads_cut/Roads_cut.shp", "Roads_cut", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE) 
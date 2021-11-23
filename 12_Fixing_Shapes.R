###### Created by : David Bernal
##### Last modification : 
##### Modified by: 

### This code cuts some shapes for Medellin  




## Required libraries

### To load and transform the data
library(readr)

### For shapes
library(rgdal)
library(spdep)

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


rm(list = ls())


##-----------------------------------------
## Loading maps


## Loading SIT map
sit_zones = readOGR("Data/shapessits/SITzones2017.shp", layer="SITzones2017")
sit_zones$matrix_order = c(1:nrow(sit_zones))
sit_zones = sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),] # organizing
sit_zones = sit_zones[!duplicated(sit_zones@data[c("Name")]),] # from 544 we keep 535


## Loading metro lines
lines= readOGR("Data/maps_ecv/lineas_Sistema_de_Transporte/lineas_Sistema_de_Transporte.shp", 
               layer = "lineas_Sistema_de_Transporte", encoding ="utf-8")
lines = lines[lines$OBJECTID != 337,] ## deleting palmitas
lines = lines[lines$OBJECTID != 342,] ## deleting arvi
lines = lines[lines$OBJECTID != 348,] ## deleting line p


## Loading Vias
vias <- readOGR("Data/maps_ecv/Vias_UrbanoRural/Vias_UrbanoRural.shp", layer = "Vias_UrbanoRural",
                use_iconv = TRUE, encoding = "UTF-8")
vias <- vias[vias$CLASIFICAC != "Rural",]
vias <- vias[vias$JERARQUIZA == "Vía arteria",]
vias <- vias[vias$ESTADO == "Existente",]



## Loading metro stations
stations = readOGR("Data/maps_ecv/Estaciones/Estaciones.shp", layer = "Estaciones")
stations = stations[stations$LINEA %in% unique(as.character(lines$LINEA)),]

## Deleting the townships

## Townships SITS
sit_correg = c('147','320','321','322','323','324','325','326','416','418','455',
               '456','457','458','601','602','603','605','607','622','648','649')

## Ids from Medellin
ids = read_csv("Base/times_eod_2017.csv", col_types = cols(X1 = col_skip()))
ids = ids[,c("SIT_D","ID_MUNICIPIO_D")] ## 001 for medellin
ids = ids[!duplicated(ids),]
ids = subset(ids, ID_MUNICIPIO_D !="-")
ids = subset(ids, ID_MUNICIPIO_D =="001")
ids = ids = ids[!(ids$SIT_D %in% sit_correg), ]




## Merging the IDS and the sit_zones

sits = merge(sit_zones, ids, by.x="Name", by.y="SIT_D", all.x=TRUE)

## Keeping Medellin
sits = sits[!is.na(sits$ID_MUNICIPIO_D),] ## 310 remaining zones



## Saving the shape
writeOGR(sits, dsn = "Shapes/shapes_cut/SITzones_cut.shp", "SITzones_cut", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE) 


## -----------------------------------------------------
## info de proyeccion para estandarizar shapes
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"



## -----------------------------------------------------
## Saving Metro lines



lines <- spTransform(lines, CRS(crs))
lines <- raster::intersect(lines, sits)

## Saving lines shapes
writeOGR(lines, dsn = "Shapes/shapes_cut/lineas_cut.shp", "lineas_cut", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE) 

## -----------------------------------------------------
## Saving Metro stations



stations <- spTransform(stations, CRS(crs))
stations <- raster::intersect(stations, sits)

## Saving the shape
writeOGR(stations, dsn = "Shapes/shapes_cut/estaciones_cut.shp", "estaciones_cut", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE) 


## -----------------------------------------------------
## saving roads shape


vias <- spTransform(vias, CRS(crs))
vias <- raster::intersect(vias, sits)

## Saving the shape
writeOGR(vias, dsn = "Shapes/shapes_cut/vias_cut.shp", "vias_cut", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE) 


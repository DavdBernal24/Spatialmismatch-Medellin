###### Created by : David Bernal
##### Last modification : 
##### Modified by: 

### This code creates a population density map for Medellin


## Required libraries

### To load and transform the data
library(readr)
library(dplyr)
library(RColorBrewer)
library(classInt)
library(readxl)
library(modeest)
library(tidyr)
library(data.table)

### For maps
library(rgdal)
library(spdep)
library(maptools)
library(geosphere)
library(prettymapr)


##Working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##-----------------------------------------

## LOADING MAPS

#communes
comunas = readOGR("Shapes/shapescomunas/Comunas.shp", layer="Comunas")

comunas$area = comunas$SHAPE__Are/1000000


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




##-------------------------------------------

##LOADING DATA
pobl = read_excel("Data/Pob2018.xlsx")


#Merging the population info with the comuna
comunas = merge(comunas, pobl, by.x="Cod_Comuna", by.y="Comuna", all.x=TRUE)


# transforming pobl to numeric 

as.numeric(comunas$pobla)

# Computing the population density
comunas$dens = comunas$pobla/comunas$area



## groups for the maps
brks = classIntervals(comunas$dens, 
                       n = 4, style="quantile", dataPrecision=2)

## color palette to fill the map
colors = c(brewer.pal(4, "YlOrRd"))


##finding the project system for communes
st_crs(comunas)$proj4string


## Need to standardize the shapes with the same system
crs = st_crs(comunas)$proj4string

sits = spTransform(sits, CRS(crs))
sits = raster::intersect(sits, comunas)

lines = spTransform(lines, CRS(crs))
lines = raster::intersect(lines, comunas)

stations = spTransform(stations, CRS(crs))
stations = raster::intersect(stations, comunas)





## Map
pdf(file = paste("Output/Density_income/Density.pdf", sep = ""))
par(mar = c(0, 0, 0, 2))
plot(comunas,
     col=colors[findInterval(comunas$dens, brks$brks,
                             all.inside=TRUE)], axes=F, lwd = 2)
title(paste (""))
addnortharrow(pos='topleft', scale = 0.3, padin = c(1.5, 0.3))
addscalebar(widthhint = 0.15, unitcategory = "metric", padin = c(1.5, 0.3))
legend("bottomright", # location of legend
       legend = leglabs(round(brks$brks,0)), # categories or elements in legend
       fill = colors, # color palette
       bty = "n", # turn off the legend BORDER
       cex = 0.9, # change the font size
       x.intersp = 0.5,
       y.intersp = 0.7,
       inset = c(0.0,0.18)
)
plot(lines, col='blue', lwd = 3, add = T)
plot(stations, pch = 16, add = T)
plot(sits, border='black', lwd = 0.3, add = T)
legend(x = "bottomright", 
       legend = c("Metro lines","Metro stations", "Commune boundary"),
       col = c("blue","black", "black"), 
       lwd = 3, 
       cex = 0.8,
       lty = c(1,NA,1),
       pch = c(NA,16,NA),
       inset = c(0.02,0.09))
dev.off()
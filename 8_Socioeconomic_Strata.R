### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code creates a socioeconomic strata map for Medellin

#Required libraries


# These libraries help you to read and transform data
library(readr)



# Library to work with spatial data
#library(rgeos) 
library(rgdal) # Main library to read spatial data
library(RColorBrewer) # Colors
library(classInt)# Intervals for the maps
library(spdep) # Spatial transformations
library(prettymapr) #Add stuff to maps, like the north line
library(maptools) # Help with labels
library(raster) # Allows to use the spatial intersect function



## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################


#-------------------------------------------------------------#
#               1.Loading data                                #  
#                                                             #
#-------------------------------------------------------------#

## Loading 2017 SITs map 
sits = readOGR("Data/Shapefiles/SITs_2017_cut/SITs_2017.shp", layer = "SITs_2017") #Maps for 2017



## Loading metro lines 2012
lines_2012= readOGR("Data/Shapefiles/Metro_Lines_cut/Lines_2012.shp", 
                    layer = "Lines_2012", encoding ="utf-8")

## Loading metro lines 2017
lines_2017= readOGR("Data/Shapefiles/Metro_Lines_cut/Lines_2017.shp", 
                    layer = "Lines_2017", encoding ="utf-8")



## Loading metro stations
stations = readOGR("Data/Shapefiles/Stations_cut/Stations_cut.shp", layer = "Stations_cut")


##Loading blocks (The Strata is at block-level)
blocks = readOGR("Data/Shapefiles/Blocks/BARRIOS CON ESTRATO.shp")




#-------------------------------------------------------------#
#               2.Spatial transformation and maps              #  
#                                                             #
#-------------------------------------------------------------#

##Creating groups for the maps

blocks$income = NA

blocks$income = ifelse(blocks$ESTRATO == 1, 'Low income', NA)
blocks$income = ifelse(blocks$ESTRATO == 2, 'Low income', blocks$income)
blocks$income = ifelse(blocks$ESTRATO == 3, 'Middle income', blocks$income)
blocks$income = ifelse(blocks$ESTRATO == 4, 'Middle income', blocks$income)
blocks$income = ifelse(blocks$ESTRATO == 5, 'High income', blocks$income)
blocks$income = ifelse(blocks$ESTRATO == 6, 'High income', blocks$income)

##Colors
blocks$color = ifelse(blocks$income  == 'High income', '#e31a1c', NA)
blocks$color = ifelse(blocks$income  == 'Middle income', '#fd8d3c', blocks$color)
blocks$color = ifelse(blocks$income  == 'Low income', '#fed976', blocks$color)

## finding the project system for the sits
st_crs(sits)$proj4string


## Need to standardize the shapes with the same system
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


rgeos::set_RGEOS_CheckValidity(2L) ## It allows to valid by zero-width buffering
blocks = spTransform(blocks, CRS(crs))
blocks = raster::intersect(blocks, sits)


# Note: I save this map in png because this one in pdf it is quite heavy in pdf

## Map in png
png(paste("Output/Figures/Socioeconomic_strata.png", sep = ""), width = 2000, height = 2000, res = 300)
par(mar = c(0, 0, 0, 0))
plot(blocks, border=blocks$color,
     col = blocks$color)
title(paste (""))
addnortharrow(pos='topleft', scale = 0.3, padin = c(1.5, 0.3))
addscalebar(widthhint = 0.15, unitcategory = "metric", padin = c(1.5, 0.3))
legend("bottomright", # location of legend
       legend = c("Low income (1-2)","Middle income (3-4)","High income (5-6)"),
       fill = c('#fed976','#fd8d3c','#e31a1c'),
       bty = "n", # turn off the legend BORDER
       cex = 0.9, # change the font size
       x.intersp = 0.5,
       y.intersp = 0.7,
       inset = c(0.0,0.18))
plot(sits, border='black', lwd = 2, add = T)
plot(lines_2012, col='blue', lwd = 3, add = T)
plot(lines_2017, col='purple', lwd = 3, add = T)
plot(stations, pch = 16, add = T)
legend(x = "bottomright", 
       legend = c("Metro lines","New Metro lines", "Metro stations","SIT zones"),
       col = c("blue", "purple", "black","black"), 
       lwd = 2, 
       cex = 0.6,
       lty = c(1,1,NA,1),
       pch = c(NA,NA,16,NA),
       inset = c(0.02,0.09))
dev.off()






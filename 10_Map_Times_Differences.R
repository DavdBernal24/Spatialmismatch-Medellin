### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code creates a Map of travel time differences by commune and transportation mode
# Using merely the EOD

#Required libraries


# These libraries help you to read and transform data
library(readr)
library(readxl)
library(tidyr) 


# Library to work with spatial data
library(rgdal) # Main library to read spatial data
library(RColorBrewer) # Colors
library(classInt)# Intervals for the maps
library(spdep) # Spatial transformations
library(prettymapr) #Add stuff to maps, like the north line
library(maptools) # Help with labels

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################


#-------------------------------------------------------------#
#               1.Loading and merging data                    #  
#                                                             #
#-------------------------------------------------------------#

## Loading communes and travel time differences
comunas = readOGR("Data/Shapefiles/Communes/Comunas.shp", layer="Comunas") # Communes level map

eod_diff = read_csv("Base/diff_means_commune.csv")

## Loading metro lines
lines_2012 = readOGR("Data/Shapefiles/Metro_Lines_cut/Lines_2012.shp", 
               layer = "Lines_2012", encoding ="utf-8")

lines_2017 = readOGR("Data/Shapefiles/Metro_Lines_cut/Lines_2017.shp", 
                     layer = "Lines_2017", encoding ="utf-8")

##SIT Zones
sits = readOGR("Data/Shapefiles/SITs_2017_cut/SITs_2017.shp", layer = "SITs_2017") #Maps for 2017


## Loading metro stations
stations = readOGR("Data/Shapefiles/Stations_cut/Stations_cut.shp", layer = "Stations_cut")


## I made this file manually, its to homologate the IDs of the communes in both files
comunas_IDs = read_csv("Data/Comunas_shapes.csv")

comunas$Cod_Comuna = as.numeric(comunas$Cod_Comuna) # Need the numeric IDs

comunas = merge(comunas, comunas_IDs, by.x = "Cod_Comuna", by.y = "ID_REAL", all.x = TRUE)


## Now I need to create variables with the time differences

eod_diff = subset(eod_diff, select = c(ID_COMUNA_O, Medio_reg, diff_mean_percent))

eod_diff = eod_diff[eod_diff$Medio_reg != "A pie", ]

eod_diff_wide = eod_diff %>%
  spread(data = ., key = Medio_reg, value= diff_mean_percent)

colnames(eod_diff_wide) = c("ID_COMUNA_O", "Private", "Public")

## Merging with the shapefile
comunas = merge(comunas, eod_diff_wide, by.x = "ID_EOD", by.y = "ID_COMUNA_O", all.x = FALSE)


#-------------------------------------------------------------#
#               2.Spatial transformations and mapping         #  
#                                                             #
#-------------------------------------------------------------#

## groups for the maps
brks = classIntervals(eod_diff$diff_mean_percent, 
                      n = 4, style="quantile", dataPrecision=2) ## 4 possible groups

## color palette to fill the map
colors = c(brewer.pal(4, "YlOrRd"))

##finding the project system for communes
st_crs(comunas)$proj4string

## Need to standardize the shape of communes with the same system
crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

comunas = spTransform(comunas, CRS(crs))


## Variables for the loop (I will only use public and private)

modes_names = c("Private", "Public")

## Maps

for (i in 1:length(modes_names)){
  pdf(file = paste("Output/Figures/", modes_names[i], "_time_diff.pdf", sep = ""))
  par(mar = c(0, 0, 0, 2))
  plot(comunas,
       col=colors[findInterval(unlist(comunas@data[7+i]), brks$brks,
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
  plot(lines_2012, col='blue', lwd = 3, add = T)
  plot(lines_2017, col='purple', lwd = 3, add = T)
  plot(stations, pch = 16, add = T)
  plot(sits, border='black', lwd = 0.3, add = T)
  legend(x = "bottomright", 
         legend = c("Metro lines","New Metro lines", "Metro stations", "Commune boundary"),
         col = c("blue","purple", "black", "black"), 
         lwd = 3, 
         cex = 0.7,
         lty = c(1,1,NA,1),
         pch = c(NA, NA, 16, NA),
         inset = c(0.02,0.09))
 dev.off()
}











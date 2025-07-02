### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 

### Here is the link of the population by commune

## https://www.medellin.gov.co/es/centro-documental/proyecciones-poblacion-viviendas-y-hogares/


#### Purpose: This code creates a population density map for Medellin

#Required libraries

# These libraries help you to read and transform data
library(readr)
library(readxl)


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
#               1.Loading maps and merging data               #  
#                                                             #
#-------------------------------------------------------------#


## Loading Communes
comunas = readOGR("Data/Shapefiles/Communes/Comunas.shp", layer="Comunas") ## Communes level map


## Need the area in km, now it is in cm
comunas$area = comunas$SHAPE__Are/1000000

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



#-------------------------------------------------------------#
#               2.Adding Population and Mapping               #  
#                                                             #
#-------------------------------------------------------------#

##LOADING DATA
pobl = read_excel("Data/Population.xlsx")


#Merging the population info with the comuna
comunas = merge(comunas, pobl, by.x="Cod_Comuna", by.y="Comuna", all.x=TRUE)


as.numeric(comunas$pobla) # transforming pobl to numeric 


## Computing the population density
comunas$dens_2017 = comunas$pob2017/comunas$area
comunas$dens_2012 = comunas$pob2012/comunas$area

##Differences in density
comunas$diff = ((comunas$dens_2017-comunas$dens_2012)/comunas$dens_2012)*100

### groups for the maps
brks_2017 = classIntervals(comunas$dens_2017, 
                           n = 4, style="quantile", dataPrecision=2) ## 4 possible groups

#brks_2012 = classIntervals(comunas$dens_2012, 
#                         n = 4, style="quantile", dataPrecision=2) ## 4 possible groups

brks_diff = classIntervals(comunas$diff, 
                           n = 4, style="quantile", dataPrecision=2) ## 4 possible groups

## color palette to fill the map
colors = c(brewer.pal(4, "YlOrRd"))


##finding the project system for communes
st_crs(comunas)$proj4string

## Need to standardize the shapes with the same system
crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


comunas_lines = spTransform(comunas, CRS(crs)) # I will use this to graph the boundaries of the comunas

comunas = spTransform(comunas, CRS(crs))
comunas = raster::intersect(comunas, sits)



## Map 2017
pdf(file = paste("Output/Figures/Population_density_2017.pdf", sep = ""))
par(mar = c(0, 0, 0, 2))
plot(comunas,
     col=colors[findInterval(comunas$dens_2017, brks_2017$brks,
                             all.inside=TRUE)], axes=F, lwd = 1)
title(paste (""))
addnortharrow(pos='topleft', scale = 0.3, padin = c(1.5, 0.3))
addscalebar(widthhint = 0.15, unitcategory = "metric", padin = c(1.5, 0.3))
legend("bottomright", # location of legend
       legend = leglabs(round(brks_2017$brks,0)), # categories or elements in legend
       fill = colors, # color palett
       bty = "n", # turn off the legend BORDER
       cex = 0.9, # change the font size
       x.intersp = 0.5,
       y.intersp = 0.7,
       inset = c(0.0,0.18)
)
plot(lines_2012, col='blue', lwd = 3, add = T)
plot(lines_2017, col='purple', lwd = 3, add = T)
plot(stations, pch = 16, add = T)
plot(comunas_lines, border = "black", lwd= 2.5, add = T )
plot(sits, border='black', lwd = 0.3, add = T)
legend(x = "bottomright", 
       legend = c("Metro lines","New Metro lines", "Metro stations", "Commune boundary"),
       col = c("blue","purple", "black", "black"), 
       lwd = 3, 
       cex = 0.6,
       lty = c(1,1,NA,1),
       pch = c(NA, NA, 16, NA),
       inset = c(0.02,0.09))
dev.off()






## Map 2012
pdf(file = paste("Output/Figures/Population_density_2012.pdf", sep = ""))
par(mar = c(0, 0, 0, 2))
plot(comunas,
     col=colors[findInterval(comunas$dens_2012, brks_2017$brks,
                             all.inside=TRUE)], axes=F, lwd = 1)
title(paste (""))
addnortharrow(pos='topleft', scale = 0.3, padin = c(1.5, 0.3))
addscalebar(widthhint = 0.15, unitcategory = "metric", padin = c(1.5, 0.3))
legend("bottomright", # location of legend
       legend = leglabs(round(brks_2017$brks,0)), # categories or elements in legend
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
plot(comunas_lines, border = "black", lwd= 2.5, add = T )
plot(sits, border='black', lwd = 0.3, add = T)
legend(x = "bottomright", 
       legend = c("Metro lines","New Metro lines", "Metro stations", "Commune boundary"),
       col = c("blue","purple", "black", "black"), 
       lwd = 3, 
       cex = 0.6,
       lty = c(1,1,NA,1),
       pch = c(NA, NA, 16, NA),
       inset = c(0.02,0.09))
dev.off()


## Map differences in Density
pdf(file = paste("Output/Figures/Population_density_diff.pdf", sep = ""))
par(mar = c(0, 0, 0, 2))
plot(comunas,
     col=colors[findInterval(comunas$diff, brks_diff$brks,
                             all.inside=TRUE)], axes=F, lwd = 1)
title(paste (""))
addnortharrow(pos='topleft', scale = 0.3, padin = c(1.5, 0.3))
addscalebar(widthhint = 0.15, unitcategory = "metric", padin = c(1.5, 0.3))
legend("bottomright", # location of legend
       legend = leglabs(paste0(round(brks_diff$brks,0), "%")), # categories or elements in legend
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
plot(comunas_lines, border = "black", lwd= 2.5, add = T )
plot(sits, border='black', lwd = 0.3, add = T)
legend(x = "bottomright", 
       legend = c("Metro lines","New Metro lines", "Metro stations", "Commune boundary"),
       col = c("blue","purple", "black", "black"), 
       lwd = 3, 
       cex = 0.6,
       lty = c(1,1,NA,1),
       pch = c(NA, NA, 16, NA),
       inset = c(0.02,0.09))
dev.off()


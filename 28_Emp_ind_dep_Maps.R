### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code creates Maps of the employment by SIT Zones


## Required libraries

### To load and transform the data
library(readr)
library(dplyr)
library(RColorBrewer)
library(classInt)

### For maps
library(rgdal)
library(spdep)
library(maptools)
library(prettymapr)

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


rm(list = ls())

source("Maps_Function.R") # Function I created for the maps


#-------------------------------------------------------------#
#        1. Loading Data                                      #  
#                                                             #
#-------------------------------------------------------------#

## ----- Loading SIT map
sit_zones = readOGR("Data/Shapefiles/SITs_2017_cut/SITs_2017.shp") #Maps for 2017

## Loading metro lines
Lines_2012 = readOGR("Data/Shapefiles/Metro_Lines_cut/Lines_2012.shp", 
                     layer = "Lines_2012", encoding ="utf-8")

Lines_2017 = readOGR("Data/Shapefiles/Metro_Lines_cut/Lines_2017.shp", 
                     layer = "Lines_2017", encoding ="utf-8")

## Loading metro stations
Stations = readOGR("Data/Shapefiles/Stations_cut/Stations_cut.shp", layer = "Stations_cut")

## Loading roads
Roads = readOGR("Data/Shapefiles/Roads_cut/Roads_cut.shp", layer = "Roads_cut")


## Loading employment data
emp_2017 = read_csv("Base/emp2017.csv")
emp_2012 = read_csv("Base/emp2012.csv")

#-------------------------------------------------------------#
#        2. Employment Maps                                   #  
#                                                             #
#-------------------------------------------------------------#

# Map 2017
sits = merge(sit_zones, emp_2017, by.x = "Name", by.y = "k", all.x = TRUE)  
data_to_map("Output/Figures/", "Employment_2017.pdf", sits, sits$emp, "YlOrRd", 0,
            Roads, Lines_2012, Lines_2017,  Stations, sits$emp)

# Map 2012
sits = merge(sit_zones, emp_2012, by.x = "Name", by.y = "k", all.x = TRUE)  
data_to_map("Output/Figures/", "Employment_2012.pdf", sits, sits$emp, "YlOrRd", 0,
            Roads, Lines_2012, Lines_2017,  Stations, sits$emp)


#-------------------------------------------------------------#
#        3. Employment Maps dep INDEP                         #  
#                                                             #
#-------------------------------------------------------------#

## Loading employment data
emp_2017_dep = read_csv("Base/emp2017_dependiente.csv")
emp_2017_indep = read_csv("Base/emp2017_independiente.csv")


# Map dependent
sits = merge(sit_zones, emp_2017_dep, by.x = "Name", by.y = "k", all.x = TRUE)  
data_to_map("Output/Figures/", "Employment_2017_dependent.pdf", sits, sits$emp, "YlOrRd", 0,
            Roads, Lines_2012, Lines_2017,  Stations, sits$emp)

# Map independent
sits = merge(sit_zones, emp_2017_indep, by.x = "Name", by.y = "k", all.x = TRUE)  
data_to_map("Output/Figures/", "Employment_2017_independent.pdf", sits, sits$emp, "YlOrRd", 0,
            Roads, Lines_2012, Lines_2017,  Stations, sits$emp)

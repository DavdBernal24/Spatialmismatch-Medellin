### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 



#### Purpose: This code creates maps for the new measure 


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


##Reading data

## A private
A_priv_2017 = read.csv("Output/Results/New_Ta_Private_2017.csv")
A_priv_2012 = read.csv("Output/Results/New_Ta_Private_2012.csv")


## A public
A_pub_2017 = read.csv("Output/Results/New_Ta_Public_2017.csv")
A_pub_2012 = read.csv("Output/Results/New_Ta_Public_2012.csv")


## Merging Data
sits = merge(sit_zones, A_priv_2017, by.x = "Name", by.y = "k", all.x = TRUE)  
sits = merge(sits, A_pub_2017, by.x = "Name", by.y = "k", all.x = TRUE)  
sits = merge(sits, A_priv_2012, by.x = "Name", by.y = "k", all.x = TRUE)  
sits = merge(sits, A_pub_2012, by.x = "Name", by.y = "k", all.x = TRUE)  

#-------------------------------------------------------------#
#        2. Maps Measure                                      #  
#                                                             #
#-------------------------------------------------------------#

##Breaks for the maps
BreaKS2017 = read_csv("Base/breaks_NewAccessibility_2017.csv")
Breaks2012 = read_csv("Base/breaks_NewAccessibility_2012.csv")

#### Maps 2017
#Private 2017
data_to_map("Output/Figures/", "New_A_private_2017.pdf", sits, sits$tapriv2017, "YlOrRd", 1,
            Roads, Lines_2012, Lines_2017,  Stations, BreaKS2017$A2017)
#public 2017
data_to_map("Output/Figures/", "New_A_public_2017.pdf", sits, sits$tapub2017, "YlOrRd", 1,
            Roads, Lines_2012, Lines_2017,  Stations, BreaKS2017$A2017)
#Private 2012
data_to_map("Output/Figures/", "New_A_private_2012.pdf", sits, sits$tapriv2012, "YlOrRd", 1,
            Roads, Lines_2012, Lines_2017,  Stations, Breaks2012$A2012)
#Public 2012
data_to_map("Output/Figures/", "New_A_public_2012.pdf", sits, sits$tapub2012, "YlOrRd", 1,
            Roads, Lines_2012, Lines_2017,  Stations, Breaks2012$A2012)
#Adjusted private 2017
data_to_map("Output/Figures/", "New_A_private_Adjusted_2017.pdf", sits, sits$tapriv2017_percap, "YlOrRd", 2,
            Roads, Lines_2012, Lines_2017,  Stations, BreaKS2017$Adj2017)
#Adjusted Public 2017
data_to_map("Output/Figures/", "New_A_public_Adjusted_2017.pdf", sits, sits$tapub2017_percap, "YlOrRd", 2,
            Roads, Lines_2012, Lines_2017,  Stations, BreaKS2017$Adj2017)

#Adjusted private 2012
data_to_map("Output/Figures/", "New_A_private_Adjusted_2012.pdf", sits, sits$tapriv2012_percap, "YlOrRd", 2,
            Roads, Lines_2012, Lines_2017,  Stations, Breaks2012$Adj2012)


#Adjusted public 2012
data_to_map("Output/Figures/", "New_A_public_Adjusted_2012.pdf", sits, sits$tapub2012_percap, "YlOrRd", 2,
            Roads, Lines_2012, Lines_2017,  Stations, Breaks2012$Adj2012)



#-------------------------------------------------------------#
#        2. Maps Adjusted measure differences                 #  
#                                                             #
#-------------------------------------------------------------#

# Creating differences
sits$diff_pri_persit <- (sits$tapriv2017_percap - sits$tapriv2012_percap)*100/sits$tapriv2012_percap
sits$diff_pub_persit <- (sits$tapub2017_percap  - sits$tapub2012_percap)*100/sits$tapub2012_percap


#Private
source("Maps_Function.R") 
data_to_map_reverse = data_to_map_reverse("Output/Figures/", "New_A_private_diff.pdf", sits, sits$diff_pri_persit, "Purples", 2,
                                          Roads, Lines_2012, Lines_2017, Stations, sits$diff_pri_persit)

#Public
source("Maps_Function.R") # Not sure why it does not work if I don't call the function right before the map
data_to_map_reverse = data_to_map_reverse("Output/Figures/", "New_A_public_diff.pdf", sits, sits$diff_pub_persit, "Purples", 2,
                                          Roads, Lines_2012, Lines_2017, Stations, sits$diff_pub_persit)


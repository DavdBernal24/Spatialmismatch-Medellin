### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code creates Maps for the travel times collapsed by each origin for 2012

#Required libraries

### To load and transform the data
library(readr)
library(dplyr)
library(reshape2)
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
##############################################################################


#-------------------------------------------------------------#
#        1. Organizing data                                   #  
#                                                             #
#-------------------------------------------------------------#


## Loading sit_zones
sit_zones = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017
sit_zones$matrix_order = c(1:nrow(sit_zones))
sit_zones = sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),]
sit_zones = sit_zones[!duplicated(sit_zones@data[c("Name")]),] # from 544 we get 535



## I import the sits we have information from the employment data (it is easier)
emp = read_csv("Base/emp2017.csv", col_types = cols(X1 = col_skip()))
emp = emp[c("k","emp")]


sits_order = data.frame(sit_zones$Name,sit_zones$matrix_order)
colnames(sits_order) = c("SIT","matrix_order")

#### Private 2012

private_times2012 = read_csv("Base/travel_times_private_2012.csv", col_types = cols(X1 = col_skip()))

temp_times2012 = private_times2012[c(sits_order$matrix_order)]
temp_times2012 = temp_times2012[c(sits_order$matrix_order), ]
rownames(temp_times2012) = sits_order$SIT
colnames(temp_times2012) = sits_order$SIT
temp_times2012 = as.data.frame(temp_times2012)
temp_times2012$sit = rownames(temp_times2012)
longpriv_2012 = melt(temp_times2012)   
colnames(longpriv_2012) = c('sito','sitd','time')

## Collapsing private travel times in 2012
longpriv_2012 = longpriv_2012 %>%
  group_by(sito) %>%
  summarise(timepriv_2012 = mean(time, na.rm = T))

## Merging the 2012 times 
sit_zones = merge(sit_zones, longpriv_2012,by.x = "Name", by = "sito", all.x = T)

######## public 2012

pub_times2012 = read_csv("Base/travel_times_public_2012.csv", col_types = cols(X1 = col_skip()))

temp_times2012 = pub_times2012[c(sits_order$matrix_order)]
temp_times2012 = temp_times2012[c(sits_order$matrix_order), ]
rownames(temp_times2012) = sits_order$SIT
colnames(temp_times2012) = sits_order$SIT
temp_times2012 = as.data.frame(temp_times2012)
temp_times2012$sit = rownames(temp_times2012)
longpub_2012 = melt(temp_times2012)   
colnames(longpub_2012) = c('sito','sitd','time')

## Collapsing at origin 
longpub_2012 = longpub_2012 %>%
  group_by(sito) %>%
  summarise(timepub_2012 = mean(time, na.rm = T))


## Merging data
sit_zones = merge(sit_zones, longpub_2012, by.x = "Name", by = "sito", all.x = T)


#-------------------------------------------------------------#
#        2.Mapping the times                                  #  
#                                                             #
#-------------------------------------------------------------#


### Now I will cut the sit zones and add the shapefiles for the maps
sit_zones = sit_zones[(sit_zones$Name %in% unique(emp$k)), ] 

## Saving the shapefile to map the difference later on
writeOGR(sit_zones, "Data/Shapefiles/sit_Zones_data_2012.shp", layer = "sit_zones",
         driver= "ESRI Shapefile", overwrite_layer= TRUE, 
         delete_dsn=TRUE)


## Loading metro lines
Lines_2012 = readOGR("Data/Shapefiles/Metro_Lines_cut/Lines_2012.shp", 
                     layer = "Lines_2012", encoding ="utf-8")

Lines_2017 = readOGR("Data/Shapefiles/Metro_Lines_cut/Lines_2017.shp", 
                     layer = "Lines_2017", encoding ="utf-8")

## Loading metro stations
Stations = readOGR("Data/Shapefiles/Stations_cut/Stations_cut.shp", layer = "Stations_cut")

## Loading roads
Roads = readOGR("Data/Shapefiles/Roads_cut/Roads_cut.shp", layer = "Roads_cut")

#### Maps

# #Private
# data_to_map("Output/Figures/", "privatetime2012.pdf", sit_zones, sit_zones$timepriv_2012, "YlOrRd", 1,
#             Roads, Lines_2012, Lines_2017,  Stations, sit_zones$timepriv_2012)
# 
# 
# #Public
# data_to_map("Output/Figures/", "publictime2012.pdf", sit_zones, sit_zones$timepub_2012, "YlOrRd", 1,
#             Roads, Lines_2012, Lines_2017,  Stations, sit_zones$timepub_2012)


##Reading data for the breaks 
breaks = readOGR("Data/Shapefiles/sit_Zones_data_2017.shp")

brks_private = breaks$tmpr_2017
brks_public = breaks$tmpb_2017

#Private
data_to_map("Output/Figures/", "privatetime2012.pdf", sit_zones, sit_zones$timepriv_2012, "YlOrRd", 1,
            Roads, Lines_2012, Lines_2017,  Stations, brks_private)


#Public
data_to_map("Output/Figures/", "publictime2012.pdf", sit_zones, sit_zones$timepub_2012, "YlOrRd", 1,
            Roads, Lines_2012, Lines_2017,  Stations, brks_public)













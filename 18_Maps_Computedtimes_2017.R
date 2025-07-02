### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code creates Maps for the travel times collapsed by each origin for 2017

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

source("Maps_Function.R") # Function I created for the maps


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

source("Maps_Function.R")
##############################################################################


#-------------------------------------------------------------#
#        1. Organizing data                                   #  
#                                                             #
#-------------------------------------------------------------#

## Loading 2017 SITs map 
sit_zones = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017
sit_zones$matrix_order = c(1:nrow(sit_zones))
sit_zones = sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),]
sit_zones = sit_zones[!duplicated(sit_zones@data[c("Name")]),] # from 544 we get 535

## I import the sits we have information from the employment data (it is easier)
emp = read_csv("Base/emp2017.csv")
emp = emp[c("k","emp")]




sits_order = data.frame(sit_zones$Name,sit_zones$matrix_order)
colnames(sits_order) = c("SIT","matrix_order")


######## Private 2017
private_times2017 = read_csv("Base/bing_travel_duration_full.csv")
private_times2017  = private_times2017 /60 #Passing it to minutes

temp_times2017 = private_times2017[c(sits_order$matrix_order)]
temp_times2017 = temp_times2017[c(sits_order$matrix_order), ]
rownames(temp_times2017) = sits_order$SIT
colnames(temp_times2017) = sits_order$SIT
temp_times2017 = as.data.frame(temp_times2017)
temp_times2017$sit = rownames(temp_times2017)
longpriv_2017 = melt(temp_times2017)   
colnames(longpriv_2017) = c('sito','sitd','time')

## Collapsing private travel times in 2017
longpriv_2017 = longpriv_2017 %>%
  group_by(sito) %>%
  summarise(timepriv_2017 = mean(time, na.rm = T))

## Merging the 2017 times
sit_zones = merge(sit_zones, longpriv_2017,by.x = "Name", by = "sito", all.x = T)



######## Public 2017 
pub_times2017 <- read_csv("Data/times_2017_w2012_v2.csv")
rate = mean(as.matrix(pub_times2017), na.rm = T)/mean(as.matrix(private_times2017), na.rm = T) ## Rate to complete public transport

# step 2: Matrix of 1 and 0
# 1 is Na, 0 otherwise
inverse_times = pub_times2017
inverse_times[inverse_times==0] = NA
inverse_times[!is.na(inverse_times)] = 0
inverse_times[is.na(inverse_times)] = 1

# Paso 3: Multiplying to complete missings
priv_times = private_times2017*inverse_times

# Paso 4: Equivalence to public transport trips
priv_times <- priv_times*rate

# Na by 0
pub_times2017[is.na(pub_times2017)] = 0

## Completing public transport matrix
pub_times2017 = pub_times2017+priv_times
rm(priv_times, inverse_times)
pub_times2017 = pub_times2017/60


temp_times2017 = pub_times2017[c(sits_order$matrix_order)]
temp_times2017 = temp_times2017[c(sits_order$matrix_order), ]
rownames(temp_times2017) = sits_order$SIT
colnames(temp_times2017) = sits_order$SIT
temp_times2017 = as.data.frame(temp_times2017)
temp_times2017$sit = rownames(temp_times2017)
longpub_2017 = melt(temp_times2017)   
colnames(longpub_2017) = c('sito','sitd','time')


longpub_2017 = longpub_2017 %>%
  group_by(sito) %>%
  summarise(timepub_2017 = mean(time, na.rm = T))

## Merging data

sit_zones = merge(sit_zones, longpub_2017, by.x = "Name", by = "sito", all.x = T)




#-------------------------------------------------------------#
#        2.Mapping the times                                  #  
#                                                             #
#-------------------------------------------------------------#


sit_zones = sit_zones[(sit_zones$Name %in% unique(emp$k)), ] 

writeOGR(sit_zones, "Data/Shapefiles/sit_Zones_data_2017.shp", layer = "sit_zones",
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

## Saving the 2017 breaks 

## I will use this on the next code to have the same breaks, code 20
brks_private = sit_zones$timepriv_2017
brks_public = sit_zones$timepub_2017


#### Maps
#Private
data_to_map("Output/Figures/", "privatetime2017.pdf", sit_zones, sit_zones$timepriv_2017, "YlOrRd", 1,
            Roads, Lines_2012, Lines_2017,  Stations, sit_zones$timepriv_2017)


#Public
data_to_map("Output/Figures/", "publictime2017.pdf", sit_zones, sit_zones$timepub_2017, "YlOrRd", 1,
            Roads, Lines_2012, Lines_2017,  Stations, sit_zones$timepub_2017)















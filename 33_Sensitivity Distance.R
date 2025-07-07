### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medell?n, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This Code finds the average and the median distance of trips in public transport
# that use more than one mode of public transportation

#Required libraries


# These libraries help you to read and transform data
library(readxl)
library(readr)
library(dplyr)
library(rstatix)
library(reshape2)
library(ggplot2)



# Library to work with spatial data
library(rgdal)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#               1.Loading EOD Data                            #  
#                                                             #
#-------------------------------------------------------------#

### Loading EOD data

#2012
eod_2012 = read.csv("Base/times_eod_2012_filter.csv")

# Keeping trips categorized as public transport and with more than 2 initial modes
#eod_2012 = eod_2012[eod_2012$Medio_reg == "Publico" & eod_2012$Multimode == 1, ]

## Keeping one observation by origin and destination
# eod_2012 = eod_2012 %>%
#   distinct(IdSito, IdSitd)


#2017
eod_2017 = read.csv("Base/times_eod_2017_filter.csv")

# Keeping trips categorized as public transport and with more than 2 initial modes
#eod_2017 = eod_2017[eod_2017$Medio_reg == "Publico" & eod_2017$Multimode == 1, ]

## Keeping one observation by origin and destination
# eod_2017 = eod_2017 %>%
#   distinct(SIT_O, SIT_D)


# Homologation of SIT zones 
homol_sit = read_csv("Base/homol_sit_v1.csv")

## homologation with 2012
homol_sit = homol_sit[,c('SIT_D','IdSitd','IdMunicipiod')]
eod_2012 = merge(eod_2012, homol_sit, by = c("IdSitd"), all.x = T) #IDs at destination

# Doing same with IDs at origin
colnames(homol_sit) = c('SIT_O','IdSito','IdMunicipioo')
eod_2012 = merge(eod_2012, homol_sit, by = c("IdSito"), all.x = T) #IDs at destination

## Keep the ones without missing data
eod_2012 = eod_2012[!is.na(eod_2012$SIT_O) & !is.na(eod_2012$SIT_D), ]





#-------------------------------------------------------------#
#               2.Loading SIT Zones and distance data         #  
#                                                             #
#-------------------------------------------------------------#

## ----- Loading SIT map
sit_zones = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017
sit_zones$matrix_order = c(1:nrow(sit_zones))

# Cleaning a little bit the SIT zones
sits_complete = sit_zones
sits = sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),]
sits = sits[!duplicated(sits@data[c("Name")]),] 

sits_order = data.frame(sits$Name,sits$matrix_order)
colnames(sits_order) = c("SIT","matrix_order")


## Loading distance data
travel_distances = read.csv("Base/bing_distances_matrix.csv")


## Filtering the matrix of distances
travel_distances = travel_distances[c(sits_order$matrix_order)]
travel_distances = travel_distances[c(sits_order$matrix_order), ]
colnames(travel_distances) = sits_order$SIT
travel_distances$sit = sits_order$SIT

## Distances from wide to long
travel_dist_long = melt(travel_distances)   
colnames(travel_dist_long) = c('sito','sitd','dist')
write.csv(travel_dist_long, "Base/Distances_long.csv", row.names = FALSE)


#-------------------------------------------------------------#
#               3.Merging the distance and calculations       #  
#                                                             #
#-------------------------------------------------------------#

#Distance for 2012
eod_2012 = merge(eod_2012, travel_dist_long, by.x = c("SIT_O", "SIT_D"), by.y = c("sito", "sitd"), all.x = TRUE)

# Distance for 2017
eod_2017 = merge(eod_2017, travel_dist_long, by.x = c("SIT_O", "SIT_D"), by.y = c("sito", "sitd"), all.x = TRUE)


## Checking descriptives
summary(eod_2012$dist) #2012
summary(eod_2017$dist) #2017

##Keep only the trips with more than one mode of public

eod_2012 = eod_2012[eod_2012$Medio_reg == "Publico"  & !is.na(eod_2012$dist),  ]

eod_2017 = eod_2017[eod_2017$Medio_reg == "Publico" & !is.na(eod_2017$dist), ]

##Bins
eod_2012 = eod_2012 %>%
  mutate(distance_bin = cut(dist, breaks = seq(0, max(dist), by = 3), include.lowest = TRUE))

eod_2017 = eod_2017 %>%
  mutate(distance_bin = cut(dist, breaks = seq(0, max(dist), by = 3), include.lowest = TRUE))

##Saving these files
write.csv(eod_2012, "Base/pre-bins_2012.csv")
write.csv(eod_2017, "Base/pre-bins_2017.csv")





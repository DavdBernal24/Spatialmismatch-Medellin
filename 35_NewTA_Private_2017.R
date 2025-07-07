### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 




# Here I compute the TA function for private transportation in 2017 using 
# the new version of the cost (The continuous cost). 

#Required libraries

### To load and transform the data
library(readr)
library(dplyr)
library(reshape2)

### For maps
library(rgdal)
library(spdep)




## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())


source("New_Function_Private.R") #New function with the transport cost 

##############################################################################


#-------------------------------------------------------------#
#        1. Loading Data                                      #  
#                                                             #
#-------------------------------------------------------------#

## ----- Loading SIT map
sit_zones = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017
sit_zones$matrix_order = c(1:nrow(sit_zones))

## ----- Employment
emp = read_csv("Base/emp2017.csv")
emp = emp[c("k","emp")]

## ----- Travel times
travel_times = read_csv("Base/bing_travel_duration_full.csv")
travel_times = travel_times/60 #Para que quede en minutos

## ----- Distance by roads
travel_distances = read_csv("Base/bing_distances_matrix.csv")


## costs (Average wage)
costs = read_csv("Base/costs_wmin.csv")
costs = costs[c("k","wmin2017")]

#-------------------------------------------------------------#
#        2. Some Transformations                              #  
#                                                             #
#-------------------------------------------------------------#

## Distance to the borders
d = read_csv("Base/mean_distance_border.csv")


div = travel_distances/travel_times
div[is.na(div)] = 0
div[!is.finite(as.matrix(div))] = 0

prom = mean(as.matrix(div))

d$time = (d$distance/1000)/prom
d$time2 = ifelse(d$time==0,mean(d$time),d$time)

for (i in 1:nrow(sit_zones)){
  travel_times[i,i] = d[i,3:3]
}

#-------------------------------------------------------------#
#        3. Computing TA 2017                                 #  
#                                                             #
#-------------------------------------------------------------#


### Loading data with the distances I want to compute the measure. 
transport_cost = read_csv("Base/Transport_costs.csv")

#Keeping the vars I need for public 2017
transport_cost = subset(transport_cost, select = c(sito, sitd, cost_2017_private))

transport_cost = transport_cost %>%
  rename(transpcost = cost_2017_private)

##Variables in transport cost as characters
transport_cost$sitd = as.character(transport_cost$sitd)
transport_cost$sito = as.character(transport_cost$sito)

## Loop to compute the measure
adjustment = 1
ipc = 1
year = 2017


compute_ta_newcost(sit_zones, travel_times, travel_distances, costs, emp,
                   transport_cost,  adjustment, ipc, "tapriv2017", "tapriv2017_percap",
                   "Output/Results/", "New_Ta_Private_2017.csv" ,year)



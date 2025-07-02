### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code computes the public transport accessibility measure for 2012

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

source("TA_Function_Public.R") # Function to compute the accessibility 

##############################################################################

#-------------------------------------------------------------#
#        1. Loading Data                                      #  
#                                                             #
#-------------------------------------------------------------#

## ----- Loading SIT map
sit_zones = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017
sit_zones$matrix_order = c(1:nrow(sit_zones))

## ----- Employment
emp = read_csv("Base/emp2012.csv")
emp = emp[c("k","emp")]

## ----- Travel times
pub_times = read_csv("Base/travel_times_public_2012.csv")

travel_times = pub_times
colnames(travel_times) = sit_zones$Name

## ----- Roads Distances (BING)
travel_distances = read_csv("Base/bing_distances_matrix.csv")


#-------------------------------------------------------------#
#        2. Doing some transformations                        #  
#                                                             #
#-------------------------------------------------------------#

div = travel_distances/travel_times
div[is.na(div)] <- 0
div[!is.finite(as.matrix(div))] = 0

prom <- mean(as.matrix(div))

d = read_csv("Base/mean_distance_border.csv")
d$time = (d$distance/1000)/prom
d$time2 = ifelse(d$time==0,mean(d$time),d$time)

for (i in 1:nrow(sit_zones)){
  travel_times[i,i] <- d[i,3:3]
}
rm(div, prom, d, i)

## Costs (Average wage)
costs = read_csv("Base/costs_wmin.csv")
costs = costs[c("k","wmin2012")]

#-------------------------------------------------------------#
#        3. Computing TA                                      #  
#                                                             #
#-------------------------------------------------------------#


### Loading data with the distances I want to compute the measure. 
cost_parameters = read_csv("Base/reg_fix_var_cost.csv")


## Loop to compute the measure
for(i in 1:nrow(cost_parameters)){
  
  cost_over_10 = 3200
  cost_under_10 = 1600
  adjustment = 1
  ipc = (89.08/76.27)
  year = 2012
  distance = cost_parameters$Distance[i]
  
  
  
  compute_ta(sit_zones, travel_times, travel_distances, costs, emp,
             cost_over_10, cost_under_10, adjustment, ipc, "tapub2012", "tapub2012_percap",
             "Output/Results/", paste0("Ta_public_dist", cost_parameters$Distance[i], "_2012.csv"), year, distance)
  
  print(paste0("Ready Distance ", cost_parameters$Distance[i], "KM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
  
  
}








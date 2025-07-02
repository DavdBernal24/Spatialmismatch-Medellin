### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code computes the private transport accessibility measure for 2017

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

source("TA_Private_Function.R") # Function to compute the accessibility 

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

## Loading cost information from regressions 
cost_parameters = read_csv("Base/reg_fix_var_cost.csv")


## Computing the measure for different distances
for(i in 1:nrow(cost_parameters)){
  
  fix_cost = cost_parameters$fix_cost_2017[i]
  variable_cost = cost_parameters$var_cost_2017[i]
  adjustment = 1
  ipc = 1
  year = 2017
  distance = cost_parameters$Distance[i]

  compute_ta_priv(sit_zones, travel_times, travel_distances,costs, emp,
                  fix_cost, variable_cost, adjustment, ipc, "tapriv2017", "tapriv2017_percap", "Output/Results/", 
                  paste0("Ta_private_dist", cost_parameters$Distance[i], "_2017.csv"), year, distance)
  
  print(paste0("Ready Distance ", cost_parameters$Distance[i], "KM"))
  
}








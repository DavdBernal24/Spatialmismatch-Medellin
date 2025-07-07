### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 




# Here I compute the TA function for public transportation in 2017 using 
# the new version of the cost. 

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


source("New_Function_Public.R") #New function with the transport cost 

##############################################################################


#-------------------------------------------------------------#
#        1. Loading Data                                      #  
#                                                             #
#-------------------------------------------------------------#


## ----- Loading SIT map
sit_zones = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017
sit_zones$matrix_order = c(1:nrow(sit_zones))

## ----- Empleo
emp = read_csv("Base/emp2017.csv")
emp = emp[c("k","emp")]

## ----- Travel times
priv_times = read_csv("Base/bing_travel_duration_full.csv")
pub_times = read_csv("Data/times_2017_w2012_v2.csv")


## Ratio between public and private (I use it to fill the gaps from Google)
rate = mean(as.matrix(pub_times), na.rm = T)/mean(as.matrix(priv_times), na.rm = T)


#-------------------------------------------------------------#
#        2. Doing some transformations                        #  
#                                                             #
#-------------------------------------------------------------#

inverse_times = pub_times
inverse_times[inverse_times==0] = NA
inverse_times[!is.na(inverse_times)] = 0
inverse_times[is.na(inverse_times)] = 1

# Matrix with private times in public times empty spots
priv_times = priv_times*inverse_times

# Multiplying by the ratio
priv_times = priv_times*rate

# N/As = 0
pub_times[is.na(pub_times)] = 0

## Sum of both matrixes
travel_times = pub_times+priv_times
rm(priv_times, inverse_times, pub_times)
travel_times = travel_times/60

## ----- Roads Distances (BING)
travel_distances = read_csv("Base/bing_distances_matrix.csv")

## Costs
costs = read_csv("Base/costs_wmin.csv")
costs = costs[c("k","wmin2017")]

## ----- Distance to border
d = read_csv("Base/mean_distance_border.csv", col_types = cols(X1 = col_skip()))

div = travel_distances/travel_times
div[is.na(div)] <- 0
div[!is.finite(as.matrix(div))] <- 0

prom <- mean(as.matrix(div))

d$time = (d$distance/1000)/prom
d$time2 = ifelse(d$time==0,mean(d$time),d$time)

for (i in 1:nrow(sit_zones)){
  travel_times[i,i] = d[i,3:3]
}

#-------------------------------------------------------------#
#        3. Computing TA                                      #  
#                                                             #
#-------------------------------------------------------------#


### Loading data with the distances I want to compute the measure. 
transport_cost = read_csv("Base/Transport_costs.csv")

#Keeping the vars I need for public 2017
transport_cost = subset(transport_cost, select = c(sito, sitd, cost_2017_public))

transport_cost = transport_cost %>%
  rename(transpcost = cost_2017_public)

##Variables in transport cost as characters
transport_cost$sitd = as.character(transport_cost$sitd)
transport_cost$sito = as.character(transport_cost$sito)

## Loop to compute the measure
adjustment = 1
ipc = 1
year = 2017

  
  
compute_ta_newcost(sit_zones, travel_times, travel_distances, costs, emp,
                   transport_cost,  adjustment, ipc, "tapub2017", "tapub2017_percap",
                   "Output/Results/", "New_Ta_Public_2017.csv" ,year)
  
  


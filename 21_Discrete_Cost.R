### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


## This code computes the estimated betas in the private transport measure 
# Which represents the private fixed cost and the private variable cost.

## Note: 2.18 is the parameter we get from the Ingresos y Gastos Survey

## Required libraries
library(rgdal)

### To load and transform the data
library(readr)
library(dplyr)
library(reshape2)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


rm(list = ls())

## -----------------------------------------------------
## LOADING DATA

## ----- Loading SIT map
sit_zones = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017
sit_zones$matrix_order <- c(1:nrow(sit_zones))


## ----- Travel times private
travel_times_priv = read_csv("Base/travel_times_private_2012.csv")

## ----- Employment
emp = read_csv("Base/emp2012.csv")
emp = emp[c("k","emp")]


## -----Distances through the roads
travel_distances = read_csv("Base/bing_distances_matrix.csv")


## -------costs (Average wage)
costs = read_csv("Base/costs_wmin.csv")
costs = costs[c("k","wmin2017")]


## ----------public transport times
## ----- Travel times

priv_times = read_csv("Base/bing_travel_duration_full.csv")
pub_times = read_csv("Data/times_2017_w2012_v2.csv")

## Ratio between public and private (I use it to fill the gaps from Google)
rate = mean(as.matrix(pub_times), na.rm = T)/mean(as.matrix(priv_times), na.rm = T)

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
travel_times_pub = pub_times+priv_times
rm(priv_times, inverse_times, pub_times)
travel_times_pub = travel_times_pub/60


############################Organizing data and the cost variables

# Deleting duplicates for sit_zones
sits_complete = sit_zones
sits = sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),]
sits = sits[!duplicated(sits@data[c("Name")]),] # from 544 we keep 535

sits_order = data.frame(sits$Name,sits$matrix_order)
colnames(sits_order) = c("SIT","matrix_order")

# The ones that are in the survey
sits_order = sits_order[(sits_order$SIT %in% unique(emp$k)), ] 



## Filtering travel times and adding ids
times = list(travel_times_priv, travel_times_pub, 
             travel_distances)

times_names = c("travel_times_priv", "travel_times_pub", 
                "travel_distances")

treatment_name = c("private_time", "public_time", 
                   "distance")

for (i in 1:length(times)){
  table = data.frame(times[[i]])
  colnames(table) <- sit_zones$Name
  table <- table[c(sits_order$matrix_order)]
  table$sit <- sit_zones$Name
  table <- table[c(sits_order$matrix_order),]
  table <- melt(table)   
  colnames(table) <- c('sito','sitd', treatment_name[i])
  assign(times_names[i], table)
}


Data = merge(travel_distances, travel_times_priv, 
             by = c("sito", "sitd"), all.x = T)

Data = merge(Data, travel_times_pub, by = c("sito", "sitd"), all.x = T)


## Adding the time cost 
Data = merge(Data, costs, 
              by.x = 'sitd', by.y = 'k', all.x = T)


####################################### Creating the costs variables

## Creating an empty Data Frame
results = data.frame(Distance = numeric(0), fix_cost_2012 =numeric(0), var_cost_2012 = numeric(0), 
                     fix_cost_2017 = numeric(0), var_cost_2017 = numeric(0)) 


## Dropping distances = 0
Data = Data[Data$distance != 0, ]

## Looping regressions over distances 7, 8, 9, 10, 10.2, 11, 12, 13

distance_regression = c(7,8, 9, 10, 10.2, 11, 12, 13)


for(dist in unique(distance_regression)){
  
  Data$pb_2017 = ifelse(Data$distance >= dist, 
                        4000, 
                        2000)
  Data$pb_2012 = ifelse(Data$distance >= dist, 
                        3200, 
                        1600)
  
  Data$pb_delta_2012 = 2.18*Data$pb_2012
  Data$pb_delta_2017 = 2.18*Data$pb_2017
  
  ###### Regressions
  reg2012 = lm(pb_delta_2012 ~ distance, data = Data)
  
  fix_2012 = coef(reg2012)[1] ## Fixed Cost
  var_2012 = coef(reg2012)[2] ## Variable Cost
  
  reg2017 = lm(pb_delta_2017 ~ distance, data = Data)
  
  fix_2017 = coef(reg2017)[1] ## Fixed cost
  var_2017 = coef(reg2017)[2] ## Variable Cost
  
  # Store the results in a data frame
  results = rbind(results, data.frame(Distance = dist, fix_cost_2012 = fix_2012, var_cost_2012 = var_2012, 
                                      fix_cost_2017 = fix_2017, var_cost_2017 = var_2017))
  
  print(paste0("Distance-------", dist, "----------Ready"))
}


## Saving the data
write.csv(results, "Base/reg_fix_var_cost.csv", row.names = FALSE)



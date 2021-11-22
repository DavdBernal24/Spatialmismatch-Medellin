###### Created by : David Bernal
##### Last modification : 06/09/2021
##### Modified by: 




## Required libraries

#install.packages("gmapsdistance")
library(gmapsdistance)

# For maps & graphs
library(maptools)
library(spdep)
library(rgdal)


rm(list = ls())

## ------------------------------------------------------------
## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## --------------------------------------------------
## Loading map
sit_zones <- readOGR("Datos/shapessits/SITzones2017.shp", layer="SITzones2017")

centroides <- as.data.frame(coordinates(sit_zones))
colnames(centroides) <- c("lon","lat")
centroides$location <- paste(centroides$lat, centroides$lon, sep="+")

inicio <- 1 #Set the initial sit zone
fin <- 35 #Set the last SIT zone, I did it in packs of 35 zones



origen <- centroides[inicio:fin,]$location
destino <- centroides$location

set.api.key("Here goes your key- be careful!")

## I choose public transport
results = gmapsdistance(origin = origen, 
                        destination = destino, 
                        mode = "transit", 
                        dep_date = "2020-02-25", 
                        dep_time = "07:00:00"
)


# Here I am just saving the times. 
tiempos <- results$Time
write.csv(tiempos, file = paste0("google_travel_times_",inicio,"_",fin,".csv")) ## saving travel times

distancias <- results$Distance
write.csv(distances, file = paste0("google_travel_distance_",inicio,"_",fin,".csv")) ## Saving distance- it is different to euclidean

status <- results$Status
write.csv(distances, file = paste0("google_travel_status_",inicio,"_",fin,".csv")) ## It tells me the status of the trips, if there is route or not

### This is the matrix we use
#write.csv(tiempos, file = paste0("Base/public_travel_duration_full.csv",inicio,"_",fin,".csv"))

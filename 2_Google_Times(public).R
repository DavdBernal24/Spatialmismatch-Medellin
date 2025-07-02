### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code computes travel times from using Public transport from the Google API
# You cannot run it without a key (using this code could cost you money, be careful)

#Required libraries

library(rgdal)
library(gmapsdistance)

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#                     1.Setting things up                     #  
#                                                             #
#-------------------------------------------------------------#

sit_zones = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") # Map 2017

centroids = as.data.frame(coordinates(sit_zones))
colnames(centroids) = c("lon","lat")
centroids$location = paste(centroids$lat, centroids$lon, sep="+")

beginning = 1 #Set the initial sit zone
end = 35 #Set the last SIT zone, I did it in packs of 35 zones


origin = centroids[beginning:end,]$location
destination = centroids$location

## Computing the origin-destination results

set.api.key("Here goes your key- be careful!") ## Here goes your key

results = gmapsdistance(origin = origin, 
                        destination = destination, 
                        mode = "transit", 
                        dep_date = "2020-02-25", 
                        dep_time = "07:00:00")


#-------------------------------------------------------------#
#                     2.Saving values                         #  
#                                                             #
#-------------------------------------------------------------#

## Here I am just saving the times. 
times = results$Time
write.csv(times, file = paste0("google_travel_times_",inicio,"_",fin,".csv")) ## saving travel times

distances = results$Distance
write.csv(distances, file = paste0("google_travel_distance_",inicio,"_",fin,".csv")) ## Saving distance- it is different to euclidean

status = results$Status
write.csv(status, file = paste0("google_travel_status_",inicio,"_",fin,".csv")) ## It tells me the status of the trips, if there is route or not


### This is the matrix we use with the full observations so check the code carefully if you want to replicate something
# similar

#write.csv(times, row.names = FALSE, file = paste0("Base/public_travel_duration_full.csv",inicio,"_",fin,".csv"))







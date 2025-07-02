### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code computes travel times from using private transport (car or mortorcycle) from the Google API
# You cannot run it without a key (using this code could cost you money, be careful)

#Required libraries

library(RCurl) #To get the result of an url
library(rjson) #To read json files
library(rgdal) # For spatial data
library(beepr) # Makes nice sounds 


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
colnames(centroids) <- c("lon","lat")


## Creating an empty matrix to fill with the data
distances = array(dim = c(nrow(centroids),nrow(centroids)))
times = array(dim = c(nrow(centroids),nrow(centroids)))


#-------------------------------------------------------------#
#                     2.Computing times using BING            #  
#                                                             #
#-------------------------------------------------------------#

getData = function(StartLocation,EndLocation) {
  
  ## You can get the key here https://www.bingmapsportal.com/
  bingkey = "Put your bing key here, be careful!"
  
  #starting_point <- "6.2217837,-75.5957932"
  #end_point <- "6.229549,-75.5541398"
  
  url = paste0("http://dev.virtualearth.net/REST/V1/Routes/Driving?wp.0=",
                StartLocation,
                "&wp.1=",EndLocation,#"&optmz=timeWithTraffic",
                "&key=",bingkey)
  
  suppressWarnings(json_bing = fromJSON(paste(readLines(url), collapse = "")))
  
  travelDuration = json_bing$resourceSets[[1]]$resources[[1]]$travelDuration # Here I get the travel times in private transport
  #travelDistance <- json_bing$resourceSets[[1]]$resources[[1]]$travelDistance ## Here I get the distances 
  
  #return(travelDistance)
  return(travelDuration)
}



#-------------------------------------------------------------#
#                     3.Saving data                           #  
#                                                             #
#-------------------------------------------------------------#

## NOTE: You need to repeat the process if you need distances or travel times
# Uncomment the line 70 and change times by distances in line 89 if you want travel times

for(i in 1:544){
  for(j in 1:nrow(centroids)){
    print(paste("Posicion",i,"-",j))
    times[i,j] <- print(
      getData(
        paste0(centroids$lat,",",centroids$lon)[i],
        paste0(centroids$lat,",",centroids$lon)[j]
      )
    )
  }
  beep('mario')
}


write.csv(times, row.names = FALSE, file = "Base/bing_travel_duration_full.csv") # Saving travel times
#write.csv(distances, row.names = FALSE, file = "Base/bing_distances_matrix.csv") # Saving travel distances





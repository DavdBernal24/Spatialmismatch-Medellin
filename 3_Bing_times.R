###### Created by : David Bernal
##### Last modification : 06/23/2021
##### Modified by: 


## Required libraries
library(RCurl) #To get the result of an url
library(rjson) #To read json files

# For maps
library(rgdal)

# Other
library(beepr) 


rm(list = ls())


## ------------------------------------------------------------
## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


## --------------------------------------------------
## Loading map
sit_zones <- readOGR("Datos/shapessits/SITzones2017.shp", layer="SITzones2017")


## --------------------------------------------------
## Lat and lon for each zone
centroides <- as.data.frame(coordinates(sit_zones))
colnames(centroides) <- c("lon","lat")

#n <- 5 # SIT de prueba

#centroides <- centroides[1:n,]

## Creating an empty matrix to fill with the data
distances <- array(dim = c(nrow(centroides),nrow(centroides)))


## --------------------------------------------------
## Computing travel times in bing
getData <- function(StartLocation,EndLocation) {
  
 
  ## You can get the key here https://www.bingmapsportal.com/
  bingkey <- "Put your bing key here, be careful!"
  
  #starting_point <- "6.2217837,-75.5957932"
  #end_point <- "6.229549,-75.5541398"
  
  url <- paste0("http://dev.virtualearth.net/REST/V1/Routes/Driving?wp.0=",
                StartLocation,
                "&wp.1=",EndLocation,#"&optmz=timeWithTraffic",
                "&key=",bingkey)
  
  suppressWarnings(json_bing <- fromJSON(paste(readLines(url), collapse="")))
  
  travelDuration <- json_bing$resourceSets[[1]]$resources[[1]]$travelDuration # Here I get the travel times in private transport
  #travelDistance <- json_bing$resourceSets[[1]]$resources[[1]]$travelDistance ## Here I get the distances 
  
  #return(travelDistance)
  return(travelDuration)
}

#distances <- as.matrix(distances)
for(i in 1:544){
  for(j in 1:nrow(centroides)){
    print(paste("Posicion",i,"-",j))
    distances[i,j] <- print(
      getData(
        paste0(centroides$lat,",",centroides$lon)[i],
        paste0(centroides$lat,",",centroides$lon)[j]
      )
    )
  }
  beep('mario')
}

# write.csv(distances, file = "Base/bing_distances_matrix.csv") # Saving distances
write.csv(distances, file = "Base/bing_travel_duration_full.csv") # Saving travel times








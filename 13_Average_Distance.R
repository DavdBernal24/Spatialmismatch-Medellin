### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code computes the max and min distance from a centroid to a polygon
# I do it in order to compute the travel time inside the same zone

#Required libraries


# Library to work with spatial data
library(rgdal) # Reads shapefiles
#library(rgeos)  #Working with spatial data
library(spatstat) # for cross distance
library(geosphere) # for haversine formula


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#               1.Computing distances                         #  
#                                                             #
#-------------------------------------------------------------#


sit_zones = readOGR("Data/Shapefiles/SITs_2017_cut/SITs_2017.shp", layer = "SITs_2017") #Maps for 2017
sit_zones$matrix_order = c(1:nrow(sit_zones))


## Centroids for each polygon 
coords <- as.data.frame(coordinates(sit_zones))
colnames(coords) <- c("x","y")

## Empty table (to fill with the computations)
dist <- data.frame()


## Computing the distances
for (i in 1:nrow(coords)){
  
  ## Looking for the vertixes
  vertices = as.data.frame(sit_zones@polygons[[i]]@Polygons[[1]]@coords)
  colnames(vertices) = c("x","y")
  
  ## computing the distance between the centroid and all the possible vertixes
  crossd = crossdist(vertices$x, vertices$y, coords$x[i], coords$y[i])
  
  ## Looking for the max and min distance
  mindist = which.min(crossd[,1])
  maxdist = which.max(crossd[,1])
  
  ## Coordinates
  c = c(coords$x[i], coords$y[i])
  ## Coordinates max distance
  ld = c(vertices$x[c(maxdist)], vertices$y[c(maxdist)])
  ## Coordinates min distance
  md = c(vertices$x[c(mindist)], vertices$y[c(mindist)])
  
  ## transforming into distances
  distHaversine(c, md)
  distHaversine(c, ld)
  
  ## Average
  distance = (distHaversine(c, md) + distHaversine(c, ld))/2
  
  ## adding to the table
  dist = rbind(dist,distance)
}

colnames(dist) = c("distance")
write.csv(dist, file = "Base/mean_distance_border.csv", row.names = FALSE)





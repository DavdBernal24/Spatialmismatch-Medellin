###### Created by : David Bernal
##### Last modification : 
##### Modified by: 


## This code finds the max distance from a centroid to a polygon and the min distance, I average them in order to
# find




## Required libraries

library(rgdal) # para leer shapes
library(rgeos)
library(spatstat) # for cross distance
library(geosphere) # for haversine formula



## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()



rm(list = ls())


## Loading SIT map
sit_zones <- readOGR("Data/shapessits/SITzones2017.shp", layer = "SITzones2017")
sit_zones$matrix_order <- c(1:nrow(sit_zones))

## Centroids for each polygon 
coords <- as.data.frame(coordinates(sit_zones))
colnames(coords) <- c("x","y")


## Empty table (to fill with the computations)
dist <- data.frame()


## Computing the distances
for (i in 1:nrow(coords)){
  
  ## Looking for the vertixes
  vertices <- as.data.frame(sit_zones@polygons[[i]]@Polygons[[1]]@coords)
  colnames(vertices) <- c("x","y")
  
  ## computing the distance between the centroid and all the possible vertixes
  crossd <- crossdist(vertices$x, vertices$y, coords$x[i], coords$y[i])
  
  ## Looking for the max and min distance
  mindist <- which.min(crossd[,1])
  maxdist <- which.max(crossd[,1])
  
  ## Coordinates
  c <- c(coords$x[i], coords$y[i])
  ## Coordinates max distance
  ld <- c(vertices$x[c(maxdist)], vertices$y[c(maxdist)])
  ## Coordinates min distance
  md <- c(vertices$x[c(mindist)], vertices$y[c(mindist)])
  
  ## transforming into distances
  distHaversine(c, md)
  distHaversine(c, ld)
  
  ## Average
  distance <- (distHaversine(c, md) + distHaversine(c, ld))/2
  
  ## adding to the table
  dist <- rbind(dist,distance)
}

colnames(dist) <- c("distance")
write.csv(dist, file = "Base/mean_distance_border.csv")



## para graficar los puntos
#i <- 544 ## sit a graficar
#plot(vertices, cex = 0.1, col = "gray")
#points(coords$x[i], coords$y[i], col = "red", cex = 1)
#points(vertices$x[c(mindist)], vertices$y[c(mindist)], col = "blue", cex = 1)
#points(vertices$x[c(maxdist)], vertices$y[c(maxdist)], col = "green", cex = 1)









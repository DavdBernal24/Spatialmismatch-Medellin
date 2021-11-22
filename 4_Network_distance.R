###### Created by : David Bernal
##### Last modification : 06/09/2021
##### Modified by: 


## This code computes the travel times for walking using OpenStreetMaps, we assume a speed of 4km/H



## Required libraries

### To load and transform the data
library(readr)
library(dplyr)
library(RColorBrewer)
library(classInt)
library(tibble)

## for plots
library(ggplot2)

### For maps
library(rgdal)
library(spdep)
library(maptools)

### Other libraries to compute short path distance
library(osmdata)
library(sf)
library(tidygraph)
library(igraph)
library(units)
library(tmap)
library(rgrass7)
library(link2GI)
library(nabor)
library(stplanr)

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


rm(list = ls())

## See here for more information:
## https://www.r-spatial.org/r/2019/09/26/spatial-networks.html
## https://docs.ropensci.org/stplanr/reference/SpatialLinesNetwork.html#examples

## -----------------------------------------------------

## info for standarizing shapes
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


## Loading SIT map
sit_zones <- readOGR("Datos/shapessits/SITzones2017.shp", layer = "SITzones2017")
sit_zones$matrix_order <- c(1:nrow(sit_zones))
# quitando duplicados del SIT
#sit_zones <- sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),]
#sit_zones <- sit_zones[!duplicated(sit_zones@data[c("Name")]),] # de 544 quedan 535

sits <- sit_zones

## ids we don't consider
#sit_correg <- c('147','320','321','322','323','324','325','326','416','418','455',
#                '456','457','458','601','602','603','605','607','622','648','649')

## ids from Medellin
#ids <- read_csv("times_eod_2017.csv", col_types = cols(X1 = col_skip()))
#ids <- ids[,c("SIT_D","ID_MUNICIPIO_D")] ## 001 for medellin
#ids <- ids[!duplicated(ids),]
#ids <- subset(ids, ID_MUNICIPIO_D !="-")
#ids <- subset(ids, ID_MUNICIPIO_D =="001")
#ids <- ids[!(ids$SIT_D %in% sit_correg), ]

## filtering for Medellin
#sits <- merge(sit_zones, ids, by.x = "Name", by.y = "SIT_D", all.x = T)
#sits <- sits[!is.na(sits$ID_MUNICIPIO_D),]
#plot(sits)
#rm(ids, sit_correg, sit_zones)


## -----------------------------------------------------
## Getting coordinates for the SITS' centroids
sits_points <- data.frame(coordinates(sits))
colnames(sits_points) <- c('x','y')


## -----------------------------------------------------
## Getting the Medellin streets from OpenStreetMaps (it was done in 2020)
med_highways <- opq(bbox =  c(min(sits_points$x), min(sits_points$y), max(sits_points$x), max(sits_points$y))) %>% #opq("Medellín, Colombia") %>%
  add_osm_feature(key = "highway"#, value = "primary"
  ) %>%
  osmdata_sf() %>%
  osm_poly2line()


## Organizing information as lines to graph
med_streets <- med_highways$osm_lines %>% 
  st_transform(crs = crs) %>% 
  select(highway)
ggplot(data = med_streets) + geom_sf()


## Step 1: Clean the network (we need one conected network)
sln_sf <- stplanr::SpatialLinesNetwork(med_streets)
sln_sf <- stplanr::sln_clean_graph(sln_sf)


## Lines that are going to be used as routes 
med_streets <- sln_sf@sl %>% 
  st_transform(crs = crs) %>% 
  select(highway)
rm(sln_sf)

ggplot(data = med_streets) + geom_sf()


## Step 2: Give each edge a unique index
edges <- med_streets %>%
  mutate(edgeID = c(1:n()))

edges


## Step 3: Create nodes at the start and end point of each edge
nodes <- edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))

nodes


## Step 4: Give each node a unique index
nodes <- nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)

nodes


## Step 5: Combine the node indices with the edges
source_nodes <- nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes <- nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges = edges %>%
  mutate(from = source_nodes, to = target_nodes)

edges


## Step 6: Remove duplicate nodes
nodes <- nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))

nodes


## Step 7: Convert to tbl_graph
graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

graph

## add a variable describing the length of each edge
graph <- graph %>%
  activate(edges) %>%
  mutate(length = st_length(geometry))

graph


## Checking the plot is still ok
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()) + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), size = 0.1)


## -----------------------------------------------------
## Shortest path

## testing with a couple of SITS
sit_o <- 1
sit_d <- 38

o_point <- st_point(c(sits_points[sit_o,]$x, sits_points[sit_o,]$y)) %>% 
  st_sfc(crs = crs)

d_point <- st_point(c(sits_points[sit_d,]$x, sits_points[sit_d,]$y)) %>%
  st_sfc(crs = crs)


## Coordinates of the origin and destination node, as matrix
coords_o <- o_point %>%
  st_coordinates() %>%
  matrix(ncol = 2)

coords_d <- d_point %>%
  st_coordinates() %>%
  matrix(ncol = 2)

## Coordinates of all nodes in the network
nodes <- graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  st_as_sf()

coords <- nodes %>%
  st_coordinates()

## Calculate nearest points on the network.
node_index_o <- nabor::knn(data = coords, query = coords_o, k = 1)
node_index_d <- nabor::knn(data = coords, query = coords_d, k = 1)
node_o <- nodes[node_index_o$nn.idx, ]
node_d <- nodes[node_index_d$nn.idx, ]


## We use the ID to calculate the shortest path, and plot it:
path <- shortest_paths(
  graph = graph,
  from = node_o$nodeID, # new origin
  to = node_d$nodeID,   # new destination
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)

path$vpath
path$epath

## A subgraph of the original graph can now be created, containing only those nodes 
## and edges that are part of the calculated path.
path_graph <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()

## We can plot everything together
ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey', size = 0.5) +
  geom_sf(data = path_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = o_point, size = 2) +
  geom_sf(data = d_point, size = 2)  +
  geom_sf_label(data = o_point, aes(label = 'o'), nudge_x = 0.004) +
  geom_sf_label(data = d_point, aes(label = 'd'), nudge_x = 0.005)


## We can use this subgraph to compute the total length of the path
path_graph %>%
  activate(edges) %>%
  as_tibble() %>%
  summarise(length = sum(length))


## -----------------------------------------------------
## Shortest path for all the SITs

## Coordinates of all nodes in the network
nodes <- graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  st_as_sf()

coords <- nodes %>%
  st_coordinates()

## Empty matrix
wdistances <- array(dim = c(nrow(sits_points),nrow(sits_points)))


for (o in 1:nrow(sits_points)){
  for (d in 1:nrow(sits_points)){
    
    ## origin and destination SITS
    o_point <- st_point(c(sits_points[o,]$x, sits_points[o,]$y)) %>% 
      st_sfc(crs = crs)
    
    d_point <- st_point(c(sits_points[d,]$x, sits_points[d,]$y)) %>%
      st_sfc(crs = crs)
    
    ## Coordinates of the origin and destination node, as matrix
    coords_o <- o_point %>%
      st_coordinates() %>%
      matrix(ncol = 2)
    
    coords_d <- d_point %>%
      st_coordinates() %>%
      matrix(ncol = 2)
    
    ## Calculate nearest points on the network.
    node_index_o <- nabor::knn(data = coords, query = coords_o, k = 1)
    node_index_d <- nabor::knn(data = coords, query = coords_d, k = 1)
    node_o <- nodes[node_index_o$nn.idx, ]
    node_d <- nodes[node_index_d$nn.idx, ]
    
    ## We use the ID to calculate the shortest path, and plot it:
    path <- shortest_paths(
      graph = graph,
      from = node_o$nodeID, # new origin
      to = node_d$nodeID,   # new destination
      output = 'both',
      weights = graph %>% activate(edges) %>% pull(length)
    )
    
    path$vpath
    path$epath
    
    ## subgraph with the route
    path_graph <- graph %>%
      subgraph.edges(eids = path$epath %>% unlist()) %>%
      as_tbl_graph()
    
    ##
    route_length <- path_graph %>%
      activate(edges) %>%
      as_tibble() %>%
      summarise(length = sum(length))
    
    print(paste("the path length is",route_length[[1]]))
    wdistances[o,d] <- route_length[[1]]
    
    print(paste0("=========== LISTO EL SIT O y D: ",o,"-",d," ==========="))
  }
}


write.csv(wdistances, file = "Base/networkdistances.csv")



write.csv(wdistances, file = "networkdistances.csv")

## Here I get to transform the distance to kilometers
wdistances <- wdistances/1000
prom <- 4/60 # I assume people walk at 4km/h
travel_times <- wdistances/prom # Travel times in minutes
write.csv(travel_times, file = "Base/pie_travel_duration_full.csv")







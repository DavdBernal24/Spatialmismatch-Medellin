## Required libraries

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


source("ta_function.R")

## -----------------------------------------------------
## LOADING DATA

## ----- Loading SIT map
sit_zones <- readOGR("Data/shapessits/SITzones2017.shp", layer = "SITzones2017")
sit_zones$matrix_order <- c(1:nrow(sit_zones))

## ----- Employment
emp <- read_csv("Base/emp2017.csv", col_types = cols(X1 = col_skip()))
emp <- emp[c("k","emp")]

## ----- Travel times
travel_times <- read_csv("Base/bing_travel_duration_full.csv", col_types = cols(X1 = col_skip()))
travel_times <- travel_times/60 #Para que quede en minutos

## ----- Distance by roads
travel_distances <- read_csv("Base/bing_distances_matrix.csv", col_types = cols(X1 = col_skip()))

## costs (Average wage)
costs <- read_csv("Base/costs_wmin.csv", col_types = cols(X1 = col_skip()))
costs <- costs[c("k","wmin2017")]

## ----- Distance to the borders
d <- read_csv("Base/mean_distance_border.csv", col_types = cols(X1 = col_skip()))

div <- travel_distances/travel_times
div[is.na(div)] <- 0
div[!is.finite(as.matrix(div))] <- 0

prom <- mean(as.matrix(div))

d$time <- (d$distance/1000)/prom
d$time2 <- ifelse(d$time==0,mean(d$time),d$time)

for (i in 1:nrow(sit_zones)){
  travel_times[i,i] <- d[i,6:6]
}
#prom <- mean(as.matrix(travel_times))


compute_ta(sit_zones, travel_times, travel_distances, costs, emp,
           4000, 2000, 2.18, 1, "tapriv2017", "tapriv2017_percap",
           "Output/TA_computation/", "A_private_2017.csv")





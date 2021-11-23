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
priv_times <- read_csv("Base/bing_travel_duration_full.csv", col_types = cols(X1 = col_skip()))
pub_times <- read_csv("Base/times_2017_w2012_v2.csv", col_types = cols(X1 = col_skip()))


rate <- pub_times/priv_times
rate <- as.matrix(rate)
rate[is.nan(rate)] <- NA
rate <- mean(rate, na.rm = T)



inverse_times <- pub_times
inverse_times[inverse_times==0] <- NA
inverse_times[!is.na(inverse_times)] <- 0
inverse_times[is.na(inverse_times)] <- 1


priv_times <- priv_times*inverse_times
priv_times <- priv_times*rate


pub_times[is.na(pub_times)] <- 0


travel_times <- pub_times+priv_times
rm(priv_times, inverse_times, pub_times)
travel_times <- travel_times/60

## ----- Distance through the roads
travel_distances <- read_csv("Base/bing_distances_matrix.csv", col_types = cols(X1 = col_skip()))

## Costs (Average wage)
costs <- read_csv("Base/costs_wmin.csv", col_types = cols(X1 = col_skip()))
costs <- costs[c("k","wmin2017")]

## ----- Distance to border
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
           4000, 2000, 1, 1, "tapub2017", "tapub2017_percap",
           "Output/TA_computation/", "A_public_2017.csv")





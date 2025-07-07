### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medell�n, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 



### This code computes Descriptive stats of the TA measure using the new way to compute transport costs
#### 


## Required libraries

### To load and transform the data
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#               1.Loading Results TA                          #  
#                                                             #
#-------------------------------------------------------------#


##############################################################################
# Accessibility measures

Results_Private_2017 = read.csv("Output/Results/New_Ta_Private_2017.csv")
Results_Public_2017 = read.csv("Output/Results/New_Ta_Public_2017.csv")
Results_Private_2012 = read.csv("Output/Results/New_Ta_Private_2012.csv")
Results_Public_2012 = read.csv("Output/Results/New_Ta_Public_2012.csv")

# ID variable
Results_Private_2017$id = "private_2017"
Results_Public_2017$id = "public_2017"
Results_Public_2012$id = "public_2012"
Results_Private_2012$id = "private_2012"

#Renaming variables for the RBIND
names(Results_Private_2017) = c("k", "ta", "percap", "id")
names(Results_Public_2017) = c("k", "ta", "percap", "id")
names(Results_Private_2012) = c("k", "ta", "percap", "id")
names(Results_Public_2012) = c("k", "ta", "percap", "id")

#append 
Results = rbind(Results_Private_2017, Results_Public_2017, Results_Private_2012, Results_Public_2012)


## Average Accessibility 
Results_ta = Results %>%
  group_by(id) %>%
  summarise(mean_ta = mean(ta))

##Average percap Accessibility 
Results_ta_percap = Results %>%
  group_by(id) %>%
  summarise(mean_percap = mean(percap))

# Merging both 
Results = merge(Results_ta, Results_ta_percap, by = "id", all.x = TRUE)

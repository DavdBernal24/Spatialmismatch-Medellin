### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code creates breaks for the new accessibility measure maps

## Required libraries

library(readr)
library(rgdal)
library(dplyr)
library(classInt)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

#-------------------------------------------------------------#
#        1. Loading Data                                      #  
#                                                             #
#-------------------------------------------------------------#
## A private
A_priv_2017 = read.csv("Output/Results/New_Ta_Private_2017.csv")
A_priv_2012 = read.csv("Output/Results/New_Ta_Private_2012.csv")


## A public
A_pub_2017 = read.csv("Output/Results/New_Ta_Public_2017.csv")
A_pub_2012 = read.csv("Output/Results/New_Ta_Public_2012.csv")


## Keeping the columns I need
A_priv_2017 = subset(A_priv_2017, select = c(tapriv2017, tapriv2017_percap))
names(A_priv_2017) = c("A2017", "Adj2017")


A_pub_2017 = subset(A_pub_2017, select = c(tapub2017, tapub2017_percap))
names(A_pub_2017) = c("A2017", "Adj2017")

A_priv_2012 = subset(A_priv_2012, select = c(tapriv2012, tapriv2012_percap))
names(A_priv_2012) = c("A2012", "Adj2012")


A_pub_2012 = subset(A_pub_2012, select = c(tapub2012, tapub2012_percap))
names(A_pub_2012) = c("A2012", "Adj2012")

#-------------------------------------------------------------#
#        2. Appending and Saving data for breaks              #  
#                                                             #
#-------------------------------------------------------------#

## Now I need to append the bases
breaks_2017 = rbind(A_priv_2017, A_pub_2017)

breaks_2012 = rbind(A_priv_2012, A_pub_2012)

write.csv(breaks_2017, file = "Base/breaks_NewAccessibility_2017.csv", row.names = FALSE)
write.csv(breaks_2012, file = "Base/breaks_NewAccessibility_2012.csv", row.names = FALSE)
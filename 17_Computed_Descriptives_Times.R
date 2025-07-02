### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


## This code computes descriptive stats for computed travel times. 

## Required libraries

### To load and transform the data
library(readr)
library(dplyr)
library(xtable) # creating latex tables
library(reshape2) # It allows me to melt the dataset


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


rm(list = ls())


##############################################################################

#-------------------------------------------------------------#
#         1.Computed travel times for public and private      #                          
#                                                             #
#-------------------------------------------------------------#

## Private times 2017
private_times2017 = read_csv("Base/bing_travel_duration_full.csv")
private_times2017  = private_times2017 /60 #Passing it to minutes

mean(as.matrix(private_times2017 ), na.rm = T)
private_times2017  = melt(private_times2017 , value.name = "time")

private_times2017 = subset(private_times2017, select = -c(variable))

private_times2017$mode = "Private"


## Public times 2017
public_times2017 = read_csv("Data/times_2017_w2012_v2.csv")
public_times2017  = public_times2017 /60 #Passing it to minutes


mean(as.matrix(public_times2017 ), na.rm = T)
public_times2017  = melt(public_times2017 , value.name = "time")


public_times2017 = na.omit(public_times2017)

public_times2017 = subset( public_times2017, select = -c(variable))

public_times2017$mode = "Public"




## Private times 2012 
private_times2012 = read_csv("Base/travel_times_private_2012.csv")


mean(as.matrix(private_times2012 ), na.rm = T)
private_times2012  = melt(private_times2012 , value.name = "time")


private_times2012 = na.omit(private_times2012)

private_times2012 = subset( private_times2012, select = -c(variable))

private_times2012$mode = "Private"

## public times 2012 (It is already in minutes)
public_times2012 = read_csv("Base/travel_times_public_2012.csv")


public_times2012  = melt(public_times2012 , value.name = "time")

## Removing some outliers, need to check this

public_times2012 = na.omit(public_times2012)

public_times2012 = public_times2012[public_times2012$time < (mean(public_times2012$time) + 0.8*sd(public_times2012$time)), ]


public_times2012 = subset( public_times2012, select = -c(variable))

public_times2012$mode = "Public"


#-------------------------------------------------------------#
#         2. Organizing information for latex                 #                          
#                                                             #
#-------------------------------------------------------------#


### Appending and merging the info
times2017 = rbind(private_times2017, public_times2017)
times2012 = rbind(private_times2012, public_times2012)


#Averages by mode
mu1 = times2017 %>%
  group_by(mode) %>% 
  dplyr::summarise(mean_2017 = mean(time, na.rm = T)) 


mu2 = times2012 %>%
  group_by(mode) %>% 
  dplyr::summarise(mean_2012 = mean(time, na.rm = T)) 


means = merge(mu1, mu2, by = "mode")

### Computing differences

means$diff_mean = means$mean_2017-means$mean_2012
means$per_diff_mean = paste0(round((means$diff_mean/means$mean_2012)*100,2),"%")



## organizing for latex table
colnames(means) = c("Transport","Mean 2017","Mean 2012","Diff Means","% Diff Means")
means$Transport = gsub("Privado", "Private", means$Transport)
means$Transport = gsub("Publico", "Public", means$Transport)
means = means[order(-means$`Mean 2012`),]



print(xtable(means, type = "latex", label = NULL, align = "lccccc"), 
      file = "Output/Tex/means_commuting_computed.tex", include.rownames=FALSE)









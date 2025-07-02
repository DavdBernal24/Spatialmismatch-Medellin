### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


## This code computes descriptive stats for travel times reported in the EOD

## Required libraries

### To load and transform the data
library(readr)
library(dplyr)
library(xtable) # creating latex tables

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


rm(list = ls())


##############################################################################

#-------------------------------------------------------------#
#               1.Reported times without                      #
#                 expansion factors                           #
#                                                             #
#-------------------------------------------------------------#

##2012
times_eod_2012 = read_csv("Base/times_eod_2012_filter.csv")

##2017
times_eod_2017 = read_csv("Base/times_eod_2017_filter.csv")


## Average time by transportation mode

## for 2017
mu1 = subset(times_eod_2017) %>%
  group_by(Medio_reg) %>% 
  dplyr::summarise(mean_2017 = mean(minutos, na.rm = T)) 

## For 2012
mu2 = subset(times_eod_2012) %>%
  group_by(Medio_reg) %>% 
  dplyr::summarise(mean_2012 = mean(minutos, na.rm = T)) 

means = merge(mu2, mu1, by="Medio_reg")


## Computing differences
means$diff_mean = means$mean_2017 - means$mean_2012
means$per_diff_mean = paste0(round((means$diff_mean/means$mean_2012)*100,2),"%")

## Organizing values for latex
colnames(means) = c("Transport","Mean 2012","Mean 2017","Diff Means","% Diff Means")
means$Transport = gsub("A pie", "Walking", means$Transport)
means$Transport = gsub("Privado", "Private", means$Transport)
means$Transport = gsub("Publico", "Public", means$Transport)
means = means[order(-means$`Mean 2012`),]



print(xtable(means, type = "latex", label = NULL, align = "lccccc"), 
      file = "Output/Tex/means_commuting_od.tex", include.rownames=FALSE)

#-------------------------------------------------------------#
#               2.Reported times with                         #
#                 expansion factors                           #
#                                                             #
#-------------------------------------------------------------#


## 2012
times_eod_2012 = read_csv("Base/times_eod_2012_filter.csv")
times_eod_2012$rtime = times_eod_2012$minutos*times_eod_2012$PrimeroDeFactorExpansion

# #2017
times_eod_2017 = read_csv("Base/times_eod_2017_filter.csv")
times_eod_2017$rtime = times_eod_2017$minutos*times_eod_2017$factor_expansion


### Average time by transportation mode

## for 2017
mu1 = subset(times_eod_2017) %>%
  group_by(Medio_reg) %>% 
  dplyr::summarise(total_2017 = sum(rtime, na.rm = T), 
                   f_2017 = sum(factor_expansion, na.rm = T)) 
## for 2012
mu2 = subset(times_eod_2012) %>%
  group_by(Medio_reg) %>% 
  dplyr::summarise(total_2012 = sum(rtime, na.rm = T),
                   f_2012 = sum(PrimeroDeFactorExpansion, na.rm = T)) 

mu1$mean_2017 = mu1$total_2017/mu1$f_2017
mu2$mean_2012 = mu2$total_2012/mu2$f_2012


means = merge(mu2, mu1, by="Medio_reg")

## Computing differences
means$diff_mean <- means$mean_2017 - means$mean_2012
means$per_diff_mean <- paste0(round((means$diff_mean/means$mean_2012)*100,2),"%")

means = subset(means, select = -c(total_2012, total_2017, f_2012, total_2017, f_2017 ))



## table for latex
colnames(means) <- c("Transport","Mean 2012","Mean 2017","Diff Means","% Diff Means")

means = means[ , c("Transport","Mean 2017", "Mean 2012","Diff Means","% Diff Means")]


means$Transport <- gsub("A pie", "Walking", means$Transport)
means$Transport <- gsub("Privado", "Private", means$Transport)
means$Transport <- gsub("Publico", "Public", means$Transport)
means <- means[order(-means$`Mean 2012`),]



print(xtable(means, type = "latex", label = NULL, align = "lccccc"), 
      file = "Output/Tex/means_commuting_od_EF.tex", include.rownames=FALSE)

### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code computes the travel times differences between 2017 and 2012
# by SIT zones and transportation modes

#Required libraries


# These libraries help you to read and transform data
library(readr)
library(dplyr)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#               1.Loading data and doing                      #
#                 some transformations                        #                                   
#-------------------------------------------------------------#

#2012
times_eod_2012 = read_csv("Base/times_eod_2012_filter.csv")

#2017
times_eod_2017 = read_csv("Base/times_eod_2017_filter.csv")

# Homologation of SIT zones 
homol_sit = read_csv("Base/homol_sit_v1.csv")


## Average travel time by SIT zone and mode in 2017
stats_2017 = times_eod_2017 %>% 
  group_by(SIT_O, SIT_D, Medio_reg) %>%
  dplyr::summarise(mean = round(mean(minutos),3))

stats_2012 = times_eod_2012 %>%
  group_by(IdSito, IdSitd, Medio_reg) %>%
  dplyr::summarise(mean = round(mean(minutos), 3))

## Columns I need from 2017
temp = stats_2017[,c('SIT_O','SIT_D','Medio_reg','mean')]
colnames(temp) = c('SIT_O','SIT_D','Medio_tte','mean_2017')


## homologation with 2012
homol_sit = homol_sit[,c('SIT_D','IdSitd','IdMunicipiod')]
temp = merge(temp, homol_sit, by = c("SIT_D"), all.x = T)
colnames(homol_sit) = c("SIT_O","IdSito","IdMunicipioo")
temp = merge(temp, homol_sit, by = c("SIT_O"), all.x = T)

## Deleting letters from 2012 SITs IDs
temp$IdSito = gsub("([ABCDEFGHIJKL])", "", temp$IdSito)
temp$IdSitd = gsub("([ABCDEFGHIJKL])", "", temp$IdSitd)

## Columns I need from 2012
temp2 = stats_2012[,c('IdSito','IdSitd','Medio_reg','mean')]
colnames(temp2) = c('IdSito','IdSitd','Medio_tte','mean_2012')


#-------------------------------------------------------------#
#               2.Merging data and computing                  #
#                 differences                                 #   
#-------------------------------------------------------------#

temp3 = merge(temp, temp2, by = c('IdSito','IdSitd','Medio_tte'), all.x = T)


## Computing differences
temp3$diff_mean = temp3$mean_2017-temp3$mean_2012


write.csv(temp3, row.names = FALSE, file = "Base/diff_mean_sits_medio_tte_origen_destino.csv")






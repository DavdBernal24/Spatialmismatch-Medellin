### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medell?n, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 



### This code does exactly the same this that Time_Differences_Communes.R 
# But using only morning rush hours from 5 to 9 Am



#Required libraries


# These libraries help you to read and transform data
library(dplyr)
library(readxl)
library(readr)


# Library to work with spatial data
library(rgdal)

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
times_eod_2012 = read_csv("Base/times_eod_2012_filterEarly5.csv")
times_eod_2012$rtime = times_eod_2012$minutos*times_eod_2012$PrimeroDeFactorExpansion # Multiplying by expansion factors

#2017
times_eod_2017 = read_csv("Base/times_eod_2017_filterEarly5.csv")
times_eod_2017$rtime = times_eod_2017$minutos*times_eod_2017$factor_expansion  # Multiplying by expansion factors


## Average travel time by mode and origin 
mu1 = times_eod_2017 %>%
  group_by(Medio_reg, ID_COMUNA_O) %>% 
  dplyr::summarise(mean_2017 = mean(minutos, na.rm = T))

mu2 = times_eod_2012 %>%
  group_by(Medio_reg, IdComunao) %>% 
  dplyr::summarise(mean_2012 = mean(minutos, na.rm = T))


mu2$IdComunao = as.numeric(as.character(mu2$IdComunao)) ## Lets make this id numeric



#-------------------------------------------------------------#
#               2. Communes info and differences              #
#                  computations                               #               
#-------------------------------------------------------------#

## Note: I made this file manually, it contains the IDs of the communes in 2017 and 2012 EOD,
# tt is an homologation

hom_comunas = read_csv("Data/comunas_IDs.csv") 

##  the variable comuna is the ID of 2017, we merge with the travel time
mu2 = merge(mu2, hom_comunas, by.x = "IdComunao", by.y = "Cod_Comuna", all.x = T)
mu2 = subset(mu2, select = -c(IdComunao))
rm(hom_comunas)

## Not all the communes have information in 2012. Therefore, 
# we use the average by transportation mode

mu3 = times_eod_2012 %>%
  group_by(Medio_reg) %>% 
  dplyr::summarise(mean_2012_mode = mean(minutos, na.rm = T))

mu4 = times_eod_2017  %>%
  group_by(Medio_reg)  %>%
  dplyr::summarise(mean_2017_mode = mean(minutos, na.rm = T))


## Merging all the data

means = merge(mu1, mu2, by.x = c("Medio_reg","ID_COMUNA_O"), by.y = c("Medio_reg","comuna"), all.x = T)
means = merge(means, mu3, by = "Medio_reg", all.x = T)  
means = merge(means, mu4, by = "Medio_reg", all.x = T)

## Replacing the values of 2012 that have missings
means$mean_2012 = ifelse(is.na(means$mean_2012), means$mean_2012_mode, means$mean_2012) 

## Computing differences
means$diff_mean = means$mean_2017 - means$mean_2012

## Lets compute the difference in %
means$diff_mean_percent = ((means$mean_2017 - means$mean_2012)/means$mean_2012)*100

## Saving it
write.csv(means, "Base/diff_means_communeEarly5.csv", row.names = F)
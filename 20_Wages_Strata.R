### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code identifies the socieconomic strata from the SIT zones and add the wage by SIT
## We ended up keeping it constant, but we have the strata information in case we want to modify it on the future


#Required libraries

### To load and transform the data
library(readr)
library(readxl)
library(dplyr)
library(classInt)
library(modeest)





## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())



##############################################################################


#-------------------------------------------------------------#
#        1. Organizing data                                   #  
#                                                             #
#-------------------------------------------------------------#



## In order to have the strata by neighborhood and SIT
infoviajes = read_excel("Data/EOD_2017.xlsx", sheet = "DATOS VIAJES")
infoviajes = infoviajes[infoviajes$MOTIVO_VIAJE == '01',]

## Neighbors of the origin-SITS
viajes = infoviajes[infoviajes$ID_MUNICIPIO_O == '001',]
viajes = viajes[,c('SIT_O','ID_BARRIO_O')]
colnames(viajes) = c('k','id_barrio')

## Neighborhoods of the destination-SITS
viajes2 = infoviajes[infoviajes$ID_MUNICIPIO_D == '001',]
viajes2 = viajes2[,c('SIT_D','ID_BARRIO_D')]
colnames(viajes2) = c('k','id_barrio')

## We put the neighborhoods together (origin-destination)
viajes = rbind(viajes,viajes2)

## Counting the strata by neighborhood
viajes = viajes %>%
  group_by(k, id_barrio) %>%
  summarise(conteo_barrio = length(id_barrio))
rm(infoviajes, viajes2)

## Keeping the mode
viajes = viajes[order(viajes$k, viajes$conteo_barrio),] # ordering from max to min strata
viajes = viajes[!duplicated(viajes[, c("k")], fromLast=T),] # We keep the highest
viajes = viajes[,c('k','id_barrio')]
viajes = viajes[!is.na(viajes$k),]


## Households info
hogares = read_excel("Data/EOD_2017.xlsx", sheet = "DATOS HOGARES")
hogares = hogares[,c('ID_MUNICIPIO','ESTRATO_EPM','ID_SIT')]
hogares = subset(hogares, hogares$ID_MUNICIPIO == '001')

# Strata for each SIT
count = hogares %>%
  group_by(ID_SIT) %>%
  summarise(est = mlv(ESTRATO_EPM, method = "mfv"),
            conteo = length(ESTRATO_EPM))

# Deleting duplicates
count <- count[order(count$ID_SIT, count$est),] # ordering from max to min strata
count <- count[!duplicated(count[, c("ID_SIT")], fromLast=T),] # We keep the highest

## SITS with not household information
hogares <- read_excel("Data/EOD_2017.xlsx", sheet = "DATOS HOGARES")
hogares <- hogares[,c('ID_MUNICIPIO','ESTRATO_EPM','ID_BARRIO')]
hogares <- subset(hogares, hogares$ID_MUNICIPIO == '001')

# Computing the strata by SIT (We keep the mode)
est_barrio <- hogares %>%
  group_by(ID_BARRIO) %>%
  summarise(est_barrio = mlv(ESTRATO_EPM, method = "mfv"),
            conteo = length(ESTRATO_EPM))

# Deleting duplicates
est_barrio <- est_barrio[order(est_barrio$ID_BARRIO, est_barrio$est_barrio),] # ordering from max to min strata
est_barrio <- est_barrio[!duplicated(est_barrio[, c("ID_BARRIO")], fromLast=T),] # We keep the highest
rm(hogares)

## assigning strata
viajes <- merge(viajes, count, by.x = 'k', by.y = 'ID_SIT', all.x = T)
viajes <- merge(viajes, est_barrio, by.x = 'id_barrio', by.y = 'ID_BARRIO', all.x = T)

## for the ones without trips we assign the neighborhood strata
viajes$estrato2 <- ifelse(!is.na(viajes$est), viajes$est, viajes$est_barrio)
viajes$estrato <- substr(viajes$estrato2,9,9)
viajes <- viajes[,c('k','estrato')]

# costs information
costs <- read_csv("Base/Wmin.csv")
costs <- costs[,c('estrato','wmin2017','wmin2012')]

# Merging with strata
costs <- merge(viajes, costs, by = "estrato", all.x = T)
rm(viajes, count, est_barrio)


#Average wage
write.csv(costs, file = "Base/costs_wmin.csv", row.names = FALSE)
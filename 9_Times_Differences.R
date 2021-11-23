###### Created by : David Bernal
##### Last modification : 18/10/2021
##### Modified by: 

## Required libraries

library(readr)
library(dplyr)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())


## -------------------------------------------------------------------------

#info 2012
times_eod_2012 <- read_csv("Base/times_eod_2012.csv", col_types = cols(X1 = col_skip()))
times_eod_2012 <- subset(times_eod_2012, Medio_reg != "Avion")

#info 2017
times_eod_2017 <- read_csv("Base/times_eod_2017_2.csv", col_types = cols(X1 = col_skip()))
times_eod_2017 <- subset(times_eod_2017, Medio_reg != "Avion")

#homologacion de sits
homol_sit      <- read_csv("Base/homol_sit_v1.csv", col_types = cols(X1 = col_skip()))

## -----------------------------------------------------
## -----------------------------------------------------
## -----------------------------------------------------
## descriptives for 2017 for each SIT

stats <- times_eod_2017 %>% 
  group_by(SIT_O, SIT_D, Medio_reg) %>%
  dplyr::summarise(obs = length(Medio_reg),
                   mean = round(mean(minutos),3),
                   sd   = round(sd(minutos),2),
                   min  = min(minutos),
                   p05  = quantile(minutos, 0.05),
                   p25  = quantile(minutos, 0.25),
                   p50  = quantile(minutos, 0.50),
                   p75  = quantile(minutos, 0.75),
                   p95  = quantile(minutos, 0.95),
                   p99  = quantile(minutos, 0.95),
                   max  = max(minutos))

# Number of observations for each SIT
count_2017 <- times_eod_2017 %>% 
  group_by(SIT_O,SIT_D) %>%
  dplyr::summarise(group = n())

# Participation by transportation mode
stats <- merge(stats, count_2017, by = c('SIT_O','SIT_D'), all.x = T)
stats$part_2017 <- round(stats$obs/stats$group,2)
rm(count_2017)


### Identifying tails
stats2 <- stats[,c('SIT_O','SIT_D','Medio_reg','p99')]
times_eod_2017 <- merge(times_eod_2017, stats2, 
                        by = c('SIT_O','SIT_D','Medio_reg'), all.x = T)
rm(stats2)

times_eod_2017$cortar <- ifelse(times_eod_2017$minutos > times_eod_2017$p99, "SI", "NO")

## Descriptive stats after cutting tails
stats_2017 <- subset(times_eod_2017, cortar != "SI") %>% 
  group_by(SIT_O, SIT_D, Medio_reg) %>%
  dplyr::summarise(obs = length(Medio_reg),
                   mean = round(mean(minutos),2),
                   sd   = round(sd(minutos),2),
                   min  = min(minutos),
                   p05  = quantile(minutos, 0.05),
                   p25  = quantile(minutos, 0.25),
                   p50  = quantile(minutos, 0.50),
                   p75  = quantile(minutos, 0.75),
                   p95  = quantile(minutos, 0.95),
                   p99  = quantile(minutos, 0.99),
                   max  = max(minutos))

stats2 <- stats[,c('SIT_O','SIT_D','Medio_reg','part_2017')]
stats_2017 <- merge(stats_2017, stats2, by = c('SIT_O','SIT_D','Medio_reg'), all.x = T)
rm(stats, stats2)


## -----------------------------------------------------
## -----------------------------------------------------
## -----------------------------------------------------
## Descriptive stats for 2012
stats <- times_eod_2012 %>% 
  group_by(IdMunicipioo,IdSito,IdMunicipiod,IdSitd, Medio_reg) %>%
  dplyr::summarise(obs = length(Medio_reg),
                   mean = round(mean(minutos),3),
                   sd   = round(sd(minutos),2),
                   min  = min(minutos),
                   p05  = quantile(minutos, 0.05),
                   p25  = quantile(minutos, 0.25),
                   p50  = quantile(minutos, 0.50),
                   p75  = quantile(minutos, 0.75),
                   p95  = quantile(minutos, 0.95),
                   p99  = quantile(minutos, 0.95),
                   max  = max(minutos))

count_2012 <- times_eod_2012 %>% 
  group_by(IdMunicipioo,IdSito,IdMunicipiod,IdSitd) %>%
  dplyr::summarise(group = n())

stats <- merge(stats, count_2012, 
               by = c('IdMunicipioo','IdSito','IdMunicipiod','IdSitd'), all.x = T)
stats$part_2012 <- round(stats$obs/stats$group,2)
rm(count_2012)


### --------------------- Identifying tales
stats2 <- stats[,c('IdMunicipioo', 'IdSito','IdMunicipiod','IdSitd','p99')]
times_eod_2012 <- merge(times_eod_2012, stats2, 
                        by = c('IdMunicipioo', 'IdSito','IdMunicipiod','IdSitd'), all.x = T)
rm(stats2)

times_eod_2012$cortar <- ifelse(times_eod_2012$minutos > times_eod_2012$p99, "SI", "NO")

## -----------------------------------------------------
## Descriptive stats after cutting tails
stats_2012 <- subset(times_eod_2012, cortar != "SI") %>% 
  group_by(IdMunicipioo,IdSito,IdMunicipiod,IdSitd,Medio_reg) %>%
  dplyr::summarise(obs = length(Medio_reg),
                   mean = round(mean(minutos),2),
                   sd   = round(sd(minutos),2),
                   min  = min(minutos),
                   p05  = quantile(minutos, 0.05),
                   p25  = quantile(minutos, 0.25),
                   p50  = quantile(minutos, 0.50),
                   p75  = quantile(minutos, 0.75),
                   p95  = quantile(minutos, 0.95),
                   p99  = quantile(minutos, 0.99),
                   max  = max(minutos))


stats2 <- stats[,c('IdMunicipioo','IdSito','IdMunicipiod','IdSitd','Medio_reg','part_2012')]
stats_2012 <- merge(stats_2012, stats2, 
                    by = c('IdMunicipioo','IdSito','IdMunicipiod','IdSitd','Medio_reg'), 
                    all.x = T)
rm(stats, stats2)

## -----------------------------------------------------
## Columns I need from 2017
temp <- stats_2017[,c('SIT_O','SIT_D','Medio_reg','mean','part_2017')]
colnames(temp) <- c('SIT_O','SIT_D','Medio_tte','mean_2017','part_2017')

## homologation with 2012
homol_sit <- homol_sit[,c('SIT_D','IdSitd','IdMunicipiod')]
temp <- merge(temp, homol_sit, by = c("SIT_D"), all.x = T)
colnames(homol_sit) <- c("SIT_O","IdSito","IdMunicipioo")
temp <- merge(temp, homol_sit, by = c("SIT_O"), all.x = T)

## Deleting letters from 2012 SITS (we don't have them in 2017)
temp$IdSito <- gsub("([ABCDEFGHIJKL])", "", temp$IdSito)
temp$IdSitd <- gsub("([ABCDEFGHIJKL])", "", temp$IdSitd)

## Columns I need from 2012
temp2 <- stats_2012[,c('IdMunicipioo','IdSito','IdMunicipiod','IdSitd','Medio_reg','mean','part_2012')]
colnames(temp2) <- c('IdMunicipioo','IdSito','IdMunicipiod','IdSitd','Medio_tte','mean_2012','part_2012')

## Merge 2017 and 2012 information
temp3 <- merge(temp, temp2, by = c('IdMunicipioo','IdSito','IdMunicipiod','IdSitd','Medio_tte'), all.x = T)
rm(temp, temp2)

## Computing differences
temp3$diff_mean <- temp3$mean_2017-temp3$mean_2012
temp3$diff_part <- temp3$part_2017-temp3$part_2012


write.csv(temp3, file = "Base/diff_mean_sits_medio_tte_origen_destino.csv")



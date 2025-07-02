### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


## This code computes the employment using the formula from the paper

## Required libraries

### To load and transform the data
library(readr)
library(readxl)
library(dplyr)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


rm(list = ls())


##############################################################################

## These numbers come from the Gran Encuesta
# Integrada de hogares

#e1 = 1717000 # Employment 2017 
#e2 = 1557000 # Employment 2012

e1 = 1100509 # Employment 2017 
e2 = 1024055 #2012
#-------------------------------------------------------------#
#               1.Employment in 2017                          #  
#                                                             #
#-------------------------------------------------------------#

times_eod_2017 = read_csv("Base/times_eod_2017_filter.csv", col_types = cols(X1 = col_skip()))

# Expansion factor 2017
factor_exp = read_csv("Data/factor_expansion.csv")
factor_exp = factor_exp[c("ID_COMUNA_O","factor_expansion")]
colnames(factor_exp) = c("ID_COMUNA","factor_expansion")


## Employment by commune
test1 = times_eod_2017 %>%
  group_by(ID_COMUNA_D) %>%
  summarise(empcom = sum(factor_expansion))

## Merge with the expansion factor by commune
temp = merge(factor_exp, test1, by.x = "ID_COMUNA", by.y = "ID_COMUNA_D")

## expansion factor multiplied by employment in each commune
temp$fe_empcom = temp$factor_expansion * temp$empcom

## Sum
temp$sum_fe_empcom = sum(temp$fe_empcom)

## Employment by SIT
test2 = times_eod_2017 %>%
  group_by(SIT_D, ID_COMUNA_D) %>%
  summarise(empsit = sum(factor_expansion))

## Merging data
temp2 = merge(test2, temp, by.x = "ID_COMUNA_D", by.y = "ID_COMUNA", all.x = T)
colnames(temp2) = c("ID_COMUNA_D","k","empsit","factor_expansion","empcom","fe_empcom","sum_fe_empcom")

temp2$emp = e1 * (temp2$fe_empcom/temp2$sum_fe_empcom) * (temp2$empsit/temp2$empcom)

# Employment by SIT in 2017 using the papers' formula
write.csv(temp2, row.names = FALSE,  file = "Base/emp2017.csv")

#-------------------------------------------------------------#
#               2.Employment in 2012                          #  
#                                                             #
#-------------------------------------------------------------#

times_eod_2012 = read_csv("Base/times_eod_2012_filter.csv", col_types = cols(X1 = col_skip()))


# SITS Homologation
homol_sit = read_csv("Base/homol_sit_v1.csv", col_types = cols(X1 = col_skip()))
homol_sit = homol_sit[homol_sit$IdMunicipiod == "10",]
homol_sit$IdSitd = gsub("([ABCDEFGHIJKL])", "", homol_sit$IdSitd)

## Expansion Factors in 2012
EOD2012 = read_excel("Data/EOD_2012.xlsx")
factor_exp = EOD2012[EOD2012$IdMunicipioo == "10",]
factor_exp = factor_exp[!is.na(factor_exp$IdComunao),]
factor_exp = factor_exp[c("IdComunao","PrimeroDeFactorExpansion")]
factor_exp = factor_exp[!duplicated(factor_exp[,c("IdComunao")]),]


# Employment by commune
test1 = times_eod_2012 %>%
  group_by(IdComunad) %>%
  summarise(empcom = sum(PrimeroDeFactorExpansion))

# Merge with the expansion factor by commune
temp = merge(factor_exp, test1, by.x = "IdComunao", by.y = "IdComunad")

# multiplying the employment by the expansion factor by commune
temp$fe_empcom = temp$PrimeroDeFactorExpansion * temp$empcom

# Sum
temp$sum_fe_empcom = sum(temp$fe_empcom)

# Employment per SIT
test2 = times_eod_2012 %>%
  group_by(IdSitd, IdComunad) %>%
  summarise(empsit = sum(PrimeroDeFactorExpansion))

# Merging data
temp2 = merge(test2, temp, by.x = "IdComunad", by.y = "IdComunao", all.x = T)

# Add to the SITs names
temp = merge(temp2, homol_sit, by = "IdSitd", all.x = T)

count_sit = temp %>%
  group_by(IdSitd) %>%
  summarise(n = n())

temp = merge(temp, count_sit, by = c("IdSitd"), all.x = T)
temp$empsit2 = temp$empsit*(1/temp$n)
temp = temp[, c("SIT_D","empsit2","PrimeroDeFactorExpansion","empcom","fe_empcom","sum_fe_empcom")]
colnames(temp) = c("k","empsit","PrimeroDeFactorExpansion","empcom","fe_empcom","sum_fe_empcom")

#-----------------------------------------------
temp$emp = e2 * (temp$fe_empcom/temp$sum_fe_empcom) * (temp$empsit/temp$empcom)

# Saving file for employment in 2012 at SIT level
write.csv(temp, row.names = FALSE, file = "Base/emp2012.csv")





### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


## This code computes the employment using the formula from the paper for dependent
# and independent employment 

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

e1 = 1100509 # Employment 2017 
e2 = 1024055 # Employment 2012
e2_indep = e2*0.374 # Independent employment 2012
e2_dep = e2*(1-0.374) #Dependent employment 2012
e1_indep = e1*0.344 # Independent employment 2017
e1_dep = e1*(1-0.344) # Dependent employment 2017

#-------------------------------------------------------------#
#               1.Employment in 2017                          #  
#                                                             #
#-------------------------------------------------------------#

indep_dep = c("04", "05") ## 04 is independent and 05 is dependent.
empl_dep = c(e1_indep, e1_dep)
name_emp = c("independiente", "dependiente")

emp_dep_2017 = data.frame(indep_dep, empl_dep, name_emp)

for (i in 1:nrow(emp_dep_2017)){
  
  times_eod_2017 = read_csv("Base/times_eod_2017_filter.csv", col_types = cols(X1 = col_skip()))
  
  times_eod_2017 = times_eod_2017[times_eod_2017$OCUPACION == emp_dep_2017$indep_dep[i],]
  
  
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
  
  temp2$emp = emp_dep_2017$empl_dep[i] * (temp2$fe_empcom/temp2$sum_fe_empcom) * (temp2$empsit/temp2$empcom)
  
  write.csv(temp2, row.names = FALSE,  file = paste0("Base/emp2017_", emp_dep_2017$name_emp[i], ".csv"))
  
}

###Note: We just did it for 2017 since we only have the data division for those years. 











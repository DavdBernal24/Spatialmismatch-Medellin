#--------------------------------------------------------------------------------------------------------
# Project:            Better or worse job accessibility? Understanding changes in spatial mismatch at
#                     the intra-urban level: evidence from  Medell?n, Colombia
# Authors:            David Bernal; Gustavo Garcia; Jorge Perez
# Code created by :   Jorge Emilio Melendez
# Last modification : 18/06/2025
# Modified by: 
#--------------------------------------------------------------------------------------------------------
# Purpose:            This code creates the employment data we need to do the bootstrapping
#--------------------------------------------------------------------------------------------------------



# To load and transform the data
library(readr)
library(dplyr)
library(reshape2)
library(tidyverse)

# For maps
#library(rgdal)
#library(spdep)



# Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())


# bases de datos para 2012 y 2017
bases <- c("2012", "2017")


# hacemos el for loop
for (i in 1:length(bases)) {
  
  # abrimos base de datos de empleo
  emp = read_csv(paste0("Base/emp", bases[i], "_new.csv"))
  
  
  # variables que necesitamos para genrar simulaciones de empleo a nivel indiviudal
  empleo_aux <- emp %>% dplyr::select(k, empsit, empcom)
  
  
  # redondemaos el empleo dentro de la sit (pues es el n?mero de observaciones que necesitamos)
  empleo_aux <- empleo_aux %>% mutate(empsit = round(empsit, digits = 0))
  
  
  # sacamos proporci?n de empleo de cada SIT dentro de la comuna
  empleo_aux <- empleo_aux %>% mutate(proporcion_empleo = empsit/empcom)
  
  
  # generamos ID de la comuna a partir del empleo
  empleo_aux <- empleo_aux %>%
    group_by(empcom) %>%
    mutate(id_comuna = group_indices())
  
  
  # Se expande el n?mero de observaciones tal que para cada SIT el n?mero de observaciones es la del empleo dentro de la SIT
  expanded_sit <- empleo_aux %>% uncount(weights = empsit)
  
  
  # generamos una variable de ID individuo
  expanded_sit <- cbind(id_persona = 1:nrow(expanded_sit), expanded_sit)
  
  
  # nos quedamos con variables de inter?s (El remuestro se hace sobre id_individuo con reemplazo dentro de la comuna)
  final_data <- expanded_sit %>% dplyr::select(k, id_comuna, id_persona)
  
  
  write_csv(final_data, paste0("Base/empleo_individual_", bases[i], ".csv"))
  
  
}









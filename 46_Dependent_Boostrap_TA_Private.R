#------------------------------------------------------------------------------------------------------------------------
# Project:            Better or worse job accessibility? Understanding changes in spatial mismatch at
#                     the intra-urban level: evidence from  Medellin, Colombia
# Authors:            David Bernal; Gustavo Garcia; Jorge Perez
# Script name:        34_Dependent_Boostrap_TA_Private_v4.R
# Code created by :   Jorge Emilio Melendez
# Last modification : 18/06/2025
# Modified by:        Jorge Emilio Melendez
#------------------------------------------------------------------------------------------------------------------------
# Purpose:            This code does a bootstrap over employment within comunas to calculate job accessibility variable
#                     and obtain interval coefficients of the SIT mean variable. The resampled employment in 2012 
#                     should be correlated with resampled employment in 2017.
#------------------------------------------------------------------------------------------------------------------------



# To load and transform the data
library(readr)
library(dplyr)
library(reshape2)
library(tidyverse)
library(data.table)
library(ggplot2)
library(MASS)

# For maps
#library(rgdal)
library(spdep)
library(sf)

# For bootstrap
library(boot)
library(rsample)
library(sampling)
library(lmtest)


# Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())





#-------------------------------------------------------------#
#    0. Aquí se revisa la relacion entre empleo de 2012 y 
#       2017 para ver que se relacionan postivamente
#-------------------------------------------------------------#


## ----- Empleo
emp17 = read_csv("T:/Spatial Mismatch/Base/emp2017.csv")
emp12 = read_csv("T:/Spatial Mismatch/Base/emp2012.csv")

setDT(emp12)
setDT(emp17)

emp = merge(emp12, emp17,by='k', all.X=TRUE)

#plotemp1217 = ggplot(emp, aes(x = empsit.x, y = empsit.y)) + geom_point()
plotemp1217 = ggplot(emp, aes(x = emp.x, y = emp.y)) + geom_point()


emp[, lnemp12 := log(emp.x)]
emp[, lnemp17 := log(emp.y)]

plotlogemp1217 = ggplot(emp, aes(x = lnemp12, y = lnemp17)) + geom_point()



rel1217 = lm(lnemp17 ~ lnemp12, emp)
summary(rel1217)
bptest(rel1217)


# Esto muestra que la relacion entre el empleo 2012 y empleo 2017, en logs
# es homoscedastica.

# Vamos a usar esa relacion para sacar realizaciones de 2017 a partir
# del bootstrap de 2012

cov_employment <- cov(emp$empsit.x, emp$empsit.y)



#----------------------------
#    Setup del bootstrap
#----------------------------

# Definimos el numero de bootstraps
bs = 1000

# vectores donde se guardan los resultados del bootstrap
ta_priv_12 <- numeric(bs)
ta_priv_pc_12 <- numeric(bs)
ta_priv_17 <- numeric(bs)
ta_priv_pc_17 <- numeric(bs)


# generamos semilla
set.seed(162817)


# for loop con numero de remuestreos
for (iter in 1:bs) {
  
  # para ver en que numero de bootsrap vamos  
  print(paste0("Iteration number ", iter," of boostrap"))  
  
  #------------------------------------------------
  #  1. Aqui empezamos a cargar datos para obtener
  #     medida para transporte privado 2012.
  #------------------------------------------------
  
  ## ----- Loading SIT map
  sit_zones = read_sf("T:/Spatial Mismatch/Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017
  sit_zones$matrix_order = c(1:nrow(sit_zones))
  
  ## ----- Employment
  emp = read_csv("T:/Spatial Mismatch/Base/emp2012.csv")
  
  ## ----- Going to try to get some comuna ids from this
  comunaids = emp %>%  group_by(k) %>% filter(row_number()==1)
  
  comunaids = comunaids[,c("k","empcom")]
  comunaids = rename(comunaids, comid = empcom)
  
  ## ----- Travel times
  travel_times = read_csv("T:/Spatial Mismatch/Base/travel_times_private_2012.csv")
  colnames(travel_times) = sit_zones$Name
  
  ## ----- Roads Distances (BING)
  travel_distances = read_csv("T:/Spatial Mismatch/Base/bing_distances_matrix.csv")
  
  ## ----- Individual employment
  ind_emp = read_csv("T:/Spatial Mismatch/Base/empleo_individual_2012.csv")
  
  ## Merge the comuna ids to this
  ind_emp = merge(ind_emp,comunaids, by = "k")
  
  
  #------------------------------------
  #     Doing some transformations                        
  #------------------------------------
  
  div = travel_distances/travel_times
  div[is.na(div)] <- 0
  div[!is.finite(as.matrix(div))] = 0
  
  prom <- mean(as.matrix(div))
  
  d = read_csv("T:/Spatial Mismatch/Base/mean_distance_border.csv")
  d$time = (d$distance/1000)/prom
  d$time2 = ifelse(d$time==0,mean(d$time),d$time)
  
  for (i in 1:nrow(sit_zones)){
    travel_times[i,i] <- d[i,3:3]
  }
  rm(div, prom, d, i)
  
  
  ## Costs (Average wage)
  costs = read_csv("T:/Spatial Mismatch/Base/costs_wmin.csv")
  costs = costs[c("k","wmin2012")]
  
  ## Loading cost information from regressions 
  #cost_parameters = read_csv("Base/reg_fix_var_cost.csv") JE(07/03/2025): Por ahora usamos 10.2
  
  
  
  # Aqui creamos una base con medidas importantes de empleo que pegaremos despu?s del bootrsap
  #emp <- as.data.frame(emp)
  #variables_emp <- emp %>% select(k, PrimeroDeFactorExpansion, empcom, fe_empcom, sum_fe_empcom)
  variables_emp <- emp %>% dplyr::select(k, PrimeroDeFactorExpansion, empcom, fe_empcom, sum_fe_empcom)
  
  
  ### Loading data with the distances I want to compute the measure. 
  transport_cost = read_csv("T:/Spatial Mismatch/Base/Transport_costs.csv")
  
  #Keeping the vars I need for public 2017
  transport_cost = subset(transport_cost, select = c(sito, sitd, cost_2012_private))
  
  transport_cost = transport_cost %>%
    rename(transpcost = cost_2012_private)
  
  ##Variables in transport cost as characters
  transport_cost$sitd = as.character(transport_cost$sitd)
  transport_cost$sito = as.character(transport_cost$sito)
  
  
  # paramteros para calcular medida
  adjustment = 1
  ipc = (89.08/76.27)
  year = 2012
  
  # nuevos parametros de costos
  fix_cost = 1882.976489
  variable_cost = 320.5318296
  
  #distance = cost_parameters$Distance[i]
  
  #-------------------------------------------------
  #     3. Aqui empieza el remuestreo del empleo                                                                                         
  #-------------------------------------------------
  
  # nombre de variables de medidas 
  varname <- "tapriv2012"
  percap_varname <- "tapriv2012_percap"
  
  
  # se remuestra el empleo de 2012
  ind_emp_res <- ind_emp[sample(nrow(ind_emp), replace = TRUE), ]
  
  
  e2 = 1024055 # Employment 2012
  e2_17 = 1100509 # Employment 2017
  
  mean_emp <- c(e2, e2_17)
  
  # Promedio de todos los meses de 2017 en Medellin AM, ECH
  # Error relativo en poblacion ocupada 13 áreas, Dic 2017
  relerr = 0.008 
  se = e2*relerr
  
  cov_matrix <- matrix(c(se^2, cov_employment, cov_employment, se^2), nrow = 2)
  
  random_observation <- mvrnorm(n = 1, mu = mean_emp, Sigma = cov_matrix)
  
  # se obtiene una observacion del empleo total
  e2bs <- random_observation[1]
  e2bs_17 <- random_observation[2]
  
  
  # calulamos empleo del remuestreado
  counts <- table(ind_emp_res$k)
  counts <- as.data.frame(counts)
  names(counts) <- c("k", "empsitbs")
  
  countscom <- table(ind_emp_res$comid)
  countscom <- as.data.frame(countscom)
  names(countscom) <-c("comid","empcombs")
  
  # Pegamos el empleo remuestreado
  emp_2 <- merge(counts, variables_emp, by = "k")
  emp_2 <- merge(emp_2,countscom, by.x = "empcom", by.y="comid")
  
  emp_2 <- emp_2 %>% mutate(fe_empcombs = empcombs * PrimeroDeFactorExpansion)
  
  
  # Sumar estos fe por comuna
  comfesbs = emp_2 %>% group_by(empcom) %>%
    filter(row_number()==1)
  
  sum_fe_empcombs = sum(comfesbs$fe_empcombs)
  
  
  # Aqui se genera la variable de empleo bootstrapeada a nivel SIT para 2012
  emp_2 <- emp_2 %>% mutate(emp = e2bs* (fe_empcom/sum_fe_empcom) * (empsitbs/empcombs))
  
  
  
  
  #-----------------------------------------
  #   4. Aqui se calcula TA Private 2012
  #-----------------------------------------
  
  # Cleaning a little bit the SIT zones
  sits_complete <- sit_zones
  sits <- sit_zones[order(sit_zones$Name, -sit_zones$Shape_Area), ]
  sits <- sits[!duplicated(sits$Name), ]
  
  sits_order <- data.frame(sits$Name,sits$matrix_order)
  colnames(sits_order) <- c("SIT","matrix_order")
  
  
  # Filtering the SIT zones for which I have employment
  sits_order <- sits_order[(sits_order$SIT %in% unique(emp_2$k)), ] 
  
  
  ## Filtering times
  colnames(travel_times) <- sit_zones$Name
  travel_times <- travel_times[c(sits_order$matrix_order)]
  travel_times$sit <- sit_zones$Name
  travel_times <- travel_times[c(sits_order$matrix_order),]
  
  
  print("Reshaping data wide to long")
  ## Travel times from wide to long
  travel_times_long <- melt(travel_times)   
  colnames(travel_times_long) <- c('sito','sitd','time')
  
  
  ## Filtering distance matrix
  travel_distances <- travel_distances[c(sits_order$matrix_order)]
  travel_distances <- travel_distances[c(sits_order$matrix_order), ]
  colnames(travel_distances) <- sits_order$SIT
  travel_distances$sit <- sits_order$SIT
  
  
  # Distances from wide to long
  travel_dist_long <- melt(travel_distances)   
  colnames(travel_dist_long) <- c('sito','sitd','dist')
  
  
  print("Merging data sets") 
  ## Merging distances, costs and times
  colnames(costs) <- c("k","wmin")
  times_distances <- merge(travel_times_long, costs, by.x = 'sitd', by.y = 'k', all.x = T)
  times_distances <- merge(times_distances, travel_dist_long, 
                           by = c('sitd','sito'), all.x = T)
  
  
  print("Computing cost") 
  ## Assigning costs
  #times_distances$transpcost <- (fix_cost + (variable_cost * times_distances$dist))*adjustment
  times_distances = merge(times_distances, transport_cost, by = c("sito", "sitd"), all.x = TRUE)
  
  ## Opportunity cost 
  times_distances$opcost <- (((times_distances$time*times_distances$wmin) + times_distances$transpcost))*ipc
  
  
  ## merge with emploment
  times_distances <- merge(times_distances, emp_2, by.x = 'sitd', by.y = 'k', all.x = T)
  
  
  print("Computing TA") 
  
  
  ## Accessibility measure
  times_distances$access <- times_distances$emp/times_distances$opcost
  times_distances <- times_distances[!is.infinite(times_distances$access),]
  
  
  print("Collapsing data") 
  
  
  ## Info at sit level
  ta_info <- times_distances %>% group_by(sito) %>%
    summarise(ta = sum(access, na.rm = T))
  
  
  ta_info$ta_percap = ta_info$ta/nrow(ta_info)
  ta_info$e = e2bs
  
  
  # La base final tiene el la medida por SIT y la medida per capita por SIT
  colnames(ta_info) <- c('k',varname, percap_varname, "empbs")
  ta_private_2012 <- ta_info
  ta_private_2012 <- ta_private_2012 %>% dplyr::select(-c(empbs))
  
  
  
  
  
  #----------------------------------------------------------------
  #   5. Aqui se calcula TA Private 2017 con empleo bootstrapeado
  #----------------------------------------------------------------
  
  ### Loading data with the distances I want to compute the measure. 
  transport_cost = read_csv("T:/Spatial Mismatch/Base/Transport_costs.csv")
  
  #Keeping the vars I need for public 2017
  transport_cost = subset(transport_cost, select = c(sito, sitd, cost_2017_private))
  
  transport_cost = transport_cost %>%
    rename(transpcost = cost_2017_private)
  
  ##Variables in transport cost as characters
  transport_cost$sitd = as.character(transport_cost$sitd)
  transport_cost$sito = as.character(transport_cost$sito)
  
  
  # Parametros para calcular medida
  fix_cost = 2353.72062285056
  variable_cost = 400.664787029461
  adjustment = 1
  ipc = 1
  year = 2017
  varname <- "tapriv2017"
  percap_varname <- "tapriv2017_percap"
  
  
  # Volvemos a abrir bases porque hay unas especificas para 2017
  
  ## ----- Loading SIT map
  sit_zones = read_sf("T:/Spatial Mismatch/Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") # Maps for 2017
  sit_zones$matrix_order = c(1:nrow(sit_zones))
  
  ## ----- Employment
  emp = read_csv("T:/Spatial Mismatch/Base/emp2017.csv")
  
  ## ----- Going to try to get some comuna ids from this
  comunaids = emp %>% group_by(k) %>%
    filter(row_number()==1)
  
  comunaids = comunaids[,c("k","empcom")]
  comunaids = rename(comunaids, comid = empcom)
  
  ## ----- Travel times
  travel_times = read_csv("T:/Spatial Mismatch/Base/bing_travel_duration_full.csv")
  travel_times = travel_times/60 #Para que quede en minutos
  
  ## ----- Distance by roads
  travel_distances = read_csv("T:/Spatial Mismatch/Base/bing_distances_matrix.csv")
  
  
  ## ----- Individual employment
  ind_emp = read_csv("T:/Spatial Mismatch/Base/empleo_individual_2017.csv")
  
  ## Merge the comuna ids to this
  ind_emp = merge(ind_emp,comunaids, by = "k")
  
  
  ## costs (Average wage)
  costs = read_csv("T:/Spatial Mismatch/Base/costs_wmin.csv")
  costs = costs[c("k","wmin2017")]
  
  
  ## Distance to the borders
  d = read_csv("T:/Spatial Mismatch/Base/mean_distance_border.csv")
  
  div = travel_distances/travel_times
  div[is.na(div)] = 0
  div[!is.finite(as.matrix(div))] = 0
  
  prom = mean(as.matrix(div))
  
  d$time = (d$distance/1000)/prom
  d$time2 = ifelse(d$time==0,mean(d$time),d$time)
  
  for (i in 1:nrow(sit_zones)){
    travel_times[i,i] = d[i,3:3]
  }
  
  variables_emp <- emp %>% dplyr::select(k, factor_expansion, empcom, fe_empcom, sum_fe_empcom)
  
  
  #--------------------------
  #    Calculo de medida
  #--------------------------
  
  
  # calulamos empleo del bootsrap
  counts <- table(ind_emp$k)
  counts <- as.data.frame(counts)
  names(counts) <- c("k", "empsitbs")
  
  countscom <- table(ind_emp$comid)
  countscom <- as.data.frame(countscom)
  names(countscom) <-c("comid","empcombs")
  
  
  # Pegamos el empleo bootstrapeado
  emp_2 <- merge(counts, variables_emp, by = "k")
  emp_2 <- merge(emp_2, countscom, by.x = "empcom", by.y="comid")
  
  emp_2 <- emp_2 %>% mutate(fe_empcombs = empcombs * factor_expansion)
  
  
  # Sumar estos fe por comuna
  comfesbs = emp_2 %>% group_by(empcom) %>% filter(row_number()==1)
  sum_fe_empcombs = sum(comfesbs$fe_empcombs)
  
  emp_2 <- emp_2 %>% mutate(emp = e2bs_17* (fe_empcom/sum_fe_empcom) * (empsitbs/empcombs))
  
  emp_2 <- emp_2 %>% mutate(emp_2_17 = emp)
  
  sits_complete <- sit_zones
  sits <- sit_zones[order(sit_zones$Name, -sit_zones$Shape_Area), ]
  sits <- sits[!duplicated(sits$Name), ]
  
  
  sits_order <- data.frame(sits$Name,sits$matrix_order)
  colnames(sits_order) <- c("SIT","matrix_order")
  
  
  # Filtering the SIT zones for which I have employment
  sits_order <- sits_order[(sits_order$SIT %in% unique(emp_2$k)), ] 
  
  
  ## Filtering times
  colnames(travel_times) <- sit_zones$Name
  travel_times <- travel_times[c(sits_order$matrix_order)]
  travel_times$sit <- sit_zones$Name
  travel_times <- travel_times[c(sits_order$matrix_order),]
  
  
  print("Reshaping data wide to long") 
  ## Travel times from wide to long
  travel_times_long <- melt(travel_times)   
  colnames(travel_times_long) <- c('sito','sitd','time')
  
  
  ## Filtering distance matrix
  travel_distances <- travel_distances[c(sits_order$matrix_order)]
  travel_distances <- travel_distances[c(sits_order$matrix_order), ]
  colnames(travel_distances) <- sits_order$SIT
  travel_distances$sit <- sits_order$SIT
  
  
  # Distances from wide to long
  travel_dist_long <- melt(travel_distances)   
  colnames(travel_dist_long) <- c('sito','sitd','dist')
  
  
  print("Merging data sets") 
  ## Merging distances, costs and times
  colnames(costs) <- c("k","wmin")
  times_distances <- merge(travel_times_long, costs, by.x = 'sitd', by.y = 'k', all.x = T)
  times_distances <- merge(times_distances, travel_dist_long, 
                           by = c('sitd','sito'), all.x = T)
  
  
  
  print("Computing cost") 
  ## Assigning costs
  #times_distances$transpcost <- (fix_cost + (variable_cost * times_distances$dist))*adjustment
  times_distances = merge(times_distances, transport_cost, by = c("sito", "sitd"), all.x = TRUE)
  
  
  ## Opportunity cost 
  times_distances$opcost <- (((times_distances$time*times_distances$wmin) + times_distances$transpcost))*ipc
  
  
  ## merge with emploment
  times_distances <- merge(times_distances, emp_2, by.x = 'sitd', by.y = 'k', all.x = T)
  
  
  print("Computing TA") 
  
  
  ## Accessibility measure
  times_distances$access <- times_distances$emp_2_17/times_distances$opcost
  times_distances <- times_distances[!is.infinite(times_distances$access),]
  
  
  print("Collapsing data") 
  
  
  ## Info at sit level
  ta_info <- times_distances %>% group_by(sito) %>%
    summarise(ta = sum(access, na.rm = T))
  
  ta_info$ta_percap = ta_info$ta/nrow(ta_info)
  
  
  # La base final tiene el la medida por SIT y la medida per capita por SIT
  colnames(ta_info) <- c('k', varname, percap_varname)
  ta_private_2017 <- ta_info
  
  
  
  
  
  #----------------------------------------------------------------
  #   6. Se guardan en vectores los resultados de cada medida
  #----------------------------------------------------------------
  ta_priv_12[iter] <- mean(ta_private_2012$tapriv2012)
  ta_priv_pc_12[iter] <- mean(ta_private_2012$tapriv2012_percap)
  
  ta_priv_17[iter] <- mean(ta_private_2017$tapriv2017)
  ta_priv_pc_17[iter] <- mean(ta_private_2017$tapriv2017_percap)
  
  print(paste0(iter))
  
  
}





#---------------------------------------------------------------------------
#   7. Se crea una base de datos con las medias de medidas bootstrapeadas
#---------------------------------------------------------------------------

results <- data.frame(ta_priv_12, ta_priv_pc_12, ta_priv_17, ta_priv_pc_17)
names(results) <- c("mean_2012", "mean_adj_2012", "mean_2017", "mean_adj_2017")

# guardamos la base de datos
write.csv(results, 
          "//bmdgiesan/DGIESAN/PROYECTOS/DASPERI/DatosInvestigación/Spatial Mismatch/Base/bootstrap_dependiente_TA_Private_remuestreo_parejas_2025.csv")






#---------------------------------------------------------------------------
#   8. Se genera csv con outputs finales para hacer tex de tabla
#---------------------------------------------------------------------------

# abrimos csv con resultados
results_final <- fread("//bmdgiesan/DGIESAN/PROYECTOS/DASPERI/DatosInvestigación/Spatial Mismatch/Base/bootstrap_dependiente_TA_Private_remuestreo_parejas_2025.csv")
results_final <- results_final %>% dplyr::select(-V1)

# generamos diferencia de medida ajustada y cambio porcentual de medida ajustada
results_final <- results_final %>% mutate(diff_private_adj = mean_adj_2017 - mean_adj_2012)
results_final <- results_final %>% mutate(pct_diff_private_adj = 100*(mean_adj_2017 - mean_adj_2012)/mean_adj_2012)

# generamos intervalos de confianza de medida ajustada en 2012, medida ajustada en 2017 y cambio porcentual de medida ajustada
results_final <- results_final %>% mutate(lower_ci_mean_adj_2012 = quantile(mean_adj_2012, 0.025),
                                          upper_ci_mean_adj_2012 = quantile(mean_adj_2012, 0.975),
                                          lower_ci_mean_adj_2017 = quantile(mean_adj_2017, 0.025),
                                          upper_ci_mean_adj_2017 = quantile(mean_adj_2017, 0.975),
                                          lower_ci_pct_change = quantile(pct_diff_private_adj, 0.025),
                                          upper_ci_pct_change = quantile(pct_diff_private_adj, 0.975))

# generamos medias para tenerlas ahi
results_final <- results_final %>% mutate(mean_priv_2012 = mean(mean_2012), mean_priv_2017 = mean(mean_2017),
                                          mean_priv_2012_adj = mean(mean_adj_2012), mean_priv_2017_adj = mean(mean_adj_2017),
                                          mean_diff = mean(diff_private_adj), mean_diff_pct = mean(pct_diff_private_adj))


# variables de interes
results_final <- results_final %>% dplyr::select(mean_priv_2012, mean_priv_2017, mean_priv_2012_adj, mean_priv_2017_adj, mean_diff, mean_diff_pct,
                                                 lower_ci_mean_adj_2012, upper_ci_mean_adj_2012, lower_ci_mean_adj_2017, upper_ci_mean_adj_2017,
                                                 lower_ci_pct_change, upper_ci_pct_change)

# duplicates drop
results_final <- results_final %>% distinct(mean_priv_2012, .keep_all = TRUE)


# guardamos csv para tex
write.csv(results_final, 
          "//bmdgiesan/DGIESAN/PROYECTOS/DASPERI/DatosInvestigación/Spatial Mismatch/Base/bootstrap_dependiente_TA_Private_for_tab_remuestreo_parejas_2025.csv")


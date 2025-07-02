### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code computes travel times for 2012 using backtracking

#Required libraries


# These libraries help you to read and transform data
library(readr)
library(dplyr)

# Library to work with spatial data
library(rgdal) # Main library to read spatial data



## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#               1. Loading and Organizing                     #  
#                  Data                                       #
#-------------------------------------------------------------#

sit_zones = readOGR("Data/Shapefiles/SITs_2017_cut/SITs_2017.shp") 

# Ids from each SIT
sits_order = data.frame(sit_zones$Name,sit_zones$mtrx_rd)
colnames(sits_order) = c("SIT","matrix_order")

# Differences in travel times by SITs
diff_sits = read_csv("Base/diff_mean_sits_medio_tte_origen_destino.csv")

# Differences in travel times by communes
mean_diff = read_csv("Base/diff_means_commune.csv")
mean_diff$per_diff_mean = round(mean_diff$diff_mean/mean_diff$mean_2012,4)

## Average differences by mode
mean_medios = mean_diff %>%
  group_by(Medio_reg) %>% 
  dplyr::summarise(mean_2017 = mean(mean_2017_mode, na.rm = T), 
                   mean_2012 = mean(mean_2012_mode, na.rm = T))

## Difference % by mode
 mean_medios$per_diff_mean = round((mean_medios$mean_2017-mean_medios$mean_2012)/mean_medios$mean_2012, 4)

## transforming these defferences to scalars
 mean_diff_pri = mean_medios[mean_medios$Medio_reg == "Privado", ]$per_diff_mean
 mean_diff_pub = mean_medios[mean_medios$Medio_reg == "Publico", ]$per_diff_mean
 mean_diff_pie = mean_medios[mean_medios$Medio_reg == "A pie", ]$per_diff_mean


##Loading homologation
homol_sit = read_csv("Base/homol_sit_v1.csv")
homol_sit = homol_sit[homol_sit$IdMunicipiod == "10",] # Just keeping Medellin
homol_sit$IdSitd = gsub("([ABCDEFGHIJKL])", "", homol_sit$IdSitd)
homol_sit = subset(homol_sit, !is.na(homol_sit$SIT_D)) # Keeping the ones without NA

## communes OD 2017 at origin
eod_2017 = read_csv("Base/times_eod_2017_filter.csv")
comunas1 = subset(eod_2017, select = c(ID_COMUNA_O, SIT_O)) #Communes
comunas1 = comunas1[!duplicated(comunas1), ] # I just need one combination by commune and SIT
colnames(comunas1) = c("ID_COMUNA", "SIT")

## communes OD 2017 at destination
comunas2 = subset(eod_2017, select = c(ID_COMUNA_D, SIT_D))
comunas2 = comunas2[!duplicated(comunas2), ]
colnames(comunas2) = c("ID_COMUNA", "SIT")

## Appending communes
comunas = rbind(comunas1, comunas2)
comunas = comunas[!duplicated(comunas), ] # I just need one combination by commune and SIT


## Merging homologation and SITs order IDs
sits_order = merge(sits_order, homol_sit, by.x = c("SIT"), by.y = c("SIT_D"), all.x = T)

rm(homol_sit, sit_zones, comunas1, comunas2, eod_2017)

## Merging with the SITs and commune IDs
sits_order = merge(sits_order, comunas, by = "SIT", all.x = T)
sits_order$ID_COMUNA = ifelse(is.na(sits_order$ID_COMUNA), sits_order$C_COMUNA, sits_order$ID_COMUNA)
sits_order$ID_COMUNA = ifelse(sits_order$ID_COMUNA == 90, 47, sits_order$ID_COMUNA)
sits_order$ID_COMUNA = ifelse(sits_order$ID_COMUNA == 80, 48, sits_order$ID_COMUNA)
sits_order$ID_COMUNA = ifelse(sits_order$ID_COMUNA == 70, 46, sits_order$ID_COMUNA)
sits_order = subset(sits_order, select = -c(C_COMUNA))


#-------------------------------------------------------------#
#               2. Matrix differences for                     #  
#                  Public transport                           #
#-------------------------------------------------------------#


mat = matrix(nrow = 544, ncol = 544) # Empty matrix of 544x544
datatemp = diff_sits[diff_sits$Medio_tte == "Publico", ] # Public travel times
datatemp = datatemp[!is.na(datatemp$diff_mean), ]

travel_times = read_csv("Base/public_travel_duration_full.csv") # These times come from Google

for (i in c(1:nrow(travel_times))){
  for (j in c(1:nrow(travel_times))){
    
    ## id de sit para match con matriz
    origen = sits_order[which(sits_order$matrix_order == i),]$IdSitd
    destino = sits_order[which(sits_order$matrix_order == j),]$IdSitd
    comuna = sits_order[which(sits_order$matrix_order == i),]$ID_COMUNA
    diff_com = mean_diff[which((mean_diff$Medio_reg == "Publico") & 
                                  (mean_diff$ID_COMUNA_O == comuna)), ]$per_diff_mean
    diff_prom = ifelse(is.na(diff_com), mean_diff_pub, diff_com)
    
    if (i==j) {
      diff = 0
      mat[i,j] = diff
    } else {
      temp = datatemp[which(datatemp$IdSito == origen),]
      temp = temp[which(temp$IdSitd == destino),]
      pertime = as.numeric(travel_times[i,j]*diff_prom)
      diff = ifelse(nrow(temp)==0, pertime, temp$diff_mean)
      mat[i,j] = diff
    }
    print(paste("posicion",i,"-",j))
  }
}

write.csv(mat, row.names = FALSE, file = "Base/diff_matrix_publico.csv")
rm(datatemp, temp, destino, diff, i, j, origen, pertime)


mat = read_csv("Base/diff_matrix_publico.csv")
times = travel_times-mat
length(times[times < 0])

negatives = times
negatives[negatives>=0] = 0
negatives[negatives<0] = 1
negatives = (travel_times/60)*negatives*(1-diff_prom)

length(negatives[!is.na(negatives)]) 
#length(negatives[negatives = 0]) 


temp_matrix = times
temp_matrix[temp_matrix>=0] = 1
temp_matrix[temp_matrix<0] = 0
length(temp_matrix[temp_matrix<1]) 

times = times * temp_matrix
times = times + negatives
length(times[times<0])

write.csv(times, row.names = FALSE,  file = "Base/travel_times_public_2012.csv", na = "")


#-------------------------------------------------------------#
#               3. Matrix differences for                     #  
#                  Private transport                          #
#-------------------------------------------------------------#

travel_times = read_csv("Base/bing_travel_duration_full.csv", col_types = cols(X1 = col_skip()))

# Empty matrix
mat = matrix(nrow = 544, ncol = 544)
datatemp = diff_sits[diff_sits$Medio_tte == "Privado", ]
datatemp = datatemp[!is.na(datatemp$diff_mean), ]

for (i in c(1:nrow(travel_times))){
  for (j in c(1:nrow(travel_times))){
    
    ## ids SITs for the matrix
    origen = sits_order[which(sits_order$matrix_order == i),]$IdSitd
    destino = sits_order[which(sits_order$matrix_order == j),]$IdSitd
    diff_com = mean_diff[which((mean_diff$Medio_reg == "Privado") & 
                                  (mean_diff$ID_COMUNA_O == comuna)), ]$per_diff_mean
    diff_prom = ifelse(is.na(diff_com), mean_diff_pri, diff_com)
    
    if (i==j) {
      diff = 0
      mat[i,j] = diff
    } else {
      temp = datatemp[which(datatemp$IdSito == origen),]
      temp = temp[which(temp$IdSitd == destino),]
      pertime = as.numeric(travel_times[i,j]/60*diff_prom)
      diff = ifelse(nrow(temp)==0, pertime, temp$diff_mean)
      mat[i,j] = diff
    }
    print(paste("posicion",i,"-",j))
  }
}

write.csv(mat, row.names = FALSE,  file = "Base/diff_matrix_privado.csv")
rm(datatemp, temp, destino, diff, i, j, origen, pertime)
length(mat[mat<0])

mat = read_csv("Base/diff_matrix_privado.csv", col_types = cols(X1 = col_skip()))
times = (travel_times/60)-mat
length(times[times<0]) 



negatives = times
negatives[negatives>=0] = 0
negatives[negatives<0] = 1
negatives = (travel_times/60)*negatives*(1-diff_prom)

length(negatives[!is.na(negatives)]) 
length(negatives[negatives>0]) 


temp_matrix = times
temp_matrix[temp_matrix>=0] = 1
temp_matrix[temp_matrix<0] = 0
length(temp_matrix[temp_matrix<1]) 

times = times * temp_matrix
times = times + negatives
length(times[times<0])

write.csv(times, row.names = FALSE, file = "Base/travel_times_private_2012.csv", na = "")




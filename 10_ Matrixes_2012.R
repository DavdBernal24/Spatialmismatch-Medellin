###### Created by : David Bernal
##### Last modification : 
##### Modified by: 


#### I Backtrack travel times from 2017 to 2012 using the formula from the paper. 

## Required libraries

library(rgdal)
library(dplyr)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

#------------------------------------------------------------------

## Loading SIT map
sit_zones <- readOGR("Data/shapessits/SITzones2017.shp", layer = "SITzones2017")
sit_zones$matrix_order <- c(1:nrow(sit_zones))

# Dropping duplicates
sit_zones <- sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),]
sit_zones <- sit_zones[!duplicated(sit_zones@data[c("Name")]),] # de 544 quedan 535

# Ids for each SIT
sits_order <- data.frame(sit_zones$Name,sit_zones$matrix_order)
colnames(sits_order) <- c("SIT","matrix_order")

# Travel times differences between sits
diff_sits <- read_csv("Base/diff_mean_sits_medio_tte_origen_destino.csv")
#diff_sits <- diff_sits[diff_sits$Medio_tte == "Publico", ]
diff_sits <- diff_sits[diff_sits$IdMunicipioo == "10", ]
diff_sits <- diff_sits[diff_sits$IdMunicipiod == "10", ]

# Loading homologation(See code 1)
homol_sit <- read_csv("Base/homol_sit_v1.csv", col_types = cols(X1 = col_skip()))
homol_sit <- homol_sit[homol_sit$IdMunicipiod == "10",]
homol_sit$IdSitd <- gsub("([ABCDEFGHIJKL])", "", homol_sit$IdSitd)

## merge order with homologation
sits_order <- merge(sits_order, homol_sit, by.x = c("SIT"), by.y = c("SIT_D"), all.x = T)
rm(homol_sit, sit_zones)

### ----------------------------------------------------------------------
### PUBLIC TRANSPORT MATRIX

# Empty matrix
mat <- matrix(nrow = 544, ncol = 544)
datatemp <- diff_sits[diff_sits$Medio_tte == "Publico", ]
datatemp <- datatemp[!is.na(datatemp$diff_mean), ]

travel_times <- read_csv("Base/public_travel_duration_full.csv", col_types = cols(X1 = col_skip()))

for (i in c(1:nrow(travel_times))){
  for (j in c(1:nrow(travel_times))){
    
    ## id from each SIT to match with the matrix
    origen <- sits_order[which(sits_order$matrix_order == i),]$IdSitd
    destino <- sits_order[which(sits_order$matrix_order == j),]$IdSitd
    diff_prom <- 0.14
    
    if (i==j) {
      diff <- 0
      mat[i,j] = diff
    } else {
      temp <- datatemp[which(datatemp$IdSito == origen),]
      temp <- temp[which(temp$IdSitd == destino),]
      pertime <- as.numeric(travel_times[i,j]*diff_prom)
      diff <- ifelse(nrow(temp)==0, pertime, temp$diff_mean)
      mat[i,j] = diff
    }
    print(paste("posicion",i,"-",j))
  }
}

write.csv(mat, file = "Base/diff_matrix_publico.csv")
rm(datatemp, temp, destino, diff, i, j, origen, pertime)
length(mat[mat<0])

mat <- read_csv("Base/diff_matrix_publico.csv", col_types = cols(X1 = col_skip()))
times <- travel_times-mat
length(times[times<0]) #73



negatives <- times
negatives[negatives>=0] <- 0
negatives[negatives<0] <- 1
negatives <- (travel_times/60)*negatives*(1-diff_prom)
#negatives[negatives==0] <- NA
#negatives <- negatives-diff_prom
length(negatives[!is.na(negatives)]) #478
length(negatives[negatives>0]) #679


temp_matrix <- times
temp_matrix[temp_matrix>=0] <- 1
temp_matrix[temp_matrix<0] <- 0
length(temp_matrix[temp_matrix<1]) #679

times <- times * temp_matrix
times <- times + negatives
length(times[times<0])

write.csv(times, file = "Base/travel_times_public_2012.csv")




### ----------------------------------------------------------------------
### Differences for private transport


travel_times <- read_csv("Base/bing_travel_duration_full.csv", col_types = cols(X1 = col_skip()))

# Empty matrix
mat <- matrix(nrow = 544, ncol = 544)
datatemp <- diff_sits[diff_sits$Medio_tte == "Privado", ]
datatemp <- datatemp[!is.na(datatemp$diff_mean), ]

for (i in c(1:nrow(travel_times))){
  for (j in c(1:nrow(travel_times))){
    
    ## Ids to match with the matrix
    origen <- sits_order[which(sits_order$matrix_order == i),]$IdSitd
    destino <- sits_order[which(sits_order$matrix_order == j),]$IdSitd
    diff_prom <- 0.17
    
    if (i==j) {
      diff <- 0
      mat[i,j] = diff
    } else {
      temp <- datatemp[which(datatemp$IdSito == origen),]
      temp <- temp[which(temp$IdSitd == destino),]
      pertime <- as.numeric(travel_times[i,j]/60*diff_prom)
      diff <- ifelse(nrow(temp)==0, pertime, temp$diff_mean)
      mat[i,j] = diff
    }
    print(paste("posicion",i,"-",j))
  }
}

write.csv(mat, file = "Base/diff_matrix_privado.csv")
rm(datatemp, temp, destino, diff, i, j, origen, pertime)
length(mat[mat<0])

mat <- read_csv("Base/diff_matrix_privado.csv", col_types = cols(X1 = col_skip()))
times <- (travel_times/60)-mat
length(times[times<0]) #365



negatives <- times
negatives[negatives>=0] <- 0
negatives[negatives<0] <- 1
negatives <- (travel_times/60)*negatives*(1-diff_prom)
#negatives[negatives==0] <- NA
#negatives <- negatives-diff_prom
length(negatives[!is.na(negatives)]) #365
length(negatives[negatives>0]) #365


temp_matrix <- times
temp_matrix[temp_matrix>=0] <- 1
temp_matrix[temp_matrix<0] <- 0
length(temp_matrix[temp_matrix<1]) #365

times <- times * temp_matrix
times <- times + negatives
length(times[times<0])

write.csv(times, file = "Base/travel_times_private_2012.csv")








### ----------------------------------------------------------------------
### Differences for walking

travel_times <- read_csv("Base/pie_travel_duration_full.csv", col_types = cols(X1 = col_skip()))

# Empty matrix
mat <- matrix(nrow = 544, ncol = 544)
datatemp <- diff_sits[diff_sits$Medio_tte == "A pie", ]
datatemp <- datatemp[!is.na(datatemp$diff_mean), ]

for (i in c(1:nrow(travel_times))){
  for (j in c(1:nrow(travel_times))){
    
    ## Ids to match with the matrix
    origen <- sits_order[which(sits_order$matrix_order == i),]$IdSitd
    destino <- sits_order[which(sits_order$matrix_order == j),]$IdSitd
    diff_prom <- 0.20
    
    if (i==j) {
      diff <- 0
      mat[i,j] = diff
    } else {
      temp <- datatemp[which(datatemp$IdSito == origen),]
      temp <- temp[which(temp$IdSitd == destino),]
      pertime <- as.numeric(travel_times[i,j]*diff_prom)
      diff <- ifelse(nrow(temp)==0, pertime, temp$diff_mean)
      mat[i,j] = diff
    }
    print(paste("posicion",i,"-",j))
  }
}

write.csv(mat, file = "Base/diff_matrix_pie.csv")
rm(datatemp, temp, destino, diff, i, j, origen, pertime)
length(mat[mat<0])

mat <- read_csv("Base/diff_matrix_pie.csv", col_types = cols(X1 = col_skip()))
times <- (travel_times)-mat
length(times[times<0]) #46



negatives <- times
negatives[negatives>=0] <- 0
negatives[negatives<0] <- 1
negatives <- (travel_times/60)*negatives*(1-diff_prom)
#negatives[negatives==0] <- NA
#negatives <- negatives-diff_prom
length(negatives[!is.na(negatives)]) #
length(negatives[negatives>0]) #46


temp_matrix <- times
temp_matrix[temp_matrix>=0] <- 1
temp_matrix[temp_matrix<0] <- 0
length(temp_matrix[temp_matrix<1]) #46

times <- times * temp_matrix
times <- times + negatives
length(times[times<0])



## Saving the new matrix
#write.csv(times, file = "pta/travel_times_pie_2012.csv")
write.csv(times, file = "Base/travel_times_pie_2012_nd.csv")

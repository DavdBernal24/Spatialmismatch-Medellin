### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


### This code filters travel times data for the 2012 EOD





# These libraries help you to read and transform data
library(dplyr)
library(readxl)
library(readr)
library(rstatix)
library(stringr) # detect specific strings


# Library to work with spatial data
library(rgdal)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#               1.Categorizing transport modes                #  
#                                                             #
#-------------------------------------------------------------#

EOD2012 = read_excel("Data/EOD_2012.xlsx")

## I will create a variables with all the modes used in different stages

EOD2012$Medios = paste(EOD2012$Etapa1MT, EOD2012$Etapa2MT, EOD2012$Etapa3MT,
                       EOD2012$Etapa4MT, EOD2012$Etapa5MT, EOD2012$Etapa6MT,
                       EOD2012$Etapa7MT, sep = "-") 

## Setting transportation mode "Walking" if there is not mode in stage 2 and stage 1 is walking
EOD2012$MedioTTE = ifelse(is.na(EOD2012$Etapa2MT), EOD2012$Etapa1MT, 
                          ifelse(EOD2012$Etapa1MT == 0, EOD2012$Etapa2MT,
                                 EOD2012$Etapa1MT))


## if in any stage the transportation mode is Metro system, I keep it over the others
EOD2012$MedioTTE2 = ifelse(grepl("3", EOD2012$Medios), 3, EOD2012$MedioTTE)

## Creating a dummy variable for trips in public transport with more
# than one mode
EOD2012$Multimode =  sapply(EOD2012$Medios, function(obs) {
  # Extract unique numbers from the string
  nums <- unique(str_extract_all(obs, "\\d")[[1]])
  # Check if at least two of '1', '2', '3', "6", and "50" are present
  sum(c("1", "2", "3", "4", "5", "6", "50") %in% nums) >= 2
}) * 1  # Convert logical to numeric (1/0)


EOD2012$Number_modes =  sapply(EOD2012$Medios, function(obs) {
  # Extract unique numbers from the string
  nums = unique(str_extract_all(obs, "\\d")[[1]])
  # Check if at least two of '1', '2', '3', "6", and "50" are present
  sum(c("1", "2", "3", "4", "5", "6", "50") %in% nums) 
  
})



## I categorize the modes here
EOD2012$Medio_reg = ifelse(EOD2012$MedioTTE2 == 0, "A pie", 
                           ifelse(EOD2012$MedioTTE2 == 1, "Publico",
                                  ifelse(EOD2012$MedioTTE2 == 2, "Publico",
                                         ifelse(EOD2012$MedioTTE2 == 3, "Publico",
                                                ifelse(EOD2012$MedioTTE2 == 4, "Privado",
                                                       ifelse(EOD2012$MedioTTE2 == 5, "Publico", 
                                                              ifelse(EOD2012$MedioTTE2 == 6, "Publico", 
                                                                     ifelse(EOD2012$MedioTTE2 == 7, "Privado",
                                                                            ifelse(EOD2012$MedioTTE2 == 8, "Privado",
                                                                                   ifelse(EOD2012$MedioTTE2 == 9, "Privado", 
                                                                                          ifelse(EOD2012$MedioTTE2 == 50, "Publico", "Avion"
                                                                                          )))))))))))

## I categorize the modes for every stage 

EOD2012$Medio1 = ifelse(EOD2012$Etapa1MT == 0, "A pie", 
                        ifelse(EOD2012$Etapa1MT == 1, "Publico",
                               ifelse(EOD2012$Etapa1MT == 2, "Publico",
                                      ifelse(EOD2012$Etapa1MT == 3, "Publico",
                                             ifelse(EOD2012$Etapa1MT == 4, "Privado",
                                                    ifelse(EOD2012$Etapa1MT == 5, "Publico", 
                                                           ifelse(EOD2012$Etapa1MT == 6, "Publico", 
                                                                  ifelse(EOD2012$Etapa1MT == 7, "Privado",
                                                                         ifelse(EOD2012$Etapa1MT == 8, "Privado",
                                                                                ifelse(EOD2012$Etapa1MT == 9, "Privado", 
                                                                                       ifelse(EOD2012$Etapa1MT == 50, "Publico", "Avion"
                                                                                       )))))))))))

EOD2012$Medio2 = ifelse(EOD2012$Etapa2MT == 0, "A pie", 
                        ifelse(EOD2012$Etapa2MT == 1, "Publico",
                               ifelse(EOD2012$Etapa2MT == 2, "Publico",
                                      ifelse(EOD2012$Etapa2MT == 3, "Publico",
                                             ifelse(EOD2012$Etapa2MT == 4, "Privado",
                                                    ifelse(EOD2012$Etapa2MT == 5, "Publico", 
                                                           ifelse(EOD2012$Etapa2MT == 6, "Publico", 
                                                                  ifelse(EOD2012$Etapa2MT == 7, "Privado",
                                                                         ifelse(EOD2012$Etapa2MT == 8, "Privado",
                                                                                ifelse(EOD2012$Etapa2MT == 9, "Privado", 
                                                                                       ifelse(EOD2012$Etapa2MT == 50, "Publico", "Avion"
                                                                                       )))))))))))

EOD2012$Medio3 = ifelse(EOD2012$Etapa3MT == 0, "A pie", 
                        ifelse(EOD2012$Etapa3MT == 1, "Publico",
                               ifelse(EOD2012$Etapa3MT == 2, "Publico",
                                      ifelse(EOD2012$Etapa3MT == 3, "Publico",
                                             ifelse(EOD2012$Etapa3MT == 4, "Privado",
                                                    ifelse(EOD2012$Etapa3MT == 5, "Publico", 
                                                           ifelse(EOD2012$Etapa3MT == 6, "Publico", 
                                                                  ifelse(EOD2012$Etapa3MT == 7, "Privado",
                                                                         ifelse(EOD2012$Etapa3MT == 8, "Privado",
                                                                                ifelse(EOD2012$Etapa3MT == 9, "Privado", 
                                                                                       ifelse(EOD2012$Etapa3MT == 50, "Publico", "Avion"
                                                                                       )))))))))))


## Group them in a variable
EOD2012$Medios3 = paste(EOD2012$Medio1, 
                        EOD2012$Medio2, 
                        EOD2012$Medio3, 
                        sep = " + ") 

## Dummies for transportation mode
EOD2012$dpie = ifelse(grepl("A pie", EOD2012$Medios3), 1, 0)
EOD2012$dpub = ifelse(grepl("Publico", EOD2012$Medios3), 1, 0)
EOD2012$dpri = ifelse(grepl("Privado", EOD2012$Medios3), 1, 0)

## We keep people who go to work
eodw_2012 = subset(EOD2012, EOD2012$IdMotivoViaje == "1")


#-------------------------------------------------------------#
#               2. Filtering data                             #  
#                                                             #
#-------------------------------------------------------------#

## Keeping the variables I will be using

times_eod_2012 = eodw_2012[c("IdMunicipioo", "IdMunicipiod", 
                             "IdSito", "Horao", 
                             "IdSitd", "Horad", 
                             "MedioTTE2", 
                             "Medios3", "Medio_reg", "dpie","dpub","dpri", "Multimode", 
                             "Number_modes"
)]

## Computing travel times in minutes

times_eod_2012$minutos = as.numeric(difftime(times_eod_2012$Horad, times_eod_2012$Horao, units="mins"))



## Getting all the communes with their IDs and IDs for the SIT zones
temp1 = EOD2012[c("IdMunicipiod","IdComunad","IdSitd")] ## Communes of origin
temp2 = EOD2012[c("IdMunicipioo","IdComunao","IdSito")] ## communes of destination
colnames(temp2) <- c("IdMunicipiod","IdComunad","IdSitd")
comunas = rbind(temp1, temp2)
comunas = comunas[!is.na(comunas$IdComunad),]
comunas = comunas[comunas$IdComunad != "00",]
comunas = comunas[!duplicated(comunas[,c("IdMunicipiod","IdSitd")]),]
rm(temp1, temp2)


## Merging communes and EOD at destination level
times_eod_2012 =  merge(times_eod_2012, comunas, 
                        by.x = c("IdMunicipiod","IdSitd"), 
                        by.y = c("IdMunicipiod","IdSitd"), all.x = T)

## Merging communes and EOD at origin level
colnames(comunas) = c("IdMunicipioo","IdComunao","IdSito")

times_eod_2012 = merge(times_eod_2012, comunas, 
                       by.x = c("IdMunicipioo","IdSito"), 
                       by.y = c("IdMunicipioo","IdSito"), all.x = T)


colnames(comunas) = c("IdMunicipiod","IdComunad","IdSitd")


## Expansion factors for Medellin
factor_exp = EOD2012[EOD2012$IdMunicipioo == "10",]
factor_exp = factor_exp[!is.na(factor_exp$IdComunao),]
factor_exp = factor_exp[c("IdComunao","PrimeroDeFactorExpansion")]
factor_exp = factor_exp[!duplicated(factor_exp[,c("IdComunao")]),]


times_eod_2012 = merge(times_eod_2012, factor_exp, 
                       by.x = c("IdComunao"), by.y = c("IdComunao"), all.x = T)

#Saving it 
write.csv(times_eod_2012, row.names = FALSE, file = "Base/times_eod_2012.csv")


#-------------------------------------------------------------#
#             3. Filtering data for Medellin using the        #
#             shapes equivalence we made on the first code    #
#-------------------------------------------------------------#

## Filtering data for Medellin

# We keep the communes < 61 since the larger IDs are twonships
times_eod_2012 = subset(times_eod_2012, times_eod_2012$IdMunicipiod == "10")
times_eod_2012 = subset(times_eod_2012, times_eod_2012$IdMunicipioo == "10")
times_eod_2012 = times_eod_2012[times_eod_2012$IdComunad <= 61,]


## Now we do what we did in the shapes equivalence to delete duplicates using the SITs' size
# But first we load the shapefile for 2017 (we use it because we have the equivalence between 2012 and 2017)

sit_zones = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017
sit_zones$matrix_order = c(1:nrow(sit_zones))

sit_zones = sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),]
sit_zones = sit_zones[!duplicated(sit_zones@data[c("Name")]),] # we keep 535 from 544

## Homologation file
homol_sit = read_csv("Base/homol_sit_v1.csv")
homol_sit = homol_sit[homol_sit$IdMunicipiod == "10",] ## Remember 10 is from Medellin
homol_sit$IdSitd = gsub("([ABCDEFGHIJKL])", "", homol_sit$IdSitd) ## Some IDs have letters, I delete them


#  Creating a frame with the SIts Name and order
sits_order = data.frame(sit_zones$Name,sit_zones$matrix_order)
colnames(sits_order) = c("SIT","matrix_order")

## merging with the homologation (to keep the ones that are in both years)
sits_order = merge(sits_order, homol_sit, by.x = c("SIT"), by.y = c("SIT_D"), all.x = T)

## Available SITs for 2012 that are in the shapefile and the EOD
match = unique(times_eod_2012$IdSitd) #239

## Now I use the ones I got from the homologation with the EOD 2012
sits_order = sits_order[(sits_order$IdSitd %in% match), ] 
sits_order = sits_order[(!is.na(sits_order$IdSitd)), ]

times_eod = times_eod_2012[(times_eod_2012$IdSitd %in% sits_order$IdSitd),]

##creating an ID variable 
times_eod$ID_temp = c(1:nrow(times_eod))


## Cleaning times by transportation mode and origin

#Note: There are more outliers for public transport than for private transport, I use extreme for private
# and I am more restrictive with public


## Identifying and deleting outliers
times_eod_outliers = times_eod%>% 
  group_by(Medio_reg, IdComunao) %>% 
  identify_outliers(minutos)

outliers_public = times_eod_outliers[times_eod_outliers$Medio_reg == "Publico" &
                                       times_eod_outliers$is.outlier == "TRUE",]

outliers_private = times_eod_outliers[times_eod_outliers$Medio_reg == "Privado" &
                                        times_eod_outliers$is.extreme == "TRUE",]


## Deleting outliers

times_eod = times_eod %>% 
  anti_join(outliers_public, by = "ID_temp") # For public transport

times_eod = times_eod %>% 
  anti_join(outliers_private, by = "ID_temp") # For private transport


times_eod = subset(times_eod, select = -ID_temp) # I don't need this variables


# Saving data
write.csv(times_eod, row.names = FALSE,  file = "Base/times_eod_2012_filter.csv")


#-------------------------------------------------------------#
#            Ratio travel times between 7 and 9am             #
#                                                             #
#-------------------------------------------------------------#

## Data with hours between 7am and 9am to find the radius
times_eod_early = subset(times_eod, format(Horao, "%H") >= "07" & format(Horao, "%H") <= "09")


## Data with hours between 5am and 9am to find the radius
times_eod_early_5 = subset(times_eod, format(Horao, "%H") >= "05" & format(Horao, "%H") <= "09")


## Saving the files for future computations
write.csv(times_eod_early_5, row.names = FALSE, file = "Base/times_eod_2012_filterEarly5.csv")





## For all the eod files I will delete trips to townships and walking
eod_list = list(all = times_eod, early = times_eod_early, very_early = times_eod_early_5)

for (name in names(eod_list)){
  
  data = eod_list[[name]]
  
  ## Dropping walking data and commune 61
  data = data[data$IdComunao < "61" & data$Medio_reg != "A pie", ]
  
  eod_list[[name]] = data
  
}


### Finding ratios of sample

# 7 am vs all
sample_ratio_7_all = nrow(eod_list[["early"]])/nrow(eod_list[["all"]])

#7 am vs 5am 
sample_ratio_7_5 = nrow(eod_list[["early"]])/nrow(eod_list[["very_early"]])

#5am vs all
sample_ratio_5_all = nrow(eod_list[["very_early"]])/nrow(eod_list[["all"]])


### Now let's create the data to compute the time ratios

for(name in names(eod_list)){
  data = eod_list[[name]]
  
  data = data %>% 
    group_by(IdComunao, Medio_reg) %>% 
    summarize(minutes = mean(minutos, na.rm = TRUE)) # I call it minutes early because I am restricting the time
  
  data = data %>% 
    rename(!!paste0("minutes_", name) := minutes)
  
  
  eod_list[[name]] = data
  
  assign(name, data)
  
}


eod_collapsed_2012 = Reduce(function(x, y) merge(x, y, by = c("IdComunao", "Medio_reg"), all = TRUE), eod_list)



### Ratios 
eod_collapsed_2012 = eod_collapsed_2012 %>% 
  mutate(ratio_7_all = minutes_early/minutes_all, 
         ratio_5_all = minutes_very_early/minutes_all, 
         ratio_7_5 = minutes_early/minutes_very_early)


# Difference in means 2012
t_test_result_2012 = t.test(eod_collapsed_2012$minutes_early, eod_collapsed_2012$minutes_very_early, alternative = "two.sided")

print(t_test_result_2012)


## Saving Data
write.csv(eod_collapsed_2012, "Base/Times_early_late_2012.csv", row.names = FALSE)



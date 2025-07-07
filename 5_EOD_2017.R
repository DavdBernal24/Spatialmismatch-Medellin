### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 



### This code computes computes travel times from the EOD for 2017

## Here I also take the share of trips to work to Medellin from other municipalities

#Required libraries


# This code filters travel times data for the 2017 EOD

library(readxl)
library(readr)
library(dplyr)
library(rstatix)



# Library to work with spatial data
library(rgdal)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#               1.Organizing data                             #  
#                                                             #
#-------------------------------------------------------------#


## Expansion factors- We use the ones provided by the RISE

eod_rise = read_excel("Data/factor_rise.xlsx") ## This information was provided by the RISE
eod_rise = subset(eod_rise, !is.na(id_comuna))
colnames(eod_rise)[5] = "factor_expansion"
eod_rise = eod_rise[ , names(eod_rise) %in% c("id_comuna", "factor_expansion")]
eod_rise = na.omit(eod_rise)



## The EOD for 2017 is divided in different files, 
# Here I import them

viajes = read_delim("Data/EOD_2017_DatosViajes.csv", ";", escape_double = FALSE, trim_ws = TRUE)
moradores = read_delim("Data/EOD_2017_DatosMoradores.csv", ";", escape_double = FALSE, trim_ws = TRUE)


## Merging the files (viajes + moradores)

eod2017 = merge(viajes, moradores, by.x = c("ID_HOGAR", "ID_MORADOR"), 
                by.y = c("ID_HOGAR", "ORDEN_MORADOR"), all.x = T)
rm(moradores, viajes)

#-------------------------------------------------------------#
#               2.Categorizing transportation modes           #  
#                                                             #
#-------------------------------------------------------------#


## Loading categorization (I made this file manually!)

categories = read_delim("Data/clasif_medios_grupos.csv", ";", escape_double = FALSE, trim_ws = TRUE)


## Categories for each stage

for (i in 1:7){
  colnames(categories) = assign(paste0("categorie_name", i),
                                c(paste0("MODO_TTE_E", i), paste0("Medio_TTE", i),
                                  paste0("Medio_Desc_TTE", i), 
                                  paste0("Medio_res", i), paste0("Medio_reg", i))) 
  eod2017 = merge(eod2017, categories, 
                  by = c(paste0("MODO_TTE_E", i)),
                  all.x = TRUE)
}

## creating a column with different modes (I take the first 3 stages)

eod2017$Medios3 = paste(eod2017$Medio_reg1, 
                        eod2017$Medio_reg2, 
                        eod2017$Medio_reg3, 
                        sep = " + ") 

## I do the same thing with all the stages

eod2017$Medios = paste(eod2017$Medio_TTE1, eod2017$Medio_TTE2, eod2017$Medio_TTE3,
                       eod2017$Medio_TTE4, eod2017$Medio_TTE5, eod2017$Medio_TTE6,
                       eod2017$Medio_TTE7, sep = "-") 


## This the rule we use to get a final mode: if the second stage exists we take it
# as long as the first one is walking. Otherwise, we take the first one

eod2017$MedioTTE = ifelse(is.na(eod2017$Medio_TTE2), eod2017$Medio_TTE1, 
                          ifelse(eod2017$Medio_TTE1 == 0, eod2017$Medio_TTE2,
                                 eod2017$Medio_TTE1))

## We also use other rule, if in any stage the mode is "Metro", 
# we take it 

eod2017$MedioTTE2 = ifelse(grepl("3", eod2017$Medios), 3, eod2017$MedioTTE)



## Creating a dummy variable for trips in public transport with more
# than one mode
eod2017$Multimode =  sapply(eod2017$Medios, function(obs) {
  # Extract unique numbers from the string
  nums <- unique(str_extract_all(obs, "\\d")[[1]])
  # Check if at least two of '1', '2', '3', "6", and "50" are present
  sum(c("1", "2", "3", "4", "5", "6", "7", "12", "13", "14", "15", 
        "19", "20") %in% nums) >= 2
}) * 1  # Convert logical to numeric (1/0)


eod2017$Number_modes =  sapply(eod2017$Medios, function(obs) {
  # Extract unique numbers from the string
  nums = unique(str_extract_all(obs, "\\d")[[1]])
  # Check if at least two of '1', '2', '3', "6", and "50" are present
  sum(c("1", "2", "3", "4", "5", "6", "7", "12", "13", "14", "15", 
        "19", "20") %in% nums) 
})




## Final classification

colnames(categories) = c("MODO_TTE","MedioTTE2","Medio_TTE","Medio_res","Medio_res2")

cat = data.frame(categories$MedioTTE2, categories$Medio_TTE)
cat = cat[!duplicated(cat),]
colnames(cat) <- c("MedioTTE2","Medio_TTE")
eod2017 <- merge(eod2017, cat, 
                 by = c("MedioTTE2"),
                 all.x = TRUE)
rm(cat)

## Doing the same thing but with text

colnames(categories) = c("MODO_TTE","var","Medio_Desc_TTE","Medio_res","Medio_reg")
categories = categories[,c("var","Medio_reg")]
categories = categories[!duplicated(categories),]

eod2017 = merge(eod2017, categories, 
                by.x = c("MedioTTE2"), by.y = c("var"),
                all.x = TRUE)

## Dummies for the modes
eod2017$dpie <- ifelse(grepl("A pie", eod2017$Medios3), 1, 0)
eod2017$dpub <- ifelse(grepl("Publico", eod2017$Medios3), 1, 0)
eod2017$dpri <- ifelse(grepl("Privado", eod2017$Medios3), 1, 0)




#-------------------------------------------------------------#
#               3.Filtering Data                              #  
#                                                             #
#-------------------------------------------------------------#

## We keep people that go to work

eodw_2017 = subset(eod2017, eod2017$DESC_MOTIVO_VIAJE == "Al Trabajo")

## Adding IDs for the expansion factors (Notice we only keep the IDs and add the factors later)

eodw_2017 = merge(eodw_2017, eod_rise, by.x = c("ID_COMUNA_O"), 
                  by.y = c("id_comuna"), all.x = T)


eodw_2017$factor_expansion = as.numeric(eodw_2017$factor_expansion)


## Variables we keep from the survey
times_eod_2017 = eodw_2017[c("ID_MORADOR","ID_MUNICIPIO_O", "ID_MUNICIPIO_D", 
                             "SIT_O", "HORA_O", 
                             "SIT_D", "HORA_D", 
                             "MedioTTE2","Medio_TTE", 
                             "Medios3", "Medio_reg", "dpie","dpub","dpri",
                             "OCUPACION","DESC_OCUPACION",
                             "NIVEL_LABORAL","DESC_NIVEL_LABORAL",
                             "DEDICACION_LABORAL","DESC_DEDICACION_LABORAL", "Multimode", "Number_modes")]

## Organizing format of the dates

times_eod_2017$time1 = paste("2019-01-01",times_eod_2017$HORA_O)
times_eod_2017$time2 = paste("2019-01-01",times_eod_2017$HORA_D)


## Computing travel times in minutes
times_eod_2017$minutos = as.numeric(difftime(times_eod_2017$time2, times_eod_2017$time1, units="mins"))


## Communes & SITs
comunas = read_csv("Data/comunas.csv") ## I made this file manually

comunas = comunas[comunas$ID_COMUNA_D != "-",]
comunas = comunas[!duplicated(comunas[,c("ID_MUNICIPIO_D","SIT_D")]),]
comunas = comunas[!duplicated(comunas[,c("SIT_D")]),]

## Merging communes and EOD
times_eod_2017 = merge(times_eod_2017, comunas, 
                       by.x = c("ID_MUNICIPIO_D","SIT_D"), 
                       by.y = c("ID_MUNICIPIO_D","SIT_D"), all.x = T)


## Doing the same for origins
colnames(comunas) = c("ID_MUNICIPIO_O","ID_COMUNA_O","SIT_O")

times_eod_2017 = merge(times_eod_2017, comunas, 
                       by.x = c("ID_MUNICIPIO_O","SIT_O"), 
                       by.y = c("ID_MUNICIPIO_O","SIT_O"), all.x = T)

rm(comunas)

# Expansion factors ( I made this file manually using the RISE info)

factor_exp = read_csv("Data/factor_expansion.csv")
factor_exp = factor_exp[c("ID_COMUNA_O","factor_expansion")]
colnames(factor_exp) = c("ID_COMUNA","factor_expansion")

times_eod_2017 <- merge(times_eod_2017, factor_exp, 
                        by.x = c("ID_COMUNA_O"), by.y = c("ID_COMUNA"), all.x = T)
rm(factor_exp)

#Saving it 
write.csv(times_eod_2017, row.names = FALSE, file = "Base/times_eod_2017.csv")


#-------------------------------------------------------------#
#             4. Filtering data for Medellin using the        #
#             shapes equivalence we made on the first code    #
#-------------------------------------------------------------#

## Here Medellin is 001 and it communes IDs are represented with values < 44

times_eod_2017 = subset(times_eod_2017, times_eod_2017$ID_MUNICIPIO_D == "001")
times_eod_2017 = subset(times_eod_2017, times_eod_2017$ID_MUNICIPIO_O == "001")
times_eod_2017 = times_eod_2017[times_eod_2017$ID_COMUNA_D < 44,]

## Loading SIT map
sit_zones = readOGR("Data/Shapefiles/SITs_2017/SITzones2017.shp", layer = "SITzones2017") #Maps for 2017
sit_zones$matrix_order = c(1:nrow(sit_zones))

# Deleting duplicated zones
sit_zones = sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),]
sit_zones = sit_zones[!duplicated(sit_zones@data[c("Name")]),] # we keep 535 from 544

# Order of each SIT and its ID
sits_order = data.frame(sit_zones$Name,sit_zones$matrix_order)
colnames(sits_order) <- c("SIT","matrix_order")
rm(sit_zones)

# Available SITs on the survey
match = unique(times_eod_2017$SIT_D) #308

# The ones that are in the survey and the shapefile
sits_order = sits_order[(sits_order$SIT %in% match), ] 


## Filtering the EOD with the ones that are in both
times_eod <- times_eod_2017[(times_eod_2017$SIT_D %in% sits_order$SIT),]

##creating an ID variable 
times_eod$ID_temp = c(1:nrow(times_eod))


## Cleaning times by transportation mode and origin

times_eod_outliers = times_eod%>% 
  group_by(Medio_reg, ID_COMUNA_O) %>% 
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

#Saving it 
write.csv(times_eod, row.names = FALSE, file = "Base/times_eod_2017_filter.csv")


#-------------------------------------------------------------#
#            Ratio travel times between 7 and 9am             #
#                                                             #
#-------------------------------------------------------------#

## Data with hours between 7am and 9am to find the radius
times_eod_early = subset(times_eod, format(HORA_O, "%H") >= "07" & format(HORA_O, "%H") <= "09")


## Data with hours between 5am and 9am to find the radius
times_eod_early_5 = subset(times_eod, format(HORA_O, "%H") >= "05" & format(HORA_O, "%H") <= "09")

## Saving early data from 5am to use it later
write.csv(times_eod_early_5, row.names = FALSE, file = "Base/times_eod_2017_filterEarly5.csv")




## For all the eod files I will delete trips to townships and walking
eod_list = list(all = times_eod, early = times_eod_early, very_early = times_eod_early_5)

for (name in names(eod_list)){
  
  data = eod_list[[name]]
  
  data = data[data$ID_COMUNA_O < "44" & data$Medio_reg != "A pie",]
  
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
    group_by(ID_COMUNA_O,Medio_reg) %>% 
    summarize(minutes = mean(minutos, na.rm = TRUE)) # I call it minutes early because I am restricting the time
  
  data = data %>% 
    rename(!!paste0("minutes_", name) := minutes)
  
  
  eod_list[[name]] = data
  
  assign(name, data)
  
}


eod_collapsed_2017 = Reduce(function(x, y) merge(x, y, by = c("ID_COMUNA_O", "Medio_reg"), all = TRUE), eod_list)

### Ratios 

eod_collapsed_2017 = eod_collapsed_2017 %>% 
  mutate(ratio_7_all = minutes_early/minutes_all, 
         ratio_5_all = minutes_very_early/minutes_all, 
         ratio_7_5 = minutes_early/minutes_very_early)

# Difference in means 2012
t_test_result_2017 = t.test(eod_collapsed_2017$minutes_early, eod_collapsed_2017$minutes_very_early, alternative = "two.sided")

print(t_test_result_2017)


## Saving data
write.csv(eod_collapsed_2017, "Base/Times_early_late_2017.csv", row.names = FALSE)



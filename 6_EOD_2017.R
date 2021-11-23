###### Created by : David Bernal
##### Last modification : 
##### Modified by: 



## Required libraries

### To load and transform the data
library(readr)
library(dplyr)
library(readxl)
library(tidyr)
library(data.table)

### For maps
library(rgdal)
library(spdep)
library(geosphere)

### For graphs
library(ggplot2)
library(binsreg)

### For tables
library(stargazer)
library(texreg)

### For regression trees
library(rpart)
library(rpart.plot)

## For multinomial reg
library(mlogit)



## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

## ------------------------------------------------------------
## Expansion factor RISE ( provided by : Research in spatial econometrics group-EAFIT)
eod_rise <- read_excel("Data/factor_rise.xlsx") 
eod_rise <- subset(eod_rise, !is.na(id_comuna))
colnames(eod_rise)[5] <- "factor_expansion"
eod_rise <- eod_rise[ , names(eod_rise) %in% c("id_comuna", "factor_expansion")]
eod_rise <- na.omit(eod_rise)

## Loading the data
viajes <- read_delim("Data/EOD_2017_DatosViajes.csv", ";", escape_double = FALSE, trim_ws = TRUE)
moradores <- read_delim("Data/EOD_2017_DatosMoradores.csv", ";", escape_double = FALSE, trim_ws = TRUE)

## Merging the files (viajes + moradores)
eod2017 <- merge(viajes, moradores, by.x = c("ID_HOGAR", "ID_MORADOR"), 
             by.y = c("ID_HOGAR", "ORDEN_MORADOR"), all.x = T)
rm(moradores, viajes)


## ------------------------------------------------------------
## Organizing information from the EOD

## Counting transportation mode by category 
temp5 <- eod2017 %>%
  group_by(MODO_TTE_E1, DESC_MODO_TTE_E1) %>% 
  summarise(Obs = n()) 

## -----------------------------------------------------
# Loading classification for transportation modes (I did the csv in excel manually)
categories <- read_delim("Base/clasif_medios_grupos.csv", ";", escape_double = FALSE, trim_ws = TRUE)

## -----------------------------------------------------

### Adding new categorization for Stage 1
colnames(categories) <- c("MODO_TTE_E1","Medio_TTE1","Medio_Desc_TTE1","Medio_res1","Medio_reg1")
eod2017 <- merge(eod2017, categories, 
                 by = c("MODO_TTE_E1"),
                 all.x = TRUE)

### Adding new categorization for Stage 2
colnames(categories) <- c("MODO_TTE_E2","Medio_TTE2","Medio_Desc_TTE2","Medio_res2","Medio_reg2")
eod2017 <- merge(eod2017, categories, 
                 by = c("MODO_TTE_E2"),
                 all.x = TRUE)

### Adding new categorization for Stage 3
colnames(categories) <- c("MODO_TTE_E3","Medio_TTE3","Medio_Desc_TTE3","Medio_res3","Medio_reg3")
eod2017 <- merge(eod2017, categories, 
                 by = c("MODO_TTE_E3"),
                 all.x = TRUE)

### Adding new categorization for Stage 4
colnames(categories) <- c("MODO_TTE_E4","Medio_TTE4","Medio_Desc_TTE4","Medio_res4","Medio_reg4")
eod2017 <- merge(eod2017, categories, 
                 by = c("MODO_TTE_E4"),
                 all.x = TRUE)

### Adding new categorization for Stage 5
colnames(categories) <- c("MODO_TTE_E5","Medio_TTE5","Medio_Desc_TTE5","Medio_res5","Medio_reg5")
eod2017 <- merge(eod2017, categories, 
                 by = c("MODO_TTE_E5"),
                 all.x = TRUE)

### Adding new categorization for Stage 6
colnames(categories) <- c("MODO_TTE_E6","Medio_TTE6","Medio_Desc_TTE6","Medio_res6","Medio_reg6")
eod2017 <- merge(eod2017, categories, 
                 by = c("MODO_TTE_E6"),
                 all.x = TRUE)

### Adding new categorization for Stage 7
colnames(categories) <- c("MODO_TTE_E7","Medio_TTE7","Medio_Desc_TTE7","Medio_res7","Medio_reg7")
eod2017 <- merge(eod2017, categories, 
                 by = c("MODO_TTE_E7"),
                 all.x = TRUE)

## -----------------------------------------------------
## Adding the transportation modes in one column
eod2017$Medios3 <- paste(eod2017$Medio_reg1, 
                         eod2017$Medio_reg2, 
                         eod2017$Medio_reg3, 
                         sep = " + ") 

## -----------------------------------------------------
## Adding the transportation modes in one column
eod2017$Medios <- paste(eod2017$Medio_TTE1, eod2017$Medio_TTE2, eod2017$Medio_TTE3,
                        eod2017$Medio_TTE4, eod2017$Medio_TTE5, eod2017$Medio_TTE6,
                        eod2017$Medio_TTE7, sep = "-") 

## defining final transportation mode
## If the transportation mode 2 exists, we take it as main only if the first transportation mode is walking
eod2017$MedioTTE <- ifelse(is.na(eod2017$Medio_TTE2), eod2017$Medio_TTE1, 
                           ifelse(eod2017$Medio_TTE1 == 0, eod2017$Medio_TTE2,
                                  eod2017$Medio_TTE1))

## If in any of the stages the transportation mode is Metro, we keep it over the others
eod2017$MedioTTE2 <- ifelse(grepl("3", eod2017$Medios), 3, eod2017$MedioTTE)

## -----------------------------------------------------
## final classification
colnames(categories) <- c("MODO_TTE","MedioTTE2","Medio_TTE","Medio_res","Medio_res2")

cat <- data.frame(categories$MedioTTE2, categories$Medio_TTE)
cat <- cat[!duplicated(cat),]
colnames(cat) <- c("MedioTTE2","Medio_TTE")
eod2017 <- merge(eod2017, cat, 
                 by = c("MedioTTE2"),
                 all.x = TRUE)
rm(cat)

### Mode in text
colnames(categories) <- c("MODO_TTE","var","Medio_Desc_TTE","Medio_res","Medio_reg")
categories <- categories[,c("var","Medio_reg")]
categories <- categories[!duplicated(categories),]

eod2017 <- merge(eod2017, categories, 
                 by.x = c("MedioTTE2"), by.y = c("var"),
                 all.x = TRUE)

## Dummies for each mode
eod2017$dpie <- ifelse(grepl("A pie", eod2017$Medios3), 1, 0)
eod2017$dpub <- ifelse(grepl("Publico", eod2017$Medios3), 1, 0)
eod2017$dpri <- ifelse(grepl("Privado", eod2017$Medios3), 1, 0)


rm(temp5, categories)


## -----------------------------------------------------


## We only keep commuting to work (Employees)
eodw_2017 <- subset(eod2017, eod2017$DESC_MOTIVO_VIAJE == "Al Trabajo")


## The commues in the OD survey 2017 have different codes
## We use the homologation done by the RISE
## And we merge it
eodw_2017  <- merge(eodw_2017, eod_rise, by.x = c("ID_COMUNA_O"), 
                   by.y = c("id_comuna"), all.x = T)
rm(eod_rise)

eodw_2017$factor_expansion <- as.numeric(eodw_2017$factor_expansion)

## counting modes by categories
temp <- eodw_2017 %>%
  group_by(Medios3) %>% 
  summarise(Total = sum(factor_expansion, na.rm = T)) 
#write.csv(temp, file = "conteo_categorias_fexp_2017.csv")
rm(temp)

## -----------------------------------------------------
## We keep the important variables from the OD 2017 survey
times_eod_2017 <- eodw_2017[c("ID_MORADOR","ID_MUNICIPIO_O", "ID_MUNICIPIO_D", 
                              "SIT_O", "HORA_O", 
                              "SIT_D", "HORA_D", 
                              "MedioTTE2","Medio_TTE", 
                              "Medios3", "Medio_reg", "dpie","dpub","dpri",
                              "OCUPACION","DESC_OCUPACION",
                              "NIVEL_LABORAL","DESC_NIVEL_LABORAL",
                              "DEDICACION_LABORAL","DESC_DEDICACION_LABORAL")]

## Format to the starting and ending time of a trip
times_eod_2017$time1 <- paste("2019-01-01",times_eod_2017$HORA_O)
times_eod_2017$time2 <- paste("2019-01-01",times_eod_2017$HORA_D)

## Computing travel times
times_eod_2017$minutos <- difftime(times_eod_2017$time2, times_eod_2017$time1, units="mins")

## -----------------------------------------------------
## Descriptive stats
stats <- times_eod_2017 %>% 
  group_by(Medio_TTE) %>%
  dplyr::summarise(obs = length(Medio_TTE),
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
#write.csv(stats, file = "stats_categorias_2017_sin_cortar.csv")

### --------------------- Cortando colas
times_eod_2017$cortar <- ifelse(times_eod_2017$Medio_TTE == "A pie" & 
                                  times_eod_2017$minutos > stats$p99[1], "SI", "NO")
times_eod_2017$cortar <- ifelse(times_eod_2017$Medio_TTE == "Auto" & 
                                  times_eod_2017$minutos > stats$p99[2], "SI", times_eod_2017$cortar)
times_eod_2017$cortar <- ifelse(times_eod_2017$Medio_TTE == "Auto comp" & 
                                  times_eod_2017$minutos > stats$p99[3], "SI", times_eod_2017$cortar)
times_eod_2017$cortar <- ifelse(times_eod_2017$Medio_TTE == "Bicicleta" & 
                                  times_eod_2017$minutos > stats$p99[4], "SI", times_eod_2017$cortar)
times_eod_2017$cortar <- ifelse(times_eod_2017$Medio_TTE == "Bus-Bta-Microbus" & 
                                  times_eod_2017$minutos > stats$p99[5], "SI", times_eod_2017$cortar)
times_eod_2017$cortar <- ifelse(times_eod_2017$Medio_TTE == "Informal" & 
                                  times_eod_2017$minutos > stats$p99[6], "SI", times_eod_2017$cortar)
times_eod_2017$cortar <- ifelse(times_eod_2017$Medio_TTE == "Metro" & 
                                  times_eod_2017$minutos > stats$p99[7], "SI", times_eod_2017$cortar)
times_eod_2017$cortar <- ifelse(times_eod_2017$Medio_TTE == "Moto" & 
                                  times_eod_2017$minutos > stats$p99[8], "SI", times_eod_2017$cortar)
times_eod_2017$cortar <- ifelse(times_eod_2017$Medio_TTE == "Taxi" & 
                                  times_eod_2017$minutos > stats$p99[9], "SI", times_eod_2017$cortar)
times_eod_2017$cortar <- ifelse(is.na(times_eod_2017$Medio_TTE) & 
                                  times_eod_2017$minutos > stats$p99[10], "SI", times_eod_2017$cortar)

write.csv(times_eod_2017, file = "Base/times_eod_2017_2.csv")

## -----------------------------------------------------
## Descriptive stats(After cutting tails)
stats2 <- subset(times_eod_2017, cortar != "SI") %>% 
  group_by(Medio_TTE) %>%
  dplyr::summarise(obs = length(Medio_TTE),
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
#write.csv(stats2, file = "stats_categorias_2017_cortados.csv")

rm(stats, stats2, eodw_2017)



## -----------------------------------------------------
## Adding information of communes and Expansion factors


# Communes & SITs
comunas <- read_csv("Data/comunas.csv")
comunas <- comunas[comunas$ID_COMUNA_D != "-",]
comunas <- comunas[!duplicated(comunas[,c("ID_MUNICIPIO_D","SIT_D")]),]
comunas <- comunas[!duplicated(comunas[,c("SIT_D")]),]

# Merging communes and EOD
times_eod_2017 <- merge(times_eod_2017, comunas, 
                        by.x = c("ID_MUNICIPIO_D","SIT_D"), 
                        by.y = c("ID_MUNICIPIO_D","SIT_D"), all.x = T)

colnames(comunas) <- c("ID_MUNICIPIO_O","ID_COMUNA_O","SIT_O")

times_eod_2017 <- merge(times_eod_2017, comunas, 
                        by.x = c("ID_MUNICIPIO_O","SIT_O"), 
                        by.y = c("ID_MUNICIPIO_O","SIT_O"), all.x = T)

colnames(comunas) <- c("ID_MUNICIPIO_D","ID_COMUNA_D","SIT_D")
rm(comunas)

# Expansion factor
factor_exp <- read_csv("Data/factor_expansion.csv")
factor_exp <- factor_exp[c("ID_COMUNA_O","factor_expansion")]
colnames(factor_exp) <- c("ID_COMUNA","factor_expansion")

times_eod_2017 <- merge(times_eod_2017, factor_exp, 
                        by.x = c("ID_COMUNA_O"), by.y = c("ID_COMUNA"), all.x = T)
rm(factor_exp)

## -----------------------------------------------------
## Filtering for Medellin

# 001 is the ID from Medellin
# We delete townships (< 44)
times_eod_2017 <- subset(times_eod_2017, times_eod_2017$ID_MUNICIPIO_D == "001")
times_eod_2017 <- subset(times_eod_2017, times_eod_2017$ID_MUNICIPIO_O == "001")
times_eod_2017 <- times_eod_2017[times_eod_2017$ID_COMUNA_D < 44,]


## -----------------------------------------------------
## We keep commune SITS with the shape

## Loading SIT map
sit_zones <- readOGR("Data/shapessits/SITzones2017.shp", layer = "SITzones2017")
sit_zones$matrix_order <- c(1:nrow(sit_zones))

# Deleting duplicates
sit_zones <- sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),]
sit_zones <- sit_zones[!duplicated(sit_zones@data[c("Name")]),] # de 544 quedan 535

# ids and order
sits_order <- data.frame(sit_zones$Name,sit_zones$matrix_order)
colnames(sits_order) <- c("SIT","matrix_order")
rm(sit_zones)

# available SITS in the EOD
match <- unique(times_eod_2017$SIT_D) #308

# Filtering with the ones that are in the survey
sits_order <- sits_order[(sits_order$SIT %in% match), ] 


# Seems like we lost the SIT 238
times_eod <- times_eod_2017[(times_eod_2017$SIT_D %in% sits_order$SIT),]


## Saving reported travel times in 2017
write.csv(times_eod, file = "Base/times_eod_2017_filter.csv")










###### Created by : David Bernal
##### Last modification : 
##### Modified by: 


### This code categorizes transportation modes from the survey



## Required libraries

### To load and transform the data
library(readr)
library(dplyr)
library(readxl)

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



## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

## -----------------------------------------------------
## Loading data
EOD2012 <- read_excel("Data/EOD_2012.xlsx")

## -----------------------------------------------------
## Categorizing transportation modes in one column
EOD2012$Medios <- paste(EOD2012$Etapa1MT, EOD2012$Etapa2MT, EOD2012$Etapa3MT,
                        EOD2012$Etapa4MT, EOD2012$Etapa5MT, EOD2012$Etapa6MT,
                        EOD2012$Etapa7MT, sep = "-") 

## Defining the transportation mode
## If the transportation mode 2 exists, we take it as main only if the first transportation mode is walking
EOD2012$MedioTTE <- ifelse(is.na(EOD2012$Etapa2MT), EOD2012$Etapa1MT, 
                           ifelse(EOD2012$Etapa1MT == 0, EOD2012$Etapa2MT,
                                  EOD2012$Etapa1MT))

## If in any of the stages the transportation mode is Metro, we keep it over the others
EOD2012$MedioTTE2 <- ifelse(grepl("3", EOD2012$Medios), 3, EOD2012$MedioTTE)
EOD2012$Medio_reg <- ifelse(EOD2012$MedioTTE2 == 0, "A pie", 
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


EOD2012$Medio1 <- ifelse(EOD2012$Etapa1MT == 0, "A pie", 
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

EOD2012$Medio2 <- ifelse(EOD2012$Etapa2MT == 0, "A pie", 
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

EOD2012$Medio3 <- ifelse(EOD2012$Etapa3MT == 0, "A pie", 
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

EOD2012$Medios3 <- paste(EOD2012$Medio1, 
                         EOD2012$Medio2, 
                         EOD2012$Medio3, 
                         sep = " + ") 

## Dummies for transportation mode
EOD2012$dpie <- ifelse(grepl("A pie", EOD2012$Medios3), 1, 0)
EOD2012$dpub <- ifelse(grepl("Publico", EOD2012$Medios3), 1, 0)
EOD2012$dpri <- ifelse(grepl("Privado", EOD2012$Medios3), 1, 0)



## We only keep commuting to work (Employees)
eodw_2012 <- subset(EOD2012, EOD2012$IdMotivoViaje == "1")

temp <- eodw_2012 %>%
  group_by(Medios3) %>% 
  summarise(Total = sum(PrimeroDeFactorExpansion, na.rm = T)) 
#write.csv(temp, file = "conteo_categorias_fexp_2012.csv")
rm(temp)


## -----------------------------------------------------
## We keep the importan variables from the OD2012 survey
times_eod_2012 <- eodw_2012[c("IdMunicipioo", "IdMunicipiod", 
                              "IdSito", "Horao", 
                              "IdSitd", "Horad", 
                              "MedioTTE2", 
                              "Medios3", "Medio_reg", "dpie","dpub","dpri"
                              )]

## -----------------------------------------------------
## Computing reported travel times in minutes
times_eod_2012$minutos <- difftime(times_eod_2012$Horad, times_eod_2012$Horao, units="mins")

## -----------------------------------------------------
## Classifying transportation modes
times_eod_2012$Medio_TTE <- ifelse(times_eod_2012$MedioTTE2 == 0, "A pie", NA)
times_eod_2012$Medio_TTE <- ifelse(times_eod_2012$MedioTTE2 == 1, "Bus-Bta-Microbus", times_eod_2012$Medio_TTE)
times_eod_2012$Medio_TTE <- ifelse(times_eod_2012$MedioTTE2 == 2, "Bus-Bta-Microbus", times_eod_2012$Medio_TTE)
times_eod_2012$Medio_TTE <- ifelse(times_eod_2012$MedioTTE2 == 3, "Metro", times_eod_2012$Medio_TTE)
times_eod_2012$Medio_TTE <- ifelse(times_eod_2012$MedioTTE2 == 4, "Taxi", times_eod_2012$Medio_TTE)
times_eod_2012$Medio_TTE <- ifelse(times_eod_2012$MedioTTE2 == 5, "Informal", times_eod_2012$Medio_TTE)
times_eod_2012$Medio_TTE <- ifelse(times_eod_2012$MedioTTE2 == 6, "Auto comp", times_eod_2012$Medio_TTE)
times_eod_2012$Medio_TTE <- ifelse(times_eod_2012$MedioTTE2 == 7, "Auto", times_eod_2012$Medio_TTE)
times_eod_2012$Medio_TTE <- ifelse(times_eod_2012$MedioTTE2 == 8, "Moto", times_eod_2012$Medio_TTE)
times_eod_2012$Medio_TTE <- ifelse(times_eod_2012$MedioTTE2 == 9, "Bicicleta", times_eod_2012$Medio_TTE)

## -----------------------------------------------------
## Descriptive stats
stats <- times_eod_2012 %>% 
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
#write.csv(stats, file = "stats_categorias_2012_sin_cortar.csv")

### --------------------- Cutting tails
times_eod_2012$cortar <- ifelse(times_eod_2012$Medio_TTE == "A pie" & 
                                  times_eod_2012$minutos > stats$p99[1], "SI", "NO")
times_eod_2012$cortar <- ifelse(times_eod_2012$Medio_TTE == "Auto" & 
                                  times_eod_2012$minutos > stats$p99[2], "SI", times_eod_2012$cortar)
times_eod_2012$cortar <- ifelse(times_eod_2012$Medio_TTE == "Auto comp" & 
                                  times_eod_2012$minutos > stats$p99[3], "SI", times_eod_2012$cortar)
times_eod_2012$cortar <- ifelse(times_eod_2012$Medio_TTE == "Bicicleta" & 
                                  times_eod_2012$minutos > stats$p99[4], "SI", times_eod_2012$cortar)
times_eod_2012$cortar <- ifelse(times_eod_2012$Medio_TTE == "Bus-Bta-Microbus" & 
                                  times_eod_2012$minutos > stats$p99[5], "SI", times_eod_2012$cortar)
times_eod_2012$cortar <- ifelse(times_eod_2012$Medio_TTE == "Informal" & 
                                  times_eod_2012$minutos > stats$p99[6], "SI", times_eod_2012$cortar)
times_eod_2012$cortar <- ifelse(times_eod_2012$Medio_TTE == "Metro" & 
                                  times_eod_2012$minutos > stats$p99[7], "SI", times_eod_2012$cortar)
times_eod_2012$cortar <- ifelse(times_eod_2012$Medio_TTE == "Moto" & 
                                  times_eod_2012$minutos > stats$p99[8], "SI", times_eod_2012$cortar)
times_eod_2012$cortar <- ifelse(times_eod_2012$Medio_TTE == "Taxi" & 
                                  times_eod_2012$minutos > stats$p99[9], "SI", times_eod_2012$cortar)
times_eod_2012$cortar <- ifelse(is.na(times_eod_2012$Medio_TTE) & 
                                  times_eod_2012$minutos > stats$p99[10], "SI", times_eod_2012$cortar)

## -----------------------------------------------------
## Descriptive stats after cutting tails
stats2 <- subset(times_eod_2012, cortar != "SI") %>% 
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
#write.csv(stats2, file = "stats_categorias_2012_cortados.csv")
rm(stats, stats2)

write.csv(times_eod_2012, file = "Base/times_eod_2012.csv")



## -----------------------------------------------------
## Expansion factors information

# Communes & SITs
temp1 <- EOD2012[c("IdMunicipiod","IdComunad","IdSitd")]
temp2 <- EOD2012[c("IdMunicipioo","IdComunao","IdSito")]
colnames(temp2) <- c("IdMunicipiod","IdComunad","IdSitd")
comunas <- rbind(temp1, temp2)
comunas <- comunas[!is.na(comunas$IdComunad),]
comunas <- comunas[comunas$IdComunad != "00",]
comunas <- comunas[!duplicated(comunas[,c("IdMunicipiod","IdSitd")]),]
rm(temp1, temp2)

# Merging communes and EOD
times_eod_2012 <- merge(times_eod_2012, comunas, 
                        by.x = c("IdMunicipiod","IdSitd"), 
                        by.y = c("IdMunicipiod","IdSitd"), all.x = T)

colnames(comunas) <- c("IdMunicipioo","IdComunao","IdSito")

times_eod_2012 <- merge(times_eod_2012, comunas, 
                        by.x = c("IdMunicipioo","IdSito"), 
                        by.y = c("IdMunicipioo","IdSito"), all.x = T)

colnames(comunas) <- c("IdMunicipiod","IdComunad","IdSitd")

## Expansion factors
factor_exp <- EOD2012[EOD2012$IdMunicipioo == "10",]
factor_exp <- factor_exp[!is.na(factor_exp$IdComunao),]
factor_exp <- factor_exp[c("IdComunao","PrimeroDeFactorExpansion")]
factor_exp <- factor_exp[!duplicated(factor_exp[,c("IdComunao")]),]


times_eod_2012 <- merge(times_eod_2012, factor_exp, 
                        by.x = c("IdComunao"), by.y = c("IdComunao"), all.x = T)


## -----------------------------------------------------
## Filtering for Medellin

# 10 is the ID for Medellin
# We delete townships (<= 61)
times_eod_2012 <- subset(times_eod_2012, times_eod_2012$IdMunicipiod == "10")
times_eod_2012 <- subset(times_eod_2012, times_eod_2012$IdMunicipioo == "10")
times_eod_2012 <- times_eod_2012[times_eod_2012$IdComunad <= 61,]


## -----------------------------------------------------


## Loading SIT map
sit_zones <- readOGR("Data/shapessits/SITzones2017.shp", layer = "SITzones2017")
sit_zones$matrix_order <- c(1:nrow(sit_zones))

# Deleting duplicates for SITS
sit_zones <- sit_zones[order(sit_zones@data$Name, -sit_zones@data$Shape_Area),]
sit_zones <- sit_zones[!duplicated(sit_zones@data[c("Name")]),] # de 544 quedan 535


# Using homologation
homol_sit <- read_csv("Base/homol_sit_v1.csv", col_types = cols(X1 = col_skip()))
homol_sit <- homol_sit[homol_sit$IdMunicipiod == "10",]
homol_sit$IdSitd <- gsub("([ABCDEFGHIJKL])", "", homol_sit$IdSitd)


# IDs from each SIT and its order
sits_order <- data.frame(sit_zones$Name,sit_zones$matrix_order)
colnames(sits_order) <- c("SIT","matrix_order")

## Merge order and homologation
sits_order <- merge(sits_order, homol_sit, by.x = c("SIT"), by.y = c("SIT_D"), all.x = T)

# Available sits in the eood
match <- unique(times_eod_2012$IdSitd) #239

# Filtering with the ones that are in the survey
sits_order <- sits_order[(sits_order$IdSitd %in% match), ] 
sits_order <- sits_order[(!is.na(sits_order$IdSitd)), ]

# filtering with the ones that are in the shapefiles
times_eod <- times_eod_2012[(times_eod_2012$IdSitd %in% sits_order$IdSitd),]

## Saving reported travel times for 2012
write.csv(times_eod, file = "Base/times_eod_2012_filter.csv")


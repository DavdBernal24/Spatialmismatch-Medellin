###### Created by : David Bernal
##### Last modification : 
##### Modified by: 

### This code creates a map of people who lives and work inside a same commune

### To load and transform the data
library(readr)
library(dplyr)
library(RColorBrewer)

### For maps
library(rgdal)
library(spdep)
library(maptools)
library(prettymapr)
library(classInt)

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())




# EOD 2017
times_eod_2017 <- read_csv("Base/times_eod_2017_2.csv", col_types = cols(X1 = col_skip()))

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

# Expansion factors
factor_exp <- read_csv("Data/factor_expansion.csv")
factor_exp <- factor_exp[c("ID_COMUNA_O","factor_expansion")]
colnames(factor_exp) <- c("ID_COMUNA","factor_expansion")

times_eod_2017 <- merge(times_eod_2017, factor_exp, 
                        by.x = c("ID_COMUNA_O"), by.y = c("ID_COMUNA"), all.x = T)
rm(factor_exp)

# Filtering for Medellin
times_eod_2017 <- subset(times_eod_2017, times_eod_2017$ID_MUNICIPIO_D == "001")
times_eod_2017 <- subset(times_eod_2017, times_eod_2017$ID_MUNICIPIO_O == "001")
#times_eod_2017 <- times_eod_2017[times_eod_2017$ID_COMUNA_D < 44,]

## employment in each  cada j
data_emp <- times_eod_2017 %>%
  group_by(ID_COMUNA_O) %>%
  summarise(emp = sum(factor_expansion))

# Filtering for communes
times_eod <- subset(times_eod_2017, times_eod_2017$ID_COMUNA_O == times_eod_2017$ID_COMUNA_D)

data_emp2 <- times_eod %>%
  group_by(ID_COMUNA_O) %>%
  summarise(emp = sum(factor_expansion))

emp_comuna <- merge(data_emp, data_emp2, by = 'ID_COMUNA_O', all.x = T)
colnames(emp_comuna) <- c('comuna','emp_comuna','emp_misma_comuna')
emp_comuna$part_emp_misma_comuna <- (emp_comuna$emp_misma_comuna / emp_comuna$emp_comuna)*100

comunas <- read_csv("Data/comunas_shape.csv")
emp_comuna <- merge(emp_comuna, comunas, by = 'comuna', all.x = T)


## Communes shapes
shp_comunas <- readOGR("Shapes/shapescomunas/Comunas.shp")
shp_comunas$Cod_Comuna <- as.numeric(as.character(shp_comunas$Cod_Comuna))
shp_comunas <- merge(shp_comunas, emp_comuna, by = 'Cod_Comuna', all.x = T)


## -----------------------------------------------------
coords <- data.frame(coordinates(shp_comunas))

## groups for the map
brks <- classIntervals(shp_comunas$part_emp_misma_comuna,
                       n=5, style="quantile", dataPrecision=2)

## color palette to fill the map
colors <- c(brewer.pal(5, "YlOrRd"))


pdf(file = paste("Output/Additional_figures/emp_commune.pdf", sep = ""))
par(mar = c(0, 0, 0, 0))
plot(shp_comunas,
     col=colors[findInterval(shp_comunas$part_emp_misma_comuna, brks$brks,
                             all.inside=TRUE)], axes=F)
title(paste (""))
text(coords, labels = paste0(round(shp_comunas$part_emp_misma_comuna,2),'%'), cex = 0.6)
addnortharrow(pos='topleft', scale = 0.5, padin = c(1.5, 0.3))
addscalebar(widthhint = 0.2,unitcategory = "metric", padin = c(1.5, 0.3))
legend("bottomright", # location of legend
       legend = leglabs(paste0(round(brks$brks,1),'%')), # categories or elements in legend
       fill = colors, # color palette
       bty = "n", # turn off the legend BORDER
       cex = 0.9, # change the font size
       x.intersp = 0.5,
       y.intersp = 0.7,
       inset = c(0.0,0.18)
       )
dev.off()



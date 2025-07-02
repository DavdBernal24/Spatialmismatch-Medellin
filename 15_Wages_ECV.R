### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This codes computes wages using the ECV (Encuesta de calidad de vida) for 2012 and 2017

#Required libraries


# These libraries help you to read and transform data
library(readr)
library(dplyr) 



## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################


#-------------------------------------------------------------#
#               1.2017 Wages                                  #  
#                                                             #
#-------------------------------------------------------------#


salarios2017 = read_csv("Data/ECV/ECV_2017.csv")

#I just need wages and estrato

salarios2017 = data.frame(salarios2017$P_10, salarios2017$P_87,salarios2017$FEP )
names(salarios2017) = c("estrato", "w2017", "FEP")





#cleaning data
salarios2017 = salarios2017[salarios2017$w2017!= -88,]
salarios2017 = salarios2017[salarios2017$w2017!= -98,]
salarios2017 = salarios2017[salarios2017$w2017!= -97,]
salarios2017 = salarios2017[salarios2017$w2017!= 0,]
salarios2017 = salarios2017[salarios2017$w2017!= -99,]

salarios_2017 = salarios2017$w2017*salarios2017$FEP
mean_salario2017 = sum(salarios_2017)/sum(salarios2017$FEP)

wage_per_min_2017 = mean_salario2017/(192*60)



# Get wages by Socioeconomic strata
salarios2017 <- salarios2017 %>%
  group_by(estrato) %>%
  summarise(w2017 = mean(w2017))

salarios2017$wmin2017 = salarios2017$w2017/(192*60)




#-------------------------------------------------------------#
#               2.2012 Wages and savings                      #  
#                                                             #
#-------------------------------------------------------------#



salarios2012 = read_csv("Data/ECV/ECV_2011.csv")

salarios2012 = salarios2012[c("Estrato","P_223","FEpers2011")]

names(salarios2012) = c("estrato", "w2012","FEP2012")

salarios_2012 = salarios2012$w2012*salarios2012$FEP2012
mean_salarios2012 = sum(salarios_2012)/sum(salarios2012$FEP2012)

mean_per_min_2012 = mean_salarios2012/(192*60)


## Get wages by estrato
salarios2012 = salarios2012 %>%
  group_by(estrato) %>%
  summarise(w2012 = mean(w2012))

salarios2012$wmin2012 = salarios2012$w2012/(192*60)


## Merging wage 2012 
salarios = merge(salarios2012, salarios2017, 
                  by.x = c("estrato"), by.y = c("estrato"), all.x = T)

salarios$wmin2017 = wage_per_min_2017
salarios$wmin2012 = mean_per_min_2012


write.csv(salarios, file="Base/Wmin.csv")


### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medellín, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code categorizes transportation modes from the survey for 2012 and compute travel times from
# Origins and destinations and creates an instagram to compare different modes
# Here we use more categories than just public or private

#Required libraries

# These libraries help you to read and transform data
library(dplyr)
library(readxl)
library(readr)
library(rstatix)
library(ggplot2)
#remotes::install_github("idmn/ggview") # I use this for the graphs. 
library(ggview)
library(forcats)


# Library to work with spatial data
library(rgdal)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())



##############################################################################

#-------------------------------------------------------------#
#               2012 preamble                                 #  
#                                                             #
#-------------------------------------------------------------#

EOD2012 = read_excel("Data/EOD_2012.xlsx")



## I will create a variables with all the modes used in different stages

EOD2012$Medios = paste(EOD2012$Etapa1MT, EOD2012$Etapa2MT, EOD2012$Etapa3MT,
                       EOD2012$Etapa4MT, EOD2012$Etapa5MT, EOD2012$Etapa6MT,
                       EOD2012$Etapa7MT, sep = "-") 

## Setting transportation mode "Walking" if there is not mode in stage 2 and stage 1 is walking
EOD2012$MedioTTE =  ifelse(is.na(EOD2012$Etapa2MT), EOD2012$Etapa1MT, 
                           ifelse(EOD2012$Etapa1MT == 0, EOD2012$Etapa2MT,
                                  EOD2012$Etapa1MT))

## if in any stage the transportation mode is Metro system, I keep it over the others
EOD2012$MedioTTE2 = ifelse(grepl("3", EOD2012$Medios), 3, EOD2012$MedioTTE)


# Final classification
EOD2012$Modes_final = ifelse(EOD2012$MedioTTE2 == 0,  "Walking", "Others") # Walking
EOD2012$Modes_final = ifelse(EOD2012$MedioTTE2 == 1 | EOD2012$MedioTTE2 == 2,  "Bus",  EOD2012$Modes_final) ##Bus
EOD2012$Modes_final = ifelse(EOD2012$MedioTTE2 == 3,  "Metro", EOD2012$Modes_final) # Metro
EOD2012$Modes_final = ifelse(EOD2012$MedioTTE2 == 4,  "Taxi", EOD2012$Modes_final) #Taxi
EOD2012$Modes_final = ifelse(EOD2012$MedioTTE2 == 7,  "Car", EOD2012$Modes_final) #Car
EOD2012$Modes_final = ifelse(EOD2012$MedioTTE2 == 8,  "Motorcycle", EOD2012$Modes_final) #Motorcycle
EOD2012$Modes_final = ifelse(EOD2012$MedioTTE2 == 9,  "Bicycle", EOD2012$Modes_final) #Bicycle
EOD2012$Modes_final = ifelse(EOD2012$MedioTTE2 == 50,  "Metroplus", EOD2012$Modes_final) #Metroplus

# Removing NAs
EOD2012 = EOD2012[!is.na(EOD2012$Modes_final), ]

## Keeping trips to work
eodw_2012 = subset(EOD2012, EOD2012$IdMotivoViaje == "1")

times_eod_2012 = eodw_2012


## Computing travel times in minutes
times_eod_2012$minutos = as.numeric(difftime(times_eod_2012$Horad, times_eod_2012$Horao, units="mins"))
## Medellin
times_eod_2012 = subset(times_eod_2012, times_eod_2012$IdMunicipioo == "10")
times_eod_2012 = times_eod_2012[times_eod_2012$IdComunad <= 61,]




## Times by mode and counting
times_allmodes_2012 = times_eod_2012%>%
  group_by(Modes_final) %>%
  dplyr::summarize(Mean = mean(minutos, na.rm=TRUE), Total_number = n(), sd_time = sd(minutos, na.rm = TRUE))

times_allmodes_2012 = times_allmodes_2012[!is.na(times_allmodes_2012$Modes_final), ]

times_allmodes_2012$frequency = times_allmodes_2012$Total_number/sum(times_allmodes_2012$Total_number)

## Confidence intervals
times_allmodes_2012$cof_int_inf = times_allmodes_2012$Mean - 
  1.96* times_allmodes_2012$sd_time/sqrt(times_allmodes_2012$Total_number) 

times_allmodes_2012$cof_int_sup = times_allmodes_2012$Mean + 
  1.96* times_allmodes_2012$sd_time/sqrt(times_allmodes_2012$Total_number) 


times_allmodes_2012$cof_int_freqinf = times_allmodes_2012$frequency + 
  1.96*sqrt((times_allmodes_2012$frequency*(1-times_allmodes_2012$frequency))/times_allmodes_2012$Total_number)

times_allmodes_2012$cof_int_freqinf = times_allmodes_2012$frequency - 
  1.96*sqrt((times_allmodes_2012$frequency*(1-times_allmodes_2012$frequency))/times_allmodes_2012$Total_number)

times_allmodes_2012$cof_int_freqsup = times_allmodes_2012$frequency + 
  1.96*sqrt((times_allmodes_2012$frequency*(1-times_allmodes_2012$frequency))/times_allmodes_2012$Total_number)

# Multiplying the intervals for the frequency by 100

times_allmodes_2012$cof_int_freqinf = times_allmodes_2012$cof_int_freqinf*100
times_allmodes_2012$cof_int_freqsup = times_allmodes_2012$cof_int_freqsup*100


#-------------------------------------------------------------#
#               Plots 2012                                    #  
#                                                             #
#-------------------------------------------------------------#

## Commuting times 2012
pdf(file ="Output/Figures/HistogramTimes_2012.pdf") 
ggplot(times_allmodes_2012, aes(x = Modes_final, y = Mean, fill = Modes_final)) +
  geom_bar(stat = "identity", alpha = 0.5, fill = "orange") +
  geom_text(aes(label = round(Mean, 2)), vjust = -0.5, color = "black", size = 3) +
  labs(title = "", x = "Category", y = "Minutes") +
  theme_minimal()
dev.off()

## Frequencies 2012

#Original
pdf(file ="Output/Figures/HistogramFrequency_2012.pdf") 
ggplot(times_allmodes_2012, aes(x = Modes_final, y = frequency, fill = Modes_final)) +
  geom_bar(stat = "identity", alpha = 0.5, fill = "orange") +
  geom_text(aes(label = round(frequency, 2)), vjust = -0.5, color = "black", size = 3) +
  labs(title = "", x = "Category", y = "Frequency") +
  theme_minimal()
dev.off()







#-------------------------------------------------------------#
#               2017 preamble                                 #  
#                                                             #
#-------------------------------------------------------------#

EOD2017 = read_delim("Data/EOD_2017_DatosViajes.csv", ";", escape_double = FALSE, trim_ws = TRUE)


### Creating a variable for all the modes in different stages. 
EOD2017$Medios = paste(EOD2017$MODO_TTE_E1, EOD2017$MODO_TTE_E2, EOD2017$MODO_TTE_E3,
                       EOD2017$MODO_TTE_E4, EOD2017$MODO_TTE_E5, EOD2017$MODO_TTE_E6,
                       EOD2017$MODO_TTE_E7, sep = "-") 

## Setting transportation mode "Walking" if there is not mode in stage 2 and stage 1 is walking
EOD2017$MedioTTE =  ifelse(is.na(EOD2017$MODO_TTE_E2), EOD2017$MODO_TTE_E1, 
                           ifelse(EOD2017$MODO_TTE_E1 >= 21 & EOD2017$MODO_TTE_E1 <= 39, EOD2017$MODO_TTE_E2,
                                  EOD2017$MODO_TTE_E1))



## if in any stage the transportation mode is Metro system, I keep it over the others
EOD2017$MedioTTE2 = ifelse(grepl("3", EOD2017$Medios), 3, EOD2017$MedioTTE)
EOD2017$MedioTTE2 = ifelse(grepl("4", EOD2017$Medios), 4, EOD2017$MedioTTE2)
EOD2017$MedioTTE2 = ifelse(grepl("6", EOD2017$Medios), 6, EOD2017$MedioTTE2)
EOD2017$MedioTTE2 = ifelse(grepl("7", EOD2017$Medios), 7, EOD2017$MedioTTE2)
EOD2017$MedioTTE2 = ifelse(grepl("16", EOD2017$Medios), 16, EOD2017$MedioTTE2)
EOD2017$MedioTTE2 = ifelse(grepl("17", EOD2017$Medios), 17, EOD2017$MedioTTE2)



# Final classification
EOD2017$Modes_final = ifelse(EOD2017$MedioTTE2 >= 21 & EOD2017$MedioTTE2 <= 39,  "Walking", "Others") # Walking
EOD2017$Modes_final = ifelse(EOD2017$MedioTTE2 %in% c(1, 2, 3),  "Bus",  EOD2017$Modes_final) ##Bus
EOD2017$Modes_final = ifelse(EOD2017$MedioTTE2 %in% c(4, 6, 7),  "Metro", EOD2017$Modes_final) # Metro
EOD2017$Modes_final = ifelse(EOD2017$MedioTTE2 >= 16 & EOD2017$MedioTTE2 <= 18,  "Taxi", EOD2017$Modes_final) #Taxi
EOD2017$Modes_final = ifelse(EOD2017$MedioTTE2 == 8 | EOD2017$MedioTTE == 9,  "Car", EOD2017$Modes_final) #Car
EOD2017$Modes_final = ifelse(EOD2017$MedioTTE2 == 10 | EOD2017$MedioTTE == 11,  "Motorcycle", EOD2017$Modes_final) #Motorcycle
EOD2017$Modes_final = ifelse(EOD2017$MedioTTE2 == 40 | EOD2017$MedioTTE == 41,  "Bicycle", EOD2017$Modes_final) #Bicycle
EOD2017$Modes_final = ifelse(EOD2017$MedioTTE2 == 5,  "Metroplus", EOD2017$Modes_final) #Metroplus


## Keeping trips to work
EOD2017 = subset(EOD2017, EOD2017$DESC_MOTIVO_VIAJE == "Al Trabajo")


## Keeping Medellin 
EOD2017 = subset(EOD2017, EOD2017$ID_MUNICIPIO_D == "001")
EOD2017 = subset(EOD2017, EOD2017$ID_MUNICIPIO_O == "001")
EOD2017 = EOD2017[EOD2017$ID_COMUNA_D < 44,]

times_eod_2017 = EOD2017


## Organizing format of the dates
times_eod_2017$time1 = paste("2019-01-01",times_eod_2017$HORA_O)
times_eod_2017$time2 = paste("2019-01-01",times_eod_2017$HORA_D)


## Computing travel times in minutes
times_eod_2017$minutos = as.numeric(difftime(times_eod_2017$time2, times_eod_2017$time1, units="mins"))


## Times by mode and counting
times_allmodes_2017 = times_eod_2017%>%
  group_by(Modes_final) %>%
  dplyr::summarize(Mean = mean(minutos, na.rm=TRUE), Total_number = n(), sd_time = sd(minutos, na.rm = TRUE))

times_allmodes_2017 = times_allmodes_2017[!is.na(times_allmodes_2017$Modes_final), ]

times_allmodes_2017$frequency = times_allmodes_2017$Total_number/sum(times_allmodes_2017$Total_number)


## Confidence intervals
times_allmodes_2017$cof_int_inf = times_allmodes_2017$Mean - 
  1.96* times_allmodes_2017$sd_time/sqrt(times_allmodes_2017$Total_number) 

times_allmodes_2017$cof_int_sup = times_allmodes_2017$Mean + 
  1.96* times_allmodes_2017$sd_time/sqrt(times_allmodes_2017$Total_number) 


times_allmodes_2017$cof_int_freqinf = times_allmodes_2017$frequency + 
  1.96*sqrt((times_allmodes_2017$frequency*(1-times_allmodes_2017$frequency))/times_allmodes_2017$Total_number)

times_allmodes_2017$cof_int_freqinf = times_allmodes_2017$frequency - 
  1.96*sqrt((times_allmodes_2017$frequency*(1-times_allmodes_2017$frequency))/times_allmodes_2017$Total_number)

times_allmodes_2017$cof_int_freqsup = times_allmodes_2017$frequency + 
  1.96*sqrt((times_allmodes_2017$frequency*(1-times_allmodes_2017$frequency))/times_allmodes_2017$Total_number)

# Multiplying the intervals for the frequency by 100

times_allmodes_2017$cof_int_freqinf = times_allmodes_2017$cof_int_freqinf*100
times_allmodes_2017$cof_int_freqsup = times_allmodes_2017$cof_int_freqsup*100



#-------------------------------------------------------------#
#               Plots 2017                                    #  
#                                                             #
#-------------------------------------------------------------#

## Commuting times 2017
pdf(file ="Output/Figures/HistogramTimes_2017.pdf") 
ggplot(times_allmodes_2017, aes(x = Modes_final, y = Mean, fill = Modes_final)) +
  geom_bar(stat = "identity", alpha = 0.5, fill = "orange") +
  geom_text(aes(label = round(Mean, 2)), vjust = -0.5, color = "black", size = 3) +
  labs(title = "", x = "Category", y = "Minutes") +
  theme_minimal()
dev.off()

## Frequencies 2012
pdf(file ="Output/Figures/HistogramFrequency_2017.pdf") 
ggplot(times_allmodes_2017, aes(x = Modes_final, y = frequency, fill = Modes_final)) +
  geom_bar(stat = "identity", alpha = 0.5, fill = "orange") +
  geom_text(aes(label = round(frequency, 2)), vjust = -0.5, color = "black", size = 3) +
  labs(title = "", x = "Category", y = "Frequency") +
  theme_minimal()
dev.off()


# #-----------------------------------------------------------#
#               Combining graphs                              #  
#                                                             #
#-------------------------------------------------------------#

# Creating a year variable
times_allmodes_2012$year = 2012 
times_allmodes_2017$year = 2017

# Appending the data
times_allmodes = rbind(times_allmodes_2012, times_allmodes_2017)

colnames(times_allmodes) = c("mode", "time", "number", "sd", "share", "cinf", "csup",
                             "shareinf", "sharesup", "year")

#colnames(times_allmodes) = c("mode", "time", "number", "share", "year")
data = times_allmodes 
data$share = data$share*100

###Saving a file with the data

write.csv(data, "Base/modes_shares_times.csv", row.names = FALSE)

## Graph for travel times
time = data %>% filter(!mode %in% c("Others")) %>% 
  ggplot(., aes(x=fct_relevel(mode, "Metro", "Bus", "Metroplus", "Car", "Motorcycle", "Taxi", 
                              "Bicycle", "Walking"),
                y=time, fill=as.character(year))) +
  geom_bar(stat="identity", position=position_dodge(0.9)) +
  labs(x="Modes", y="Travel times (minutes)", fill = "") +
  geom_text(aes(label=round(time)), vjust= 1.6, color="white",
            position = position_dodge(0.9), size = 2.5)+
  # geom_errorbar(data = data[data$mode != "Others", ], aes(x=fct_relevel(mode, "Metro", "Bus", "Metroplus", "Car", "Motorcycle", "Taxi", 
  #                                                        "Bicycle", "Walking"), 
  #                                          ymin = cinf, ymax = csup), 
  #               color = "blue", size = 0.2,  width= 0.2, position=position_dodge(.9)) +
  #scale_fill_brewer(palette="Paired") +
  scale_fill_manual(values=c("#fecc5c","#fd8d3c")) +
  scale_y_continuous(limits = c(0, 62), breaks = c(0, 20, 40, 60)) +
  theme_minimal() +
  theme(legend.position = c(0.865,0.875),
        legend.key.size = unit(0.6,"cm"),
        legend.text=element_text(size=12),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold"))

ggsave("Output/Figures/Histograma_time.png",
       width = 15, height = 12, units = "cm", dpi = 300, bg="white")


## Graph for frequencies
share = data  %>% filter(!mode %in% c("Others")) %>% 
  ggplot(., aes(x=fct_relevel(mode, "Metro", "Bus", "Metroplus", "Car", "Motorcycle", "Taxi", 
                              "Bicycle", "Walking"),
                y=share, fill=as.character(year))) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x="Modes", y="(%) Participation", fill = "") +
  geom_text(aes(label=round(share)), vjust = 1.6, color="white",
            position = position_dodge(0.9), size=2.5)+
  #scale_fill_brewer(palette="Paired") +
  scale_fill_manual(values=c("#fecc5c","#fd8d3c")) +
  scale_y_continuous(limits = c(0, 42), breaks = c(0, 15, 30, 40)) +
  theme_minimal() +
  theme(legend.position = "none",
        legend.key.size = unit(0.6,"cm"),
        legend.text=element_text(size=12),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold"))


ggsave("Output/Figures/Histograma_frequency.png",
       width = 15, height = 12, units = "cm", dpi = 300, bg="white")





    
    
    
    
    
    
    
    
    
    
    


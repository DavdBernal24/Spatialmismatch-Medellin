### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medell?n, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


#### Purpose: This code creates a data frame of the accessibility 
# measure accross different groups. 

#Required libraries


## Data
library(sf) #Allows to manipulate spatial data
library(dplyr) #Allows to manipulate data
library(readr) #Read csv files
library(readxl) #Read excel files


# Library to work with spatial data
library(rgdal)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#               1.Loading Data                                #  
#                                                             #
#-------------------------------------------------------------#


# Loading the Map with  SIT Zones and strata data
sit_zones = st_read("Data/Shapefiles/SITs_with_estrato/SITs_with_estrato.shp") #Maps for 2017

#Loading communes shapefile
comunas = st_read("Data/Shapefiles/Communes/Comunas.shp")
comunas = subset(comunas, select = c(Cod_Comuna, geometry))

# Ensure both shapefiles have the same CRS
comunas = st_transform(comunas, st_crs(sit_zones))

##Getting the comuna ID
sit_zones <- st_intersection(sit_zones, comunas) %>%
  mutate(intersection_area = st_area(.)) %>%
  group_by(id_sit) %>%  # Assuming "ID_shp1" uniquely identifies shp1
  slice_max(order_by = intersection_area, n = 1) %>%  # Keep the largest overlap
  ungroup() %>%
  dplyr::select(id_sit, Cod_Comuna, estrt_s)  # Keep only relevant columns


sit_zones = st_drop_geometry(sit_zones)



##### Accessibility measures
Results_Private_2017 = read.csv("Output/Results/New_Ta_Private_2017.csv")
Results_Public_2017 = read.csv("Output/Results/New_Ta_Public_2017.csv")
Results_Private_2012 = read.csv("Output/Results/New_Ta_Private_2012.csv")
Results_Public_2012 = read.csv("Output/Results/New_Ta_Public_2012.csv")


#Renaming variables for the RBIND
names(Results_Private_2017) = c("k", "ta", "percap")
names(Results_Public_2017) = c("k", "ta", "percap")
names(Results_Private_2012) = c("k", "ta", "percap")
names(Results_Public_2012) = c("k", "ta", "percap")

# Year and Mode
Results_Private_2017$mode = "Private"
Results_Private_2017$year = 2017

Results_Private_2012$mode = "Private"
Results_Private_2012$year = 2012

Results_Public_2012$mode = "public"
Results_Public_2012$year = 2012

Results_Public_2017$mode = "public"
Results_Public_2017$year = 2017



#append 
Results = rbind(Results_Private_2017, Results_Public_2017, Results_Private_2012, Results_Public_2012)


### Loading shares
shares = read_excel("Base/MedXcomunaGEIH2011_2017.Xlsx")
shares$year = ifelse(shares$year == 2011, 2012, shares$year)

shares$comuna = as.numeric(shares$comuna) #Numeric for the merge

#-------------------------------------------------------------#
#               2. Merging all Data                           #  
#                                                             #
#-------------------------------------------------------------#

# Strata to main results. 
Results = merge(Results, sit_zones, by.x = "k", by.y = "id_sit", all.x = TRUE)


#Renaming variables
Results = Results %>%
  rename(comuna = Cod_Comuna, 
         SIT = k, 
         strata = estrt_s)

Results$comuna = as.numeric(Results$comuna)

##Adding the commune level data to the Results
Results = merge(Results, shares, by = c("year", "comuna"), all.x = TRUE) 

##Dropping NAs
Results = Results[!is.na(Results$strata), ]


#####Saving
write.csv(Results, "Base/Heterogeneity.csv")


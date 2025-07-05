

####Better or Worse Job Accessibility? Understanding Changes in Spatial Mismatch at the Intra-urban Level in Medellín

### This is a master code that replicates all the estimations and figures from the paper


## Working directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


rm(list = ls())



## -----------------------------------------------------

# 1. I find the equivalence of SIT zone in 2012 and 2017
source("1_shapes_equivalence.R", encoding = "utf-8")

# 2. This code gets the travel times for public transport using the Google API in 2020 for the 2017 SIT zones

#source("2_Google_Times.R", encoding = "utf-8")

# 3. This code gets the travel times for private transport using the Microsoft Bing API in 2019 for the 2017 SIT zones

#source("3_Bing_Times.R", encoding = "utf-8")

# 4. This code filters the transportation modes and additionally filters the OD survey for 2012, it also compute
# the reported travel times 
source("4_EOD_2012.R") 

# 5. This code filters the transportation modes and additionally filters the OD survey for 2017, it also compute
# the reported travel times and adds the expansion factors provided by RISE
source("5_EOD_2017.R") 


# 6. This code cuts some shapefiles for the city, the roads and the Metro lines
source("6_Shapefile_Medellin_Fix.R")

#7. Getting the population densities, we use data from Alcaldia de Medellin, you can find it on this link. 
# http://medata.gov.co/medell%C3%ADn-en-cifras/proyecci%C3%B3n-poblaci%C3%B3n-2016-2020, we also make some maps of it
source("7_Population_Density.R")

#8. Income distribution, we use the the strata lvl in Colombia
source("8_Socioeconomic_Strata.R")

#9. This code computes travel times differences between 2017 and 2012, we will use it later to fill the 2012 travel
#times, this code is at commune level
source("9_Time_Differences_Communes.R") 


#10. This code creates maps of travel times differences at commune level
source("10_Map_Times_Differences.R")


#11. This code computes travel times differences between 2017 and 2012, we will use it later to fill the 2012 travel
#times, this code is at SIT zone level
source("11_Time_Differences_SITs.R") 


#12. It estimates the travel times in 2012 using the information from Google, Bing, the network and the reported
#travel times. 
source("12_Matrixes_2012.R") 


#13. Here I find the distance of a SIT centroid to the nearest point in the polygon and to the farthest point then I compute the average, 
# I do this in order to compute the travel times inside a zone eventually
source("13_Average_Distance.R")


#14. This code computes employment in each SIT zone using the employment equation from the paper
source("14_Employment_Data.R")

#15.Using this code we get the wage per-min from the "Encuesta de calidad de vida"
source("15_Wages_ECV.R") 


#16. This code computes decriptives for reported travel times
source("16_Reported_Descriptives.R") 

#17. This code computes decriptives for computed travel times
source("17_Computed_Descriptives_Times.R")


#18. This code creates Maps for computed travel times in 2017
source("18_Maps_Computedtimes_2017.R")

#19. This code creates Maps for computed travel times in 2012
source("19_Maps_Computedtimes_2012.R")

#20. This code computes wages by socioeconomic strata. However, 
# at the end we used a fixed wage Wt that only changes in time
source("20_Wages_Strata.R")

#21. This code computes discrete cost measures of public and private
# Transportation regressions, this how we initially used to compute the costs
source("21_Discrete_Cost.R")

#22. Computes Accessibility measure for private transport 2017
source("22_TA_Private_2017.R")

#23. Computes Accessibility measure for private transport 2012
source("23_TA_Private_2012.R")

#24. Computes Accessibility measure for public transport 2017
source("24_TA_Public_2017.R")

#25. Computes Accessibility measure for public transport 2012
source("25_TA_Public_2012.R")

#26. Creates descriptives fo the accessibility using discrete cost (tables B1 and B2)
source("26_Descriptives_DistancesDiscrete_TA.R")

#27. This code computes dependent and independent employment for 2017 
source("27_Emp_ind_dep.R")

#28. This code creates maps for employment for 2017 and 2012 and also for dependent
# and independent employment in 2017
source("28_Emp_ind_dep_Maps.R")

#29. Creates histograms by mode of transportation using OD data, it creates for both travel times and participation
source("29_Modes_Histograms.R")
























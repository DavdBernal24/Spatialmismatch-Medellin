

####Better or Worse Job Accessibility? Understanding Changes in Spatial Mismatch at the Intra-urban Level in Medell�n

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

#30 This code does the same as code 9 but using morning rush data
source("30_TimesDifferences_Communes_Early.R")

#31 This code does the same as code 11 but using morning rush data
source("31_TimeDifferences_SIT_Early.R")

#32 This code does the same as code 12 but using morning rush data
source("32_Matrixes_2012_Early.R")

#33 This code 
source("33_Sensitivity Distance.R")

#34 This code does the same as code 12 but using morning rush data
source("34_Binsreg_Costs.R")

#35 This code computes the Accessibility measure for private transport in 2017 using the new cost
source("35_NewTA_Private_2017.R")

#36. This code computes the Accessibility measure for private transport in 2012 using the new cost
source("36_NewTA_Private_2012.R")

#37. This code computes the Accessibility measure for public transport in 2017 using the new cost
source("37_NewTA_Public_2017.R")

#38. This code computes the Accessibility measure for public transport in 2012 using the new cost
source("38_NewTA_Public_2012.R")

#39. This code computes descriptive stats for  the main results of the paper
source("39_Descriptives_NewTA.R")

#40. This code creates breaks for the new accessibility measure maps
source("40_Breaks_NewTA.R")

#41. This code creates maps for the new accessibility measure
source("41_Maps_NewTA.R")

#42. This code computes the accessibility measure using morning rush hour data for private transport in 2012
source("42_NewTA_Private_Early_2012.R")

#43. This code computes the accessibility measure using morning rush hour data for public transport in 2012
source("43_NewTA_Public_Early_2012.R")

#44. This code descriptive stats for the accessibility measure using morning rush data
source("44_DescriptivesNewTA_MorningRush.R")

#45. This code prepares employment data for the bootstrap
source("45_Build_Individual_Employment.R")

#46. This code computes the Bootstrap for private transport (It takes a while to run)
source("46_Dependent_Bootstrap_TA_Private.R")

#47. This code computes the Bootstrap for public transport (It takes a while to run)
source("47_Dependent_Bootstrap_TA_Publice.R")

#48. This code organizes data from the GEIH
##Note: you have to run this code in R 4.1.0 or above! 
#Otherwise the package "summarytools" won't work
source("48_MedXcomunaGEIH2011_2017.R")

#49. It mergers SIT zones data with socioeconomic strata data
source("49_Merge_SITs_estrato.R")

#50. It creates a data set with the measure by different groups. 

############## STATA CODES
library(RStata)
options("RStata.StataPath" = "C:/Program Files/Stata18/StataMP-64.exe")  # adjust path to your system
options("RStata.StataVersion" = 18)

#Note: Please, also fix your directories on the do-files

#51. Creates figures in Stata, hetereogenous effects of the measure
stata("C:/Github_Folders/Spatial_Mismatch_Rep/51_Figure_B5,do", data.in = NULL, data.out = FALSE, stata.echo = TRUE)


#52. Creates more figures in Stata, hetereogenous effects of the measure
stata("52_Figure_B6,do", data.in = NULL, data.out = FALSE, stata.echo = TRUE)
























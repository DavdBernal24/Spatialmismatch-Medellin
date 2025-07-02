

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


















#11. It creates a map representing people who live and work inside the same commune
source("11_Commune_Employment.R")

#12. This code cuts the shapes for Medellin, deleting info of surrounding areas we do not use. 
source("12_Fixing_Shapes.R")

#13.Using this code we get the wage per-min from the "Encuesta de calidad de vida"
source("13_Wages_ECV.R") # I made a small change on this code

# 14. Here I identify the socioeconomic strata of each SIT (We did not end up using it)

source("14_Identifying_estrato.R")

#15. Here I find the distance of a SIT centroid to the nearest point in the polygon, we do this in order to compute the 
# the travel times inside a same zone
source("13_Average_Distance.R")

#16. computes the employment data using the equation we propose on the paper. 
source("16_Employment_data.R")

#17. Computes the accessibility measure for private transport in 2017
source("17_TA_privado_2017_wmin.R")

#18. Computes the accessibility measure for private transport in 2012
source("18_TA_privado_2012_wmin.R")

#19. Computes the accessibility measure for public transport in 2017
source("19_TA_publico_2017_wmin.R")

#20. Computes the accessibility measure for public transport in 2012
source("20_TA_publico_2012_wmin.R")

#21. Computes the accessibility measure for walking(network) in 2017
source("21_TA_pie_2017_wmin.R")

#22. Computes the accessibility measure for walking(network) in 2012
source("22_TA_pie_2012_wmin.R")

#22.5 Creates constant breaks for the maps
source("22_5_Breaks_Maps.R")

#23. Computes the accessibility measure for walking(network) in 2012
source("23_Maps_for_ta.R")

#24. Creates maps using the computed employment from step 16. 
source("24_Maps_for_employment.R")

#25. It creates maps that describe the commuting patterns from the OD survey
source("25_Map_journeys.R")

#26. Creates a .tex table with descriptive stats for the commuting times
source("26_Descriptves_times.R")





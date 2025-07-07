### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medell?n, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


# Here we restructure the cost using binreg (We create a continues public and private transport cost)

#### Purpose: This code computes the public transport predicted number of modes

## Data
library(binsreg) # Run a binned regression
library(dplyr) #Allows to manipulate data
library(readr) #Read csv files
library(readxl) #Read excel files
library(ggplot2) #Figures




## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#          1.Loading Data and Running Regression              #  
#                                                             #
#-------------------------------------------------------------#


data_2012 = read.csv("Base/pre-bins_2012.csv")
data_2017 = read.csv("Base/pre-bins_2017.csv")

## Running the Binsreg

reg_2012 = binsreg(Number_modes, dist, data = data_2012, polyreg = 1)

pdf(file = paste("Output/Figures/Binsreg_2012.pdf", sep = ""))
reg_2012$bins_plot + 
  labs(x = "Distance (km)", y = "Number of Modes")
dev.off()



reg_2017 = binsreg(Number_modes, dist, data = data_2017, polyreg = 1)

pdf(file = paste("Output/Figures/Binsreg_2017.pdf", sep = ""))
reg_2017$bins_plot + 
  labs(x = "Distance (km)", y = "Number of Modes")
dev.off()


# ## Creating data frame with the info for the estimation
#  data_2012_binsreg = data.frame(reg_2012$data.plot$`Group Full Sample`$data.bin$left.endpoint, reg_2012$data.plot$`Group Full Sample`$data.bin$right.endpoint, 
#                            reg_2012$data.plot$`Group Full Sample`$data.dots$fit)
# ###
#  data_2017_binsreg = data.frame(reg_2017$data.plot$`Group Full Sample`$data.bin$left.endpoint, reg_2017$data.plot$`Group Full Sample`$data.bin$right.endpoint, 
#                        reg_2017$data.plot$`Group Full Sample`$data.dots$fit)
#  
 
 

# #Renaming data
 # colnames(data_2012_binsreg) = c("min_dist", "max_dist", "prediction_modes")
 # colnames(data_2017_binsreg) = c("min_dist", "max_dist", "prediction_modes")

#Linear regression
linear_reg2012 = lm(Number_modes ~ dist, weights = PrimeroDeFactorExpansion,  data = data_2012)
linear_reg2017 = lm(Number_modes ~ dist, weights = factor_expansion, data = data_2017)

summary(linear_reg2012)
summary(linear_reg2017)

#Linear Regression 2
#linear_reg2012_check = lm(Number_modes ~ dist, data = data_2012)
#linear_reg2017_check = lm(Number_modes ~ dist, data = data_2017)


# ggplot(data_2017, aes(x = dist, y = Number_modes)) +
#   geom_point() + 
#   geom_smooth(method = "lm", se = FALSE, color = "blue") +
#   labs(title = "Linear Regression", x = "X", y = "Y") +
#   theme_minimal()
# 
# fitted_values = fitted(linear_reg2017_check)
# 
# print(fitted_values)

## Loading distances data
distances = read.csv("Base/Distances_long.csv")

#-------------------------------------------------------------#
#  2.Predicting Number of modes with Linear reg and binsreg   #  
#                                                             #
#-------------------------------------------------------------#


####### Binsreg Number of Modes
##Adding values
# distances_2012 = distances %>%
#   rowwise() %>%
#   mutate(prediction_modes_2012 = data_2012_binsreg$prediction_modes[which(dist >= data_2012_binsreg$min_dist & dist <= data_2012_binsreg$max_dist)[1]]) %>%
#   ungroup()
# 
# 
# 
# distances_all = distances_2012 %>%
#   rowwise() %>%
#   mutate(prediction_modes_2017 = data_2017_binsreg$prediction_modes[which(dist >= data_2017_binsreg$min_dist & dist <= data_2017_binsreg$max_dist)[1]]) %>%
#   ungroup()

##Same SIT trips
# distances_all$prediction_modes_2012 = ifelse(distances_all$dist == 0, 1, distances_all$prediction_modes_2012)
# distances_all$prediction_modes_2017 = ifelse(distances_all$dist == 0, 1, distances_all$prediction_modes_2017)


####### Linear Regression distance vs more than 1 mode
distances$prediction_modes_2012 = predict(linear_reg2012, newdata= distances)
distances$prediction_modes_2017 = predict(linear_reg2017, newdata= distances)

##Same SIT trips
distances$prediction_modes_2012 = ifelse(distances$dist == 0, 1, distances$prediction_modes_2012)
distances$prediction_modes_2017 = ifelse(distances$dist == 0, 1, distances$prediction_modes_2017)



# Since it is a linear model I am capping the probabilities.
#distances$prediction_modes_2012 = pmax(0, pmin(1, distances$prediction_modes_2012))
#distances$prediction_modes_2017 = pmax(0, pmin(1, distances$prediction_modes_2017))





#-------------------------------------------------------------#
#          3.Costs                                            #  
#                                                             #
#-------------------------------------------------------------#

##Using bins reg
##Computing the cost
# distances_all = distances_all %>%
#   mutate(cost_2012_public = 1600*prediction_modes_2012, 
#          cost_2017_public = 2000*distances_all$prediction_modes_2017, 
#          cost_2012_private = cost_2012_public*2.18, 
#          cost_2017_private = cost_2017_public*2.18)


#Note: 
#Right now the predictions we have are probabilities.
#Therefore, to compute the cost we use the expected value

## Using linear reg
distances_all = distances %>%
  mutate(cost_2012_public = 1600*prediction_modes_2012, 
         cost_2017_public = 2000*prediction_modes_2017, 
         cost_2012_private = cost_2012_public*2.18, 
         cost_2017_private = cost_2017_public*2.18)



#-------------------------------------------------------------#
#          4.Saving Data                                      #  
#                                                             #
#-------------------------------------------------------------#

write.csv(distances_all, "Base/Transport_costs.csv", row.names = FALSE)




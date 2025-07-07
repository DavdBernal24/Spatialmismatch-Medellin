### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medell�n, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 


# REFEREE COMMENT ABOUT MORNING RUSH HOUR


### This code creates descriptives about the computations using morning rush data
# This code also uses the New accessibility data with the new transport cost function. 

#Required libraries

# These libraries help you to read and transform data
library(readr)
library(dplyr)
library(stargazer)


## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#               1.Loading data and doing                      #
#                 some transformations                        #                                   
#-------------------------------------------------------------#

##Morning rush results
public_2012_morning = read.csv("Output/Results/New_Ta_Public_2012Early5.csv")
private_2012_morning = read.csv("Output/Results/New_Ta_Private_2012Early5.csv")

public_2012_morning = public_2012_morning %>%
  rename(ta = tapub2012, ta_percap = tapub2012_percap)

private_2012_morning = private_2012_morning %>%
  rename(ta = tapriv2012, ta_percap = tapriv2012_percap)


## Creating variables for the append
public_2012_morning$status = "morning"
private_2012_morning$status = "morning"
public_2012_morning$mode = "public"
private_2012_morning$mode = "private"

##Original results
public_2012 = read.csv("Output/Results/New_Ta_Public_2012.csv")
private_2012 = read.csv("Output/Results/New_Ta_Private_2012.csv")

public_2012 = public_2012 %>%
  rename(ta = tapub2012, ta_percap = tapub2012_percap)

private_2012 = private_2012 %>%
  rename(ta = tapriv2012, ta_percap = tapriv2012_percap)


## Creating variables for the append
public_2012$status = "original"
private_2012$status = "original"
public_2012$mode = "public"
private_2012$mode = "private"





#-------------------------------------------------------------#
#               2.Appending and Descriptives                  #
#                                                             #                                   
#-------------------------------------------------------------#

# Creating a list
ta_measure = list(public_2012,public_2012_morning, private_2012, private_2012_morning)

## Appending
result = do.call(rbind, ta_measure) 

result = result %>%
  group_by(mode, status) %>%
  summarize(across(c(ta, ta_percap), mean, na.rm = TRUE))

# Round the numeric columns in the dataset to 2 decimal places
result = result %>%
  mutate(across(where(is.numeric), round, 2))

# Rename 'status' to 'Travel Time' rename Mode
result <- result %>%
  rename(`Time of the Day` = status, 
         Accessibility = ta) %>%
  mutate(Mode = ifelse(mode == "public", "Public", "Private")) %>%
  select(Mode, `Time of the Day`, Accessibility, ta_percap) %>%
  rename(`Adjusted Accessibility` = ta_percap)


result <- result %>%
  mutate(`Time of the Day` = case_when(
    `Time of the Day` == "original" ~ "Entire day",
    `Time of the Day` == "morning" ~ "Morning",
    TRUE ~ `Time of the Day`
  ))

# Generate the LaTeX table with stargazer
stargazer(result, 
          summary = FALSE, 
          rownames = FALSE, 
          title = "Accessibility with All Day Data and Morning Rush Data", 
          label = "tab:accessibility_per_mode_time", 
          style = "aer",
          colnames = TRUE,
          out = "Output/Tex/NewTA_AccessibilityEarly_per_mode_time.tex")

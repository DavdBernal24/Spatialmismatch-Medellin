### Project: Better or worse job accessibility? Understanding changes 
#in spatial mismatch at the intra-urban level: evidence from  Medell?n, Colombia
### Authors: David Bernal; Gustavo Garcia; Jorge Perez
### Code created by : David Bernal
### Last modification : 
### Modified by: 

# REFEREE COMMENT ABOUT THE MEASURE USING DIFFERENT DISTANCES


### This code computes Descriptive statistics for the measure using different
# distance thresholds. 



#Required libraries


# These libraries help you to read and transform data
library(dplyr)
library(readxl)
library(readr)
library(stargazer)


# Library to work with spatial data
library(rgdal)

## Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls())

##############################################################################

#-------------------------------------------------------------#
#        1. Reading all the Files                             #  
#                                                             #
#-------------------------------------------------------------#

distance = c(7,8, 9, 10, 11, 12, 13)

########-------------- Private accessibility

folder_path = "Output/Results"

# Creating an empty list to store the data 
private_accessibility = list()

# Loop over the years and district numbers
for (year in c(2012, 2017)) {
  for (dist in unique(distance)) {
    # Construct the file name
    file_name = paste0("Ta_private_dist", dist, "_", year, ".csv")
    file_path = file.path(folder_path, file_name) # Full path to the file
    
    # Check if the file exists before reading
    if (file.exists(file_path)) {
      # Read the CSV and store it in the list with a meaningful name
      temp = read.csv(file_path)
      
    }
    
    temp$year = year # Year variable
    temp$distance = dist # Distance variable
    
    colnames(temp) = c("k", "private", "private_percap", "year", "distance")
  
    
    private_accessibility[[paste0("dist", dist, "_", year)]] = temp
  }
}


## Appending all the files
private_accessibility = bind_rows(private_accessibility)

## Descriptives private
private_accessibility = private_accessibility %>%
  group_by(distance, year) %>%
  summarize(
    accessibility_private = mean(private, na.rm = TRUE), 
    adjusted_accessibility_private = mean(private_percap, na.rm = TRUE),
    count = n()
  )



########-------------- public accessibility


# Creating an empty list to store the data 
public_accessibility = list()

# Loop over the years and district numbers
for (year in c(2012, 2017)) {
  for (dist in unique(distance)) {
    # Construct the file name
    file_name = paste0("Ta_public_dist", dist, "_", year, ".csv")
    file_path = file.path(folder_path, file_name) # Full path to the file
    
    # Check if the file exists before reading
    if (file.exists(file_path)) {
      # Read the CSV and store it in the list with a meaningful name
      temp = read.csv(file_path)
      
    }
    
    temp$year = year # Year variable
    temp$distance = dist # Distance variable
    
    colnames(temp) = c("k", "public", "public_percap", "year", "distance")
    
    
    public_accessibility[[paste0("dist", dist, "_", year)]] = temp
  }
}


## Appending  files
public_accessibility = bind_rows(public_accessibility)


## Descriptives public
public_accessibility = public_accessibility %>%
  group_by(distance, year) %>%
  summarize(
    accessibility_public = mean(public, na.rm = TRUE),
    adjusted_accessibility_public = mean(public_percap, na.rm = TRUE),
    count = n()
  )



##Mutate to round outputs 

private_accessibility <- private_accessibility %>%
  mutate(
    accessibility_private = format(round(accessibility_private, 3), nsmall = 3),
    adjusted_accessibility_private = format(round(adjusted_accessibility_private, 3), nsmall = 3),
    distance = format(round(distance, 1), nsmall = 1),
    year = as.character(year),
    count = as.character(count)
  )




public_accessibility <- public_accessibility %>%
  mutate(
    accessibility_public = format(round(accessibility_public, 3), nsmall = 3),
    adjusted_accessibility_public = format(round(adjusted_accessibility_public, 3), nsmall = 3),
    distance = format(round(distance, 1), nsmall = 1),
    year = as.character(year),
    count = as.character(count)
  )

## Generate LaTeX tables using stargazer
stargazer(private_accessibility, summary = FALSE, rownames = FALSE,
          title = "Private accessibility measures across distance thresholds", label = "tab:private",
          covariate.labels = c("Distance", "Year", "Accessiility", "Adjusted Accessibility", "Count"),
          out = "Output/Tex/private_accessibility_distance.tex")


stargazer(public_accessibility, summary = FALSE, rownames = FALSE,
          title = "Public accessibility measures across distance thresholds", label = "tab:public",
          covariate.labels = c("Distance", "Year", "Accessiility", "Adjusted Accessibility", "Count"),
          out = "Output/Tex/public_accessibility_distance.tex")




















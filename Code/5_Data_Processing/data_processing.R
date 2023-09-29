#Data cleaning and processing of Alaska WQP pull 

#Written by: Hannah Ferriby
#Date Created: 9-29-2023
#Date of Last Updated:

##Required Inputs:
#1. csv outputs from data_pull.R broken up by site type


####Set Up####
library(TADA)
library(tidyverse)


####Load Data####
#Find all file names that end in .csv from the data_pull output folder
csv_names <- list.files('Data/data_pull', pattern = '.csv', full.names = T)

#Read in csvs and combine into one table
all_input_data <- tibble()
for(i in 1:length(csv_names)) {
  csv <- read_csv(csv_names[i])
  all_input_data <- all_input_data %>%
    rbind(csv)
  remove(csv)
}

#clean up environment
remove(i, csv_names)



####1. Check Result Unit Validity####
data_1 <- TADA_FlagResultUnit(all_input_data, clean = 'none') #required

####2. Check Sample Fraction Validity####
data_2 <- TADA_FlagFraction(data_1, clean = F) #required

####3. Check Method Speciation Validity####
data_3 <- TADA_FlagSpeciation(data_2, clean = 'none') #required

####4. Harmonize Characteristic Names####
data_4 <- TADA_HarmonizeSynonyms(data_3)

####5. Flag unrealistic values####
data_5a <- TADA_FlagAboveThreshold(data_4, clean = F)
data_5b <- TADA_FlagBelowThreshold(data_4, clean = F)

####6. Convert all depth to meters####
data_6 <- TADA_ConvertDepthUnits(data_5b, unit = 'm', transform = T)

####7. Find continuous data####
data_7 <- TADA_FindContinuousData(data_6, clean = F)

####8. Check method flags####
data_8 <- TADA_FlagMethod(data_7, clean = F)

####9. Find potential duplictes####
#Buffer distance set to 50 m, can change
data_9a <- TADA_FindPotentialDuplicatesMultipleOrgs(data_7, dist_buffer = 50) 
data_9b <- TADA_FindPotentialDuplicatesSingleOrg(data_9a, handling_method = 'none')

####10. Find QC samples####
data_10 <- TADA_FindQCActivities(data_9b, clean = F)

####11. Flag invalid coordinates####
data_11 <- TADA_FlagCoordinates(data_10, clean_outsideUSA = 'no')

####12. Find any 'SUSPECT' samples####
data_12 <- TADA_FlagMeasureQualifierCode(data_11, clean = F)

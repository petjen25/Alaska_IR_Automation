#Data cleaning and processing of Alaska WQP pull 

#Written by: Hannah Ferriby
#Date Created: 9-29-2023
#Date of Last Updated: 10-2-2023

##Required Inputs:
#1. csv outputs from data_pull.R broken up by site type


####Set Up####
library(TADA)
library(tidyverse)


####Load Data####
#Find all file names that end in .csv from the data_pull output folder
csv_names1 <- list.files('Data/data_pull', pattern = '.csv', full.names = T)
csv_names <- csv_names1[!str_detect(csv_names1, pattern = 'all')]
  
#Read in csvs and combine into one table
all_input_data <- tibble()
for(i in 1:length(csv_names)) {
  csv <- read_csv(csv_names[i])
  all_input_data <- all_input_data %>%
    rbind(csv)
  remove(csv)
}

#clean up environment
remove(i, csv_names, csv_names1)

####Identify Flags####

#####1. Check Result Unit Validity#####
data_1 <- TADA_FlagResultUnit(all_input_data, clean = 'none') #required

#####2. Check Sample Fraction Validity#####
data_2 <- TADA_FlagFraction(data_1, clean = F) #required

#####3. Check Method Speciation Validity#####
data_3 <- TADA_FlagSpeciation(data_2, clean = 'none') #required

#####4. Harmonize Characteristic Names#####
data_4 <- TADA_HarmonizeSynonyms(data_3)

#####5. Flag unrealistic values#####
data_5a <- TADA_FlagAboveThreshold(data_4, clean = F)
data_5b <- TADA_FlagBelowThreshold(data_5a, clean = F)

#####6. Find continuous data#####
data_6 <- TADA_FindContinuousData(data_5b, clean = F)

#####7. Check method flags#####
data_7 <- TADA_FlagMethod(data_6, clean = F)

#####8. Find potential duplictes#####
#Buffer distance set to 50 m, can change
data_8a <- TADA_FindPotentialDuplicatesMultipleOrgs(data_7, dist_buffer = 50) 
data_8b <- TADA_FindPotentialDuplicatesSingleOrg(data_8a, handling_method = 'none')

#####9. Find QC samples#####
data_9 <- TADA_FindQCActivities(data_8b, clean = F)

#####10. Flag invalid coordinates#####
data_10 <- TADA_FlagCoordinates(data_9, clean_outsideUSA = 'no')

#####11. Find any 'SUSPECT' samples#####
data_11 <- TADA_FlagMeasureQualifierCode(data_10, clean = F)

#####12. Replace non-detects#####
data_12 <- TADA_SimpleCensoredMethods(data_11, 
                                      nd_method = 'multiplier',
                                      nd_multiplier = 0.5)

#Export data with flags
write_csv(data_12, 'Data/all_data_with_flags.csv')



####Remove Flags####
#####1. Check Result Unit Validity#####
data_c_1 <- TADA_FlagResultUnit(all_input_data, clean = 'both') #required

#####2. Check Sample Fraction Validity#####
data_c_2 <- TADA_FlagFraction(data_c_1, clean = T) #required

#####3. Check Method Speciation Validity#####
data_c_3 <- TADA_FlagSpeciation(data_c_2, clean = 'both') #required

#####4. Harmonize Characteristic Names#####
data_c_4 <- TADA_HarmonizeSynonyms(data_c_3)

#####5. Flag unrealistic values#####
data_c_5a <- TADA_FlagAboveThreshold(data_c_4, clean = T)
data_c_5b <- TADA_FlagBelowThreshold(data_c_5a, clean = T)

#####6. Find continuous data#####
data_c_6 <- TADA_FindContinuousData(data_c_5b, clean = T)

#####7. Check method flags#####
data_c_7 <- TADA_FlagMethod(data_c_6, clean = T)

#####8. Find potential duplictes#####
#Buffer distance set to 50 m, can change
data_c_8a <- TADA_FindPotentialDuplicatesMultipleOrgs(data_c_7, dist_buffer = 50) 
data_c_8b <- TADA_FindPotentialDuplicatesSingleOrg(data_c_8a, handling_method = 'pick_one')

#####9. Find QC samples#####
data_c_9 <- TADA_FindQCActivities(data_c_8b, clean = T)

#####10. Flag invalid coordinates#####
data_c_10 <- TADA_FlagCoordinates(data_c_9, clean_outsideUSA = 'no')

#####11. Find any 'SUSPECT' samples#####
data_c_11 <- TADA_FlagMeasureQualifierCode(data_c_10, clean = T)

#####12. Replace non-detects#####
data_c_12 <- TADA_SimpleCensoredMethods(data_c_11, 
                                        nd_method = 'multiplier',
                                        nd_multiplier = 0.5)







####Remove Flags####
data_wo_flags <- data_12 %>%
  #Remove QC samples
  filter(TADA.ActivityType.Flag == 'Non_QC') %>%
  #Remove non-water samples
  filter(TADA.ActivityMediaName == 'WATER') %>%
  #Remove censored data
  filter(TADA.CensoredData.Flag == 'Uncensored') %>%
  #Remove duplicate data
  case_when()

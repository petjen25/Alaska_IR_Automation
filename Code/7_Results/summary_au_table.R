#Function to create table with designated use, constituent, magnitude,
#frequency, and duration for the relevant parameters in a given AU

#Written by: Hannah Ferriby

#Required libraries
library(tidyverse)

#Test data
categorize_output <- read_csv('Output/results/categorized_aus_20240222.csv')
au_id_crosswalk <- read_csv('Data/data_analysis/AUID_crosswalk.csv')
previous_au_attains <- read_xlsx('Data/data_analysis/ATTAINS_AK_AsessmentUnits_DataDownload_20240126.xlsx', sheet = 2)


table_cat <- categorize_output %>% 
  select(AUID_ATTNS, `Waterbody Type`, Use, Use_Category) %>%
  unique()

#Combine sample data with AUID crosswalk
data_cat_AUID_added <- table_cat %>%
  left_join(au_id_crosswalk, by = c('AUID_ATTNS' = 'Active_AUID'))


#Join with previous AU ATTAINS by each AUID type
data_current_AU <- data_cat_AUID_added %>%
  mutate(assessmentUnitId = AUID_ATTNS) %>%
  inner_join(previous_au_attains, by = c('assessmentUnitId'))

#Find retired AUs
data_retired_AU <- data_cat_AUID_added %>%
  mutate(assessmentUnitId = Retired_AUID) %>%
  inner_join(previous_au_attains, by = c('assessmentUnitId'))

#Find historical AUs
data_historical_AU <- data_cat_AUID_added %>%
  mutate(assessmentUnitId = Historical_AUID) %>%
  inner_join(previous_au_attains, by = c('assessmentUnitId'))

#Combine all together
data_all_AUs_listed <- data_current_AU %>%
  rbind(data_retired_AU) %>%
  rbind(data_historical_AU)

#Find AUs not in previous ATTAINS
data_current_AU_not_listed <- data_cat_AUID_added %>%
  mutate(assessmentUnitId = AUID_ATTNS) %>%
  full_join(previous_au_attains, by = c('assessmentUnitId')) %>%
  filter(!assessmentUnitId %in% data_current_AU$assessmentUnitId) %>%
  filter(!assessmentUnitId %in% data_retired_AU$assessmentUnitId) %>%
  filter(!assessmentUnitId %in% data_historical_AU$assessmentUnitId)

#Combine all AU information together
#Create new column overallStatus that is just the string version of state
#numerical categories
data_all_AUs <- data_all_AUs_listed %>% 
  rbind(data_current_AU_not_listed) 

#Summarize data
final_summary <- data_all_AUs %>%
  mutate(overallStatus = case_when(Use_Category == 2 ~ 'Fully Supporting',
                                   Use_Category == 3 ~ 'Not Assessed',
                                   Use_Category == 5 ~ 'Not Supporting',
                                   T ~ NA)) %>% 
  select(AUID_ATTNS, `Waterbody Type`, Use, Use_Category, overallStatus, 
         assessmentUnitName, locationDescription, waterSize, waterSizeUnits) %>%
  unique() 

write_csv(data_all_AUs, 'Output/results/summary_au_tables_20240320.csv')

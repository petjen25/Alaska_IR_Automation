##Create output ATTAINS for Assessment Unit
##Based on template version 1.4 updated on 7/18/2022


#Written by Hannah Ferriby
#Date updated: 3/13/2024


####Load Packages####
library(tidyverse)
library(readxl)

####Load Data####
data_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20240117.csv')
au_id_crosswalk <- read_csv('Data/data_analysis/AUID_crosswalk.csv')
previous_au_attains <- read_xlsx('Data/data_analysis/ATTAINS_AK_AsessmentUnits_DataDownload_20240126.xlsx', sheet = 2)
samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20240117.csv')

#Combine sample data with AUID crosswalk
data_sufficiency_AUID_added <- data_sufficiency %>%
  left_join(au_id_crosswalk, by = c('AUID_ATTNS' = 'Active_AUID'))


#Join with previous AU ATTAINS by each AUID type
data_current_AU <- data_sufficiency_AUID_added %>%
  mutate(assessmentUnitId = AUID_ATTNS) %>%
  inner_join(previous_au_attains, by = c('assessmentUnitId'))

#Find retired AUs
data_retired_AU <- data_sufficiency_AUID_added %>%
  mutate(assessmentUnitId = Retired_AUID) %>%
  inner_join(previous_au_attains, by = c('assessmentUnitId'))

#Find historical AUs
data_historical_AU <- data_sufficiency_AUID_added %>%
  mutate(assessmentUnitId = Historical_AUID) %>%
  inner_join(previous_au_attains, by = c('assessmentUnitId'))

#Combine all together
#This becomes the starting point for each csv export process
data_all_AUs_listed <- data_current_AU %>%
  rbind(data_retired_AU) %>%
  rbind(data_historical_AU)

#Find AUs not in previous ATTAINS
data_current_AU_not_listed <- data_sufficiency_AUID_added %>%
  mutate(assessmentUnitId = AUID_ATTNS) %>%
  full_join(previous_au_attains, by = c('assessmentUnitId')) %>%
  filter(!assessmentUnitId %in% data_current_AU$assessmentUnitId) %>%
  filter(!assessmentUnitId %in% data_retired_AU$assessmentUnitId) %>%
  filter(!assessmentUnitId %in% data_historical_AU$assessmentUnitId)

data_all_AUs <- data_all_AUs_listed %>% 
  rbind(data_current_AU_not_listed)

####Assessment Units####
assessment_units <- data_all_AUs %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         USE_CLASS_NAME = AU_Type,
         ASSESSMENT_UNIT_NAME = assessmentUnitName,
         LOCATION_DESCRIPTION = locationText) %>%
  mutate(ASSESSMENT_UNIT_STATE = 'AK',
         ASSESSMENT_UNIT_AGENCY = 'S', #S for state agency
         ASSESSMENT_UNIT_COMMENT = NA) %>% #ASK ABOUT COMMENTS
  select(ASSESSMENT_UNIT_ID, ASSESSMENT_UNIT_NAME, ASSESSMENT_UNIT_STATE,
         ASSESSMENT_UNIT_AGENCY, ASSESSMENT_UNIT_COMMENT, LOCATION_DESCRIPTION,
         USE_CLASS_NAME) %>% #Change order to reflect template
  unique() 

#Some AUs have duplicates from having use class and some having NA
duplicate_values_na <- assessment_units %>% 
  group_by_at(vars(-USE_CLASS_NAME)) %>% #Find duplicate rows based on one having
  #a use class and another having NA
  filter(n() > 1) %>%
  #filter out NA values 
  filter(is.na(USE_CLASS_NAME))

#Remove all NA duplicated values in the assessment
combine_assessment_units <- assessment_units %>%
  anti_join(duplicate_values_na, by = c('ASSESSMENT_UNIT_ID', 'ASSESSMENT_UNIT_NAME', 'ASSESSMENT_UNIT_STATE',
                                        'ASSESSMENT_UNIT_AGENCY', 'ASSESSMENT_UNIT_COMMENT', 'LOCATION_DESCRIPTION',
                                        'USE_CLASS_NAME'))

write_csv(combine_assessment_units, 'Output/results/ATTAINS/AU_Batch_Upload/Assessment_Units.csv')

####Water Types####
water_types <- data_all_AUs %>%
  select(AUID_ATTNS, waterType, waterSize, waterSizeUnits, sizeSource, sourceScale) %>%
  mutate(ESTIMATION_METHOD = NA) %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         WATER_TYPE = waterType,
         WATER_SIZE = waterSize,
         WATER_UNIT = waterSizeUnits,
         SIZE_SOURCE = sizeSource,
         SOURCE_SCALE = sourceScale) %>%
  select(ASSESSMENT_UNIT_ID, WATER_TYPE, WATER_SIZE, WATER_UNIT, SIZE_SOURCE, 
         ESTIMATION_METHOD, SOURCE_SCALE) %>% #Reorder columns
  unique()

write_csv(water_types, 'Output/results/ATTAINS/AU_Batch_Upload/Water_Types.csv')

####Locations####
locations <- data_all_AUs %>%
  select(AUID_ATTNS, locationTypeCode, locationText, locationDescription) %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         LOCATION_TYPE_CODE = locationTypeCode,
         LOCATION_TYPE_CONTEXT = locationDescription,
         LOCATION_TEXT = locationText) %>%
  unique()

write_csv(locations, 'Output/results/ATTAINS/AU_Batch_Upload/Locations.csv')

####Monitoring Stations####
monitoring_stations <- samples %>%
  select(AUID_ATTNS, ActivityConductingOrganizationText, MonitoringLocationIdentifier) %>%
  unique() %>%
  filter(!is.na(AUID_ATTNS)) %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         MS_ORG_ID = ActivityConductingOrganizationText,
         MS_LOCATION_ID = MonitoringLocationIdentifier)

write_csv(monitoring_stations, 'Output/results/ATTAINS/AU_Batch_Upload/Monitoring_Stations.csv')

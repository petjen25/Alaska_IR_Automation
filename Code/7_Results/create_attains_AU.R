##Create output ATTAINS for Assessment Unit
##Based on template version 1.4 updated on 7/18/2022


#Written by Hannah Ferriby
#Date updated: 4/4/2024


####Load Packages####
library(tidyverse)
library(readxl)
library(sf)
library(stringi)

####Load Data####
data_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20240117.csv')
# au_id_crosswalk <- read_csv('Data/data_analysis/AUID_crosswalk.csv')
previous_au_attains <- read_xlsx('Data/data_analysis/ATTAINS_AK_AsessmentUnits_DataDownload_20240126.xlsx', sheet = 2)
samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20240117.csv')
ml_au_crosswalk <- read_csv('Data/data_processing/ML_AU_Crosswalk.csv')

lake_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/lakes.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, Shape_Leng, Shape_Area) %>%
  st_zm() 

river_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/rivers.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, Shape_Leng, AU_Miles) %>%
  rename(Shape_Area = AU_Miles) %>%
  st_zm()

marine_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/marine.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, Shape_Leng, Shape_Area) %>%
  st_zm() %>%
  st_transform(crs = st_crs(lake_aus)) #Marine AU is in NAD83/Alaska Albers
#All other shapefiles are in WGS 84/Pseudo-Mercator

beach_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/beaches.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, Shape_Leng, AU_Miles) %>%
  rename(Shape_Area = AU_Miles) %>%
  st_zm()

#Combine aus into one large table, add HUC10 if missing
all_aus <- lake_aus %>%
  rbind(river_aus) %>%
  rbind(marine_aus) %>%
  rbind(beach_aus) %>%
  mutate(HUC10_ID = case_when(is.na(HUC10_ID) ~
                                paste0('190',
                                       stri_extract_first_regex(AUID_ATTNS,
                                                                '[0-9]_')),
                              T ~ HUC10_ID))


# #Combine sample data with AUID crosswalk
# data_sufficiency_AUID_added <- data_sufficiency %>%
#   left_join(au_id_crosswalk, by = c('AUID_ATTNS' = 'Active_AUID'))
# 
# 
# #Join with previous AU ATTAINS by each AUID type
# data_current_AU <- data_sufficiency_AUID_added %>%
#   mutate(assessmentUnitId = AUID_ATTNS) %>%
#   inner_join(previous_au_attains, by = c('assessmentUnitId'))
# 
# #Find retired AUs
# data_retired_AU <- data_sufficiency_AUID_added %>%
#   mutate(assessmentUnitId = Retired_AUID) %>%
#   inner_join(previous_au_attains, by = c('assessmentUnitId'))
# 
# #Find historical AUs
# data_historical_AU <- data_sufficiency_AUID_added %>%
#   mutate(assessmentUnitId = Historical_AUID) %>%
#   inner_join(previous_au_attains, by = c('assessmentUnitId'))
# 
# #Combine all together
# #This becomes the starting point for each csv export process
# data_all_AUs_listed <- data_current_AU %>%
#   rbind(data_retired_AU) %>%
#   rbind(data_historical_AU)

#Find AUs info not in previous ATTAINS
data_current_AU_not_listed <- data_sufficiency %>%
  mutate(assessmentUnitId = AUID_ATTNS) %>%
  anti_join(previous_au_attains, by = c('assessmentUnitId')) %>%
  #Join with AU shapefile information
  left_join(all_aus, by = 'AUID_ATTNS') 

####Assessment Units####
assessment_units <- data_current_AU_not_listed %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         USE_CLASS_NAME = AU_Type,
         ASSESSMENT_UNIT_NAME = Name_AU,
         LOCATION_DESCRIPTION = HUC10_ID) %>%
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

write_csv(combine_assessment_units, 'Output/results/ATTAINS/AU_Batch_Upload/Assessment_Units.csv',
          na="")

####Water Types####
water_types <- data_current_AU_not_listed %>%
  select(AUID_ATTNS, AU_Type, Shape_Area) %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         WATER_SIZE = Shape_Area) %>%
  mutate(         WATER_TYPE = toupper(AU_Type),
                  WATER_UNIT = case_when(AU_Type == 'RIVER' | AU_Type == 'BEACH' ~
                                           'Miles',
                                         T ~ 'Square Miles')) %>%
  select(ASSESSMENT_UNIT_ID, WATER_TYPE, WATER_SIZE, WATER_UNIT) %>% #Reorder columns
  unique()

write_csv(water_types, 'Output/results/ATTAINS/AU_Batch_Upload/Water_Types.csv',
          na="")

####Locations####
locations <- data_current_AU_not_listed %>%
  select(AUID_ATTNS, HUC10_ID) %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         LOCATION_TYPE_CONTEXT = HUC10_ID) %>% #To be filled in manually be AK DEC
  mutate(LOCATION_TYPE_CODE = 'HUC-10',
         LOCATION_TEXT = NA) %>%
  select(ASSESSMENT_UNIT_ID, LOCATION_TYPE_CODE, LOCATION_TYPE_CONTEXT, 
         LOCATION_TEXT) %>%
  unique()

write_csv(locations, 'Output/results/ATTAINS/AU_Batch_Upload/Locations.csv',
          na="")

####Monitoring Stations####
ml_names <- combine_assessment_units %>%
  left_join(ml_au_crosswalk, by = c('ASSESSMENT_UNIT_ID' = 'AUID_ATTNS'))

monitoring_stations <- ml_names %>%
  select(ASSESSMENT_UNIT_ID, MonitoringLocationIdentifier) %>%
  unique() %>%
  rename(MS_LOCATION_ID = MonitoringLocationIdentifier) %>%
  mutate(MS_ORG_ID = 'AKDECWQ') %>%
  select(ASSESSMENT_UNIT_ID, MS_ORG_ID, MS_LOCATION_ID)

write_csv(monitoring_stations, 'Output/results/ATTAINS/AU_Batch_Upload/Monitoring_Stations.csv',
          na="")

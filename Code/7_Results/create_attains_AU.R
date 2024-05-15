##Create output ATTAINS for Assessment Unit
##Based on template version 1.4 updated on 7/18/2022


#Written by Hannah Ferriby
#Date updated: 5/14/2024


####Load Packages####
library(tidyverse)
library(readxl)
library(sf)
library(stringi)

####Load Data####
data_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20240509.csv')

previous_au_attains <- read_xlsx('Data/data_analysis/ATTAINS_AK_AsessmentUnits_DataDownload_20240126.xlsx', sheet = 2)

samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20240509.csv')

ml_au_crosswalk <- read_csv('Data/data_processing/ML_AU_Crosswalk.csv')

lake_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/lakes.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, AU_Area) %>%
  st_zm() %>%
  mutate(Shape_4_Summary = AU_Area,
         AU_Shape_Unit = 'acres') %>%
  select(!AU_Area)

river_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/rivers.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, AU_Miles) %>%
  st_zm() %>%
  mutate(Shape_4_Summary = AU_Miles,
         AU_Shape_Unit = 'miles') %>%
  select(!AU_Miles)

marine_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/marine.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, AU_Area) %>%
  st_zm() %>%
  st_transform(crs = st_crs(lake_aus)) %>%
  mutate(Shape_4_Summary = AU_Area,
         AU_Shape_Unit = 'square miles') %>%
  select(!AU_Area)#Marine AU is in NAD83/Alaska Albers
#All other shapefiles are in WGS 84/Pseudo-Mercator

beach_aus <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/beaches.shp') %>%
  select(AUID_ATTNS, Name_AU, HUC10_ID, AU_Miles) %>%
  mutate(Shape_4_Summary = AU_Miles,
         AU_Shape_Unit = 'miles') %>%
  select(!AU_Miles)

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

#Find AUs info not in previous ATTAINS
data_current_AU_not_listed <- data_sufficiency %>%
  mutate(assessmentUnitId = AUID_ATTNS) %>%
  anti_join(previous_au_attains, by = c('assessmentUnitId')) %>%
  #Join with AU shapefile information
  left_join(all_aus, by = 'AUID_ATTNS') %>%
  #Add HUC10 if missing
  mutate(HUC10_ID = ifelse(is.na(HUC10_ID),
                                 paste0('190', str_extract(AUID_ATTNS, "[0-9.]+")),
                                 HUC10_ID))

####Assessment Units####
assessment_units <- data_current_AU_not_listed %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         ASSESSMENT_UNIT_NAME = Name_AU) %>%
  mutate(USE_CLASS_NAME = NA,
         LOCATION_DESCRIPTION = paste0('Located in HUC', HUC10_ID,', ',
                                       round(Shape_4_Summary, 3), ' ',
                                       AU_Shape_Unit, '.'),
         ASSESSMENT_UNIT_STATE = 'AK',
         ASSESSMENT_UNIT_AGENCY = 'S', #S for state agency
         ASSESSMENT_UNIT_COMMENT = NA) %>% #ASK ABOUT COMMENTS
  select(ASSESSMENT_UNIT_ID, ASSESSMENT_UNIT_NAME, ASSESSMENT_UNIT_STATE,
         ASSESSMENT_UNIT_AGENCY, ASSESSMENT_UNIT_COMMENT, LOCATION_DESCRIPTION,
         USE_CLASS_NAME) %>% #Change order to reflect template
  unique() 

write_csv(assessment_units, 'Output/results/ATTAINS/AU_Batch_Upload/Assessment_Units.csv',
          na="")

####Water Types####
water_types <- data_current_AU_not_listed %>%
  select(AUID_ATTNS, AU_Type, Shape_4_Summary, AU_Shape_Unit, Name_AU) %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         WATER_SIZE = Shape_4_Summary) %>%
  mutate(WATER_TYPE = ifelse(AU_Type == 'River', 'STREAM/CREEK/RIVER', toupper(AU_Type)),
         WATER_TYPE = case_when(AU_Type == 'MARINE' & str_detect(Name_AU, 'Harbor') ~
                                 'HARBOR',
                                AU_Type == 'MARINE' & str_detect(Name_AU, 'Estuary') ~
                                  'ESTUARY',
                                AU_Type == 'MARINE' & str_detect(Name_AU, 'Lagoon') ~
                                  'LAGOON',
                                AU_Type == 'MARINE' & (str_detect(Name_AU, 'Channel') |
                                                         str_detect(Name_AU, 'Bay') )~
                                  'CHANNEL AND BAY',
                                AU_Type == 'MARINE' ~
                                  'COASTAL',
                                T ~ WATER_TYPE), 
         WATER_UNIT = AU_Shape_Unit) %>%
  select(ASSESSMENT_UNIT_ID, WATER_TYPE, WATER_SIZE, WATER_UNIT) %>% #Reorder columns
  unique()

write_csv(water_types, 'Output/results/ATTAINS/AU_Batch_Upload/Water_Types.csv',
          na="")

####Locations####
locations <- data_current_AU_not_listed %>%
  select(AUID_ATTNS, HUC10_ID) %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS) %>% 
  mutate(LOCATION_TYPE_CODE = 'HUC-10',
         LOCATION_TYPE_CONTEXT = 'AKDECWQ',
         LOCATION_TEXT = paste0('HUC',HUC10_ID)) %>% #Add 190
  select(ASSESSMENT_UNIT_ID, LOCATION_TYPE_CODE, LOCATION_TYPE_CONTEXT, 
         LOCATION_TEXT) %>%
  unique()

write_csv(locations, 'Output/results/ATTAINS/AU_Batch_Upload/Locations.csv',
          na="")

####Monitoring Stations####
ml_names <- assessment_units %>%
  left_join(ml_au_crosswalk, by = c('ASSESSMENT_UNIT_ID' = 'AUID_ATTNS'))

monitoring_stations <- ml_names %>%
  select(ASSESSMENT_UNIT_ID, MonitoringLocationIdentifier) %>%
  unique() %>%
  rename(MS_LOCATION_ID = MonitoringLocationIdentifier) %>%
  mutate(MS_ORG_ID = 'AKDECWQ') %>%
  select(ASSESSMENT_UNIT_ID, MS_ORG_ID, MS_LOCATION_ID)

write_csv(monitoring_stations, 'Output/results/ATTAINS/AU_Batch_Upload/Monitoring_Stations.csv',
          na="")

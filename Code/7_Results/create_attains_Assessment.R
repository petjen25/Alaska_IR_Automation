##Create output ATTAINS for Assessment 
##Based on template version 1.4 updated on 7/18/2022


#Written by Hannah Ferriby
#Date updated: 4/4/2024


####Load Packages####
library(tidyverse)
library(readxl)


####Load Data####
au_id_crosswalk <- read_csv('Data/data_analysis/AUID_crosswalk.csv')
previous_assessment_attains <- read_xlsx('Data/data_analysis/ATTAINS_AK_Asessments_DataDownload_20240126.xlsx', sheet = 2)
samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20240117.csv')
categorized_aus <- read_csv('Output/results/categorized_aus_20240222.csv')


#Combine sample data with AUID crosswalk
data_category_AUID_added <- categorized_aus %>%
  left_join(au_id_crosswalk, by = c('AUID_ATTNS' = 'Active_AUID'))


#Join with previous AU ATTAINS by each AUID type
data_current_AU <- data_category_AUID_added %>%
  mutate(assessmentUnitId = AUID_ATTNS) %>%
  right_join(previous_assessment_attains, by = c('assessmentUnitId'))

#Find retired AUs
data_retired_AU <- data_category_AUID_added %>%
  mutate(assessmentUnitId = Retired_AUID) %>%
  filter(!is.na(assessmentUnitId)) %>%
  right_join(previous_assessment_attains, by = c('assessmentUnitId'))

#Find historical AUs
data_historical_AU <- data_category_AUID_added %>%
  mutate(assessmentUnitId = Historical_AUID) %>%
  filter(!is.na(assessmentUnitId)) %>%
  right_join(previous_assessment_attains, by = c('assessmentUnitId'))

#Combine all together
data_all_AUs1 <- data_current_AU %>%
  rbind(data_retired_AU) %>%
  rbind(data_historical_AU) %>%
  unique()

#Find AUs not in previous ATTAINS
data_current_AU_not_listed <- data_category_AUID_added %>%
  mutate(assessmentUnitId = AUID_ATTNS) %>%
  full_join(previous_assessment_attains, by = c('assessmentUnitId')) %>%
  filter(!assessmentUnitId %in% data_current_AU$assessmentUnitId) %>%
  filter(!assessmentUnitId %in% data_retired_AU$assessmentUnitId) %>%
  filter(!assessmentUnitId %in% data_historical_AU$assessmentUnitId)

#Bind rows together to ensure all current AUs represented
#This becomes the starting point for each csv export process
data_all_AUs <- data_all_AUs1 %>% 
  rbind(data_current_AU_not_listed)

####Assessments####
monitoring_year <- samples %>%
  select(AUID_ATTNS, ActivityStartDate) %>%
  group_by(AUID_ATTNS) %>%
  reframe(AUID_ATTNS = AUID_ATTNS,
          YEAR_LAST_MONITORED = year(max(ActivityStartDate))) %>%
  unique()

assessments <- data_all_AUs %>%
  select(AUID_ATTNS, organizationId, reportingCycle) %>%
  left_join(monitoring_year, by = 'AUID_ATTNS') %>%
  mutate(AGENCY_CODE = 'S') %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS, 
         CYCLE_LAST_ASSESSED = reportingCycle) %>% #Pulling from previous year's ATTAINS
  unique() %>%
  select(ASSESSMENT_UNIT_ID, AGENCY_CODE, CYCLE_LAST_ASSESSED, YEAR_LAST_MONITORED)

write_csv(assessments, 'Output/results/ATTAINS/Assessment_Batch_Upload/Assessments.csv',
          na="")

####Uses####
monitoring_dates <- samples %>%
  select(AUID_ATTNS, ActivityStartDate) %>%
  group_by(AUID_ATTNS) %>%
  reframe(AUID_ATTNS = AUID_ATTNS,
          USE_MONITORING_START = min(ActivityStartDate),
          USE_MONITORING_END = max(ActivityStartDate)) %>%
  unique()

uses <- data_all_AUs %>%
  filter(!is.na(Use)) %>%
  #Following mutate code from Jenny Petitt
  mutate(ATTAINS_USE = 
           case_when(Use == "Human Health" & `Use Description` == "Water and Aquatic Organisms" ~ "WATER SUPPLY",
                     Use == "Human Health" & `Use Description` == "Water & Aquatic Organisms" ~ "WATER SUPPLY",
                     Use == "Water Supply" ~ "WATER SUPPLY",    
                     `Use Description` == "Harvesting" | `Use Description` == "Marine Harvesting" ~ "HARVESTING FOR CONSUMPTION OF RAW MOLLUSKS OR OTHER RAW AQUATIC LIFE",
                     `Use Description` == "Growth and propagation" | Use == "Aquatic Life" ~ "GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE",
                     `Use Description` == "Aquatic Organisms Only" | `Use Description` == "Aquatic Organisms only" ~ "GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE",  
                     Use == "Primary Contact Recreation" | Use == "Secondary Contact Recreation" | Use == "Secondary Contact recreation" ~ "WATER RECREATION"),
         ATTAINS_DESCRIPTION = 
           case_when(`Use Description` == "Water & Aquatic Organisms" | `Use Description` == "Water and Aquatic Organisms" |
                       `Use Description` == "Drinking" | `Use Description` == "Drinking Water" | `Use Description` == "Drinking water" ~ "DRINKING, CULINARY, AND FOOD PROCESSING",
                     `Use Description` == "Irrigation " |`Use Description` == "Agriculture" | `Use Description` == "Irrigation" | `Use Description` == "Irrigation Water" | 
                       `Use Description` == "Stock water" | `Use Description` == "Stock Water" | `Use Description` == "Stockwater" ~ "AGRICULTURE, INCLUDING IRRIGATION AND STOCK WATERING",
                     Use == "Primary Contact Recreation" ~ "CONTACT RECREATION",
                     Use == "Secondary Contact Recreation" | Use == "Secondary Contact recreation" ~ "SECONDARY RECREATION",
                     `Use Description` == "Seafood Processing" ~ "SEAFOOD PROCESSING",
                     `Use Description` == "Industrial" ~ "INDUSTRIAL",
                     `Use Description` == "Aquaculture" ~ "AQUACULTURE"),
         `Waterbody Type` = toupper(`Waterbody Type`), 
         PARAM_USE_NAME = paste(`Waterbody Type`, ATTAINS_USE, ATTAINS_DESCRIPTION, sep = ' / '),
         PARAM_USE_NAME = gsub(" / NA", "", PARAM_USE_NAME)) %>%
  #End of Jenny code
  select(AUID_ATTNS, AUID_ATTNS, PARAM_USE_NAME, Use_Category) %>%
  left_join(monitoring_dates, by = 'AUID_ATTNS') %>%
  unique() %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         USE_NAME = PARAM_USE_NAME) %>% #MATCH TO ATTAINS CODES
  mutate(USE_ATTAINMENT_CODE = case_when(Use_Category == 5 ~
                                           "N", #Not supporting
                                         Use_Category == 2 ~
                                           "F", #Fully supporting
                                         Use_Category == 3 ~
                                           "I", #Insufficient Information
                                         T ~
                                           "X"), #Not assessed
         USE_AGENCY_CODE = "S",
         USE_TREND = NA, #Here down are optional columns
         USE_THREATENED = NA,
         USE_ASMT_BASIS = NA,
         USE_ASMT_DATE = NA,
         USE_ASSESSOR_NAME = NA,
         USE_COMMENT = NA,
         USE_STATE_IR_CAT = NA,
         USE_ORG_QUALIFIER_FLAG = NA) %>%
  select(!Use_Category) %>%
  select(ASSESSMENT_UNIT_ID, USE_NAME, USE_AGENCY_CODE, USE_TREND, USE_THREATENED,
         USE_ASMT_BASIS, USE_MONITORING_START, USE_MONITORING_END, USE_ASMT_DATE,
         USE_ASSESSOR_NAME, USE_COMMENT, USE_STATE_IR_CAT, 
         USE_ORG_QUALIFIER_FLAG)
  
write_csv(uses, 'Output/results/ATTAINS/Assessment_Batch_Upload/Uses.csv',
          na="")

####Parameters####
parameters <- data_all_AUs %>%
  filter(!is.na(Use)) %>%
  #Following mutate code from Jenny Petitt
  mutate(ATTAINS_USE = 
           case_when(Use == "Human Health" & `Use Description` == "Water and Aquatic Organisms" ~ "WATER SUPPLY",
                     Use == "Human Health" & `Use Description` == "Water & Aquatic Organisms" ~ "WATER SUPPLY",
                     Use == "Water Supply" ~ "WATER SUPPLY",    
                     `Use Description` == "Harvesting" | `Use Description` == "Marine Harvesting" ~ "HARVESTING FOR CONSUMPTION OF RAW MOLLUSKS OR OTHER RAW AQUATIC LIFE",
                     `Use Description` == "Growth and propagation" | Use == "Aquatic Life" ~ "GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE",
                     `Use Description` == "Aquatic Organisms Only" | `Use Description` == "Aquatic Organisms only" ~ "GROWTH AND PROPAGATION OF FISH, SHELLFISH, OTHER AQUATIC LIFE AND WILDLIFE",  
                     Use == "Primary Contact Recreation" | Use == "Secondary Contact Recreation" | Use == "Secondary Contact recreation" ~ "WATER RECREATION"),
         ATTAINS_DESCRIPTION = 
           case_when(`Use Description` == "Water & Aquatic Organisms" | `Use Description` == "Water and Aquatic Organisms" |
                       `Use Description` == "Drinking" | `Use Description` == "Drinking Water" | `Use Description` == "Drinking water" ~ "DRINKING, CULINARY, AND FOOD PROCESSING",
                     `Use Description` == "Irrigation " |`Use Description` == "Agriculture" | `Use Description` == "Irrigation" | `Use Description` == "Irrigation Water" | 
                       `Use Description` == "Stock water" | `Use Description` == "Stock Water" | `Use Description` == "Stockwater" ~ "AGRICULTURE, INCLUDING IRRIGATION AND STOCK WATERING",
                     Use == "Primary Contact Recreation" ~ "CONTACT RECREATION",
                     Use == "Secondary Contact Recreation" | Use == "Secondary Contact recreation" ~ "SECONDARY RECREATION",
                     `Use Description` == "Seafood Processing" ~ "SEAFOOD PROCESSING",
                     `Use Description` == "Industrial" ~ "INDUSTRIAL",
                     `Use Description` == "Aquaculture" ~ "AQUACULTURE"),
         `Waterbody Type` = toupper(`Waterbody Type`), 
         PARAM_USE_NAME = paste(`Waterbody Type`, ATTAINS_USE, ATTAINS_DESCRIPTION, sep = ' / '),
         PARAM_USE_NAME = gsub(" / NA", "", PARAM_USE_NAME)) %>%
  #End of Jenny code
  select(AUID_ATTNS, TADA.CharacteristicName, PARAM_USE_NAME, Individual_Category) %>%
  mutate(PARAM_STATUS_NAME = case_when(Individual_Category == 5 ~ #CATEGORIES ARE UNCLEAR
                                           "Observed effect", 
                                       Individual_Category == 2 ~
                                           "Meeting Criteria", 
                                       Individual_Category == 3 ~
                                           "Insufficient Information", 
                                         T ~
                                           "X")) %>%
  mutate(PARAM_ATTAINMENT_CODE = case_when(Individual_Category == 5 ~ #CATEGORIES ARE UNCLEAR
                                             "Not meeting criteria", 
                                           Individual_Category == 2 ~
                                             "Meeting Criteria", 
                                           Individual_Category == 3 ~
                                             "Not enough information", 
                                           T ~
                                             "Not applicable"),
         PARAM_TREND = NA,
         PARAM_COMMENT = NA,
         PARAM_AGENCY_CODE = NA,
         PARAM_POLLUTANT_INDICATOR = NA, #Should be Y/N
         PARAM_YEAR_LISTED = NA,
         PARAM_TARGET_TMDL_DATE = NA,
         PARAM_EXPECTED_TO_ATTAIN = NA,
         PARAM_PRIORITY_RANKING = NA, 
         PARAM_CONSENT_DECREE_CYCLE = NA,
         PARAM_ALT_LISTING_ID = NA,
         PARAM_ORG_QUALIFIER_FLAG = NA,
         PARAM_DELISTING_REASON = NA,
         PARAM_DELISTING_COMMENT = NA,
         PARAM_DELISTING_AGENCY = NA) %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         PARAM_NAME = TADA.CharacteristicName) %>%
  unique() %>%
  select(ASSESSMENT_UNIT_ID, PARAM_NAME, PARAM_USE_NAME, PARAM_STATUS_NAME,
         PARAM_ATTAINMENT_CODE, PARAM_TREND, PARAM_COMMENT, PARAM_AGENCY_CODE,
         PARAM_POLLUTANT_INDICATOR, PARAM_YEAR_LISTED, PARAM_TARGET_TMDL_DATE,
         PARAM_EXPECTED_TO_ATTAIN, PARAM_PRIORITY_RANKING,
         PARAM_CONSENT_DECREE_CYCLE, PARAM_ALT_LISTING_ID, 
         PARAM_ORG_QUALIFIER_FLAG, PARAM_DELISTING_REASON, 
         PARAM_DELISTING_COMMENT, PARAM_DELISTING_AGENCY)
         

write_csv(parameters, 'Output/results/ATTAINS/Assessment_Batch_Upload/Parameters.csv',
          na="")
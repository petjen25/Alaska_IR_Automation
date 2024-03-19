##Create output ATTAINS for Assessment 
##Based on template version 1.4 updated on 7/18/2022


#Written by Hannah Ferriby
#Date updated: 3/13/2024


####Load Packages####
library(tidyverse)
library(readxl)


####Load Data####
data_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20240117.csv')
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
  right_join(previous_assessment_attains, by = c('assessmentUnitId'))

#Find historical AUs
data_historical_AU <- data_category_AUID_added %>%
  mutate(assessmentUnitId = Historical_AUID) %>%
  right_join(previous_assessment_attains, by = c('assessmentUnitId'))

#Combine all together
#This becomes the starting point for each csv export process
data_all_AUs <- data_current_AU %>%
  rbind(data_retired_AU) %>%
  rbind(data_historical_AU)


####Assessments####
assessments <- data_all_AUs %>%
  select(AUID_ATTNS, organizationId, reportingCycle, Use_Category, Use) %>%
  mutate(ASSESSMENT_COMMENT = Use,
         AGENCY_CODE = 'S',
         YEAR_LAST_MONITORED = '2023',
         ASSESSMENT_RATIONALE = NA,
         TROPHIC_STATUS = NA) %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS, 
         CYCLE_LAST_ASSESSED = reportingCycle, #Pulling from previous year's ATTAINS
         ASSESSMENT_UNIT_STATE_IR_CAT = Use_Category) %>%
  unique() %>%
  select(!Use) %>%
  select(ASSESSMENT_UNIT_ID, AGENCY_CODE, CYCLE_LAST_ASSESSED, YEAR_LAST_MONITORED,
         ASSESSMENT_UNIT_STATE_IR_CAT, ASSESSMENT_COMMENT, ASSESSMENT_RATIONALE,
         TROPHIC_STATUS)

write_csv(assessments, 'Output/results/ATTAINS/Assessment_Batch_Upload/Assessments.csv')

####Uses####
monitoring_dates <- samples %>%
  select(AUID_ATTNS, ActivityStartDate) %>%
  group_by(AUID_ATTNS) %>%
  reframe(AUID_ATTNS = AUID_ATTNS,
          USE_MONITORING_START = min(ActivityStartDate),
          USE_MONITORING_END = max(ActivityStartDate)) %>%
  unique()

uses <- data_all_AUs %>%
  select(AUID_ATTNS, AUID_ATTNS, Use, Use_Category) %>%
  left_join(monitoring_dates, by = 'AUID_ATTNS') %>%
  unique() %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         USE_NAME = Use) %>%
  mutate(USE_ATTAINMENT_CODE = case_when(Use_Category == 5 ~
                                           "N", #Not supporting
                                         Use_Category == 2 ~
                                           "F", #Fully supporting
                                         Use_Category == 3 ~
                                           "I", #Insufficient Information
                                         T ~
                                           "X"), #Not assessed
         USE_AGENCY_CODE = "S",
         USE_TREND = NA,
         USE_THREATENED = NA,
         USE_ASMT_BASIS = NA,
         USE_ASMT_DATE = NA,
         USE_ASSESSOR_NAME = NA,
         USE_COMMENT = NA,
         USE_STATE_IR_CAT = Use_Category,
         USE_ORG_QUALIFIER_FLAG = NA) %>%
  select(!Use_Category) %>%
  select(ASSESSMENT_UNIT_ID, USE_NAME, USE_AGENCY_CODE, USE_TREND, USE_THREATENED,
         USE_ASMT_BASIS, USE_MONITORING_START, USE_MONITORING_END, USE_ASMT_DATE,
         USE_ASSESSOR_NAME, USE_COMMENT, USE_STATE_IR_CAT, 
         USE_ORG_QUALIFIER_FLAG)
  
write_csv(uses, 'Output/results/ATTAINS/Assessment_Batch_Upload/Uses.csv')


####Assessment Types####

data_all_AUs %>% select(`Constituent Group`) %>% unique()

#`Constituent Group`           
#1 Bacteria                      
#2 pH                            
#3 Turbidity                     
#4 NA                            
#5 Dissolved Gas                 
#6 Dissolved Inorganic Substances
#7 Sediment                      
#8 Toxics 

assessment_types <- data_all_AUs %>%
  select(AUID_ATTNS, Use, `Constituent Group`) %>%
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS,
         USE_NAME = Use) %>%
  mutate(USE_ASMT_TYPE = NA, #PULL FROM PREVIOUS ATTAINS assessmentTypes
         USE_ASMT_CONFIDENCE = NA) %>%
  select(!c(`Constituent Group`)) %>%
  unique()

write_csv(assessment_types, 'Output/results/ATTAINS/Assessment_Batch_Upload/Assessment_Types.csv')

####Assessment Method Types####
assessment_method_types <- data_all_AUs %>%
  select(AUID_ATTNS, Use)


write_csv(assessment_method_types, 'Output/results/ATTAINS/Assessment_Batch_Upload/Assessment_Method_Types.csv')

####Parameters####
parameters <- data_all_AUs %>%
  select(AUID_ATTNS, TADA.CharacteristicName, Use, Use_Category, Individual_Category) %>%
  mutate(PARAM_STATUS_NAME = case_when(Use_Category == 5 ~ #CATEGORIES ARE UNCLEAR
                                           "N", 
                                         Use_Category == 2 ~
                                           "Meeting Criteria", 
                                         Use_Category == 3 ~
                                           "Insufficient Information", 
                                         T ~
                                           "X"),
         PARAM_TREND = NA,
         PARAM_COMMENT = NA,
         PARAM_AGENCY_CODE = 'S',
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
         PARAM_NAME = TADA.CharacteristicName,
         PARAM_USE_NAME = Use,
         PARAM_ATTAINMENT_CODE = Individual_Category,
         PARAM_STATE_IR_CAT = Use_Category) %>%
  unique() %>%
  select(ASSESSMENT_UNIT_ID, PARAM_NAME, PARAM_USE_NAME, PARAM_STATUS_NAME,
         PARAM_ATTAINMENT_CODE, PARAM_TREND, PARAM_COMMENT, PARAM_AGENCY_CODE,
         PARAM_POLLUTANT_INDICATOR, PARAM_YEAR_LISTED, PARAM_TARGET_TMDL_DATE,
         PARAM_EXPECTED_TO_ATTAIN, PARAM_PRIORITY_RANKING,
         PARAM_CONSENT_DECREE_CYCLE, PARAM_ALT_LISTING_ID, 
         PARAM_ORG_QUALIFIER_FLAG, PARAM_DELISTING_REASON, 
         PARAM_DELISTING_COMMENT, PARAM_DELISTING_AGENCY)
         

write_csv(parameters, 'Output/results/ATTAINS/Assessment_Batch_Upload/Parameters.csv')


####Seasons####
seasons <- data_all_AUs %>%
  select(AUID_ATTNS) #No seasonality in attainment decisions


####Sources####
sources <- data_all_AUs %>%
  select()


####Associated-Actions####
associated_actions <- data_all_AUs %>%
  select()
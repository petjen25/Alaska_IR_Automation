##Create output ATTAINS for Assessment 
##Based on template version 1.4 updated on 7/18/2022


#Written by Hannah Ferriby
#Date updated: 5/14/2024


####Load Packages####
library(tidyverse)
library(readxl)


####Load Data####
previous_assessment_attains <- read_csv('Data/data_analysis/assessments.csv')

samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20250127.csv')

categorized_aus <- read_csv('Output/results/categorized_aus_20250127.csv') %>%
  filter(!is.na(Individual_Category))

overall_categorized_aus <- read_csv('Output/results/overall_categorized_aus_20250127.csv')

####Lists of AUs per overall Categories####

cat_5_aus <- overall_categorized_aus %>%
  filter(Overall_Category == "5") %>%
  select(ASSESSMENT_UNIT_ID = AUID_ATTNS) %>%
  unique()

cat_3_aus <- overall_categorized_aus %>%
  filter(Overall_Category == "3") %>%
  select(ASSESSMENT_UNIT_ID = AUID_ATTNS) %>%
  unique()

cat_2_aus <- overall_categorized_aus %>%
  filter(Overall_Category == "2") %>%
  select(ASSESSMENT_UNIT_ID = AUID_ATTNS) %>%
  unique()


####Assessments####
monitoring_year <- samples %>%
  select(AUID_ATTNS, ActivityStartDate) %>%
  group_by(AUID_ATTNS) %>%
  reframe(AUID_ATTNS = AUID_ATTNS,
          YEAR_LAST_MONITORED = year(max(ActivityStartDate))) %>%
  unique()

assessments <- categorized_aus %>%
  select(AUID_ATTNS) %>%
  left_join(monitoring_year, by = 'AUID_ATTNS') %>%
  mutate(AGENCY_CODE = 'S', 
         CYCLE_LAST_ASSESSED = '2024') %>% #Manual year entry
  rename(ASSESSMENT_UNIT_ID = AUID_ATTNS) %>% 
  unique() %>%
  select(ASSESSMENT_UNIT_ID, AGENCY_CODE, CYCLE_LAST_ASSESSED, YEAR_LAST_MONITORED)

#Creates small file ~ 3KB
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


uses_part1 <- categorized_aus %>%
  filter(!is.na(Use)) %>%
  select(AUID_ATTNS , PARAM_USE_NAME = ATTAINS_USE_merge, Use_Category) %>%
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
         USE_ORG_QUALIFIER_FLAG = NA) 


uses_needs_1_cat_3 <- uses_part1 %>%
  group_by(ASSESSMENT_UNIT_ID) %>%
  #If all Uses are 2, one needs to be changed to a 3
  mutate(All_Cat_2 = all(Use_Category == 2)) %>% 
  filter(All_Cat_2 == T) %>%
  mutate(Use_Category = case_when(str_detect(USE_NAME, 'INDUSTRIAL') == T ~
                                    3,
                                  T ~ Use_Category)) %>%
  select(!All_Cat_2)

uses_part2 <- uses_part1 %>%
  filter(!ASSESSMENT_UNIT_ID %in% uses_needs_1_cat_3$ASSESSMENT_UNIT_ID) %>%
  rbind(uses_needs_1_cat_3) %>%
  select(!Use_Category) %>%
  select(ASSESSMENT_UNIT_ID, USE_NAME, USE_ATTAINMENT_CODE, USE_AGENCY_CODE,
         USE_TREND, USE_THREATENED, USE_ASMT_BASIS, USE_MONITORING_START,
         USE_MONITORING_END, USE_ASMT_DATE, USE_ASSESSOR_NAME, USE_COMMENT,
         USE_STATE_IR_CAT, USE_ORG_QUALIFIER_FLAG)

##Following code for splitting uses_part2 for export

uses_cat2_export <- uses_part2 %>%
  filter(ASSESSMENT_UNIT_ID %in% cat_2_aus$ASSESSMENT_UNIT_ID)

uses_cat5_export <- uses_part2 %>%
  filter(ASSESSMENT_UNIT_ID %in% cat_5_aus$ASSESSMENT_UNIT_ID)

uses_cat3_export <- uses_part2 %>%
  filter(ASSESSMENT_UNIT_ID %in% cat_3_aus$ASSESSMENT_UNIT_ID)
 
write_csv(uses_cat2_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Uses_Cat2.csv',
          na="")

write_csv(uses_cat5_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Uses_Cat5.csv',
          na="")

write_csv(uses_cat3_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Uses_Cat3.csv',
          na="")

####Parameters####
parameters <- categorized_aus %>%
  filter(!is.na(Use)) %>%
  select(AUID_ATTNS, TADA.CharacteristicName, PARAM_USE_NAME = ATTAINS_USE_merge, Individual_Category) %>%
  group_by(AUID_ATTNS, TADA.CharacteristicName) %>%
  #Find param status for the parameter/AU combo as a group
  mutate(is_2 = sum(ifelse(Individual_Category == 2, 1, 0)),
         is_3 = sum(ifelse(Individual_Category == 3, 1, 0)),
         param_status_midstep = case_when(max(Individual_Category) == 5 ~
                                            5,
                                          max(Individual_Category) == 2 ~
                                            2,
                                          #If there are 3's present, but also 2's
                                          is_2 >= 1 & is_3 >= 1 ~
                                            2,
                                          all(Individual_Category  == 3) ~
                                            3), 
         PARAM_STATUS_NAME = case_when(param_status_midstep == 5 ~ 
                                           "Cause", 
                                       param_status_midstep == 2 ~
                                           "Meeting Criteria", 
                                       param_status_midstep == 3 ~
                                           "Insufficient Information", #MAKE EXPORT CAT BASED ON THIS
                                       T ~ NA)) %>%
  ungroup() %>%
  select(!c(is_2, is_3)) %>% 
  unique() %>%
  group_by(AUID_ATTNS, PARAM_USE_NAME, TADA.CharacteristicName) %>%
  mutate(n = n(),
         is_2 = sum(ifelse(Individual_Category == 2, 1, 0)),
         is_3 = sum(ifelse(Individual_Category == 3, 1, 0)),
         is_5 = sum(ifelse(Individual_Category == 5, 1, 0)),
         #If n > 1, choose worse category
         new_Individual_Category = case_when(n > 1 & is_5 == 1 ~
                                               5,
                                             n > 1 & is_5 == 0 & is_2 > 0 ~
                                               2,
                                             T ~ Individual_Category)) %>%
  select(!c(Individual_Category, n, is_2, is_3, is_5)) %>%
  unique() %>%
  mutate(PARAM_ATTAINMENT_CODE = case_when(new_Individual_Category == 5 ~ 
                                             "Not meeting criteria", 
                                           new_Individual_Category == 2 ~
                                             "Meeting criteria", 
                                           new_Individual_Category == 3 ~
                                             "Not enough information", 
                                           T ~
                                             "Not applicable"),
         PARAM_TREND = NA,
         PARAM_COMMENT = NA,
         PARAM_AGENCY_CODE = NA,
         PARAM_POLLUTANT_INDICATOR = if_else(PARAM_STATUS_NAME == "Cause", "Y", NA),
         PARAM_YEAR_LISTED = NA,
         PARAM_TARGET_TMDL_DATE = NA,
         PARAM_EXPECTED_TO_ATTAIN = NA,
         PARAM_PRIORITY_RANKING = NA, 
         PARAM_CONSENT_DECREE_CYCLE = NA,
         PARAM_ALT_LISTING_ID = NA, if_else(PARAM_STATUS_NAME == "Cause", "Y", NA),
         PARAM_STATE_IR_CAT = NA,
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
         PARAM_CONSENT_DECREE_CYCLE, PARAM_ALT_LISTING_ID, PARAM_STATE_IR_CAT,
         PARAM_ORG_QUALIFIER_FLAG, PARAM_DELISTING_REASON, 
         PARAM_DELISTING_COMMENT, PARAM_DELISTING_AGENCY)
         

##Following code for splitting parameters for export
param_4_export <- parameters %>%
  group_by(ASSESSMENT_UNIT_ID) %>%
  mutate(Cat_5s = sum(ifelse(PARAM_STATUS_NAME == 'Cause', 1, 0)),
         Cat_2s = sum(ifelse(PARAM_STATUS_NAME == 'Meeting criteria', 1, 0)),
         Cat_3s = sum(ifelse(PARAM_STATUS_NAME == 'Not enough information', 1, 0)))

param_cat2_export <- parameters %>%
  filter(ASSESSMENT_UNIT_ID %in% cat_2_aus$ASSESSMENT_UNIT_ID)

param_cat5_export <- parameters %>%
  filter(ASSESSMENT_UNIT_ID %in% cat_5_aus$ASSESSMENT_UNIT_ID)

param_cat3_export <- parameters %>%
  filter(ASSESSMENT_UNIT_ID %in% cat_3_aus$ASSESSMENT_UNIT_ID)


write_csv(param_cat2_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Parameters_Cat2.csv',
          na="")

write_csv(param_cat5_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Parameters_Cat5.csv',
          na="")

write_csv(param_cat3_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Parameters_Cat3.csv',
          na="")

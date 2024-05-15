##Create output ATTAINS for Assessment 
##Based on template version 1.4 updated on 7/18/2022


#Written by Hannah Ferriby
#Date updated: 5/14/2024


####Load Packages####
library(tidyverse)
library(readxl)


####Load Data####
previous_assessment_attains <- read_xlsx('Data/data_analysis/ATTAINS_AK_Asessments_DataDownload_20240126.xlsx', sheet = 2)

samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20240509.csv')

categorized_aus <- read_csv('Output/results/categorized_aus_20240513.csv') %>%
  filter(!is.na(Individual_Category))


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

uses_4_export <- uses_part2 %>%
  group_by(ASSESSMENT_UNIT_ID) %>%
  mutate(Cat_5s = sum(ifelse(USE_ATTAINMENT_CODE == 'N', 1, 0)),
         Cat_2s = sum(ifelse(USE_ATTAINMENT_CODE == 'F', 1, 0)),
         Cat_3s = sum(ifelse(USE_ATTAINMENT_CODE == 'I', 1, 0)))

uses_cat2_export <- uses_4_export %>%
  filter(Cat_5s == 0) %>%
  filter(Cat_2s >= 1)

uses_cat5_export <- uses_4_export %>%
  filter(Cat_5s >= 1)

uses_cat3_export <- uses_4_export %>%
  filter(Cat_5s == 0) %>%
  filter(Cat_2s == 0)
 
write_csv(uses_cat2_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Uses_Cat2.csv',
          na="")

write_csv(uses_cat5_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Uses_Cat5.csv',
          na="")

write_csv(uses_cat3_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Uses_Cat3.csv',
          na="")

####Parameters####
parameters <- categorized_aus %>%
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
         

##Following code for splitting parameters for export
param_4_export <- parameters %>%
  group_by(ASSESSMENT_UNIT_ID) %>%
  mutate(Cat_5s = sum(ifelse(PARAM_STATUS_NAME == 'Cause', 1, 0)),
         Cat_2s = sum(ifelse(PARAM_STATUS_NAME == 'Meeting criteria', 1, 0)),
         Cat_3s = sum(ifelse(PARAM_STATUS_NAME == 'Not enough information', 1, 0)))

param_cat2_export <- param_4_export %>%
  filter(Cat_5s == 0) %>%
  filter(Cat_2s >= 1)

param_cat5_export <- param_4_export %>%
  filter(Cat_5s >= 1)

param_cat3_export <- param_4_export %>%
  filter(Cat_5s == 0) %>%
  filter(Cat_2s == 0)

write_csv(param_cat2_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Parameters_Cat2.csv',
          na="")

write_csv(param_cat5_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Parameters_Cat5.csv',
          na="")

write_csv(param_cat3_export, 'Output/results/ATTAINS/Assessment_Batch_Upload/Parameters_Cat3.csv',
          na="")

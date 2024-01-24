#Turbidity analysis

#Written by: Hannah Ferriby

#Required packages
library(tidyverse)

set.seed(42)

#Load in data
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20240117.csv')
input_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20240117.csv')
wqs_crosswalk <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20240117.csv')


#Get turbidity samples
turbidity_samples <- input_samples %>%
  filter(TADA.CharacteristicName == 'TURBIDITY') %>%
  filter(!is.na(AUID_ATTNS))

#Pull 5 monitoring sites
sites <- turbidity_samples %>%
  select(MonitoringLocationIdentifier) %>%
  unique() %>%
  slice_sample(n=5) %>%
  pull() 

au_sites <- turbidity_samples %>%
  filter(MonitoringLocationIdentifier %in% sites) %>%
  select(AUID_ATTNS) %>%
  unique() %>%
  pull()


#Create reference site table for analysis
reference_sites <- tibble(AUID_ATTNS = au_sites,
                          ReferenceSites = sites)

#Pull only samples from these AUs
turbidity_samples_pull <- turbidity_samples %>%
  filter(AUID_ATTNS %in% reference_sites$AUID_ATTNS)


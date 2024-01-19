#Create test file for ammonia and pentachlorophenol

#Created by Hannah Ferriby

####Set up####
library(tidyverse)
# library(sf)
# library(zoo)
# library(psych)

####Load in data####
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20240117.csv')

pull_ammonia_pent <- input_samples %>% 
  filter(TADA.CharacteristicName %in% c("AMMONIA",'PENTACHLOROPHENOL', 'PH', 'TEMPERATURE, WATER', 'SALINITY'))

pent_au <- pull_ammonia_pent %>%
  filter(TADA.CharacteristicName == 'PENTACHLOROPHENOL') %>%
  select(AUID_ATTNS)%>%
  unique()

ammonia_au <- pull_ammonia_pent %>%
  filter(TADA.CharacteristicName == 'AMMONIA') %>%
  select(AUID_ATTNS) %>%
  unique() 

ammonia_au <- ammonia_au[2,1] %>% pull

#Assign all samples to one AU and resample pH, temp, and salinity to match ammonia samples
length_ammonia <- pull_ammonia_pent %>%
  filter(TADA.CharacteristicName == "AMMONIA") %>%
  nrow()

define_au <- pull_ammonia_pent %>%
  mutate(AUID_ATTNS = ammonia_au[1]) %>%
  group_by(TADA.CharacteristicName) %>%
  slice_sample(n = length_ammonia)

write_csv(define_au, "Output/data_analysis/ammonia_test_file.csv")

#Explore output from TADA_FindPotentialDuplicatesSingleOrg function

#Written by: Ben Block
#Date Created: 11-16-2023
#Date of Last Updated: 11-16-2023

##Required Inputs:
#1. csv output "Original_data_with_flags_.." from data_processing.R


# Load libraries ####
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# Declare directories ####
wd <- getwd()
input.output.dir <- "Output/data_processing"
fn.data1 <- "Original_data_with_flags_20231116.csv"
myDate <- format(Sys.Date(), "%Y%m%d")

##Load Data####
df_WQ <- read_csv(file.path(wd, input.output.dir, fn.data1)
                  , na = c("NA",""), trim_ws = TRUE, skip = 0
                  , col_names = TRUE, guess_max = 100000)

# trim data ####
df_WQ2 <- df_WQ %>% 
  filter(TADA.SingleOrgDupGroupID != "Not a duplicate")

rm(df_WQ)
# test number of rows per TADA.SingleOrgDupGroupID
test <- df_WQ2 %>% 
  count(TADA.SingleOrgDupGroupID) #2-5 duplicates per GroupID

ggplot(data = test, aes(x = n))+
  geom_bar()+
  labs(x = "# Duplicates per TADA.SingleOrgDupGroupID", y = "Frequency")+
  theme_classic()

rm(test)

# Clearly, duplicates aren't limited to a single CharacteristicName
unique(df_WQ2$CharacteristicName)

# explore duplicate differences ####
# create metadata
df_metadata <- df_WQ2 %>% 
  select(TADA.SingleOrgDupGroupID, OrganizationIdentifier
         , ActivityStartDate, MonitoringLocationIdentifier) %>% 
  distinct()
# test: provides the number of unique values per field per GroupID
test <- df_WQ2 %>%
  group_by(TADA.SingleOrgDupGroupID) %>% 
  summarize_all(n_distinct) %>% 
  pivot_longer(!c(TADA.SingleOrgDupGroupID), names_to = "ColumnName"
               , values_to = "n_Unique") %>% 
  filter(n_Unique >1) %>% 
  ungroup() %>% 
  left_join(., df_metadata, by = "TADA.SingleOrgDupGroupID")

# test2: provides the frequency at which a given column has >1 unique value
test2 <- test %>% 
  count(ColumnName) %>% 
  arrange(desc(n)) %>% 
  rename(frequency = n)

#Export data summaries
write_csv(test, file = file.path(wd, input.output.dir
                                    , paste0("test1_DuplicatesSingleOrg_"
                                             ,myDate, ".csv"))
          , na = "")

write_csv(test2, file = file.path(wd, input.output.dir
                                 , paste0("test2_DuplicatesSingleOrg_"
                                          ,myDate, ".csv"))
          , na = "")

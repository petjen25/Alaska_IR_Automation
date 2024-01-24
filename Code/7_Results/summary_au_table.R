#Function to create table with designated use, constituent, magnitude,
#frequency, and duration for the relevant parameters in a given AU

#Written by: Hannah Ferriby

#Required libraries
library(tidyverse)

#Test data
# mdf_output <- read_csv('Output/data_analysis/final_magdurfreq_output.csv')
wqs_table <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20240117.csv')

summarize_AU <- function(categorize_output, wqs_table, AU_ID) {
  
  filt_data <- categorize_output %>%
    dplyr::filter(AUID_ATTNS == AU_ID) %>%
    dplyr::select(AUID_ATTNS, `Waterbody Type`, `Constituent Group`,
                  Use, Constituent, TADA.CharacteristicName, Fraction,
                  n_Samples, n_SampDates, n_WaterYears, Data_Sufficient,
                  Magnitude_Numeric, Units, Directionality, Duration, Details,
                  Exceed, Individual_Category, Overall_Category) %>%
    unique()

}
#Function to create table with designated use, constituent, magnitude,
#frequency, and duration for the relevant parameters in a given AU

#Written by: Hannah Ferriby

#Required libraries
library(tidyverse)

#Test data
categorize_output <- read_csv('Output/results/categorized_aus.csv')
# wqs_table <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20240117.csv')

summarize_AU <- function(categorize_output, AU_ID) {
  
  filt_data <- categorize_output %>%
    dplyr::filter(AUID_ATTNS == AU_ID) %>%
    dplyr::select(AUID_ATTNS, `Waterbody Type`, `Constituent Group`,
                  Use, Constituent, TADA.CharacteristicName, Fraction,
                  n_Samples, n_SampDates, n_WaterYears, Data_Sufficient,
                  Magnitude_Numeric, Units, Directionality, Duration, Details,
                  Exceed, Individual_Category, Overall_Category) %>%
    unique()

}

output <- summarize_AU(categorize_output, AU_ID = 'AK_B_1010203_001')

#Function to create table with designated use, constituent, magnitude,
#frequency, and duration for the relevant parameters in a given AU

#Written by: Hannah Ferriby

#Required libraries
library(tidyverse)

#Test data
categorize_output <- read_csv('Output/results/categorized_aus.csv')
wqs_table <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20240131.csv')

summarize_AU <- function(categorize_output, wqs_table, AU_ID) {
  
  pull_magnitude <- wqs_table %>%
    dplyr::select(`Waterbody Type`, `Constituent Group`,
           Use, Constituent, Fraction, 
           Magnitude_Text, Units, Directionality, Duration, Details)
  
  filt_data <- categorize_output %>%
    dplyr::filter(AUID_ATTNS == AU_ID) %>%
    dplyr::select(!Magnitude_Numeric) %>%
    dplyr::select(AUID_ATTNS, `Waterbody Type`, `Constituent Group`,
                  Use, Constituent, TADA.CharacteristicName, Fraction,
                  n_Samples, n_SampDates, n_WaterYears, Data_Sufficient,
                  Units, Directionality, Duration, Details,
                  Exceed, Individual_Category, Overall_Category) %>%
    unique()
  
  join_tables <- filt_data %>%
    dplyr::left_join(pull_magnitude, by = c("Waterbody Type", "Constituent Group",
                                            "Use", "Constituent", "Fraction", 
                                            "Units", "Directionality", "Duration",
                                            "Details"),
                     relationship = "many-to-many")
  
  return(join_tables)

}


output <- list()

for (i in 1:length(unique(categorize_output$AUID_ATTNS))) {
  output[[i]] <- summarize_AU(categorize_output, wqs_table, AU_ID = unique(categorize_output$AUID_ATTNS)[i])
}

output_table <- do.call("rbind", output)

write_csv(output_table, 'Output/results/summary_au_tables_20240319.csv')

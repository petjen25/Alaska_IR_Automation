#Assign AU categories based on exceedance values

#Written by: Hannah Ferriby

#Load Packages
library(tidyverse)

####Load in data####
input_analysis <- read_csv('Output/data_analysis/final_magdurfreq_output_20240222.csv')

options <- input_analysis %>%
  select(Exceed) %>%
  unique()




categorize_AU <- function(input_analysis){ 
  
  calc_individual <- input_analysis %>%
    dplyr::mutate(Individual_Category = case_when(Data_Sufficient == "No" ~ '3',
                                           Exceed == 'Yes' ~ '5',
                                           Exceed == 'No' ~ '2',
                                           Exceed == 'Insufficient hardness' ~ '3',
                                           Exceed == 'Insufficient dependent data' ~ '3',
                                           T ~ NA))
  
  calc_overall <- calc_individual %>%
    dplyr::group_by(AUID_ATTNS, Use) %>%
    dplyr::mutate(cat_5_present = length(Individual_Category[Individual_Category=='5']),
           cat_2_present = length(Individual_Category[Individual_Category=='2']),
           Use_Category = case_when(cat_5_present > 0 ~ '5',
                                        cat_5_present == 0 & cat_2_present > 0 ~ '2',
                                        cat_5_present == 0 & cat_2_present == 0 ~ '3',
                                        T~NA)) %>%
    dplyr::select(!c(cat_5_present, cat_2_present)) %>%
    dplyr::arrange(AUID_ATTNS, Use)
  
  return(calc_overall)
  
}

output <- categorize_AU(input_analysis)

write_csv(output, 'Output/results/categorized_aus_20240222.csv')

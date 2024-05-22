#Assign AU categories based on exceedance values

#Written by: Hannah Ferriby

#Load Packages
library(tidyverse)

####Load in data####
input_analysis <- read_csv('Output/data_analysis/final_magdurfreq_output_20240515.csv')

options <- input_analysis %>%
  select(Exceed) %>%
  unique()

categorize_AU_uses <- function(input_analysis, simplify_standards){ 
  
  calc_individual <- input_analysis %>%
    filter(Exceed != 'Requires manual analysis') %>%
    filter(Exceed != 'AU not lake waters') %>%
    filter(Exceed != 'Natural conditions less than or equal to 50 NTU') %>%
    dplyr::mutate(Individual_Category = case_when(is.na(Data_Sufficient) ~ NA,
                                                  Data_Sufficient == "No" ~ '3',
                                           Exceed == 'Yes' ~ '5',
                                           Exceed == 'No' ~ '2',
                                           Exceed == 'Insufficient hardness' ~ '3',
                                           Exceed == 'Insufficient dependent data' ~ '3',
                                           T ~ NA))
  
  if(simplify_standards == T){
    
    mid_step <- calc_individual %>%
      dplyr::group_by(AUID_ATTNS, Use, `Use Description`, TADA.CharacteristicName) %>%
      dplyr::mutate(n = n(),
             is_2 = sum(ifelse(Individual_Category == '2', 1, 0)),
             is_3 = sum(ifelse(Individual_Category == '3', 1, 0)),
             is_5 = sum(ifelse(Individual_Category == '5', 1, 0)),
             #If n > 1, choose worse category
             new_Individual_Category = case_when(n > 1 & is_5 == 1 ~
                                                   '5',
                                                 n > 1 & is_5 == 0 & is_2 > 0 ~
                                                   '2',
                                                 T ~ Individual_Category)) %>%
      dplyr::filter(Individual_Category == new_Individual_Category) %>%
      dplyr::select(!c(Individual_Category, n, is_2, is_3, is_5)) %>%
      dplyr::rename(Individual_Category = new_Individual_Category)
      
    
  } else {
    mid_step <- calc_individual
  }
  
  
  calc_overall <- mid_step %>%
    dplyr::group_by(AUID_ATTNS, Use) %>%
    dplyr::mutate(cat_5_present = length(Individual_Category[Individual_Category=='5']),
           cat_2_present = length(Individual_Category[Individual_Category=='2']),
           Use_Category = case_when(cat_5_present > 0 ~ '5',
                                        cat_5_present == 0 & cat_2_present > 0 ~ '2',
                                        cat_5_present == 0 & cat_2_present == 0 ~ '3',
                                        T~ NA)) %>%
    dplyr::select(!c(cat_5_present, cat_2_present)) %>%
    dplyr::arrange(AUID_ATTNS, Use)
  
  return(calc_overall)
  
}

output <- categorize_AU_uses(input_analysis, simplify_standards = F)
output_simp <- categorize_AU_uses(input_analysis, simplify_standards = T)

write_csv(output, 'Output/results/categorized_aus_20240515.csv')
write_csv(output_simp, 'Output/results/categorized_simplified_aus_20240515.csv')



categorize_AU <- function(input_categorized_uses){ 

  calc_overall <- input_categorized_uses %>%
    dplyr::group_by(AUID_ATTNS) %>%
    dplyr::mutate(cat_5_present = length(Use_Category[Use_Category=='5']),
                  cat_2_present = length(Use_Category[Use_Category=='2']),
                  Overall_Category = case_when(cat_5_present > 0 ~ '5',
                                           cat_5_present == 0 & cat_2_present > 0 ~ '2',
                                           cat_5_present == 0 & cat_2_present == 0 ~ '3',
                                           T~ NA)) %>%
    dplyr::select(!c(cat_5_present, cat_2_present)) %>%
    dplyr::arrange(AUID_ATTNS)
  
  return(calc_overall)

}


output_overall <- categorize_AU(output)
output_simp_overall <- categorize_AU(output_simp)

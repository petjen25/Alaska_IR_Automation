#Functions for pathogens analysis

#Created by Hannah Ferriby

####Set up####

library(tidyverse)
library(sf)
library(zoo)
library(psych)

####Load in data####
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20250701.csv') 
input_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20250630.csv')
wqs_crosswalk <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20250429.csv')

#Remove insufficient data combinations to lessen mdf analysis
filterCat3samples <- function(data_samples, data_sufficiency) {
  suff_sites <- data_sufficiency %>%
    dplyr::filter(Data_Sufficient == 'Yes') %>%
    dplyr::select(AUID_ATTNS, TADA.CharacteristicName) %>%
    unique()
  
  samples <- data_samples %>% dplyr::right_join(suff_sites,
                                                by = join_by('AUID_ATTNS',
                                                             'TADA.CharacteristicName')) %>%
    dplyr::filter(!is.na(TADA.ResultMeasureValue))
  
  return(samples)
}

filterCat3sites <- function(data_sufficiency) {
  suff_sites <- data_sufficiency %>%
    dplyr::filter(Data_Sufficient == 'Yes') %>%
    unique()
  
  return(suff_sites)
}

filterCat3samples(data_samples = input_samples
                  , data_sufficiency = input_sufficiency)
filterCat3sites(input_sufficiency)

#Test samples for pathogens
input_samples_filtered <- filterCat3samples(data_samples = input_samples,
                                            data_sufficiency = input_sufficiency) %>%
  filter(TADA.CharacteristicName %in% c('ESCHERICHIA COLI',
                                        'FECAL COLIFORM',
                                        'ENTEROCOCCUS'))



MagDurFreq_pathogens <- function(input_samples_filtered, wqs_crosswalk) {
  
  #Filter to pathogen-related rows in WQS table
  pathogen_criteria <- wqs_crosswalk %>%
    filter(`Constituent Group` == "Bacteria") %>%
    select(TADA.Constituent, `Waterbody Type`, Use, `Use Description`, Type, Fraction,
           Directionality, Frequency, Duration, Details, Magnitude_Numeric) %>%
    distinct()
  
  #Filter sample data to only pathogens
  pathogen_data <- input_samples_filtered %>%
    filter(TADA.CharacteristicName %in% pathogen_criteria$TADA.Constituent) %>%
    mutate(
      year = year(ActivityStartDate),
      month = month(ActivityStartDate),
      w_year = ifelse(month < 10, year, year + 1)
      )
  
  output_list <- list()
  counter <- 0
  
  #Loop over each AUID
  for (auid in unique(pathogen_data$AUID_ATTNS)) {
    df <- pathogen_data %>% filter(AUID_ATTNS == auid)
    if (nrow(df) == 0) next
    
    constituents <- unique(df$TADA.CharacteristicName)
    
    #Loop over each constituent (e.g., E. coli, Enterococcus)
    for (constituent in constituents) {
      filt_df <- df %>% filter(TADA.CharacteristicName == constituent)
      
      #Filter WQS crosswalk for this constituent and waterbody type
      relevant_criteria <- pathogen_criteria %>%
        filter(
          TADA.Constituent == constituent,
          sapply(`Waterbody Type`, function(x) any(str_detect(x, filt_df$AU_Type)))
        )
      
      #Skip if not enough criteria
      if (nrow(relevant_criteria) < 2) next
      
      #Loop over uses (e.g., Recreation, Water Supply)
      for (u in unique(relevant_criteria$Use)) {
        crit_set <- relevant_criteria %>% filter(Use == u)
        
        ###CRITERION 1: GEOMETRIC MEAN IN 30-DAY PERIOD###
        crit1 <- crit_set %>%
          filter(
            stringr::str_detect(tolower(Details), "geometric mean"),
            Duration == "30-day period"
          ) %>% slice(1)
        
        geo_exceed_years <- filt_df %>%
          arrange(ActivityStartDate) %>%
          group_by(w_year) %>%
          group_modify(~ {
            .x %>%
              mutate(Exceed = map_lgl(ActivityStartDate, function(d) {
                values <- .x$TADA.ResultMeasureValue[
                  .x$ActivityStartDate >= (d - days(30)) & .x$ActivityStartDate <= d
                ]
                geom <- geometric.mean(values)
                return(!is.na(geom) && geom >= crit1$Magnitude_Numeric)
              }))
          }) %>%
          ungroup() %>%
          filter(Exceed) %>%
          distinct(w_year) %>%
          pull(w_year)
        
        ###CRITERION 2: 10% EXCEEDANCE OF INSTANTANEOUS VALUES###
        crit2 <- crit_set %>%
          filter(Frequency == "10% of samples",
                 Duration == "Water year average") %>%
          slice(1)
        
        pct_exceed_years <- filt_df %>%
          group_by(w_year) %>%
          summarise(
            total = n(),
            exceed = sum(TADA.ResultMeasureValue >= crit2$Magnitude_Numeric, na.rm = TRUE),
            freq = exceed / total
          ) %>%
          filter(freq >= 0.1) %>%
          pull(w_year)
        
        ###IMPAIRMENT RULE###
        all_exceed_years <- union(geo_exceed_years, pct_exceed_years)
        unique_years_exceeded <- length(unique(all_exceed_years))
        impaired <- ifelse(unique_years_exceeded >= 2, "Yes", "No")
        
        ###FORMAT RESULTS###
        for (crit_row in list(crit1, crit2)) {
          if (nrow(crit_row) == 0) next
          counter <- counter + 1
          
          output_list[[counter]] <- tibble(
            AUID_ATTNS = auid,
            `TADA.Constituent` = constituent,
            Fraction = crit_row$Fraction,
            Type = crit_row$Type,
            Use = crit_row$Use,
            `Use Description` = crit_row$`Use Description`,
            `Waterbody Type` = crit_row$`Waterbody Type`,
            Directionality = crit_row$Directionality,
            Frequency = crit_row$Frequency,
            Duration = crit_row$Duration,
            Details = crit_row$Details,
            Exceed_Num = NA,
            Exceed_Freq = NA,
            Exceed = impaired
          )
        }
      }
    }
  }
  
  #Final output
  df_pathogen_assess <- bind_rows(output_list) %>%
    distinct()
  
  return(df_pathogen_assess)
}

pathogens_output <- MagDurFreq_pathogens(input_samples_filtered, wqs_crosswalk)

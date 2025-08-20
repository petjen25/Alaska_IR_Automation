#Functions for pathogens analysis

#Created by Hannah Ferriby

####Set up####

library(tidyverse)
library(sf)
library(zoo)
library(psych)

####Load in data####
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20250731.csv') 
input_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20250731.csv')
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

   pathogen_criteria <- wqs_crosswalk %>%
    filter(`Constituent Group` == "Bacteria") %>%
     select(!Magnitude_Text)
  
  pathogen_data <- input_samples_filtered %>%
    filter(TADA.CharacteristicName %in% unique(pathogen_criteria$TADA.Constituent)) %>%
    mutate(
      year = year(ActivityStartDate),
      month = month(ActivityStartDate),
      w_year = ifelse(month < 10, year, year + 1)
    )
  
  result_list <- list()
  counter <- 0
  
  for (auid in unique(pathogen_data$AUID_ATTNS)) {
    print(auid)
    
    df <- pathogen_data %>%
      filter(AUID_ATTNS == auid) %>%
      group_by(TADA.CharacteristicName, ActivityStartDate) %>%
      mutate(TADA.ResultMeasureValue = mean(TADA.ResultMeasureValue)) %>%
      unique()
    
    if (nrow(df) == 0) next
    
    my_AU_Type <- unique(df$AU_Type)
    
    # Incorporate AU_Type to Waterbody Type mapping logic
    if (my_AU_Type %in% c("Beach", "Marine")) {
      my_WtrBdy_Type <- "Marine"
    } else if (my_AU_Type == "Lake") {
      my_WtrBdy_Type <- "Freshwater"
    } else {
      my_WtrBdy_Type <- c("Freshwater", "Freshwater streams and rivers")
    }
    
    constituents <- unique(df$TADA.CharacteristicName)
    
    for (constituent in constituents) {
      filt_df <- df %>% filter(TADA.CharacteristicName == constituent)
      
      relevant_criteria <- pathogen_criteria %>%
        filter(TADA.Constituent == constituent,
               `Waterbody Type` %in% my_WtrBdy_Type)
      
      if (nrow(relevant_criteria) < 2) next
      
      unique_uses <- relevant_criteria %>% distinct(Use, `Use Description`)
      
      for (u in 1:nrow(unique_uses)) {
        crit_set <- relevant_criteria %>%
          filter(Use == unique_uses$Use[u]) %>%
          filter(`Use Description` == unique_uses$`Use Description`[u])
        
        ###Criterion 1: Geomean###
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
        
        ###Criterion 2: 10% Exceedance###
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
        
        ###Impairment Rule###
        all_exceed_years <- union(geo_exceed_years, pct_exceed_years)
        unique_years_exceeded <- length(unique(all_exceed_years))
        impaired <- ifelse(unique_years_exceeded >= 2, "Yes", "No")
        
       
        
        ###Format Output###
        for (crit_row in list(crit1, crit2)) {
          if (nrow(crit_row) == 0) next
          counter <- counter + 1
          
          result_list[[counter]] <- crit_row %>%
            mutate(AUID_ATTNS = auid,
                   Exceed_Num = unique_years_exceeded,
                   Exceed_Freq = NA,
                   Exceed = impaired)
        }
      }
    }
  }
  
  df_loop_results <- do.call("rbind", result_list) # combine results from for loop
  df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
  df_AU_data_WQS <- df_AU_data_WQS %>% 
    distinct()
  
  df_AU_data_WQS %>% dplyr::select(Exceed) %>% dplyr::group_by(Exceed) %>% dplyr::mutate(n = n()) %>% unique()
  
  #combine with relevant WQS table, removing the constituents that are calculated in other functions
  #these constituents come back in the hardness, pH, and turbidity specific functions
  relevant_suff <- input_sufficiency %>%
    dplyr::filter(TADA.CharacteristicName %in% c('ESCHERICHIA COLI',
                                                 'FECAL COLIFORM',
                                                 'ENTEROCOCCUS'))
  
  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Use Description', 'Waterbody Type', #DEC added Use Description
                                           'Fraction', 'Type', 'Constituent Group'),
                     relationship = "many-to-many") %>%
    dplyr::relocate(c(Exceed_Num, Exceed_Freq, Exceed), .after = last_col())
  
  return(data_suff_WQS)
}


pathogens_output <- MagDurFreq_pathogens(input_samples_filtered, wqs_crosswalk)

#Functions for data analysis package
#Functions to remove Cat 3
#Function for mag/freq/dur analysis

#Created by Hannah Ferriby

####Set up####
library(tidyverse)
library(sf)
library(zoo)
library(psych)

####Load in data####
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20240117.csv')
input_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20240117.csv')
wqs_crosswalk <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20240117.csv')



#Remove insufficient data
removeCat3samples <- function(data_samples, data_sufficiency) {
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

removeCat3sites <- function(data_sufficiency) {
  suff_sites <- data_sufficiency %>%
    dplyr::filter(Data_Sufficient == 'Yes') %>%
    unique()
  
  return(suff_sites)
}

removeCat3samples(data_samples = input_samples
                  , data_sufficiency = input_sufficiency)
removeCat3sites(input_sufficiency)

input_samples_filtered <- removeCat3samples(data_samples = input_samples, data_sufficiency = input_sufficiency)


MagDurFreq <- function(wqs_crosswalk, input_samples_filtered, input_sufficiency) {
  
  ##Magnitude, Frequency, Duration
  unique_methods <- wqs_crosswalk %>%
    dplyr::filter(Constituent != 'Ammonia') %>%
    dplyr::filter(Constituent != 'Pentachloro-phenol') %>%
    dplyr::filter(Constituent != 'Turbidity') %>%
    dplyr::filter(!(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                                'Nickel', 'Silver', 'Zinc') & Use == 'Aquatic Life')) %>%
    dplyr::select(Directionality, Frequency, Duration, Details) %>%
    unique()
  
  # write_csv(unique_methods, 'Output/data_analysis/wqs_unique_methods.csv')

  # use AU_Type to choose Waterbody Type in data sufficiency table
  Unique_AUIDs <- unique(input_samples_filtered$AUID_ATTNS) %>% stats::na.omit()
  result_list <- list()
  counter <- 0
  
  #Cycle by AUs
  for(i in Unique_AUIDs){
    print(i) # print name of current AU
    
    # dplyr::filter data
    df_subset <- input_samples_filtered %>% 
      dplyr::filter(AUID_ATTNS == i) %>%
      mutate(year = lubridate::year(ActivityStartDate),
             month = lubridate::month(ActivityStartDate),
             w_year = ifelse(month < 10, year, year+1))
    
    # obtain AU_Type
    my_AU_Type <- unique(df_subset$AU_Type)
    
    # use AU_Type to choose Waterbody Type in data standards table
    if(my_AU_Type == "Beach" | my_AU_Type == "Marine"){
      my_WtrBdy_Type <- "Marine"
    } else if (my_AU_Type == "Lake"){
      my_WtrBdy_Type <- "Freshwater"
    } else {
      my_WtrBdy_Type <- c("Freshwater", "Freshwater streams and rivers")
    } # end if/else statement
    
    # obtain unique constituents from WQ dataset for the AU
    my_constituents <- unique(df_subset$TADA.CharacteristicName)
    
    # trim data WQS table to only relevant information
    my_data_magfreqdur <- wqs_crosswalk %>% 
      dplyr::filter(TADA.Constituent %in% my_constituents) %>% 
      dplyr::filter(`Waterbody Type` %in% my_WtrBdy_Type) %>%
      dplyr::select(!c(Magnitude_Text))  %>%
      dplyr::filter(Constituent != 'Ammonia') %>% #Filter out special cases
      dplyr::filter(Constituent != 'Pentachloro-phenol') %>%
      dplyr::filter(Constituent != 'Turbidity') %>%
      dplyr::filter(!(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                                         'Nickel', 'Silver', 'Zinc') & Use == 'Aquatic Life'))
    
    #If no relevant samples, skip AU
    if(nrow(my_data_magfreqdur)==0){
      next
    }
    
    #Cycle through each parameter to calculate the mag/freq/dur
    for(j in 1:nrow(my_data_magfreqdur)) {
      counter <- counter + 1
      filter_by <- my_data_magfreqdur[j,]
      
      filt <- df_subset %>% dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent)
      
      #Method: Maximum, not to exceed, 30-day geometric mean
      if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
         filter_by$Duration == '30-day period' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T) {
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::mutate(geo_mean_30d = zoo::rollapplyr(TADA.ResultMeasureValue, 
                                           seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate), 
                                           psych::geometric.mean),
                 Exceed = ifelse(geo_mean_30d >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_30d)
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))
        
        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
         filter_by$Duration == 'Water year average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T) {
        #Method: Maximum, Not to exceed, water year average, geometric mean
        results <- filt %>%
          dplyr::group_by(w_year) %>%
          dplyr::mutate(geo_mean_1yr = psych::geometric.mean(TADA.ResultMeasureValue),
                        Exceed = ifelse(geo_mean_1yr >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_1yr)
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))
        
        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')
        
      }else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10% of samples' &
                filter_by$Duration == 'Water year average') {
        #Method: Maximum, 10% of samples, Water year average
        
        results <- filt %>%
          dplyr::group_by(w_year) %>%
          dplyr::mutate(wyear_row = n(),
                 bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/wyear_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::select(w_year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Not to exceed' & filter_by$Frequency == 'Not to exceed' &
              filter_by$Duration == '30-day period' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T){
        #Method: Not to exceed, 30 day geometric mean
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::mutate(geo_mean_30d = zoo::rollapplyr(TADA.ResultMeasureValue, 
                                           seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate), 
                                           psych::geometric.mean),
                 Exceed = ifelse(geo_mean_30d >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_30d)
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))
        
        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Not to exceed' & filter_by$Frequency == 'Not to exceed' &
              filter_by$Duration == 'Water year average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T) {
        #Method: Maximum, not to exceed, geometric mean for water year
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(w_year) %>%
          dplyr::mutate(geo_mean_year = psych::geometric.mean(TADA.ResultMeasureValue),
                 Exceed = ifelse(geo_mean_year >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!geo_mean_year) %>%
          dplyr::ungroup()
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))
        
        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
              filter_by$Duration == 'Daily average') {
        #Method: Maximum, not to exceed, daily arithmetic mean
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue),
                 Exceed = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          dplyr::select(!daily_mean) %>%
          dplyr::ungroup()
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(dplyr::filter(results, Exceed == 'Yes'))
        
        filter_by$Exceed <- ifelse(bad > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
              filter_by$Duration == 'Daily average'){
        #Method: Maximum, 10% of samples, daily arithmetic mean
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = n(),
                 bad_samp = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>%
          dplyr::ungroup() %>%
          dplyr::select(year, bad_year) %>%
          unique()
        
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
        
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
              filter_by$Duration == 'Daily average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), '1m Depth') == T){
        #Method: Minimum, 10%, daily arithmetic mean, 1m Depth
        
        results <- filt %>%
          dplyr::filter(TADA.ActivityDepthHeightMeasure.MeasureValue <= 1) %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = n(),
                 bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
        
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'Estuaries and tidal tributaries') == T){
        #Method: Minimum, 10%, daily arithmetic mean
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = n(),
                 bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
        
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
              filter_by$Duration == 'Daily average'){
        #Method: Minimum, 10%, daily arithmetic mean
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          dplyr::mutate(day_row = n(),
                 bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
              filter_by$Duration == 'Daily minimum'){
        #Method: Minimum, 10%, daily minimum
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_min = min(TADA.ResultMeasureValue)) %>%
          dplyr::mutate(day_row = length(unique(filt$ActivityStartDate)),
                 bad_samp = ifelse(daily_min <= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
              filter_by$Duration == 'Daily maximum'){
        #Method: Maximum, 10%, daily Maximum
        
        results <- filt %>%
          dplyr::arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_max = max(TADA.ResultMeasureValue)) %>%
          dplyr::mutate(day_row = length(unique(filt$ActivityStartDate)),
                 bad_samp = ifelse(daily_max >= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::ungroup() %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Not to exceed'){
        #Method: Maximum, 10%, not to exceed
        
        results <- filt %>%
          dplyr::mutate(num_samples = n(),
                 bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/num_samples>=0.1, 1, 0))
        
        bad_tot <- results %>% dplyr::select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
               filter_by$Duration == 'Not to exceed'){
        #Method: Maximum, not to exceed, not to exceed
        
        results <- filt %>%
          dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0))
        
        bad_tot <- results %>% dplyr::select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
               filter_by$Duration == 'Arithmetic mean' &
               stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'Frequency - harmonic mean of most recent 3 years') == T){
        #Method: Maximum, not to exceed, arithmetic Mean of last 3 years
        
        max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::mutate(mean_samps = mean(TADA.ResultMeasureValue), 
                 bad_samp = ifelse(mean_samps >= filter_by$Magnitude_Numeric, 1, 0))
        
        bad_tot <- results %>% dplyr::select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
                 filter_by$Duration == 'Arithmetic mean'){
        #Method: Maximum, not to exceed, arithmetic mean
        #ISSUE
        max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::mutate(mean_samps = mean(TADA.ResultMeasureValue), 
                        bad_samp = ifelse(mean_samps >= filter_by$Magnitude_Numeric, 1, 0))
        
        bad_tot <- results %>% dplyr::select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '24-hour max'){
        #Method: Maximum, 1 instance in previous 3 years, 24 hour max
        
        max_year <- results %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(max_samps = max(TADA.ResultMeasureValue), 
                 bad_samp = ifelse(max_samps >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '24-hour average' & is.na(filter_by$Details) == T){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 24 hour average
        
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(roll_1day_mean = map_dbl(ActivityStartDate, 
                                          ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(1), .x)])), 
                 bad_samp = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          dplyr::ungroup() %>%
          dplyr::select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                     ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                 #Give every samples a count of 1
                 r_count = 1.0,
                 #Total up number of samples in last 3 years
                 num_samples_3yrs = map_dbl(ActivityStartDate, 
                                            ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                 #Calculate exceedance frequency
                 Exceed_Freq = Exceedances/num_samples_3yrs,
                 #Determine if exceedance criteria met
                 tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
        
        bad_sum <- sum(bad_tot$tot_exceed)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour average' & stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'Magnitude is minimum') == T){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude is min
  
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                          ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                 bad_samp = ifelse(roll_4day_mean < filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          dplyr::ungroup() %>%
          dplyr::select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                              ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                 #Give every samples a count of 1
                 r_count = 1.0,
                 #Total up number of samples in last 3 years
                 num_samples_3yrs = map_dbl(ActivityStartDate, 
                                            ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                 #Calculate exceedance frequency
                 Exceed_Freq = Exceedances/num_samples_3yrs,
                 #Determine if exceedance criteria met
                 tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
        
        bad_sum <- sum(bad_tot$tot_exceed)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour average' & is.na(filter_by$Magnitude_Numeric) == F){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude listed
        
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                          ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                 bad_samp = ifelse(roll_4day_mean >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          dplyr::ungroup() %>%
          dplyr::select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                              ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                 #Give every samples a count of 1
                 r_count = 1.0,
                 #Total up number of samples in last 3 years
                 num_samples_3yrs = map_dbl(ActivityStartDate, 
                                            ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                 #Calculate exceedance frequency
                 Exceed_Freq = Exceedances/num_samples_3yrs,
                 #Determine if exceedance criteria met
                 tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
        
        bad_sum <- sum(bad_tot$tot_exceed)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == F &
                stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
        #Method: Maximum, 1 in most recent 3 years, 1 hour average,
        #Acute
        
        max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour average' & is.na(filter_by$Magnitude_Numeric) == F &
                stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average
  
        
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                          ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                 bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0)) 
        
        bad_tot <- results %>% 
          dplyr::ungroup() %>%
          dplyr::select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                              ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                 #Give every samples a count of 1
                 r_count = 1.0,
                 #Total up number of samples in last 3 years
                 num_samples_3yrs = map_dbl(ActivityStartDate, 
                                            ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                 #Calculate exceedance frequency
                 Exceed_Freq = Exceedances/num_samples_3yrs,
                 #Determine if exceedance criteria met
                 tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
        
        bad_sum <- sum(bad_tot$tot_exceed)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
        } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == F){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 1 hour average, magnitude listed
        
        #1-hour averages: use the daily value
        #(it will be rare that multiple samples are taken in a day, and if they are they would be considered duplicates)
        results <- filt %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::arrange(ActivityStartDate) %>%
          dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          dplyr::ungroup() %>%
          dplyr::select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                              ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                 #Give every samples a count of 1
                 r_count = 1.0,
                 #Total up number of samples in last 3 years
                 num_samples_3yrs = map_dbl(ActivityStartDate, 
                                            ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                 #Calculate exceedance frequency
                 Exceed_Freq = Exceedances/num_samples_3yrs,
                 #Determine if exceedance criteria met
                 tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
        
        bad_sum <- sum(bad_tot$tot_exceed)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '24-hour average'){
        #Method: Maximum, 1 in most recent 3 years, 24 hour average
        
        max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
        results <- filt %>%
          dplyr::filter(w_year >= max_year - 3) %>%
          dplyr::group_by(ActivityStartDate) %>%
          dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
                 bad_samp = ifelse(daily_avg >= magnitude, 1, 0)) 
        
        bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else {
        filter_by$AUID_ATTNS <- i
        filter_by$Exceed <- 'Method not coded!'
      }
      
      result_list[[counter]] <- filter_by
    } 
  } # end of for loop 
  
  
  df_loop_results <- do.call("rbind", result_list) # combine results from for loop
  df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
  df_AU_data_WQS <- df_AU_data_WQS %>% 
    distinct()
  
  df_AU_data_WQS %>% dplyr::select(Exceed) %>% dplyr::group_by(Exceed) %>% dplyr::mutate(n = n()) %>% unique()
  
  #combine with relevant WQS table
  relevant_wqs <- input_sufficiency %>%
    dplyr::filter(!(TADA.CharacteristicName %in% c('(?i)Cadmium', '(?i)Chromium (III)', '(?i)Copper', '(?i)Lead',
                                                 '(?i)Nickel', '(?i)Silver', '(?i)Zinc') & Use == 'Aquatic Life')) %>%
    dplyr::filter(TADA.CharacteristicName != 'AMMONIA' | TADA.CharacteristicName != 'PENTACHLORO-PHENOL') %>%
    dplyr::filter(TADA.CharacteristicName != 'TURBIDITY')
  
  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_wqs, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Waterbody Type',
                                        'Fraction', 'Type'),
                     relationship = "many-to-many") %>%
    dplyr::relocate(Exceed, .after = last_col())
  
  return(data_suff_WQS)

}


output <- MagDurFreq(wqs_crosswalk, input_samples_filtered, input_sufficiency)

# categorize <- output %>%
#   dplyr::mutate(Overall_Category = case_when(Data_Sufficient == "(?i)No" ~ '3',
#                                       Exceed == 'Yes' ~ '5',
#                                       Exceed == 'No' ~ '2',
#                                       T ~ NA))



MagDurFreq_hardnessDependent <- function(wqs_crosswalk, input_samples, input_samples_filtered, input_sufficiency) {
  ##Magnitude, Frequency, Duration
  unique_methods <- wqs_crosswalk %>%
    dplyr::filter(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                              'Nickel', 'Silver', 'Zinc')) %>% 
    dplyr::filter(Use == 'Aquatic Life') %>%
    dplyr::filter(is.na(Magnitude_Numeric)) %>%
    dplyr::select(Directionality, Frequency, Duration, Details) %>%
    unique()
  
  input_samples_filtered_relevant <- input_samples_filtered %>%
    dplyr::filter(TADA.CharacteristicName %in% c('CADMIUM', 'CHROMIUM', 'COPPER', 'LEAD',
                                          'NICKEL', 'SILVER', 'ZINC'))
  

  # use AU_Type to choose Waterbody Type in data sufficiency table
  Unique_AUIDs <- unique(input_samples_filtered_relevant$AUID_ATTNS) %>% stats::na.omit()
  result_list <- list()
  counter <- 0
  
  #Cycle by AUs
  for(i in Unique_AUIDs){
    print(i) # print name of current AU
    
    # dplyr::filter data
    df_subset <- input_samples_filtered_relevant %>% 
      dplyr::filter(AUID_ATTNS == i) %>%
      dplyr::mutate(year = year(ActivityStartDate),
                    month = month(ActivityStartDate),
                    w_year = ifelse(month < 10, year, year+1))
    
    # obtain AU_Type
    my_AU_Type <- unique(df_subset$AU_Type)
    
    # use AU_Type to choose Waterbody Type in data sufficiency table
    if(my_AU_Type == "Beach" | my_AU_Type == "Marine"){
      my_WtrBdy_Type <- "Marine"
    } else if (my_AU_Type == "Lake"){
      my_WtrBdy_Type <- "Freshwater"
    } else {
      my_WtrBdy_Type <- c("Freshwater", "Freshwater streams and rivers")
    } # end if/else statement
    
    # obtain unique constituents from WQ dataset for the AU
    my_constituents <- unique(df_subset$TADA.CharacteristicName)
    
    # trim data WQS table to only relevant information
    my_data_magfreqdur <- wqs_crosswalk %>% 
      dplyr::filter(TADA.Constituent %in% my_constituents) %>% 
      dplyr::filter(`Waterbody Type` %in% my_WtrBdy_Type) %>%
      dplyr::select(!c(Magnitude_Text)) %>%
      dplyr::filter(Constituent %in% c('Cadmium', 'Chromium (III)', 'Copper', 'Lead',
                                       'Nickel', 'Silver', 'Zinc')) %>% 
      dplyr::filter(Use == 'Aquatic Life')
    
    #If no relevant samples, skip AU
    if(nrow(my_data_magfreqdur)==0){
      next
    }
    
    #Cycle through each parameter to calculate the mag/freq/dur
    for(j in 1:nrow(my_data_magfreqdur)) {
      counter <- counter + 1
      filter_by <- my_data_magfreqdur[j,]
      
      filt <- df_subset %>% dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent)
      
      if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
              filter_by$Duration == '96-hour average' & is.na(filter_by$Magnitude_Numeric) == T &
              stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude dependent on equations
        #Hardness dependent magnitudes - NOT pH DEPENDENT
        #Chronic
        
        #Pull matching hardness samples from all samples
        hardness <- input_samples %>%
          dplyr::filter(AUID_ATTNS == i) %>%
          dplyr::filter(TADA.CharacteristicName == "HARDNESS") %>%
          dplyr::rename(Hardness = TADA.ResultMeasureValue,
                        Hardness.Date = ActivityStartDate) %>%
          dplyr::select(Hardness.Date, ActivityStartTime.Time, AUID_ATTNS, Hardness) %>%
          dplyr::group_by(Hardness.Date) %>%
          dplyr::reframe(Hardness.Date = Hardness.Date,
                         Hardness = mean(Hardness)) %>%
          unique()
        
        
        #Mark result as insufficient if no hardness available
        if(nrow(hardness) == 0) {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- "Insufficient hardness"
        } else {
          #Need to make magnitude for: Chromium (III), copper, lead, nickel, zinc - all chronic
          if(filter_by$Constituent == 'Chromium (III)'){
            #Combine matching hardness & calculate magnitude
            data.table::setDT(filt)
            data.table::setDT(hardness)
            
            match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
            
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.819*(log(Hardness))+0.6848)*0.860)
            
          } else if(filter_by$Constituent == 'Copper') {
            #Combine matching hardness & calculate magnitude
            
            data.table::setDT(filt)
            data.table::setDT(hardness)
            
            match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
            
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.8545*(log(Hardness))-1.702)*0.960)
            
          } else if(filter_by$Constituent == 'Lead') {
            #Combine matching hardness & calculate magnitude
            data.table::setDT(filt)
            data.table::setDT(hardness)
            
            match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
            
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(1.273*(log(Hardness))-4.705)*(1.46203-log(Hardness)*0.145712))
            
          } else if(filter_by$Constituent == 'Nickel') {
            #Combine matching hardness & calculate magnitude
            data.table::setDT(filt)
            data.table::setDT(hardness)
            
            match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
            
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.846*(log(Hardness))+0.0584)*0.997)
            
          } else if(filter_by$Constituent == 'Zinc') {
            #Combine matching hardness & calculate magnitude
            data.table::setDT(filt)
            data.table::setDT(hardness)
            
            match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
            
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.8473*(log(Hardness))+0.884)*0.986)
            
          } else if(filter_by$Constituent == 'Cadmium') {
            #Combine matching hardness & calculate magnitude
            data.table::setDT(filt)
            data.table::setDT(hardness)
            
            match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
            
            joined <- match_dates %>%
              dplyr::mutate(magnitude = exp(0.7409*(log(Hardness))-4.719)*(1.101672-log(Hardness)*0.041838))
            
          }
          
          results <- joined %>%
            dplyr::group_by(ActivityStartDate) %>%
            dplyr::arrange(ActivityStartDate) %>%
            dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                                   ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                          bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0)) 
          
          bad_tot <- results %>% 
            dplyr::ungroup() %>%
            dplyr::select(year, ActivityStartDate, bad_samp) %>%
            unique() %>%
            #If more than 2 exceedances in 3 years assign value of 1, else 0
            #Left bound is date - 3 years, right bound is date
            dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
                                                       ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
                          #Give every samples a count of 1
                          r_count = 1.0,
                          #Total up number of samples in last 3 years
                          num_samples_3yrs = map_dbl(ActivityStartDate, 
                                                     ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
                          #Calculate exceedance frequency
                          Exceed_Freq = Exceedances/num_samples_3yrs,
                          #Determine if exceedance criteria met
                          tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
          
          bad_sum <- sum(bad_tot$tot_exceed)
          
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
        } 
        } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                  filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == T &
                  stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == F){
          #Method: Maximum, 1 in most recent 3 years, 1 hour average, magnitude dependent on equations
          #Hardness dependent magnitudes - NOT pH DEPENDENT
          #Acute
          
          #Pull matching hardness samples
          hardness <- input_samples %>%
            dplyr::filter(AUID_ATTNS == i) %>%
            dplyr::filter(TADA.CharacteristicName == "HARDNESS") %>%
            dplyr::rename(Hardness = TADA.ResultMeasureValue) %>%
            dplyr::select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, Hardness)%>%
            dplyr::group_by(Hardness.Date) %>%
            dplyr::reframe(Hardness.Date = Hardness.Date,
                           Hardness = mean(Hardness)) %>%
            unique()
          #Need to make magnitude for: Chromium (III), copper, lead, nickel, zinc - all chronic
          
          #Mark result as insufficient if no hardness available
          if(nrow(hardness) == 0) {
            filter_by$AUID_ATTNS <- i
            filter_by$Exceed <- "Insufficient hardness"
          } else {
            if(filter_by$Constituent == 'Chromium (III)'){
              #Combine matching hardness & calculate magnitude
              data.table::setDT(filt)
              data.table::setDT(hardness)
              
              match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
              
              joined <- match_dates %>%
                dplyr::mutate(magnitude = exp(0.819*(log(Hardness))+3.7256)*0.316)
              
            } else if(filter_by$Constituent == 'Copper') {
              #Combine matching hardness & calculate magnitude
              data.table::setDT(filt)
              data.table::setDT(hardness)
              
              match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
              
              joined <- match_dates %>%
                dplyr::mutate(magnitude = exp(0.9422*(log(Hardness))-1.700)*0.960)
              
            } else if(filter_by$Constituent == 'Lead') {
              #Combine matching hardness & calculate magnitude
              data.table::setDT(filt)
              data.table::setDT(hardness)
              
              match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
              
              joined <- match_dates %>%
                dplyr::mutate(magnitude = exp(1.273*(log(Hardness))-1.460)*(1.46203-log(Hardness)*0.145712))
              
            } else if(filter_by$Constituent == 'Nickel') {
              #Combine matching hardness & calculate magnitude
              data.table::setDT(filt)
              data.table::setDT(hardness)
              
              match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
              
              joined <- match_dates %>%
                dplyr::mutate(magnitude = exp(0.846*(log(Hardness))+2.255)*0.998)
              
            } else if(filter_by$Constituent == 'Zinc') {
              #Combine matching hardness & calculate magnitude
              data.table::setDT(filt)
              data.table::setDT(hardness)
              
              match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
              
              joined <- match_dates %>%
                dplyr::mutate(magnitude = exp(0.8473*(log(Hardness))+0.884)*0.978)
              
            } else if(filter_by$Constituent == 'Cadmium') {
              #Combine matching hardness & calculate magnitude
              data.table::setDT(filt)
              data.table::setDT(hardness)
              
              match_dates <- dplyr::as_tibble(filt[hardness, on=.(ActivityStartDate=Hardness.Date), roll="nearest"])
              
              joined <- match_dates %>%
                dplyr::mutate(magnitude = exp(1.0166*(log(Hardness))-3.924)*(1.136672-log(Hardness)*0.041838))
              
            }
            
            max_year <- filt %>% dplyr::select(w_year) %>% max() %>% unique()
            
            results <- joined %>%
              dplyr::filter(w_year >= max_year - 3) %>%
              dplyr::group_by(ActivityStartDate) %>%
              dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= magnitude, 1, 0))
            
            bad_tot <- results %>%
              dplyr::ungroup() %>%
              dplyr::select(bad_samp) %>%
              unique() 
            
            bad_sum <- sum(bad_tot$bad_samp)
            
            filter_by$AUID_ATTNS <- i
            filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
          } #End of hardness check
        } else {
          filter_by$AUID_ATTNS <- i
          filter_by$Exceed <- 'Method not coded!'
        } #End of methods if/else
        
        result_list[[counter]] <- filter_by
      } #End of MagDurFreq loop
        
    } #End of AU loop 
  
  df_loop_results <- do.call("rbind", result_list) # combine results from for loop
  df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
  df_AU_data_WQS <- df_AU_data_WQS %>% 
    distinct()
  
  #combine with relevant data standards table
  relevant_wqs <- input_sufficiency %>%
    dplyr::filter(TADA.CharacteristicName %in% c('(?i)Cadmium', '(?i)Chromium (III)', '(?i)Copper', '(?i)Lead',
                                     '(?i)Nickel', '(?i)Silver', '(?i)Zinc')) %>% 
    dplyr::filter(Use == 'Aquatic Life')
  
  data_suff_WQS <- df_AU_data_WQS %>%
    dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
    dplyr::full_join(relevant_wqs, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Waterbody Type',
                                        'Fraction', 'Type'),
              relationship = "many-to-many") %>%
    dplyr::relocate(Exceed, .after = last_col())
  
  return(data_suff_WQS)
} #End of hardness dependent function

output_hardness <- MagDurFreq_hardnessDependent(wqs_crosswalk, input_samples, input_samples_filtered, input_sufficiency)



MagDurFreq_pHDependent <- function(wqs_crosswalk, input_samples, input_samples_filtered, input_sufficiency) {
  ##Magnitude, Frequency, Duration
  unique_methods <- wqs_crosswalk %>%
    dplyr::filter(Constituent %in% c('Ammonia', 'Pentachloro-phenol')) %>% 
    dplyr::filter(is.na(Magnitude_Numeric)) %>%
    dplyr::select(Directionality, Frequency, Duration, Details) %>%
    unique()
  
  samples_filt <- input_samples_filtered %>%
    dplyr::filter(TADA.CharacteristicName %in% c('AMMONIA'))
  
  pH <- input_samples %>%
    dplyr::filter(TADA.CharacteristicName == 'PH')
  
  salinity <- input_samples %>%
    dplyr::filter(TADA.CharacteristicName == 'SALINITY')
  
  temperature <- input_samples %>%
    dplyr::filter(TADA.CharacteristicName == 'TEMPERATURE, WATER')
  
  
  # use AU_Type to choose Waterbody Type in data sufficiency table
  Unique_AUIDs <- unique(input_samples_filtered$AUID_ATTNS) %>% stats::na.omit()
  result_list <- list()
  counter <- 0
  
  if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
          filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == F &
          stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == T){
    #Method: Maximum, 1 in most recent 3 years, 1 hour average, magnitude dependent on equations
    #Acute
    #Pull matching pH and temperature samples
    pH <- df_subset %>%
      dplyr::filter(TADA.CharacteristicName == "pH") %>%
      rename(pH = TADA.ResultMeasureValue) %>%
      dplyr::select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, pH)
    
    if(filter_by$Constituent == 'Pentachloro-phenol') {
      #Combine matching pH & calculate magnitude
      joined <- filt %>%
        inner_join(pH, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
        dplyr::mutate(magnitude = exp(1.005*pH-4.869)) #Equation from WQS sheet
      
    }
    
    max_year <- joined %>% dplyr::select(w_year) %>% max() %>% unique()
    results <- joined %>%
      dplyr::filter(w_year >= max_year - 3) %>%
      dplyr::group_by(ActivityStartDate) %>%
      dplyr::mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= magnitude, 1, 0)) 
    
    bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
    bad_sum <- sum(bad_tot$bad_samp)
    
    filter_by$AUID_ATTNS <- i
    filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
    
  }
}




#Marine Ammonia stuff

# else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
#         filter_by$Duration == '96-hour average' & is.na(filter_by$Magnitude_Numeric) == T &
#         stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == T){
#   #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude dependent on equations
#   #pH Dependent 
#   #Chronic
#   
#   #Pull matching pH and temperature samples
#   pH <- df_subset %>%
#     dplyr::filter(TADA.CharacteristicName == "pH") %>%
#     rename(pH = TADA.ResultMeasureValue) %>%
#     dplyr::select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, pH)
#   
#   if(filter_by$Constituent == 'Pentachloro-phenol') {
#     #Combine matching pH & calculate magnitude
#     joined <- filt %>%
#       inner_join(pH, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
#       dplyr::mutate(magnitude = exp(1.005*pH-5.134)) #Equation from WQS sheet
#     
#   }
#   
#   results <- joined %>%
#     dplyr::group_by(ActivityStartDate) %>%
#     dplyr::arrange(ActivityStartDate) %>%
#     dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
#                                     ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
#            bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0)) 
#   
#   bad_tot <- results %>% 
#     dplyr::ungroup() %>%
#     dplyr::select(year, ActivityStartDate, bad_samp) %>%
#     unique() %>%
#     #If more than 2 exceedances in 3 years assign value of 1, else 0
#     #Left bound is date - 3 years, right bound is date
#     dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
#                                         ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
#            #Give every samples a count of 1
#            r_count = 1.0,
#            #Total up number of samples in last 3 years
#            num_samples_3yrs = map_dbl(ActivityStartDate, 
#                                       ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
#            #Calculate exceedance frequency
#            Exceed_Freq = Exceedances/num_samples_3yrs,
#            #Determine if exceedance criteria met
#            tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
#   
#   bad_sum <- sum(bad_tot$tot_exceed)
#   
#   filter_by$AUID_ATTNS <- i
#   filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
#   
# }


# else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
#         filter_by$Duration == '96-hour average' & is.na(filter_by$Magnitude_Numeric) == T &
#         stringr::str_detect(tidyr::replace_na(filter_by$Details, ''), 'pH') == T){
#   #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude dependent on equations
#   #pH Dependent & Temperature Dependent & Salinity dependent
#   #Chronic
#   
#   #Pull matching pH and temperature samples
#   pH <- df_subset %>%
#     dplyr::filter(TADA.CharacteristicName == "pH") %>%
#     rename(pH = TADA.ResultMeasureValue) %>%
#     dplyr::select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, pH)
#   
#   temperature <- df_subset %>%
#     dplyr::filter(TADA.CharacteristicName == "TEMPERATURE, WATER") %>%
#     rename(Temperature = TADA.ResultMeasureValue) %>%
#     dplyr::select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, Temperature)
#   
#   salinity <- df_subset %>%
#     dplyr::filter(TADA.CharacteristicName == "SALINITY") %>%
#     rename(Salinity = TADA.ResultMeasureValue) %>%
#     dplyr::select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, Salinity)
#   
#   #Need to make magnitude for: Ammonia & Pentachloro-phenol
#   if(filter_by$Constituent == 'Ammonia'){
#     #Combine matching matching pH and temperature & calculate magnitude
#     #Table from Toxics manual - Appendix G
#     marine_ammonia_table <- read_csv('Data/data_analysis/chronic_marine_ammonia.csv') 
#     
#     joined <- filt %>%
#       inner_join(pH, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
#       inner_join(temperature, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
#       inner_join(salinity, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
#       dplyr::mutate(pH_lookup = plyr::round_any(pH, 0.2),
#              temp_lookup = plyr::round_any(Temperature, 5),
#              salinity_lookup = plyr::round_any(Salinity, 10)) 
#     
#   } else if(filter_by$Constituent == 'Pentachloro-phenol') {
#     #Combine matching pH & calculate magnitude
#     joined <- filt %>%
#       inner_join(pH, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
#       dplyr::mutate(magnitude = exp(1.005*pH-5.134)) #Equation from WQS sheet
#     
#   }
#   
#   results <- joined %>%
#     dplyr::group_by(ActivityStartDate) %>%
#     dplyr::arrange(ActivityStartDate) %>%
#     dplyr::mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
#                                     ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - hours(96), .x)])), 
#            bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0)) 
#   
#   bad_tot <- results %>% 
#     dplyr::ungroup() %>%
#     dplyr::select(year, ActivityStartDate, bad_samp) %>%
#     unique() %>%
#     #If more than 2 exceedances in 3 years assign value of 1, else 0
#     #Left bound is date - 3 years, right bound is date
#     dplyr::mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
#                                         ~sum(bad_samp[between(ActivityStartDate, .x - years(3), .x)]))>=2, 1, 0),
#            #Give every samples a count of 1
#            r_count = 1.0,
#            #Total up number of samples in last 3 years
#            num_samples_3yrs = map_dbl(ActivityStartDate, 
#                                       ~sum(r_count[between(ActivityStartDate, .x - years(3), .x)])),
#            #Calculate exceedance frequency
#            Exceed_Freq = Exceedances/num_samples_3yrs,
#            #Determine if exceedance criteria met
#            tot_exceed = ifelse(Exceedances == 1 & Exceed_Freq >= 0.05, 1, 0)) 
#   
#   bad_sum <- sum(bad_tot$tot_exceed)
#   
#   filter_by$AUID_ATTNS <- i
#   filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
#   
# } 











#Connecting samples to Magnitude, Frequency, and Duration requirements
connect_info <- input_sufficiency %>%
  dplyr::select(AUID_ATTNS, TADA.CharacteristicName, `Waterbody Type`) %>%
  unique() %>%
  dplyr::mutate(`Waterbody Type` = ifelse(stringr::str_detect(`Waterbody Type`
                                              , 'streams and rivers')
                                   , 'Freshwater', `Waterbody Type`))

#Creates duplicates depending on if a site has both marine and freshwater requirements
join_info_samples <- input_samples %>%
  dplyr::left_join(connect_info, by = join_by('AUID_ATTNS'
                                       , 'TADA.CharacteristicName')) %>%
  unique()


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
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20231129.csv')
input_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20231129.csv')
wqs_crosswalk <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20231122.csv')



#Remove insufficient data
removeCat3samples <- function(data_samples, data_sufficiency) {
  suff_sites <- data_sufficiency %>% filter(Data_Sufficient == 'Yes') %>%
    select(AUID_ATTNS, TADA.CharacteristicName) %>% unique()
  
  samples <- data_samples %>% right_join(suff_sites,
                                        by = join_by('AUID_ATTNS',
                                                     'TADA.CharacteristicName')) %>%
    filter(!is.na(TADA.ResultMeasureValue))
  
  return(samples)
}

removeCat3sites <- function(data_sufficiency) {
  suff_sites <- data_sufficiency %>%
    filter(Data_Sufficient == 'Yes') %>%
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
    select(Directionality, Frequency, Duration, Details) %>%
    unique()
  
  # write_csv(unique_methods, 'Output/data_analysis/wqs_unique_methods.csv')
  
  #Methods Start:
  #Filter out Cat 3
  
  samples_types_changed <- input_samples %>%
    mutate(Waterbody_Type = case_when(AU_Type =="Beach" | AU_Type == "Marine" ~ "Marine",
                               AU_Type == "Lake" ~ "Freshwater",
                               T~"Freshwater"))
  
  samples_types_changed2 <- input_samples_filtered %>%
    mutate(Waterbody_Type = case_when(AU_Type =="Beach" | AU_Type == "Marine" ~ "Marine",
                                      AU_Type == "Lake" ~ "Freshwater",
                                      T~"Freshwater"))
  #Find the methods that do not have any corresponding samples
  no_samples_methods <- wqs_crosswalk %>% 
    rename(TADA.CharacteristicName = TADA.Constituent) %>%
    rename(Waterbody_Type = `Waterbody Type`) %>%
    anti_join(samples_types_changed, by = c('TADA.CharacteristicName', 'Waterbody_Type')) %>%
    unique()
  
  no_suffsamples_methods <- wqs_crosswalk %>% 
    rename(TADA.CharacteristicName = TADA.Constituent) %>%
    rename(Waterbody_Type = `Waterbody Type`) %>%
    anti_join(samples_types_changed2, by = c('TADA.CharacteristicName', 'Waterbody_Type')) %>%
    unique()
  
  diff_samps <- no_suffsamples_methods %>% anti_join(no_samples_methods) %>%
    mutate(Reason = 'Insufficient Data')
  
  no_samps <- no_samples_methods %>%
    mutate(Reason = 'No Samples') %>%
    rbind(diff_samps)
  
  # write_csv(no_samps, 'Output/data_analysis/insufficient_or_no_data_methods.csv')
  
  # use AU_Type to choose Waterbody Type in data sufficiency table
  Unique_AUIDs <- unique(input_samples_filtered$AUID_ATTNS) %>% na.omit()
  result_list <- list()
  counter <- 0
  
  #Cycle by AUs
  for(i in Unique_AUIDs){
    print(i) # print name of current AU
    
    # filter data
    df_subset <- input_samples_filtered %>% 
      filter(AUID_ATTNS == i) %>%
      mutate(year = year(ActivityStartDate),
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
      filter(TADA.Constituent %in% my_constituents) %>% 
      filter(`Waterbody Type` %in% my_WtrBdy_Type) %>%
      select(!c(Magnitude_Text))
    
    #If no relevant samples, skip AU
    if(nrow(my_data_magfreqdur)==0){
      next
    }
    
    #Cycle through each parameter to calculate the mag/freq/dur
    for(j in 1:nrow(my_data_magfreqdur)) {
      counter <- counter + 1
      filter_by <- my_data_magfreqdur[j,]
      
      filt <- df_subset %>% filter(TADA.CharacteristicName == filter_by$TADA.Constituent)
      
      #Method: Maximum, not to exceed, 30-day geometric mean
      if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
         filter_by$Duration == '30-day period' & str_detect(replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T) {
        
        results <- filt %>%
          arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          mutate(geo_mean_30d = rollapplyr(TADA.ResultMeasureValue, 
                                           seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate), 
                                           geometric.mean),
                 Impacted = ifelse(geo_mean_30d >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          select(!geo_mean_30d)
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(filter(results, Impacted == 'Yes'))
        
        filter_by$Impacted <- ifelse(bad > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10% of samples' &
                filter_by$Duration == 'Water year average') {
        #Method: Maximum, 10% of samples, Water year average
        
        results <- filt %>%
          group_by(w_year) %>%
          mutate(wyear_row = n(),
                 bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/wyear_row>=0.1, 1, 0))
        
        bad_tot <- results %>% select(w_year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Not to exceed' & filter_by$Frequency == 'Not to exceed' &
              filter_by$Duration == '30-day period' & str_detect(replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T){
        #Method: Not to exceed, 30 day geometric mean
        
        results <- filt %>%
          arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          mutate(geo_mean_30d = rollapplyr(TADA.ResultMeasureValue, 
                                           seq_along(ActivityStartDate) - findInterval(ActivityStartDate - 30, ActivityStartDate), 
                                           geometric.mean),
                 Impacted = ifelse(geo_mean_30d >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          select(!geo_mean_30d)
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(filter(results, Impacted == 'Yes'))
        
        filter_by$Impacted <- ifelse(bad > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Not to exceed' & filter_by$Frequency == 'Not to exceed' &
              filter_by$Duration == 'Water year average' & str_detect(replace_na(filter_by$Details, ''), '(?i)Geometric mean') == T) {
        #Method: Maximum, not to exceed, geometric mean for water year
        
        results <- filt %>%
          arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          group_by(w_year) %>%
          mutate(geo_mean_year = geometric.mean(TADA.ResultMeasureValue),
                 Impacted = ifelse(geo_mean_year >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          select(!geo_mean_year) %>%
          ungroup()
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(filter(results, Impacted == 'Yes'))
        
        filter_by$Impacted <- ifelse(bad > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
              filter_by$Duration == 'Daily average') {
        #Method: Maximum, not to exceed, daily arithmetic mean
        
        results <- filt %>%
          arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          group_by(ActivityStartDate) %>%
          mutate(daily_mean = mean(TADA.ResultMeasureValue),
                 Impacted = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 'Yes', 'No')) %>%
          select(!daily_mean) %>%
          ungroup()
        
        filter_by$AUID_ATTNS <- i
        
        bad <- nrow(filter(results, Impacted == 'Yes'))
        
        filter_by$Impacted <- ifelse(bad > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
              filter_by$Duration == 'Daily average'){
        #Method: Maximum, 10% of samples, daily arithmetic mean
        
        results <- filt %>%
          arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          group_by(ActivityStartDate) %>%
          mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          mutate(day_row = n(),
                 bad_samp = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% ungroup() %>% select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')  
        
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
              filter_by$Duration == 'Daily average' & str_detect(replace_na(filter_by$Details, ''), '1m Depth') == T){
        #Method: Minimum, 10%, daily arithmetic mean, 1m Depth
        
        results <- filt %>%
          filter(TADA.ActivityDepthHeightMeasure.MeasureValue <= 1) %>%
          arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          group_by(ActivityStartDate) %>%
          mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          mutate(day_row = n(),
                 bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% ungroup() %>% select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')  
        
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Daily average' & str_detect(replace_na(filter_by$Details, ''), 'Estuaries and tidal tributaries') == T){
        #Method: Minimum, 10%, daily arithmetic mean
        
        results <- filt %>%
          arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          group_by(ActivityStartDate) %>%
          mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          mutate(day_row = n(),
                 bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% ungroup() %>% select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')  
        
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
              filter_by$Duration == 'Daily average'){
        #Method: Minimum, 10%, daily arithmetic mean
        
        results <- filt %>%
          arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          group_by(ActivityStartDate) %>%
          mutate(daily_mean = mean(TADA.ResultMeasureValue)) %>%
          unique() %>%
          mutate(day_row = n(),
                 bad_samp = ifelse(daily_mean <= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% ungroup() %>% select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Minimum' & filter_by$Frequency == '10%' &
              filter_by$Duration == 'Daily minimum'){
        #Method: Minimum, 10%, daily minimum
        
        results <- filt %>%
          arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          group_by(ActivityStartDate) %>%
          mutate(daily_min = min(TADA.ResultMeasureValue)) %>%
          mutate(day_row = length(unique(filt$ActivityStartDate)),
                 bad_samp = ifelse(daily_min <= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% ungroup() %>% select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
              filter_by$Duration == 'Daily maximum'){
        #Method: Maximum, 10%, daily Maximum
        
        results <- filt %>%
          arrange(ActivityStartDate, ActivityStartTime.Time) %>%
          group_by(ActivityStartDate) %>%
          mutate(daily_max = max(TADA.ResultMeasureValue)) %>%
          mutate(day_row = length(unique(filt$ActivityStartDate)),
                 bad_samp = ifelse(daily_max >= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/day_row>=0.1, 1, 0))
        
        bad_tot <- results %>% ungroup() %>% select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '10%' &
                filter_by$Duration == 'Not to exceed'){
        #Method: Maximum, 10%, not to exceed
        
        results <- filt %>%
          mutate(num_samples = n(),
                 bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0),
                 sum = sum(bad_samp),
                 bad_year = ifelse(sum/num_samples>=0.1, 1, 0))
        
        bad_tot <- results %>% select(year, bad_year) %>% unique()
        bad_sum <- sum(bad_tot$bad_year)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')  
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
               filter_by$Duration == 'Not to exceed'){
        #Method: Maximum, not to exceed, not to exceed
        
        results <- filt %>%
          mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0))
        
        bad_tot <- results %>% select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
               filter_by$Duration == 'Arithmetic mean' &
               str_detect(replace_na(filter_by$Details, ''), 'Frequency - harmonic mean of most recent 3 years') == T){
        #Method: Maximum, not to exceed, arithmetic Mean of last 3 years
        
        max_year <- filt %>% select(year) %>% max() %>% unique()
        results <- filt %>%
          filter(year >= max_year - 3) %>%
          mutate(mean_samps = mean(TADA.ResultMeasureValue), 
                 bad_samp = ifelse(mean_samps >= filter_by$Magnitude_Numeric, 1, 0))
        
        bad_tot <- results %>% select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '24-hour max'){
        #Method: Maximum, 1 instance in previous 3 years, 24 hour max
        
        max_year <- results %>% select(year) %>% max() %>% unique()
        results <- filt %>%
          filter(year >= max_year - 3) %>%
          group_by(ActivityStartDate) %>%
          mutate(max_samps = max(TADA.ResultMeasureValue), 
                 bad_samp = ifelse(max_samps >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '24-hour average' & is.na(filter_by$Details) == T){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 24 hour average
        
        results <- filt %>%
          group_by(ActivityStartDate) %>%
          mutate(roll_1day_mean = map_dbl(ActivityStartDate, 
                                          ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(1), .x)])), 
                 bad_samp = ifelse(daily_mean >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          ungroup() %>%
          select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
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
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour average' & str_detect(replace_na(filter_by$Details, ''), 'Magnitude is minimum') == T){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude is min
  
        results <- filt %>%
          group_by(ActivityStartDate) %>%
          arrange(ActivityStartDate) %>%
          mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                          ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                 bad_samp = ifelse(roll_4day_mean < filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          ungroup() %>%
          select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
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
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour average' & is.na(filter_by$Magnitude_Numeric) == T &
                str_detect(replace_na(filter_by$Details, ''), 'pH') == F){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude dependent on equations
        #Hardness dependent magnitudes - NOT pH DEPENDENT
        #Chronic
        
        #Pull matching hardness samples
        hardness <- df_subset %>%
          filter(TADA.CharacteristicName == "HARDNESS, CA, MG") %>%
          rename(Hardness = TADA.ResultMeasureValue) %>%
          select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, Hardness)
        #Need to make magnitude for: Chromium (III), copper, lead, nickel, zinc - all chronic
        if(filter_by$Constituent == 'Chromium (III)'){
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(0.819*(log(Hardness))+0.6848)*0.860)
          
        } else if(filter_by$Constituent == 'Copper') {
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(0.8545*(log(Hardness))-1.702)*0.960)
          
        } else if(filter_by$Constituent == 'Lead') {
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(1.273*(log(Hardness))-4.705)*(1.46203-log(Hardness)*0.145712))
          
        } else if(filter_by$Constituent == 'Nickel') {
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(0.846*(log(Hardness))+0.0584)*0.997)
          
        } else if(filter_by$Constituent == 'Zinc') {
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(0.8473*(log(Hardness))+0.884)*0.986)
          
        } else if(filter_by$Constituent == 'Cadmium') {
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(0.7409*(log(Hardness))-4.719)*(1.101672-log(Hardness)*0.041838))
          
        }
        
        results <- joined %>%
          group_by(ActivityStartDate) %>%
          arrange(ActivityStartDate) %>%
          mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                          ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                 bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0)) 
        
        bad_tot <- results %>% 
          ungroup() %>%
          select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
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
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour average' & is.na(filter_by$Magnitude_Numeric) == F){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude listed
        
        results <- filt %>%
          group_by(ActivityStartDate) %>%
          arrange(ActivityStartDate) %>%
          mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                          ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                 bad_samp = ifelse(roll_4day_mean >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          ungroup() %>%
          select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
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
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                 filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == T &
                 str_detect(replace_na(filter_by$Details, ''), 'pH') == F){
        #Method: Maximum, 1 in most recent 3 years, 1 hour average, magnitude dependent on equations
        #Hardness dependent magnitudes - NOT pH DEPENDENT
        #Acute
        
        #Pull matching hardness samples
        hardness <- df_subset %>%
          filter(TADA.CharacteristicName == "HARDNESS, CA, MG") %>%
          rename(Hardness = TADA.ResultMeasureValue) %>%
          select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, Hardness)
        #Need to make magnitude for: Chromium (III), copper, lead, nickel, zinc - all chronic
        if(filter_by$Constituent == 'Chromium (III)'){
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(0.819*(log(Hardness))+3.7256)*0.316)
          
        } else if(filter_by$Constituent == 'Copper') {
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(0.9422*(log(Hardness))-1.700)*0.960)
          
        } else if(filter_by$Constituent == 'Lead') {
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(1.273*(log(Hardness))-1.460)*(1.46203-log(Hardness)*0.145712))
          
        } else if(filter_by$Constituent == 'Nickel') {
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(0.846*(log(Hardness))+2.255)*0.998)
          
        } else if(filter_by$Constituent == 'Zinc') {
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(0.8473*(log(Hardness))+0.884)*0.978)
          
        } else if(filter_by$Constituent == 'Cadmium') {
          #Combine matching hardness & calculate magnitude
          joined <- filt %>%
            inner_join(hardness, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(1.0166*(log(Hardness))-3.924)*(1.136672-log(Hardness)*0.041838))
          
        }
        
        max_year <- filt %>% select(year) %>% max() %>% unique()
        results <- joined %>%
          filter(year >= max_year - 3) %>%
          group_by(ActivityStartDate) %>%
          mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= magnitude, 1, 0))
        
        bad_tot <- results %>% select(bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == F &
                str_detect(replace_na(filter_by$Details, ''), 'pH') == F){
        #Method: Maximum, 1 in most recent 3 years, 1 hour average, magnitude dependent on equations
        #Acute
        
        max_year <- filt %>% select(year) %>% max() %>% unique()
        results <- filt %>%
          filter(year >= max_year - 3) %>%
          group_by(ActivityStartDate) %>%
          mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '96-hour average' & is.na(filter_by$Magnitude_Numeric) == F &
                str_detect(replace_na(filter_by$Details, ''), 'pH') == F){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average
  
        
        results <- filt %>%
          group_by(ActivityStartDate) %>%
          arrange(ActivityStartDate) %>%
          mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
                                          ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
                 bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0)) 
        
        bad_tot <- results %>% 
          ungroup() %>%
          select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
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
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
        
        } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                 filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == F &
                 str_detect(replace_na(filter_by$Details, ''), 'pH') == T){
        #Method: Maximum, 1 in most recent 3 years, 1 hour average, magnitude dependent on equations
        #Acute
        #Pull matching pH and temperature samples
        pH <- df_subset %>%
          filter(TADA.CharacteristicName == "pH") %>%
          rename(pH = TADA.ResultMeasureValue) %>%
          select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, pH)
        
        if(filter_by$Constituent == 'Pentachloro-phenol') {
          #Combine matching pH & calculate magnitude
          joined <- filt %>%
            inner_join(pH, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
            mutate(magnitude = exp(1.005*pH-4.869)) #Equation from WQS sheet
          
        }
        
        max_year <- joined %>% select(year) %>% max() %>% unique()
        results <- joined %>%
          filter(year >= max_year - 3) %>%
          group_by(ActivityStartDate) %>%
          mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= magnitude, 1, 0)) 
        
        bad_tot <- results %>% select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      }  else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
                filter_by$Duration == '1-hour average' & is.na(filter_by$Magnitude_Numeric) == F){
        #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 1 hour average, magnitude listed
        
        #1-hour averages: use the daily value
        #(it will be rare that multiple samples are taken in a day, and if they are they would be considered duplicates)
        results <- filt %>%
          group_by(ActivityStartDate) %>%
          arrange(ActivityStartDate) %>%
          mutate(bad_samp = ifelse(TADA.ResultMeasureValue >= filter_by$Magnitude_Numeric, 1, 0)) 
        
        bad_tot <- results %>% 
          ungroup() %>%
          select(year, ActivityStartDate, bad_samp) %>%
          unique() %>%
          #If more than 2 exceedances in 3 years assign value of 1, else 0
          #Left bound is date - 3 years, right bound is date
          mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
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
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '1 in most recent 3 years' &
                filter_by$Duration == '24-hour average'){
        #Method: Maximum, 1 in most recent 3 years, 24 hour average
        
        max_year <- filt %>% select(year) %>% max() %>% unique()
        results <- filt %>%
          filter(year >= max_year - 3) %>%
          group_by(ActivityStartDate) %>%
          mutate(daily_avg = mean(TADA.ResultMeasureValue),
                 bad_samp = ifelse(daily_avg >= magnitude, 1, 0)) 
        
        bad_tot <- results %>% select(ActivityStartDate, bad_samp) %>% unique()
        bad_sum <- sum(bad_tot$bad_samp)
        
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
        
      } else {
        filter_by$AUID_ATTNS <- i
        filter_by$Impacted <- 'Method not coded!'
      }
      
      result_list[[counter]] <- filter_by
    } 
  } # end of for loop 
  
  
  df_loop_results <- do.call("rbind", result_list) # combine results from for loop
  df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
  df_AU_data_WQS <- df_AU_data_WQS %>% 
    distinct()
  
  df_AU_data_WQS %>% select(Impacted) %>% group_by(Impacted) %>% mutate(n = n()) %>% unique()
  
  # test <- df_AU_data_WQS %>% filter(is.na(Impacted))
  
  #Find methods not coded:
  not_coded <- df_AU_data_WQS %>% filter(Impacted == 'Method not coded!')
  
  no_samps_not_coded <- no_samps %>% right_join(not_coded)
  
  #combine with data sufficiency table
  data_suff_WQS <- df_AU_data_WQS %>%
    rename(TADA.CharacteristicName = TADA.Constituent) %>%
    full_join(input_sufficiency, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Waterbody Type',
                                        'Fraction', 'Type'))
  
  return(data_suff_WQS)

}


output <- MagDurFreq(wqs_crosswalk, input_samples_filtered, input_sufficiency)

categorize <- output %>%
  mutate(Overall_Category = case_when(Data_Sufficient == "(?i)No" ~ '3',
                                      Impacted == 'Yes' ~ '5',
                                      Impacted == 'No' ~ '2',
                                      T ~ NA))












#Marine Ammonia stuff

# else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
#         filter_by$Duration == '96-hour average' & is.na(filter_by$Magnitude_Numeric) == T &
#         str_detect(replace_na(filter_by$Details, ''), 'pH') == T){
#   #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude dependent on equations
#   #pH Dependent 
#   #Chronic
#   
#   #Pull matching pH and temperature samples
#   pH <- df_subset %>%
#     filter(TADA.CharacteristicName == "pH") %>%
#     rename(pH = TADA.ResultMeasureValue) %>%
#     select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, pH)
#   
#   if(filter_by$Constituent == 'Pentachloro-phenol') {
#     #Combine matching pH & calculate magnitude
#     joined <- filt %>%
#       inner_join(pH, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
#       mutate(magnitude = exp(1.005*pH-5.134)) #Equation from WQS sheet
#     
#   }
#   
#   results <- joined %>%
#     group_by(ActivityStartDate) %>%
#     arrange(ActivityStartDate) %>%
#     mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
#                                     ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - days(4), .x)])), 
#            bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0)) 
#   
#   bad_tot <- results %>% 
#     ungroup() %>%
#     select(year, ActivityStartDate, bad_samp) %>%
#     unique() %>%
#     #If more than 2 exceedances in 3 years assign value of 1, else 0
#     #Left bound is date - 3 years, right bound is date
#     mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
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
#   filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
#   
# }


# else if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == '≥2 exceedances and >5% exceedance frequency in 3 year period' &
#         filter_by$Duration == '96-hour average' & is.na(filter_by$Magnitude_Numeric) == T &
#         str_detect(replace_na(filter_by$Details, ''), 'pH') == T){
#   #Method: Maximum, ≥2 exceedances and >5% exceedance frequency in 3 year period, 96 hour average, magnitude dependent on equations
#   #pH Dependent & Temperature Dependent & Salinity dependent
#   #Chronic
#   
#   #Pull matching pH and temperature samples
#   pH <- df_subset %>%
#     filter(TADA.CharacteristicName == "pH") %>%
#     rename(pH = TADA.ResultMeasureValue) %>%
#     select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, pH)
#   
#   temperature <- df_subset %>%
#     filter(TADA.CharacteristicName == "TEMPERATURE, WATER") %>%
#     rename(Temperature = TADA.ResultMeasureValue) %>%
#     select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, Temperature)
#   
#   salinity <- df_subset %>%
#     filter(TADA.CharacteristicName == "SALINITY") %>%
#     rename(Salinity = TADA.ResultMeasureValue) %>%
#     select(ActivityStartDate, ActivityStartTime.Time, AUID_ATTNS, Salinity)
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
#       mutate(pH_lookup = plyr::round_any(pH, 0.2),
#              temp_lookup = plyr::round_any(Temperature, 5),
#              salinity_lookup = plyr::round_any(Salinity, 10)) 
#     
#   } else if(filter_by$Constituent == 'Pentachloro-phenol') {
#     #Combine matching pH & calculate magnitude
#     joined <- filt %>%
#       inner_join(pH, by = c('ActivityStartDate', 'ActivityStartTime.Time', 'AUID_ATTNS')) %>%
#       mutate(magnitude = exp(1.005*pH-5.134)) #Equation from WQS sheet
#     
#   }
#   
#   results <- joined %>%
#     group_by(ActivityStartDate) %>%
#     arrange(ActivityStartDate) %>%
#     mutate(roll_4day_mean = map_dbl(ActivityStartDate, 
#                                     ~mean(TADA.ResultMeasureValue[between(ActivityStartDate, .x - hours(96), .x)])), 
#            bad_samp = ifelse(roll_4day_mean >= magnitude, 1, 0)) 
#   
#   bad_tot <- results %>% 
#     ungroup() %>%
#     select(year, ActivityStartDate, bad_samp) %>%
#     unique() %>%
#     #If more than 2 exceedances in 3 years assign value of 1, else 0
#     #Left bound is date - 3 years, right bound is date
#     mutate(Exceedances = ifelse(map_dbl(ActivityStartDate, 
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
#   filter_by$Impacted <- ifelse(bad_sum > 0, 'Yes', 'No')
#   
# } 











#Connecting samples to Magnitude, Frequency, and Duration requirements
connect_info <- input_sufficiency %>%
  select(AUID_ATTNS, TADA.CharacteristicName, `Waterbody Type`) %>%
  unique() %>%
  mutate(`Waterbody Type` = ifelse(str_detect(`Waterbody Type`
                                              , 'streams and rivers')
                                   , 'Freshwater', `Waterbody Type`))

#Creates duplicates depending on if a site has both marine and freshwater requirements
join_info_samples <- input_samples %>%
  left_join(connect_info, by = join_by('AUID_ATTNS'
                                       , 'TADA.CharacteristicName')) %>%
  unique()


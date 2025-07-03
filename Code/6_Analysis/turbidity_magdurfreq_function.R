#Turbidity analysis

#Written by: Hannah Ferriby

#Required packages
library(tidyverse)

set.seed(42)

#Load in data
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20250701.csv') 
input_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20250630.csv')
wqs_crosswalk <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20250429.csv')


#Create reference site table for analysis
turbidity_sites <- read_csv('Data/data_analysis/turbidity_sites.csv')


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

input_samples_filtered <- filterCat3samples(data_samples = input_samples, data_sufficiency = input_sufficiency)


#Function to output list of AUs with sufficient turbidity and their monitoring locations
#ADD MANUAL INPUT INTO REFERENCE SITE -> UPSTREAM/DOWNSTREAM!!!!!!!
findTurbidityReferenceSites <- function(input_samples_filtered) {
  
  #Find all AUs with sufficient turbidity
  AUsufficient <- input_samples_filtered %>%
    dplyr::filter(TADA.CharacteristicName == 'TURBIDITY') %>%
    dplyr::select(AUID_ATTNS, MonitoringLocationIdentifier) %>%
    unique() %>%
    dplyr::arrange(AUID_ATTNS) %>%
    dplyr::group_by(AUID_ATTNS) %>%
    dplyr::reframe(AUID_ATTNS = AUID_ATTNS,
                   MonitoringLocationIdentifier = list(unique(MonitoringLocationIdentifier)),
                   n_MonitoringLocations = n()) %>%
    unique()
  
  return(AUsufficient)
}

list_of_needed_sites <- findTurbidityReferenceSites(input_samples_filtered)


####Version 2####

wqs_crosswalk_filt <- wqs_crosswalk %>% 
  filter(Constituent == 'Turbidity') %>%
  #Pull percentage increase and make it a numeric
  mutate(Magnitude_Numeric = ifelse(str_detect(Magnitude_Text, "%"),
                                    parse_number(Magnitude_Text),
                                    Magnitude_Numeric),
         Units = ifelse(str_detect(Magnitude_Text, "%"),
                        "Percent",
                        Units))
#Prepare results list
result_list <- list()
counter <- 0

#Loop over each standards row
for(i in 1:nrow(wqs_crosswalk_filt)) {
  
  wqs_row <- wqs_crosswalk_filt[i,]
  threshold <- wqs_row$Magnitude_Numeric
  threshold_unit <- wqs_row$Units
  
  #Loop over each unique Group in turbidity_sites
  groups <- unique(turbidity_sites$Group)
  for (g in groups) {
    counter <- counter + 1
    group_meta <- turbidity_sites %>% filter(Group == g)
    method <- unique(group_meta$Method)
    
    #QA check
    if (length(method) != 1) stop(paste("Group", g, "has conflicting methods"))
    
    #Pull test site(s)
    test_sites <- group_meta %>%
      filter(Role == "Test") %>% pull(SiteID)
    
    #Pull reference site(s)
    ref_sites <- group_meta %>%
      filter(Role == "Reference") %>% pull(SiteID)
    
    #Pull test site(s) samples
    test_df <- input_samples_filtered %>%
      filter(MonitoringLocationIdentifier %in% test_sites) %>%
      group_by(ActivityStartDate) %>%
      summarise(daily_avg = mean(TADA.ResultMeasureValue, na.rm = TRUE), .groups = "drop")
    
    #Pull reference site(s) samples
    ref_df <- input_samples_filtered %>%
      filter(MonitoringLocationIdentifier %in% ref_sites) %>%
      group_by(ActivityStartDate) %>%
      summarise(daily_avg = mean(TADA.ResultMeasureValue, na.rm = TRUE), .groups = "drop")
    
    #If there's no samples - return insufficient data
    if(nrow(test_df) == 0 | nrow(ref_df) == 0){
      
      wqs_row$Group <- g
      wqs_row$Method <- method
      wqs_row$Threshold <- NA
      wqs_row$Pctl90 <- NA
      wqs_row$LCL90 <- NA
      wqs_row$Exceed <- "Insufficient Data"
      wqs_row$Exceed_Num <- NA
      wqs_row$Exceed_Freq <- NA
      
      next
    }
    
    #Calculate dynamic threshold if percentage-based
    ref_mean <- mean(ref_df$daily_avg, na.rm = TRUE)
    if (threshold_unit == "Percent") {
      #Cap maximum increase if Magnitude_Text includes "not to exceed"
      #Otherwise apply % increase
      max_increase <- ifelse(!is.na(wqs_row$Details) && str_detect(wqs_row$Details, "not to exceed .*?([0-9]+) NTU"),
                             as.numeric(str_extract(wqs_row$Details, "[0-9]+(?= NTU)")),
                             Inf)
      increase <- ref_mean * (threshold / 100)
      threshold_value <- ref_mean + min(increase, max_increase)
      
    } else {
      threshold_value <- ref_mean + threshold
    }
    
    #Calculate Distrubution of Differences
    if (method == "DoD") {
      
      #Log-transform the daily average turbidity values for test and reference sites
      test_log <- log(test_df$daily_avg)
      ref_log <- log(ref_df$daily_avg)
      
      #Calculate the mean difference between test and reference log-transformed turbidity
      diff_mean <- mean(test_log) - mean(ref_log)
      
      #Calculate the standard deviation of the difference using variance and sample size
      #Variance = st.dev^2 so can cimplify this eq
      diff_sd <- sqrt(var(test_log)/length(test_log) + var(ref_log)/length(ref_log))
      
      #Get the z-score for the 90th percentile (used to calculate the 90th percentile of the distribution)
      z90 <- qnorm(0.9)
      
      #Compute the 90th percentile of the difference distribution (back-transformed from log scale)
      pctl90 <- exp(diff_mean + z90 * diff_sd)
      
      #Get z-score for lower confidence limit (LCL), alpha = 0.1 one-sided
      z_alpha <- qnorm(0.9 - 0.1)
      
      #Estimate standard error for the percentile using binomial proportion error formula
      se_pct <- sqrt((0.9*0.1)/length(test_log) + (0.9*0.1)/length(ref_log))
      
      #Calculate the adjusted percentile corresponding to the LCL
      lcl_pctile <- 0.9 - z_alpha * se_pct
      
      #Convert the LCL from log units to NTU by applying inverse log transformation
      lcl_val <- exp(qnorm(lcl_pctile, mean = diff_mean, sd = diff_sd))
      
      #If the LCL is greater than the threshold, it's considered an exceedance (impaired)
      result <- ifelse(lcl_val > threshold_value, "Yes", "No")
      
      
      wqs_row$Group <- g
      wqs_row$Method <- method
      wqs_row$Threshold <- threshold_value
      wqs_row$Pctl90 <- pctl90
      wqs_row$LCL90 <- lcl_val
      wqs_row$Exceed <- result
      wqs_row$Exceed_Num <- NA
      wqs_row$Exceed_Freq <- NA
      
      
    } else if (method == "Avg") {
      test_mean <- mean(test_df$daily_avg, na.rm = TRUE)
      ref_mean <- mean(ref_df$daily_avg, na.rm = TRUE)
      diff <- test_mean - ref_mean
      result <- ifelse(diff > (threshold_value - ref_mean), "Yes", "No")
      
      wqs_row$Group <- g
      wqs_row$Method <- method
      wqs_row$Threshold <- threshold_value
      wqs_row$TestMean <- test_mean
      wqs_row$RefMean <- ref_mean
      wqs_row$Difference <- diff
      wqs_row$Exceed <- result
      wqs_row$Exceed_Num <- NA
      wqs_row$Exceed_Freq <- NA
      
      
      result_list[[counter]] <- wqs_row
    }
  }
}

# Combine all group results
df_loop_results <- bind_rows(result_list)
df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
df_AU_data_WQS <- df_AU_data_WQS %>% 
  distinct()

#combine with relevant WQS table, removing the constituents that are calculated in other functions
#these constituents come back in the hardness, pH, and turbidity specific functions
relevant_suff <- input_sufficiency %>%
  dplyr::filter(TADA.CharacteristicName == 'TURBIDITY')

# data_suff_WQS <- df_AU_data_WQS %>%
#   dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
#   dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Use Description', 'Waterbody Type', #DEC added Use Description
#                                          'Fraction', 'Type'),
#                    relationship = "many-to-many") %>%
#   dplyr::relocate(c(Exceed_Num, Exceed_Freq, Exceed), .after = last_col())







####OLD!!!!!!!!!!####

#FUNCTION WOULDN'T LET REFERENCE SITE AND TEST SITE BE IN SAME AUID - FIX!!!!!!!
#Turbidity Function
# MagDurFreq_turbidity <- function(wqs_crosswalk, input_samples_filtered, input_sufficiency, reference_sites) {
#   
#   ##Magnitude, Frequency, Duration - unique combinations
#   #This is not used in the code, but instead used as reference for making the methods
#   unique_methods <- wqs_crosswalk %>%
#     dplyr::filter(Constituent == 'Turbidity') %>%
#     dplyr::select(Directionality, Frequency, Duration, Details) %>%
#     unique()
#   
#   #Filter samples for just turbidity
#   input_samples_filtered_relevant <- input_samples_filtered %>%
#     dplyr::filter(TADA.CharacteristicName == 'TURBIDITY')
#   
#   #Calculate mean turbidity at reference sites
#   pull_reference <- input_samples_filtered_relevant %>%
#     dplyr::filter(MonitoringLocationIdentifier %in% reference_sites$ReferenceSites) %>%
#     dplyr::group_by(MonitoringLocationIdentifier) %>%
#     dplyr::mutate(mean_reference = mean(TADA.ResultMeasureValue)) %>%
#     dplyr::select(MonitoringLocationIdentifier, mean_reference) %>%
#     unique()
#   
#   #Find AUs without reference site and set natural conditions = 0 NTU
#   not_in_reference <- input_samples_filtered_relevant %>%
#     dplyr::filter(!AUID_ATTNS %in% reference_sites$AUID_ATTNS) %>%
#     dplyr::mutate(ReferenceSites = NA,
#                   mean_reference = 0) %>%
#     dplyr::select(AUID_ATTNS, ReferenceSites, mean_reference) %>%
#     unique()
#   
#   #Combine means with reference sites and add on AUs with no reference
#   reference_sites_mean <- reference_sites %>%
#     dplyr::left_join(pull_reference, by = c('ReferenceSites' = 'MonitoringLocationIdentifier')) %>%
#     rbind(not_in_reference) %>%
#     dplyr::filter(!is.na(AUID_ATTNS))
#   
#   
#   #Return message if no samples available
#   if(nrow(input_samples_filtered_relevant) == 0) {
#     #If no samples available - just return sufficiency with empty Exceed column
#     relevant_suff <- input_sufficiency %>%
#       dplyr::filter(TADA.CharacteristicName == 'TURBIDITY') %>%
#       dplyr::mutate(Exceed = NA)
#     
#     return(relevant_suff)
#   }
#   
#   # use AU_Type to choose Waterbody Type in WQS table
#   Unique_AUIDs <- unique(reference_sites_mean$AUID_ATTNS) %>% stats::na.omit()
#   result_list <- list()
#   counter <- 0
#   
#   #Cycle by AUs
#   for(i in Unique_AUIDs){
#     print(i) # print name of current AU
#     
#     # Filter data for just AU and make water year
#     df_subset <- input_samples_filtered_relevant %>% 
#       dplyr::filter(AUID_ATTNS == i) %>%
#       dplyr::filter(TADA.CharacteristicName == 'TURBIDITY') %>%
#       mutate(year = lubridate::year(ActivityStartDate),
#              month = lubridate::month(ActivityStartDate),
#              w_year = ifelse(month < 10, year, year+1))
#     
#     # obtain AU_Type
#     my_AU_Type <- unique(df_subset$AU_Type)
#     
#     # use AU_Type to choose Waterbody Type in data standards table
#     if(my_AU_Type == "Beach" | my_AU_Type == "Marine"){
#       my_WtrBdy_Type <- "Marine"
#     } else if (my_AU_Type == "Lake"){
#       my_WtrBdy_Type <- "Freshwater"
#     } else {
#       my_WtrBdy_Type <- c("Freshwater", "Freshwater streams and rivers")
#     } # end if/else statement
#     
#     # obtain unique constituents from WQ dataset for the AU
#     my_constituents <- unique(df_subset$TADA.CharacteristicName)
#     
#     # trim data WQS table to only relevant information
#     #remove information for instances found in the special case functions
#     my_data_magfreqdur <- wqs_crosswalk %>% 
#       dplyr::filter(TADA.Constituent %in% my_constituents) %>% 
#       dplyr::filter(`Waterbody Type` %in% my_WtrBdy_Type) %>%
#       dplyr::filter(Constituent == 'Turbidity') 
#     
#     
#     #If no relevant samples, skip AU
#     if(nrow(my_data_magfreqdur)==0){
#       next
#     }
#     
#     #Pull reference value for AU
#     au_reference_conditions <- reference_sites_mean %>%
#       filter(AUID_ATTNS == i) %>%
#       select(mean_reference) %>%
#       pull()
#     
#     #Cycle through each parameter to calculate the mag/freq/dur
#     for(j in 1:nrow(my_data_magfreqdur)) {
#       counter <- counter + 1
#       #Pull relevant methods
#       filter_by <- my_data_magfreqdur[j,]
#       
#       #Pull just that constituent data
#       filt <- df_subset %>% dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent)
#       
#       #All turbidity analysis is for maximum value, not to exceed in a 24-hour average (daily)
#       if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                              'May not exceed 25 NTU above natural conditions') == T){
#         #Method #1 ----
#         results <- filt %>%
#           dplyr::group_by(ActivityStartDate) %>%
#           dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
#                         bad_samp = ifelse(daily_avg >= (au_reference_conditions+25), 1, 0)) 
#         
#         bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
#         bad_sum <- sum(bad_tot$bad_samp)
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- bad_sum
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
#         
#       } else if((stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                      'For lake waters, may not exceed 5 NTU above natural conditions') == T |
#                  stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                      'For lakes, turbidity may not exceed 5 NTU above natural turbidity.') == T |
#                  stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                      '5 NTU above natural conditions, for all lake waters') == T) &
#                 my_AU_Type == "Lake"){
#         #Method #2 ----
#         results <- filt %>%
#           dplyr::group_by(ActivityStartDate) %>%
#           dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
#                         bad_samp = ifelse(daily_avg >= (au_reference_conditions+5), 1, 0)) 
#         
#         bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
#         bad_sum <- sum(bad_tot$bad_samp)
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- bad_sum
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
#         
#       } else if((stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                      'For lake waters, may not exceed 5 NTU above natural conditions') == T |
#                  stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                      'For lakes, turbidity may not exceed 5 NTU above natural turbidity.') == T |
#                  stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                      '5 NTU above natural conditions, for all lake waters') == T) &
#                 my_AU_Type != "Lake"){
#         #Method #3 ----
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- NA
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- 'AU not lake waters'
#         
#       } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                     '5 NTU above natural conditions, when natural turbidity is 50 NTU or less.') == T &
#                 au_reference_conditions <= 50){
#         #Method #4 ----
#         results <- filt %>%
#           dplyr::group_by(ActivityStartDate) %>%
#           dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
#                         bad_samp = ifelse(daily_avg >= (au_reference_conditions+5), 1, 0)) 
#         
#         bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
#         bad_sum <- sum(bad_tot$bad_samp)
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- bad_sum
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
#         
#       } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                     '5 NTU above natural conditions, when natural turbidity is 50 NTU or less.') == T &
#                 au_reference_conditions > 50){
#         #Method #5 ----
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- NA
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- 'Natural conditions greater than 50 NTU'
#         
#       }  else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                      'No more than 10% increase when natural condition is more than 50 NTU, not to exceed max increase of 15 NTU') == T &
#                  au_reference_conditions > 50){
#         #Method #6 ----
#         
#         max_over <- ifelse(au_reference_conditions*0.1 >= 15, 15, au_reference_conditions*0.1)
#         
#         results <- filt %>%
#           dplyr::group_by(ActivityStartDate) %>%
#           dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
#                         bad_samp = ifelse(daily_avg >= (max_over+au_reference_conditions), 1, 0)) 
#         
#         bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
#         bad_sum <- sum(bad_tot$bad_samp)
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- bad_sum
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
#         
#       } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                     'No more than 10% increase when natural condition is more than 50 NTU, not to exceed max increase of 15 NTU') == T &
#                 au_reference_conditions <= 50){
#         #Method #7 ----
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- NA
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- "Natural conditions less than or equal to 50 NTU"
#         
#       } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                     'When natural condition is more than 50 NTU, not to exceed max increase of 15 NTU') == T &
#                 au_reference_conditions > 50){
#         #Method #8 ----
#         
#         results <- filt %>%
#           dplyr::group_by(ActivityStartDate) %>%
#           dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
#                         bad_samp = ifelse(daily_avg >= (au_reference_conditions+15), 1, 0)) 
#         
#         bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
#         bad_sum <- sum(bad_tot$bad_samp)
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- bad_sum
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
#         
#       } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                     'When natural condition is more than 50 NTU, not to exceed max increase of 15 NTU') == T &
#                 au_reference_conditions <= 50){
#         #Method #9 ----
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- NA
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- "Natural conditions less than or equal to 50 NTU"
#         
#       } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                     'May not exceed 10 NTU above natural conditions when natural turbidity is 50 NTU or less') == T &
#                 au_reference_conditions <= 50){
#         #Method #10 ----
#         
#         results <- filt %>%
#           dplyr::group_by(ActivityStartDate) %>%
#           dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
#                         bad_samp = ifelse(daily_avg >= (au_reference_conditions+10), 1, 0)) 
#         
#         bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
#         bad_sum <- sum(bad_tot$bad_samp)
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- bad_sum
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
#         
#       } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                     'May not exceed 10 NTU above natural conditions when natural turbidity is 50 NTU or less') == T &
#                 au_reference_conditions > 50){
#         #Method #11 ----
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- NA
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- "Natural conditions greater than 50 NTU"
#         
#       } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                     'May not exceed 20% increase in turbidity when natural turbidity is greater than 50 NTU, not to exceed a maximum increase of 15 NTU') == T &
#                 au_reference_conditions > 50){
#         #Method #12 ----
#         max_over <- ifelse(au_reference_conditions*0.2 >= 15, 15, au_reference_conditions*0.1)
#         
#         results <- filt %>%
#           dplyr::group_by(ActivityStartDate) %>%
#           dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
#                         bad_samp = ifelse(daily_avg >= (max_over+au_reference_conditions), 1, 0)) 
#         
#         bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
#         bad_sum <- sum(bad_tot$bad_samp)
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- bad_sum
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
#         
#       } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                     'May not exceed 20% increase in turbidity when natural turbidity is greater than 50 NTU, not to exceed a maximum increase of 15 NTU') == T &
#                 au_reference_conditions <= 50){
#         #Method #13 ----
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- NA
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- "Natural conditions less than or equal to 50 NTU"
#         
#       } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                     'No more than 10% increase when natural condition is more than 50 NTU, not to exceed a maximum increase of 25 NTU.') == T &
#                 au_reference_conditions > 50){
#         #Method #14 ----
#         max_over <- ifelse(au_reference_conditions*0.1 >= 25, 25, au_reference_conditions*0.1)
#         
#         results <- filt %>%
#           dplyr::group_by(ActivityStartDate) %>%
#           dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
#                         bad_samp = ifelse(daily_avg >= (max_over+au_reference_conditions), 1, 0)) 
#         
#         bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
#         bad_sum <- sum(bad_tot$bad_samp)
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- bad_sum
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
#         
#       } else if(stringr::str_detect(tidyr::replace_na(filter_by$Details, ''),
#                                     'No more than 10% increase when natural condition is more than 50 NTU, not to exceed a maximum increase of 25 NTU.') == T &
#                 au_reference_conditions <= 50){
#         #Method #15 ----
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- NA
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- "Natural conditions less than or equal to 50 NTU"
#         
#       } else if(is.na(filter_by$Details) == T){
#         #Method #16 ----
#         #Marine turbidity - if 24-hour average turbidity is larger than specified magnitude
#         results <- filt %>%
#           dplyr::group_by(ActivityStartDate) %>%
#           dplyr::mutate(daily_avg = mean(TADA.ResultMeasureValue),
#                         bad_samp = ifelse(daily_avg >= filter_by$Magnitude_Numeric, 1, 0)) 
#         
#         bad_tot <- results %>% dplyr::select(ActivityStartDate, bad_samp) %>% unique()
#         bad_sum <- sum(bad_tot$bad_samp)
#         
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- bad_sum
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- ifelse(bad_sum > 0, 'Yes', 'No')
#         
#       } else {
#         filter_by$AUID_ATTNS <- i
#         filter_by$Exceed_Num <- NA
#         filter_by$Exceed_Freq <- NA
#         filter_by$Exceed <- 'Method not coded!'
#       } #End of methods if/else
#       
#       result_list[[counter]] <- filter_by
#     } #End of MagDurFreq loop
#     
#   } #End of AU loop 
#   
#   df_loop_results <- do.call("rbind", result_list) # combine results from for loop
#   df_AU_data_WQS <- as.data.frame(df_loop_results) # convert to data frame
#   df_AU_data_WQS <- df_AU_data_WQS %>% 
#     distinct()
#   
#   #combine with relevant data standards table
#   relevant_suff <- input_sufficiency %>%
#     dplyr::filter(TADA.CharacteristicName == 'TURBIDITY')
#   
#   data_suff_WQS <- df_AU_data_WQS %>%
#     dplyr::rename(TADA.CharacteristicName = TADA.Constituent) %>%
#     dplyr::full_join(relevant_suff, by = c('AUID_ATTNS', 'TADA.CharacteristicName', 'Use', 'Use Description','Waterbody Type',
#                                            'Fraction', 'Type'),
#                      relationship = "many-to-many") %>%
#     dplyr::relocate(c(Exceed_Num, Exceed_Freq, Exceed), .after = last_col()) %>%
#     dplyr::select(!Magnitude_Text)
#   
#   return(data_suff_WQS)
# } #End of turbidity function
# 
# output_turbidity <- MagDurFreq_turbidity(wqs_crosswalk, input_samples_filtered, input_sufficiency, turbidity_sites)







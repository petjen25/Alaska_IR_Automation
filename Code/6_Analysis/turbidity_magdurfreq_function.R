#Turbidity analysis

#Written by: Hannah Ferriby

#Required packages
library(tidyverse)

set.seed(42)

#Load in data
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20240117.csv')
input_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20240117.csv')
wqs_crosswalk <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20240117.csv')


#Get turbidity samples
turbidity_samples <- input_samples %>%
  filter(TADA.CharacteristicName == 'TURBIDITY') %>%
  filter(!is.na(AUID_ATTNS))

#Pull 5 monitoring sites
sites <- turbidity_samples %>%
  select(MonitoringLocationIdentifier) %>%
  unique() %>%
  slice_sample(n=5) %>%
  pull() 

au_sites <- turbidity_samples %>%
  filter(MonitoringLocationIdentifier %in% sites) %>%
  select(AUID_ATTNS) %>%
  unique() %>%
  pull()


#Create reference site table for analysis
reference_sites <- tibble(AUID_ATTNS = au_sites,
                          ReferenceSites = sites)

#Pull only samples from these AUs
turbidity_samples_pull <- turbidity_samples %>%
  filter(AUID_ATTNS %in% reference_sites$AUID_ATTNS)


#Turbidity Function
MagDurFreq_turbidity <- function(wqs_crosswalk, input_samples_filtered, input_sufficiency, reference_sites) {
  
  ##Magnitude, Frequency, Duration - unique combinations
  #This is not used in the code, but instead used as reference for making the methods
  unique_methods <- wqs_crosswalk %>%
    dplyr::filter(Constituent == 'Turbidity') %>%

    dplyr::select(Directionality, Frequency, Duration, Details) %>%
    unique()
  
  #Calculate mean turbidity at reference sites
  pull_reference <- input_samples_filtered %>%
    dplyr::filter(MonitoringLocationIdentifier %in% reference_sites$ReferenceSites) %>%
    dplyr::group_by(MonitoringLocationIdentifier) %>%
    dplyr::mutate(mean_reference = mean(TADA.ResultMeasureValue)) %>%
    dplyr::select(MonitoringLocationIdentifier, mean_reference) %>%
    unique()
  
  reference_sites_mean <- reference_sites %>%
    dplyr::left_join(pull_reference, by = c('ReferenceSites' = 'MonitoringLocationIdentifier'))
  
  # use AU_Type to choose Waterbody Type in WQS table
  Unique_AUIDs <- unique(reference_sites_mean$AUID_ATTNS) %>% stats::na.omit()
  result_list <- list()
  counter <- 0
  
  #Cycle by AUs
  for(i in Unique_AUIDs){
    print(i) # print name of current AU
    
    # Filter data for just AU and make water year
    df_subset <- input_samples_filtered %>% 
      dplyr::filter(AUID_ATTNS == i) %>%
      dplyr::filter(TADA.CharacteristicName == 'TURBIDITY') %>%
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
    #remove information for instances found in the special case functions
    my_data_magfreqdur <- wqs_crosswalk %>% 
      dplyr::filter(TADA.Constituent %in% my_constituents) %>% 
      dplyr::filter(`Waterbody Type` %in% my_WtrBdy_Type) %>%
      dplyr::filter(Constituent == 'Turbidity')

    
    #If no relevant samples, skip AU
    if(nrow(my_data_magfreqdur)==0){
      next
    }
    
    #Cycle through each parameter to calculate the mag/freq/dur
    for(j in 1:nrow(my_data_magfreqdur)) {
      counter <- counter + 1
      #Pull relevant methods
      filter_by <- my_data_magfreqdur[j,]
      
      #Pull just that constituent data
      filt <- df_subset %>% dplyr::filter(TADA.CharacteristicName == filter_by$TADA.Constituent)
      
      if(){
        
      }else {
        
      }
    }
    
  }
  
}
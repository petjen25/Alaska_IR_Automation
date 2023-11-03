#Functions for data analysis package
#Messing around - not official

#Created by Hannah Ferriby

####Set up####
library(tidyverse)
library(sf)
library(RColorBrewer)
library(zoo)
library(psych)

####Load in data####
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20231026.csv')
input_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20231020.csv')
wqs_crosswalk <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20231026.csv')

####Function ideas####

#Simple pull 
simplePull <- function(data, AU_num, constituent){ #AU & constituent are optional inputs
  if(missing(AU_num)) {
    filt <- data %>% filter(TADA.CharacteristicName %in% constituent)
  } else if(missing(constituent)) {
    filt <- data %>% filter(AUID_ATTNS %in% AU_num)
  } else {
    filt <- data %>% filter(AUID_ATTNS %in% AU_num) %>%
      filter(TADA.CharacteristicName %in% constituent)
  }
  return(filt)
}

simplePull(data = input_sufficiency, AU_num = 'AK_B_1010203_001'
           , constituent = 'FECAL COLIFORM')
test_pull <- simplePull(data = input_samples, AU_num = 'AK_B_1010203_001'
                        , constituent = 'FECAL COLIFORM')


#Time series function
timeSeries <- function(data, AU_num, constituent) {
  
  if(missing(AU_num)) {
    filt <- data %>% filter(TADA.CharacteristicName %in% constituent)
  } else if(missing(constituent)) {
    filt <- data %>% filter(AUID_ATTNS %in% AU_num)
  } else {
    filt <- data %>% filter(AUID_ATTNS %in% AU_num) %>%
      filter(TADA.CharacteristicName %in% constituent)
  }
  
  val_name <- filt %>% select(TADA.CharacteristicName) %>% unique() %>% nrow()
  
  pal <- if(val_name < 3) {
    c('#810f7c', '#9ebcda')
  } else{
    RColorBrewer::brewer.pal(val_name, 'PuBuGn')
  }
  
  
  if(length(AU_num) == 1 & length(constituent) == 1) {
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                       y = TADA.ResultMeasureValue,
                                       shape = TADA.CharacteristicName,
                                       color = TADA.CharacteristicName),
                 size = 2, alpha = 0.8) +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_classic() +
      scale_color_manual(values = pal, name = 'Constituent') +
      scale_shape_discrete(name = 'Constituent')
    
  } else if (length(AU_num) > 1 & length(constituent) == 1){
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue,
                                  shape = TADA.CharacteristicName,
                                  color = TADA.CharacteristicName),
                 size = 2, alpha = 0.8) +
      facet_wrap(~AUID_ATTNS) +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95')) +
      scale_color_manual(values = pal, name = 'Constituent') +
      scale_shape_discrete(name = 'Constituent')
    
  } else if(length(AU_num) == 1 & length(constituent) > 1){
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue,
                                  shape = TADA.CharacteristicName,
                                  color = TADA.CharacteristicName),
                 size = 2, alpha = 0.8) +
      facet_wrap(~TADA.CharacteristicName) +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95')) +
      scale_color_manual(values = pal, name = 'Constituent') +
      scale_shape_discrete(name = 'Constituent')
    
  } else {
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue,
                                  shape = AUID_ATTNS,
                                  color = AUID_ATTNS),
                 size = 2, alpha = 0.8) +
      facet_wrap(~TADA.CharacteristicName) +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95')) +
      scale_color_manual(values = pal, name = 'AU ID') +
      scale_shape_discrete(name = 'AU ID') 
  }
  
  return(plt)
}


timeSeries(data = input_samples, AU_num = c('AK_R_1010504_005')
           , constituent = c('PH'))

timeSeries(data = input_samples, AU_num = c('AK_R_1010504_005', 'AK_B_1010203_001')
           , constituent = c('TEMPERATURE, WATER'))

timeSeries(data = input_samples, AU_num = c('AK_R_1010504_005')
           , constituent = c('PH', 'TEMPERATURE, WATER'))

timeSeries(data = input_samples, AU_num = c('AK_R_1010504_005', 'AK_B_1010203_001')
           , constituent = c('PH', 'TEMPERATURE, WATER'))


#Boxplot function
boxPlot <- function(data, AU_num, constituent) {
  
  if(missing(AU_num)) {
    filt <- data %>% filter(TADA.CharacteristicName %in% constituent)
  } else if(missing(constituent)) {
    filt <- data %>% filter(AUID_ATTNS %in% AU_num)
  } else {
    filt <- data %>% filter(AUID_ATTNS %in% AU_num) %>%
      filter(TADA.CharacteristicName %in% constituent)
  }
  
  val_name <- filt %>% select(TADA.CharacteristicName) %>% unique() %>% nrow()
  
  pal <- if(val_name < 3) {
    c('#810f7c', '#9ebcda')
  } else{
    RColorBrewer::brewer.pal(val_name, 'PuBuGn')
  }
  
  
  if(length(AU_num) == 1 & length(constituent) == 1) {
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue,
                                    color = TADA.CharacteristicName)) +
      xlab('AU ID') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_classic() +
      scale_color_manual(values = pal)+
      theme(legend.position = "none")
  } else if (length(AU_num) > 1 & length(constituent) == 1){
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = TADA.CharacteristicName,
                                    y = TADA.ResultMeasureValue,
                                    color = TADA.CharacteristicName)) +
      facet_wrap(~AUID_ATTNS) +
      xlab('Constituent') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      scale_color_manual(values = pal)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            legend.position = "none") 
  } else if(length(AU_num) == 1 & length(constituent) > 1){
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue,
                                    color = AUID_ATTNS)) +
      facet_wrap(~TADA.CharacteristicName, scales = 'free_y') +
      xlab('AU ID') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      scale_color_manual(values = pal)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            legend.position = "none") 
  } else {
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue,
                                    color = AUID_ATTNS)) +
      facet_wrap(~TADA.CharacteristicName, scales = 'free_y') +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      xlab('AU ID') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      scale_color_manual(values = pal)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            axis.text.x = element_text(angle = 45, hjust=1),
            legend.position = "none") 
  }
  
  return(plt)
}

boxPlot(data = input_samples, AU_num = c('AK_R_1010504_005')
        , constituent = c('PH'))
boxPlot(data = input_samples, AU_num = c('AK_R_1010504_005', 'AK_B_1010203_001')
        , constituent = c('TEMPERATURE, WATER'))
boxPlot(data = input_samples, AU_num = c('AK_R_1010504_005')
        , constituent = c('PH', 'TEMPERATURE, WATER'))
boxPlot(data = input_samples, AU_num = c('AK_R_1010504_005', 'AK_B_1010203_001')
        , constituent = c('PH', 'TEMPERATURE, WATER'))


#Remove insufficient data
removeCat3samples <- function(data_samples, data_sufficiency) {
  insuff_sites <- data_sufficiency %>% filter(Data_Sufficient == 'No') %>%
    select(AUID_ATTNS, TADA.CharacteristicName) %>% unique()
  
  remove_samples <- data_samples %>% anti_join(insuff_sites,
                                               by = join_by('AUID_ATTNS'
                                                            , 'TADA.CharacteristicName'))
    
  return(remove_samples)
}

removeCat3sites <- function(data_sufficiency) {
  insuff_sites <- data_sufficiency %>%
    filter(Data_Sufficient == 'Yes') %>%
    unique()
  
  return(insuff_sites)
}

removeCat3samples(data_samples = input_samples
                  , data_sufficiency = input_sufficiency)
removeCat3sites(input_sufficiency)





##Magnitude, Frequency, Duration
#Filter out Cat 3
input_samples_filtered <- removeCat3samples(data_samples = input_samples
                                            , data_sufficiency = input_sufficiency)



#Info for making methods:
# for loop to evaluate unique values per column
result_list <- list() # loop infrastructure
counter <- 0 # loop infrastructure

for(i in names(wqs_crosswalk)){
  counter <- counter + 1 # loop infrastructure
  ColumnName <- i # obtain column name
  data_loop <- wqs_crosswalk[,i] # filter data by column name
  Class <- paste(class(data_loop), collapse = "; ") # obtain class of column
  NumberUniqueValues <- n_distinct(data_loop) # obtain number of unique values
  
  # list unique values if <= 10
  if(NumberUniqueValues > 15){
    UniqueValues <- "Too Many to list!"
  } else {
    UniqueValues <- paste(unique(data_loop), collapse = "; ")
  }# end if/else statement
  
  # combine results
  results <- c(ColumnName, Class, NumberUniqueValues, UniqueValues)
  names(results) <- c("ColumnName", "Class", "NumberUniqueValues"
                      , "UniqueValues")
  result_list[[counter]] <- results
  
}# End of for loop
df_loop_results <- do.call("rbind", result_list) # combine results from for loop
data_summary <- as.data.frame(df_loop_results) # convert to data frame
data_summary$NumberUniqueValues <- as.numeric(data_summary$NumberUniqueValues) # change to numeric
data_summary$UniqueValues <-gsub(",", ";", data_summary$UniqueValues)  # get rid of commas
data_summary <- data_summary[order(data_summary$NumberUniqueValues),] # order

write_csv(data_summary, 'Output/data_analysis/wqs_lookup_table.csv')


unique_methods <- wqs_crosswalk %>%
  select(Directionality, Frequency, Duration, Details) %>%
  mutate(Directionality_Condensed = ifelse(str_detect(Directionality, '(?i)Not to exceed'), 'Maximum', Directionality),
         Stat_Method = case_when(str_detect(Details, '(?i)geometric mean') ~ 'geometric mean',
                                 str_detect(Duration, '(?i)daily average') ~ 'arithmetic mean',
                                 str_detect(Details, '(?i)harmonic mean') ~ 'harmonic mean',
                                 str_detect(Details, '(?i)Mean of most recent 3 years not to exceed') ~
                                   'arithmetic mean most recent 3 years not to exceed')) %>%
  unique() %>%
  mutate(Method_Num = row_number())

#Methods Start:
#Filter out Cat 3
input_samples_filtered <- removeCat3samples(data_samples = input_samples, data_sufficiency = input_sufficiency)

Unique_AUIDs <- unique(input_samples_filtered$AUID_ATTNS) %>% na.omit()
result_list <- list()
counter <- 0

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
  
  
  if(nrow(my_data_magfreqdur)==0){
    next
  }
  
  for(j in 1:nrow(my_data_magfreqdur)) {
    counter <- counter + 1
    filter_by <- my_data_magfreqdur[j,]
    
    filt <- df_subset %>% filter(TADA.CharacteristicName == filter_by$TADA.Constituent)
    
    if(filter_by$Directionality == 'Maximum' & filter_by$Frequency == 'Not to exceed' &
       filter_by$Duration == '30-day period' & str_detect(filter_by$Details, '(?i)Geometric mean') == T) {
      
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


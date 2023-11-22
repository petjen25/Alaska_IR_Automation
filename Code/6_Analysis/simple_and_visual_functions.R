#Functions for data analysis package
#Simple pull/filter
#Boxplots & Time series figures

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


#Time series function --- Make duplicate but log10
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

#Remove legends and if relevant move them to the top
timeSeries(data = input_samples, AU_num = c('AK_R_1010504_005')
           , constituent = c('PH'))

#Make into rows instead of columns
timeSeries(data = input_samples, AU_num = c('AK_R_1010504_005', 'AK_B_1010203_001')
           , constituent = c('TEMPERATURE, WATER'))

timeSeries(data = input_samples, AU_num = c('AK_R_1010504_005')
           , constituent = c('PH', 'TEMPERATURE, WATER'))

#Add free_y & facet_grid - AUs = rows and parameters = columns
timeSeries(data = input_samples, AU_num = c('AK_R_1010504_005', 'AK_B_1010203_001')
           , constituent = c('PH', 'TEMPERATURE, WATER'))


#Boxplot function --- Make duplicate but log10
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
    c('#810f7c', '#9ebcda') #Change to darker blue
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

#Add geom_jitter to see all samples
#Can get rid of color for some if not all
boxPlot(data = input_samples, AU_num = c('AK_R_1010504_005')
        , constituent = c('PH'))
boxPlot(data = input_samples, AU_num = c('AK_R_1010504_005', 'AK_B_1010203_001')
        , constituent = c('TEMPERATURE, WATER'))
boxPlot(data = input_samples, AU_num = c('AK_R_1010504_005')
        , constituent = c('PH', 'TEMPERATURE, WATER'))
boxPlot(data = input_samples, AU_num = c('AK_R_1010504_005', 'AK_B_1010203_001')
        , constituent = c('PH', 'TEMPERATURE, WATER'))



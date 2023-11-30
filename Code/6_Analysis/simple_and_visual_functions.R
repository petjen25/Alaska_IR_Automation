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
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20231113.csv')
input_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20231113.csv')
wqs_crosswalk <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20231122.csv')

####Function ideas####

#Simple pull 
simplePull <- function(data, AU_ID, constituent){ #AU & constituent are optional inputs
  if(missing(AU_ID)) {
    filt <- data %>% filter(TADA.CharacteristicName %in% constituent)
  } else if(missing(constituent)) {
    filt <- data %>% filter(AUID_ATTNS %in% AU_ID)
  } else {
    filt <- data %>% filter(AUID_ATTNS %in% AU_ID) %>%
      filter(TADA.CharacteristicName %in% constituent)
  }
  return(filt)
}

simplePull(data = input_sufficiency, AU_ID = 'AK_B_1010203_001'
           , constituent = 'FECAL COLIFORM')
test_pull <- simplePull(data = input_samples, AU_ID = 'AK_B_1010203_001'
                        , constituent = 'FECAL COLIFORM')


#Time series function 
timeSeries <- function(data, AU_ID, constituent) {
  
  if(missing(AU_ID)) {
    filt <- data %>% filter(TADA.CharacteristicName %in% constituent)
  } else if(missing(constituent)) {
    filt <- data %>% filter(AUID_ATTNS %in% AU_ID)
  } else {
    filt <- data %>% filter(AUID_ATTNS %in% AU_ID) %>%
      filter(TADA.CharacteristicName %in% constituent)
  }
  
  val_name <- filt %>% select(TADA.CharacteristicName) %>% unique() %>% nrow()
  
  
  if(length(AU_ID) == 1 & length(constituent) == 1) {
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                       y = TADA.ResultMeasureValue),
                 color = 'black',
                 size = 2, alpha = 0.8) +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggtitle(paste0(AU_ID)) +
      theme_classic() +
      theme(legend.position="none")
    
  } else if (length(AU_ID) > 1 & length(constituent) == 1){
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue),
                 color = 'black',
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
            strip.background = element_rect(fill = 'gray95'),
            legend.position="none")
    
  } else if(length(AU_ID) == 1 & length(constituent) > 1){
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue),
                 color = 'black',
                 size = 2, alpha = 0.8) +
      facet_wrap(~TADA.CharacteristicName, scale = "free_y") +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggtitle(paste0(AU_ID)) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            legend.position="none")
    
  } else {
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue),
                 color = 'black', 
                 size = 2, alpha = 0.8) +
      facet_grid(cols = vars(AUID_ATTNS),
                 rows = vars(TADA.CharacteristicName), 
                 scales = "free_y") +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'))
  }
  
  return(plt)
}

timeSeries(data = input_samples, AU_ID = c('AK_R_1010504_005')
           , constituent = c('PH'))

timeSeries(data = input_samples, AU_ID = c('AK_R_1010504_005', 'AK_B_1010203_001')
           , constituent = c('TEMPERATURE, WATER'))

timeSeries(data = input_samples, AU_ID = c('AK_R_1010504_005')
           , constituent = c('PH', 'TEMPERATURE, WATER'))

timeSeries(data = input_samples, AU_ID = c('AK_R_1010504_005', 'AK_B_1010203_001')
           , constituent = c('PH', 'TEMPERATURE, WATER'))


timeSerieslog10 <- function(data, AU_ID, constituent) {
  
  if(missing(AU_ID)) {
    filt <- data %>% filter(TADA.CharacteristicName %in% constituent)
  } else if(missing(constituent)) {
    filt <- data %>% filter(AUID_ATTNS %in% AU_ID)
  } else {
    filt <- data %>% filter(AUID_ATTNS %in% AU_ID) %>%
      filter(TADA.CharacteristicName %in% constituent)
  }
  
  val_name <- filt %>% select(TADA.CharacteristicName) %>% unique() %>% nrow()
  
  
  if(length(AU_ID) == 1 & length(constituent) == 1) {
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue),
                 color = 'gray20',
                 size = 2, alpha = 0.8) +
      scale_y_log10() +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggtitle(paste0(AU_ID)) +
      theme_classic() +
      theme(legend.position="none")
    
  } else if (length(AU_ID) > 1 & length(constituent) == 1){
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue),
                 color = 'gray20',
                 size = 2, alpha = 0.8) +
      facet_wrap(~AUID_ATTNS) +
      scale_y_log10() +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            legend.position="none")
    
  } else if(length(AU_ID) == 1 & length(constituent) > 1){
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue),
                 color = 'gray20',
                 size = 2, alpha = 0.8) +
      facet_wrap(~TADA.CharacteristicName, scale = "free_y") +
      scale_y_log10() +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      ggtitle(paste0(AU_ID)) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            legend.position="none")
    
  } else {
    plt<-ggplot() +
      geom_point(data = filt, aes(x = ActivityStartDate,
                                  y = TADA.ResultMeasureValue),
                 color = 'gray20', 
                 size = 2, alpha = 0.8) +
      facet_grid(cols = vars(AUID_ATTNS),
                 rows = vars(TADA.CharacteristicName), 
                 scales = "free_y") +
      scale_y_log10() +
      xlab('Time') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'))
  }
  
  return(plt)
}


timeSerieslog10(data = input_samples, AU_ID = c('AK_R_1010504_005')
           , constituent = c('PH'))

timeSerieslog10(data = input_samples, AU_ID = c('AK_R_1010504_005', 'AK_B_1010203_001')
           , constituent = c('TEMPERATURE, WATER'))

timeSerieslog10(data = input_samples, AU_ID = c('AK_R_1010504_005')
           , constituent = c('PH', 'TEMPERATURE, WATER'))

timeSerieslog10(data = input_samples, AU_ID = c('AK_R_1010504_005', 'AK_B_1010203_001')
           , constituent = c('PH', 'TEMPERATURE, WATER'))


#Boxplot function --- Make duplicate but log10
boxPlot <- function(data, AU_ID, constituent) {
  
  if(missing(AU_ID)) {
    filt <- data %>% filter(TADA.CharacteristicName %in% constituent)
  } else if(missing(constituent)) {
    filt <- data %>% filter(AUID_ATTNS %in% AU_ID)
  } else {
    filt <- data %>% filter(AUID_ATTNS %in% AU_ID) %>%
      filter(TADA.CharacteristicName %in% constituent)
  }
  
  val_name <- filt %>% select(TADA.CharacteristicName) %>% unique() %>% nrow()
  

  if(length(AU_ID) == 1 & length(constituent) == 1) {
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      geom_jitter(data = filt, aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) + 
      xlab('AU ID') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_classic() +
      theme(legend.position = "none")
  } else if (length(AU_ID) > 1 & length(constituent) == 1){
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      geom_jitter(data = filt, aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) +
      xlab('AU ID') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_classic() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            legend.position = "none") 
  } else if(length(AU_ID) == 1 & length(constituent) > 1){
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      geom_jitter(data = filt, aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) +
      facet_wrap(~TADA.CharacteristicName, scales = 'free_y') +
      xlab('AU ID') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            legend.position = "none") 
  } else {
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      geom_jitter(data = filt, aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) + 
      facet_wrap(~TADA.CharacteristicName, scales = 'free_y') +
      xlab('AU ID') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            axis.text.x = element_text(angle = 45, hjust=1),
            legend.position = "none") 
  }
  
  return(plt)
}

boxPlot(data = input_samples, AU_ID = c('AK_R_1010504_005')
        , constituent = c('PH'))
boxPlot(data = input_samples, AU_ID = c('AK_R_1010504_005', 'AK_B_1010203_001')
        , constituent = c('TEMPERATURE, WATER'))
boxPlot(data = input_samples, AU_ID = c('AK_R_1010504_005')
        , constituent = c('PH', 'TEMPERATURE, WATER'))
boxPlot(data = input_samples, AU_ID = c('AK_R_1010504_005', 'AK_B_1010203_001')
        , constituent = c('PH', 'TEMPERATURE, WATER'))


boxPlotlog10 <- function(data, AU_ID, constituent) {
  
  if(missing(AU_ID)) {
    filt <- data %>% filter(TADA.CharacteristicName %in% constituent)
  } else if(missing(constituent)) {
    filt <- data %>% filter(AUID_ATTNS %in% AU_ID)
  } else {
    filt <- data %>% filter(AUID_ATTNS %in% AU_ID) %>%
      filter(TADA.CharacteristicName %in% constituent)
  }
  
  val_name <- filt %>% select(TADA.CharacteristicName) %>% unique() %>% nrow()
  
  
  if(length(AU_ID) == 1 & length(constituent) == 1) {
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      geom_jitter(data = filt, aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) + 
      scale_y_log10() +
      xlab('AU ID') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_classic() +
      theme(legend.position = "none")
  } else if (length(AU_ID) > 1 & length(constituent) == 1){
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      geom_jitter(data = filt, aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) +
      scale_y_log10() +
      xlab('AU ID') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_classic() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            legend.position = "none") 
  } else if(length(AU_ID) == 1 & length(constituent) > 1){
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      geom_jitter(data = filt, aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) +
      facet_wrap(~TADA.CharacteristicName, scales = 'free_y') +
      scale_y_log10() +
      xlab('AU ID') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            legend.position = "none") 
  } else {
    plt<-ggplot() +
      geom_boxplot(data = filt, aes(x = AUID_ATTNS,
                                    y = TADA.ResultMeasureValue),
                   color = 'black') +
      geom_jitter(data = filt, aes(x = AUID_ATTNS,
                                   y = TADA.ResultMeasureValue)) + 
      facet_wrap(~TADA.CharacteristicName, scales = 'free_y') +
      scale_y_log10() +
      xlab('AU ID') +
      ylab(ifelse(val_name > 1, 'Result', paste0(filt$TADA.CharacteristicName
                                                 , ' ('
                                                 , filt$TADA.ResultMeasure.MeasureUnitCode
                                                 , ')'))) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = 'gray95'),
            axis.text.x = element_text(angle = 45, hjust=1),
            legend.position = "none") 
  }
  
  return(plt)
}

boxPlotlog10(data = input_samples, AU_ID = c('AK_R_1010504_005')
        , constituent = c('PH'))
boxPlotlog10(data = input_samples, AU_ID = c('AK_R_1010504_005', 'AK_B_1010203_001')
        , constituent = c('TEMPERATURE, WATER'))
boxPlotlog10(data = input_samples, AU_ID = c('AK_R_1010504_005')
        , constituent = c('PH', 'TEMPERATURE, WATER'))
boxPlotlog10(data = input_samples, AU_ID = c('AK_R_1010504_005', 'AK_B_1010203_001')
        , constituent = c('PH', 'TEMPERATURE, WATER'))


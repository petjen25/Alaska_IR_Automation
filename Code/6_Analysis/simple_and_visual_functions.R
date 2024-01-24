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
input_samples <- read_csv('Output/data_processing/WQ_data_trimmed_long_withAU20240117.csv')
input_sufficiency <- read_csv('Output/data_processing/WQ_metadata_trimmed_with_data_sufficiency_20240117.csv')
wqs_crosswalk <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20240117.csv')


#Determine max amount of constituents in an AU
input_samples %>%
  group_by(AUID_ATTNS) %>%
  select(AUID_ATTNS, TADA.CharacteristicName) %>%
  unique() %>%
  reframe(AUID_ATTNS = AUID_ATTNS,
          n_constituents = n()) %>%
  unique() %>%
  arrange(desc(n_constituents))

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
timeSeries <- function(data, WQS_table, AU_ID, y_axis_log) {
  
  relevant_constituents <- WQS_table %>%
    dplyr::select(TADA.Constituent) %>%
    unique() %>%
    dplyr::pull()
  
  relevant_data <- data %>%
    dplyr::filter(TADA.CharacteristicName %in% relevant_constituents) %>%
    dplyr::filter(AUID_ATTNS == AU_ID)
    
  constituents <- relevant_data %>%
    dplyr::select(TADA.CharacteristicName) %>%
    unique() %>%
    dplyr::pull()
  
  results <- list()
  counter <- 0
  #Loop through constituents
  for(j in constituents) {
    
    counter<- counter+1
    
    filt <- relevant_data %>%
      dplyr::filter(TADA.CharacteristicName == j)
    
    if(y_axis_log == F) {
    plt<-ggplot2::ggplot() +
      ggplot2::geom_point(data = filt,
                          aes(x = ActivityStartDate,
                              y = TADA.ResultMeasureValue,
                              fill = MonitoringLocationIdentifier),
                          color = 'black',
                          shape = 21,
                          size = 2,
                          alpha = 0.8) +
      ggplot2::xlab('Time') +
      ggplot2::ylab(paste0(j, ' (', filt$TADA.ResultMeasure.MeasureUnitCode, ')')) +
      ggplot2::theme_classic() +
      viridis::scale_fill_viridis(discrete = T,
                                   option = "mako") +
      ggplot2::labs(fill = 'Monitoring Location') +
      ggplot2::theme(legend.position="top")
    
    results[[counter]] <- plt
    } else {
      plt<-ggplot2::ggplot() +
        ggplot2::geom_point(data = filt,
                            aes(x = ActivityStartDate,
                                y = TADA.ResultMeasureValue,
                                fill = MonitoringLocationIdentifier),
                            color = 'black',
                            shape = 21,
                            size = 2,
                            alpha = 0.8) +
        ggplot2::xlab('Time') +
        ggplot2::scale_y_log10() +
        ggplot2::ylab(paste0(j, ' (', filt$TADA.ResultMeasure.MeasureUnitCode, ')')) +
        ggplot2::theme_classic() +
        viridis::scale_fill_viridis(discrete = T,
                                    option = "mako") +
        ggplot2::labs(fill = 'Monitoring Location') +
        ggplot2::theme(legend.position="top")
      
      results[[counter]] <- plt
    }
  }
  return(results)
}

timeseries_example <- timeSeries(data = input_samples, WQS_table = wqs_crosswalk,
                                 AU_ID = c('AK_R_1010504_005'), y_axis_log = F)


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


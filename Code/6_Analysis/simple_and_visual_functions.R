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
wqs_crosswalk <- read_csv('Data/data_analysis/AK_WQS_Crosswalk_20240131.csv')


#Determine max amount of constituents in an AU
##JUST FOR TESTING - NOT FOR PACKAGE
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
  sysfonts::font_add_google("Open Sans", family = "Open_Sans") # for fonts
  showtext::showtext_auto() # for fonts
  
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
                            ggplot2::aes(x = ActivityStartDate,
                                         y = TADA.ResultMeasureValue,
                                         fill = MonitoringLocationIdentifier),
                            color = 'black',
                            shape = 21,
                            size = 2,
                            alpha = 0.8) +
        ggplot2::xlab('Time') +
        ggplot2::ylab(paste0(j, ' (', filt$TADA.ResultMeasure.MeasureUnitCode, ')')) +
        ggplot2::theme_bw() +
        viridis::scale_fill_viridis(discrete = T,
                                    option = "mako") +
        ggplot2::labs(fill = 'Monitoring Location') +
        ggplot2::theme(legend.position="top"
                       , text = ggplot2::element_text(family = "Open_Sans", size = 22)
                       , axis.text = ggplot2::element_text(family = "Open_Sans", size = 20)) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(length(unique(filt$MonitoringLocationIdentifier))/3)))
      
      results[[counter]] <- plt
    } else {
      plt<-ggplot2::ggplot() +
        ggplot2::geom_point(data = filt,
                            ggplot2::aes(x = ActivityStartDate,
                                         y = TADA.ResultMeasureValue,
                                         fill = MonitoringLocationIdentifier),
                            color = 'black',
                            shape = 21,
                            size = 2,
                            alpha = 0.8) +
        ggplot2::xlab('Time') +
        ggplot2::scale_y_log10() +
        ggplot2::ylab(paste0(j, ' (', filt$TADA.ResultMeasure.MeasureUnitCode, ')')) +
        ggplot2::theme_bw() +
        viridis::scale_fill_viridis(discrete = T,
                                    option = "mako") +
        ggplot2::labs(fill = 'Monitoring Location') +
        ggplot2::theme(legend.position="top"
                       , text = ggplot2::element_text(family = "Open_Sans", size = 22)
                       , axis.text = ggplot2::element_text(family = "Open_Sans", size = 20)) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(length(unique(filt$MonitoringLocationIdentifier))/3)))
      
      results[[counter]] <- plt
    }
  }
  return(results)
}

timeseries_example <- timeSeries(data = input_samples, WQS_table = wqs_crosswalk,
                                 AU_ID = c('AK_M_1030305_003'), y_axis_log = F)

ggsave('Output/results/test/testtime.jpg', timeseries_example[[1]], units = 'in',
       height = 5, width = 6.5)

#Boxplot function 
boxPlot <- function(data, WQS_table, AU_ID, y_axis_log) {
  
  sysfonts::font_add_google("Open Sans", family = "Open_Sans") # for fonts
  showtext::showtext_auto() # for fonts
  
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
        ggplot2::geom_boxplot(data = filt,
                              ggplot2::aes(x = AUID_ATTNS,
                                           y = TADA.ResultMeasureValue),
                              color = 'gray30') +
        ggplot2::geom_jitter(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                                       y = TADA.ResultMeasureValue,
                                                       fill = MonitoringLocationIdentifier),
                             color = 'black',
                             shape = 21,
                             size = 2,
                             width = 0.2,
                             alpha = 0.8) +
        ggplot2::xlab('AU ID') +
        ggplot2::ylab(paste0(j, ' (', filt$TADA.ResultMeasure.MeasureUnitCode, ')')) +
        ggplot2::theme_bw() +
        viridis::scale_fill_viridis(discrete = T,
                                    option = "mako") +
        ggplot2::labs(fill = 'Monitoring Location') +
        ggplot2::theme(legend.position="top"
                       , text = ggplot2::element_text(family = "Open_Sans", size = 22)
                       , axis.text = ggplot2::element_text(family = "Open_Sans", size = 20)) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(length(unique(filt$MonitoringLocationIdentifier))/3)))
      
      results[[counter]] <- plt
    } else {
      plt<-ggplot2::ggplot() +
        ggplot2::geom_boxplot(data = filt,
                              ggplot2::aes(x = AUID_ATTNS,
                                           y = TADA.ResultMeasureValue),
                              color = 'gray30') +
        ggplot2::geom_jitter(data = filt, ggplot2::aes(x = AUID_ATTNS,
                                                       y = TADA.ResultMeasureValue,
                                                       fill = MonitoringLocationIdentifier),
                             color = 'black',
                             shape = 21,
                             size = 2,
                             width = 0.2,
                             alpha = 0.8) +
        ggplot2::xlab('AU ID') +
        ggplot2::ylab(paste0(j, ' (', filt$TADA.ResultMeasure.MeasureUnitCode, ')')) +
        ggplot2::scale_y_log10() +
        ggplot2::theme_bw() +
        viridis::scale_fill_viridis(discrete = T,
                                    option = "mako") +
        ggplot2::labs(fill = 'Monitoring Location') +
        ggplot2::theme(legend.position="top"
                       , text = ggplot2::element_text(family = "Open_Sans", size = 22)
                       , axis.text = ggplot2::element_text(family = "Open_Sans", size = 20)) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = ceiling(length(unique(filt$MonitoringLocationIdentifier))/3)))
      
      results[[counter]] <- plt
    }
  }
  return(results)
}

boxplot_example <- boxPlot(data = input_samples, WQS_table = wqs_crosswalk,
                                 AU_ID = c('AK_M_1030305_003'), y_axis_log = F)

ggsave('Output/results/test/testboxplot.jpg', boxplot_example[[1]], units = 'in',
       height = 5, width = 6.5)

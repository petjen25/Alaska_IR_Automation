#Data cleaning and processing of Alaska WQP pull 

#Written by: Hannah Ferriby and Ben Block
#Date Created: 9-29-2023
#Date of Last Updated: 10-10-2023

##Required Inputs:
#1. csv outputs from data_pull.R broken up by site type


####Set Up####
library(TADA)
library(tidyverse)
library(leaflet)
myDate <- format(Sys.Date(), "%Y%m%d")


####Load Data####
#Find all file names that end in .csv from the data_pull output folder
csv_names1 <- list.files('Data/data_pull', pattern = '.csv', full.names = T)
csv_names <- csv_names1[!str_detect(csv_names1, pattern = 'all')]
  
#Read in csvs and combine into one table
all_input_data <- tibble()
for(i in 1:length(csv_names)) {
  csv <- read_csv(csv_names[i])
  all_input_data <- all_input_data %>%
    rbind(csv)
  remove(csv)
}

#clean up environment
remove(i, csv_names, csv_names1)

## TADA flags from TADA_Autoclean (data pull)
names(all_input_data %>% 
        select(starts_with("TADA")))

####Identify Flags####

#####1. Check Result Unit Validity#####
# This function adds the TADA.ResultUnit.Flag to the dataframe.
data_1 <- TADA_FlagResultUnit(all_input_data, clean = 'none') #required

#####2. Check Sample Fraction Validity#####
# This function adds the TADA.SampleFraction.Flag to the dataframe.
data_2 <- TADA_FlagFraction(data_1, clean = F) #required

#####3. Check Method Speciation Validity#####
# This function adds the TADA.MethodSpeciation.Flag to the dataframe.
data_3 <- TADA_FlagSpeciation(data_2, clean = 'none') #required

#####4. Harmonize Characteristic Names#####
# This function adds the following columns to the dataframe:
# TADA.CharacteristicNameAssumptions
# TADA.SpeciationAssumptions     
# TADA.FractionAssumptions       
# TADA.Harmonized.Flag
data_4 <- TADA_HarmonizeSynonyms(data_3)

#####5. Flag unrealistic values#####
# This function adds the TADA.ResultValueAboveUpperThreshold.Flag to the dataframe.
data_5a <- TADA_FlagAboveThreshold(data_4, clean = F)

# This function adds the TADA.ResultValueBelowLowerThreshold.Flag to the dataframe.
data_5b <- TADA_FlagBelowThreshold(data_5a, clean = F)

#####6. Find continuous data#####
# This function adds the TADA.AggregatedContinuousData.Flag to the dataframe.
data_6 <- TADA_FindContinuousData(data_5b, clean = F)

#####7. Check method flags#####
# This function adds the TADA.AnalyticalMethod.Flag to the dataframe.
data_7 <- TADA_FlagMethod(data_6, clean = F)

#####8. Find potential duplictes#####
#Buffer distance set to 50 m, can change
# This function adds the following columns to the dataframe:
# TADA.NearbySiteGroups
# TADA.MultipleOrgDuplicate
# TADA.MultipleOrgDupGroupID
# TADA.ResultSelectedMultipleOrgs
data_8a <- TADA_FindPotentialDuplicatesMultipleOrgs(data_7, dist_buffer = 50)

# This function adds the following columns to the dataframe:
# TADA.SingleOrgDupGroupID
# TADA.ResultSelectedSingleOrg
data_8b <- TADA_FindPotentialDuplicatesSingleOrg(data_8a, handling_method = 'none')

#####9. Find QC samples#####
# This function adds the TADA.ActivityType.Flag to the dataframe.
data_9 <- TADA_FindQCActivities(data_8b, clean = F)

#####10. Flag invalid coordinates#####
# This function adds the TADA.InvalidCoordinates.Flag to the dataframe.
data_10 <- TADA_FlagCoordinates(data_9, clean_outsideUSA = 'no')

#####11. Find any 'SUSPECT' samples#####
# This function adds the TADA.MeasureQualifierCode.Flag to the dataframe.
data_11a <- TADA_FlagMeasureQualifierCode(data_10, clean = F)
uncategorized_qualifiers <- data_11a %>% 
  select(MeasureQualifierCode, TADA.MeasureQualifierCode.Flag) %>% 
  filter(TADA.MeasureQualifierCode.Flag == "uncategorized") %>% 
  distinct()

data_11b <- data_11a %>% 
  mutate(TADA.MeasureQualifierCode.Flag = case_when((MeasureQualifierCode == "H;U")
                                                    ~ "Filler_BenB"
                   , (MeasureQualifierCode == "RC;U") ~ "Filler_BenB"
                   , (MeasureQualifierCode == "H;RC;U") ~ "Filler_BenB"
                   , (MeasureQualifierCode == "J-R;TOC") ~ "Filler_BenB"
                   , (MeasureQualifierCode == "TOC;U") ~ "Filler_BenB"
                   , TRUE ~ TADA.MeasureQualifierCode.Flag))

#####12. Replace non-detects#####
# This function adds the following columns to the dataframe:
# TADA.CensoredMethod
# TADA.CensoredData.Flag

data_12 <- TADA_SimpleCensoredMethods(data_11b, 
                                      nd_method = 'multiplier',
                                      nd_multiplier = 0.5)

#####13. Identify columns with all NA values#####
# Check whether you expect data in any of the columns listed below.
(cols_NA <- data_12 %>% 
   keep(~all(is.na(.x))) %>% 
   names)

# Eliminate any columns with all NA values
data_13 <- data_12 %>% 
  select(where(~sum(!is.na(.x)) > 0))

#Export data with flags
write_csv(data_13, file = file.path('Output/data_processing'
                                    , paste0("Original_data_with_flags_"
                                             ,myDate, ".csv"))
          , na = "")

#Clean up environment
rm(data_1, data_2, data_3, data_4, data_5a, data_5b, data_6, data_7,data_8a
   , data_8b, data_9, data_10, data_11a, data_11b, data_12, cols_NA
   , all_input_data, uncategorized_qualifiers)

####Evaluate and trim data ####
#####14. Data summary ######
# for loop to evaluate unique values per column
result_list <- list() # loop infrastructure
counter <- 0 # loop infrastructure

for(i in names(data_13)){
  counter <- counter + 1 # loop infrastructure
  print(i) # loop infrastructure
  ColumnName <- i # obtain column name
  data_loop <- data_13[,i] # filter data by column name
  Class <- paste(class(data_loop), collapse = "; ") # obtain class of column
  NumberUniqueValues <- n_distinct(data_loop) # obtain number of unique values
  
  # list unique values if <= 10
  if(NumberUniqueValues > 10){
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

#Export data summary
write_csv(data_summary, file = file.path('Output/data_processing'
                                    , paste0("WQ_data_summary_",myDate, ".csv")))

#Clean up environment
rm(data_summary, df_loop_results, result_list, Class, ColumnName, counter
   , data_loop, i, NumberUniqueValues, results, UniqueValues)

#####15. Trim columns ######
# Columns trimmed using Data/data_processing/WQ_Column_Manager.csv
# Update 'Keep_YN' field for a given 'Col_Name' to retain.
# Note: TADA versions of columns supersede original columns
## (e.g.,'TADA.ActivityMediaName' supersedes 'ActivityMediaName')

df_ColManager <- read_csv("Data/data_processing/WQ_Column_Manager.csv")
Keep_cols <- df_ColManager %>% 
  filter(Keep_YN == "Yes") %>% 
  pull(Col_Name)

# QC check for updated df_ColManager
Cols_data_13 <- names(data_13)
QC_Check <- df_ColManager %>% 
  filter(!Col_Name %in% Cols_data_13) %>% 
  pull(Col_Name)

if(length(QC_Check) > 0){
  print(paste("df_ColManager is out of date and needs updating."
               ,"The following fields are missing:"))
  print(QC_Check)
} else {
  print("df_ColManager is up to date.")
}# end if/else statement

# filter data by Keep_cols
data_15 <- data_13 %>% 
  select(one_of(Keep_cols))

#Clean up environment
rm(data_13, df_ColManager, Cols_data_13, QC_Check, Keep_cols)

#####16. Remove Flags#####
# Flags not well explained. Vignettes don't match unique values.
# https://github.com/USEPA/TADA/blob/0eb2cb8e6abd29f214bc130382875e164e40310f/R/GenerateRefTables.R#L58C4-L58C4
# Assume the following:
# Not Reviewed <- "Not Reviewed" 
# Valid <- c("Accepted", "Y")
# Invalid <- c("Rejected", "Rejected ", "N")
# NonStandardized <- c("NonStandardized",
#                  "InvalidMediaUnit",
#                  "InvalidChar",
#                  "MethodNeeded")

data_16 <- data_15 %>% 
  filter(TADA.ResultUnit.Flag != "Rejected") %>% # Step 1
  filter(TADA.SampleFraction.Flag != "Rejected") %>% # Step 2
  filter(TADA.MethodSpeciation.Flag != "Rejected") %>% # Step 3
  filter(TADA.AnalyticalMethod.Flag != "Rejected") %>% # Step 7
  filter(TADA.ActivityType.Flag == 'Non_QC') %>% # Step 9
  filter(TADA.ActivityMediaName == 'WATER') # Remove non-water samples
  # filter(TADA.CensoredData.Flag == 'Uncensored') #Remove censored data

#Export data summary
write_csv(data_16, file = file.path('Output/data_processing'
                                         , paste0("WQ_data_trimmed_"
                                                  ,myDate, ".csv"))
          , na = "")

#Clean up environment
rm(data_15)

#####17. Visualize data distributions#####

# For loop to plot distribution of TADA.CharacteristicName
Unique_CharName <- unique(data_16$TADA.CharacteristicName)
plot_list <- list()
counter <- 0
myPal <- c("Lake, Reservoir, Impoundment" = "#d7191c"
           , "Lake" = "#4575b4"
           , "BEACH Program Site-Ocean" = "#fc8d59"
           , "Estuary" = "#e0f3f8"
           , "Ocean" = "#fee090")

data_4loop <- data_16 %>% 
  filter(!is.na(TADA.ResultMeasureValue))%>% # remove NA values
  select(MonitoringLocationTypeName, TADA.CharacteristicName
         , TADA.ResultMeasureValue, TADA.ResultMeasure.MeasureUnitCode)

for(i in Unique_CharName){
  i # print name of current characteristic
  counter <- counter + 1
  
  #filter data
  df_subset <- data_4loop %>% 
    filter(TADA.CharacteristicName == i)
  
  #boxplot
  plot <- ggplot(data = df_subset, aes(x = MonitoringLocationTypeName
                                       , y = TADA.ResultMeasureValue
                                       , fill = MonitoringLocationTypeName))+
    geom_boxplot()+
    labs(y = df_subset$TADA.ResultMeasure.MeasureUnitCode
         , title = df_subset$TADA.CharacteristicName)+
    scale_fill_manual(values = myPal)+
    theme_classic()+
    theme(legend.position = "none")
  
  plot_list[[counter]] <- plot
  counter <- counter + 1
  
  #log boxplot
  logplot <- ggplot(data = df_subset, aes(x = MonitoringLocationTypeName
                                       , y = TADA.ResultMeasureValue
                                       , fill = MonitoringLocationTypeName))+
    geom_boxplot()+
    scale_y_log10()+
    labs(y = paste0(df_subset$TADA.ResultMeasure.MeasureUnitCode, " (Log10 Y-Axis)")
         , title = df_subset$TADA.CharacteristicName)+
    scale_fill_manual(values = myPal)+
    theme_classic()+
    theme(legend.position = "none")
  
  plot_list[[counter]] <- logplot
} # end of for loop

# Export plots
pdf(file = file.path("Output/data_processing"
                     , paste0("WQ_Boxplots_"
                              , myDate, ".pdf")))
for (i in 1:length(plot_list)) {
  print(plot_list[[i]])
}
dev.off()

#Clean up environment
rm(data_4loop, df_subset, logplot, plot, plot_list, counter, i
   , myPal, Unique_CharName)

#####18. Ultra trim data#####
data_18 <- data_16 %>% 
  select(OrganizationIdentifier
         ,ActivityStartDate
         ,MonitoringLocationIdentifier
         ,MonitoringLocationTypeName
         ,TADA.CharacteristicName
         ,TADA.ResultMeasureValue
         ,TADA.ResultMeasure.MeasureUnitCode
         ,TADA.LatitudeMeasure
         ,TADA.LongitudeMeasure)

#####19. Match data to AUs #####
# Match using Data/data_processing/ML_AU_Crosswalk.CSV
df_ML_AU_Crosswalk <- read_csv("Data/data_processing/ML_AU_Crosswalk.CSV")
df_ML_AU_Crosswalk <- df_ML_AU_Crosswalk %>% 
  select(-c(OrganizationIdentifier))

# join data
data_19 <- left_join(data_18, df_ML_AU_Crosswalk
                     , by = "MonitoringLocationIdentifier")


# interactive map for all monitoring locations
## subset data to unique ML info
df_ML <- data_19 %>% 
  select(MonitoringLocationIdentifier, MonitoringLocationTypeName
         , TADA.LatitudeMeasure, TADA.LongitudeMeasure) %>% 
  distinct()

## create palette
ML_Type <- factor(c("Lake, Reservoir, Impoundment"
                    , "Lake"
                    , "BEACH Program Site-Ocean"
                    , "Estuary"
                    , "Ocean"))

pal <- colorFactor(
  palette = c("#d7191c", "#4575b4", "#fc8d59", "#e0f3f8", "#fee090"),
  domain = ML_Type,
  ordered = TRUE)

map <- leaflet() %>% 
  addTiles() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(data = df_ML, lat = ~TADA.LatitudeMeasure
                   , lng = ~TADA.LongitudeMeasure
                   , popup = paste("MonitoringLocationIdentifier:", df_ML$MonitoringLocationIdentifier, "<br>"
                                   ,"MonitoringLocationTypeName:", df_ML$MonitoringLocationTypeName, "<br>"
                                   , "TADA.LatitudeMeasure:", df_ML$TADA.LatitudeMeasure, "<br>"
                                   , "TADA.LongitudeMeasure:", df_ML$TADA.LongitudeMeasure)
                   , color = "black", fillColor = ~pal(MonitoringLocationTypeName), fillOpacity = 1, stroke = TRUE
                   )%>%
  addLegend("bottomright", pal = pal, values = df_ML$MonitoringLocationTypeName,
            title = "ML Type", opacity = 1)

map # view map

# check for missing ML in crosswalk table (i.e., new ones)
ML_in_crosswalk <- unique(df_ML_AU_Crosswalk$MonitoringLocationIdentifier)

(missing_ML <- df_ML %>% 
  filter(!MonitoringLocationIdentifier %in% ML_in_crosswalk))

# interactive map for missing monitoring locations
missing_ML_map <- leaflet() %>% 
  addTiles() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(data = missing_ML, lat = ~TADA.LatitudeMeasure
                   , lng = ~TADA.LongitudeMeasure
                   , popup = paste("MonitoringLocationIdentifier:", missing_ML$MonitoringLocationIdentifier, "<br>"
                                   ,"MonitoringLocationTypeName:", missing_ML$MonitoringLocationTypeName, "<br>"
                                   , "TADA.LatitudeMeasure:", missing_ML$TADA.LatitudeMeasure, "<br>"
                                   , "TADA.LongitudeMeasure:", missing_ML$TADA.LongitudeMeasure)
                   , color = "black", fillColor = ~pal(MonitoringLocationTypeName), fillOpacity = 1, stroke = TRUE
  )%>%
  addLegend("bottomright", pal = pal, values = missing_ML$MonitoringLocationTypeName,
            title = "ML Type", opacity = 1)
missing_ML_map # view map




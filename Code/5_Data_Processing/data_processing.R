#Data cleaning and processing of Alaska WQP pull 

#Written by: Hannah Ferriby and Ben Block
#Date Created: 9-29-2023
#Date of Last Updated: 10-19-2023

##Required Inputs:
#1. csv outputs from data_pull.R broken up by site type
#2. 'WQ_Column_Manager.csv' to quickly subset WQ dataset fields
#3. 'ML_AU_Crosswalk.csv' to crosswalk Monitoring Locations with AUs
#4. 'AK_DataSufficiency_Crosswalk_20231012.csv' to crosswalk WQ dataset with 
     # data sufficiency table
#5. 'Beaches.shp' Beaches AU shapefile
#6. 'Lakes.shp' Lakes AU shapefile
#7. 'MAUs_FINAL_2023.shp' Marine AU shapefile
#8. 'Rivers.shp' Rivers AU shapefile
#9. 'cb_2018_us_state_500k.shp' US States shapefile

####Set Up####
library(TADA)
library(tidyverse)
library(leaflet)
library(scales)
library(sf)
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
# Complete the following code steps in order. 
## Steps 1-3 are special in that they are required by the TADA package
### before subsequent functions can be run.

#####1. Check Result Unit Validity#####
# This function adds the TADA.ResultUnit.Flag to the dataframe.
data_1 <- TADA_FlagResultUnit(all_input_data, clean = 'none')

#####2. Check Sample Fraction Validity#####
# This function adds the TADA.SampleFraction.Flag to the dataframe.
data_2 <- TADA_FlagFraction(data_1, clean = F)

#####3. Check Method Speciation Validity#####
# This function adds the TADA.MethodSpeciation.Flag to the dataframe.
data_3 <- TADA_FlagSpeciation(data_2, clean = 'none')

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

#####8. Find potential duplicates#####
#Buffer distance set to 50 m, can change
# This function adds the following columns to the dataframe:
# TADA.NearbySiteGroups
# TADA.MultipleOrgDuplicate
# TADA.MultipleOrgDupGroupID
# TADA.ResultSelectedMultipleOrgs
data_8a <- TADA_FindPotentialDuplicatesMultipleOrgs(data_7, dist_buffer = 50) # Buffer distance can be changed.

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
  
# list uncategorized qualifiers in data
(uncategorized_qualifiers <- data_11a %>% 
  select(MeasureQualifierCode, TADA.MeasureQualifierCode.Flag) %>% 
  filter(TADA.MeasureQualifierCode.Flag == "uncategorized") %>% 
  distinct())

# update qualifiers using 'IR DATA QA .xlsx'
# add any changes by making a new row below
data_11b <- data_11a %>% 
  mutate(TADA.MeasureQualifierCode.Flag = 
           case_when((MeasureQualifierCode == "H;U") ~ "Suspect"
                   , (MeasureQualifierCode == "RC;U") ~ "Non-Detect"
                   , (MeasureQualifierCode == "H;RC;U") ~ "Suspect"
                   , (MeasureQualifierCode == "J-R;TOC") ~ "Pass"
                   , (MeasureQualifierCode == "TOC;U") ~ "Non-Detect"
                   , (MeasureQualifierCode == "RC;SUS") ~ "Suspect"
                   , (MeasureQualifierCode == "O;RC") ~ "Pass"
                   , (MeasureQualifierCode == "H;RC") ~ "Suspect"
                   , (MeasureQualifierCode == "B;J-R") ~ "Pass"
                   , (MeasureQualifierCode == "H;J-R") ~ "Suspect"
                   , (MeasureQualifierCode == "IQCOL;U") ~ "Pass"
                   , (MeasureQualifierCode == "IQCOL;J-R") ~ "Pass"
                   , (MeasureQualifierCode == "LL;RC") ~ "Pass"
                   , (MeasureQualifierCode == "BQL;RC") ~ "Pass"
                   , (MeasureQualifierCode == "B;D") ~ "Pass"
                   , (MeasureQualifierCode == "SDROL;U") ~ "Suspect"
                   , TRUE ~ TADA.MeasureQualifierCode.Flag))

# re-check for uncategorized qualifiers
(uncategorized_qualifiers <- data_11b %>% 
  select(MeasureQualifierCode, TADA.MeasureQualifierCode.Flag) %>% 
  filter(TADA.MeasureQualifierCode.Flag == "uncategorized") %>% 
  distinct())

#####12. Replace non-detects #####
# This function adds the following columns to the dataframe:
# TADA.CensoredMethod
# TADA.CensoredData.Flag
# NOTE: This function uses the method detection limit

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
                                             ,myDate, ".csv")), na = "")

#Clean up environment
rm(data_1, data_2, data_3, data_4, data_5a, data_5b, data_6, data_7,data_8a
   , data_8b, data_9, data_10, data_11a, data_11b, data_12, cols_NA
   , all_input_data, uncategorized_qualifiers)

####Evaluate and trim data ####
#####14. Data summary ######
# NOTE: This step creates its own unique output but does not produce data_14 object.

# for loop to evaluate unique values per column
result_list <- list() # loop infrastructure
counter <- 0 # loop infrastructure

for(i in names(data_13)){
  counter <- counter + 1 # loop infrastructure
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
                                    , paste0("WQ_column_summary_",myDate, ".csv")))

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
  filter(TADA.MeasureQualifierCode.Flag != 'Suspect') %>% # Step 11
  filter(TADA.ActivityMediaName == 'WATER') # Remove non-water samples
# censored data are retained in this dataset.

#Export data summary
write_csv(data_16, file = file.path('Output/data_processing'
                                         , paste0("WQ_data_trimmed_"
                                                  ,myDate, ".csv"))
          , na = "")

#Clean up environment
rm(data_15)

#####17. Visualize data distributions#####
# NOTE: This step creates its own unique output but does not produce data_17 object.

# For loop to plot distribution of TADA.CharacteristicName
# CAUTION: This loop takes about a minute to run.
Unique_CharName <- unique(data_16$TADA.CharacteristicName)
plot_list <- list()
counter <- 0
myPal <- c("Lake, Reservoir, Impoundment" = "#7fc97f"
           , "Lake" = "#beaed4"
           , "BEACH Program Site-Ocean" = "#fdc086"
           , "Estuary" = "#ffff99"
           , "Ocean" = "#386cb0"
           , "Stream" = "#f0027f"
           , "River/Stream" = "#bf5b17")

data_4loop <- data_16 %>% 
  filter(!is.na(TADA.ResultMeasureValue))%>% # remove NA values
  select(MonitoringLocationTypeName, TADA.CharacteristicName
         , TADA.ResultMeasureValue, TADA.ResultMeasure.MeasureUnitCode) %>% 
  mutate(TADA.ResultMeasureValue_Log10 = case_when((TADA.ResultMeasureValue == 0)~ log10(0.01)
                                                   , TRUE ~ log10(TADA.ResultMeasureValue)))

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
    theme(legend.position = "none"
          , axis.text.x = element_text(angle = 10, vjust = 0.9, hjust = 0.5))
  
  plot_list[[counter]] <- plot
  counter <- counter + 1
  
  #log boxplot
  logplot <- ggplot(data = df_subset, aes(x = MonitoringLocationTypeName
                                       , y = TADA.ResultMeasureValue_Log10
                                       , fill = MonitoringLocationTypeName))+
    geom_boxplot()+
    labs(y = paste0(df_subset$TADA.ResultMeasure.MeasureUnitCode, " (Log10 Y-Axis)")
         , title = df_subset$TADA.CharacteristicName)+
    scale_fill_manual(values = myPal)+
    theme_classic()+
    theme(legend.position = "none"
          , axis.text.x = element_text(angle = 10, vjust = 0.9, hjust = 0.5))
  
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
         ,MonitoringLocationName
         ,MonitoringLocationTypeName
         ,TADA.CharacteristicName
         ,TADA.ResultMeasureValue
         ,TADA.ResultMeasure.MeasureUnitCode
         ,TADA.LatitudeMeasure
         ,TADA.LongitudeMeasure)

#Clean up environment
rm(data_16)

#### Match data to AUs ####
#####19. ML to AUs #####
# Match using Data/data_processing/ML_AU_Crosswalk.CSV
df_ML_AU_Crosswalk <- read_csv("Data/data_processing/ML_AU_Crosswalk.CSV")
df_ML_AU_Crosswalk <- df_ML_AU_Crosswalk %>% 
  select(-c(OrganizationIdentifier)) %>% # removed to avoid duplication in join
  rename(AU_Type = Type)

# join data
data_19 <- left_join(data_18, df_ML_AU_Crosswalk
                     , by = "MonitoringLocationIdentifier")

#Create object for export, remove columns of mostly NAs to make file small enough
#for github
data_19_long <- left_join(data_16, df_ML_AU_Crosswalk
                          , by = "MonitoringLocationIdentifier") %>%
  select(!c(HydrologicEvent, HydrologicCondition, StatisticalBaseCode, ResultTimeBasisText, 
            ActivityEndDateTime, MonitoringLocationDescriptionText,
            SamplingDesignTypeCode, QAPPApprovedIndicator, QAPPApprovalAgencyName,
            TADA.CharacteristicNameAssumptions))

#Export data summary
write_csv(data_19_long, file = file.path('Output/data_processing'
                                         , paste0("WQ_data_trimmed_long_withAU"
                                                  ,myDate, ".csv"))
          , na = "")

# interactive map for all monitoring locations
## subset data to unique ML info
df_ML <- data_19 %>% 
  select(MonitoringLocationIdentifier, MonitoringLocationName
         , MonitoringLocationTypeName, TADA.LatitudeMeasure
         , TADA.LongitudeMeasure) %>% 
  distinct()

## create palette
ML_Type <- factor(c("Lake, Reservoir, Impoundment"
                    , "Lake"
                    , "BEACH Program Site-Ocean"
                    , "Estuary"
                    , "Ocean"
                    , "Stream"
                    , "River/Stream"))

pal <- colorFactor(
  palette = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0"
              , "#f0027f", "#bf5b17"),
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

#####20. Assign missing MLs to AUs #####
# check for missing ML in crosswalk table (i.e., new ones)
# Use spatial joins to assign monitoring locations to AUs
# Note: use the interactive map below to QC results.
# Finally, update 'ML_AU_Crosswalk.CSV' with new matches.

ML_in_crosswalk <- unique(df_ML_AU_Crosswalk$MonitoringLocationIdentifier)

(missing_ML <- df_ML %>%  # these are MLs not in the crosswalk table
  filter(!MonitoringLocationIdentifier %in% ML_in_crosswalk))

######20a. Setup #####
fn_shp <- file.path(getwd(), "Data", "data_GIS")
beach_shp <- sf::st_read(dsn = paste0(fn_shp,"/Beaches"), layer = "Beaches")%>% 
  sf::st_transform(3338)
lake_shp <- sf::st_read(dsn = paste0(fn_shp,"/Lakes"), layer = "Lakes")%>% 
  sf::st_transform(3338) %>% 
  sf::st_zm()
marine_shp <- sf::st_read(dsn = paste0(fn_shp,"/Marine"), layer = "MAUs_FINAL_2023")%>% 
  sf::st_transform(3338)
river_shp <- sf::st_read(dsn = paste0(fn_shp,"/Rivers"), layer = "Rivers")%>% 
  sf::st_transform(3338) %>% 
  sf::st_zm()
USA_shp <- sf::st_read(dsn = paste0(fn_shp,"/cb_2018_us_state_500k")
                       , layer = "cb_2018_us_state_500k")
AK_shp <- USA_shp %>% 
  filter(STUSPS == "AK") %>% 
  sf::st_transform(3338)

# split by MonitoringLocationTypeName
######20b. Beaches #####
miss_ML_beaches <- missing_ML %>% # filter appropriate sites
  filter(MonitoringLocationTypeName == "BEACH Program Site-Ocean")

### QC check
num_sites <- nrow(miss_ML_beaches)

if(num_sites == 0){
  print(paste("There are NO beach monitoring locations missing AU data."
              ,"Skip to the next section."))
} else {
  print(paste("There ARE", num_sites, "beach monitoring locations missing AU data."
              ,"Continue to assign MLs to AUs using spatial joins."))
}# end if/else statement

### convert to geospatial layer (sf object)
beach_pts <- sf::st_as_sf(x = miss_ML_beaches, coords = c("TADA.LongitudeMeasure"
                                                          ,"TADA.LatitudeMeasure")
                          , crs = "+proj=longlat +datum=WGS84")%>% 
  sf::st_transform(st_crs(beach_shp))

### plot to see how they relate
ggplot() +
  geom_sf(data = AK_shp)+
  geom_sf(data = beach_shp, color = "red") +
  geom_sf(data = beach_pts, color = "red") +
  theme_minimal()

### spatial join
beach_SpatJoin <- sf::st_join(beach_pts, beach_shp, join = st_nearest_feature) %>% # join points and AUs
  select(MonitoringLocationIdentifier, MonitoringLocationName
         , MonitoringLocationTypeName, AUID_ATTNS, Name_AU, HUC10) # trim unneccessary columns

### determine distance (m) between points and nearest feature
near_feat <- sf::st_nearest_feature(beach_pts, beach_shp)
dist_to_AU_m <- sf::st_distance(beach_pts, beach_shp[near_feat,], by_element = TRUE)

### join distance measurements to join results
beach_SpatJoin2 <- cbind(beach_SpatJoin, dist_to_AU_m)

### results and export data
miss_ML_beach_results <- beach_SpatJoin2 %>%
  sf::st_transform(4326) %>% 
  mutate(Longitude = unlist(map(geometry,1)),
         Latitude = unlist(map(geometry,2))) %>% 
  sf::st_drop_geometry()

write_csv(miss_ML_beach_results, file = file.path('Output/data_processing'
                                    , paste0("Missing_MonLoc_Beaches_SpatJoin_"
                                             ,myDate, ".csv"))
          , na = "")

### Clean up environment
rm(num_sites, beach_pts, beach_SpatJoin, beach_SpatJoin2, miss_ML_beach_results
   , miss_ML_beaches, near_feat, dist_to_AU_m, data_19_long)

######20c. Lakes #####
miss_ML_lakes <- missing_ML %>%
  filter(MonitoringLocationTypeName == "Lake"
         |MonitoringLocationTypeName == "Lake, Reservoir, Impoundment")

### QC check
num_sites <- nrow(miss_ML_lakes)

if(num_sites == 0){
  print(paste("There are NO lake monitoring locations missing AU data."
              ,"Skip to the next section."))
} else {
  print(paste("There ARE", num_sites, "lake monitoring locations missing AU data."
              ,"Continue to assign MLs to AUs using spatial joins."))
}# end if/else statement

### convert to geospatial layer (sf object)
lake_pts <- sf::st_as_sf(x = miss_ML_lakes, coords = c("TADA.LongitudeMeasure"
                                                          ,"TADA.LatitudeMeasure")
                          , crs = "+proj=longlat +datum=WGS84")%>% 
  sf::st_transform(st_crs(lake_shp))

### plot to see how they relate
ggplot() + # takes ~15 seconds to load all the lakes
  geom_sf(data = AK_shp)+
  geom_sf(data = lake_shp, color = "blue") +
  geom_sf(data = lake_pts, color = "red") +
  theme_minimal()

### spatial join
lake_SpatJoin <- sf::st_join(lake_pts, lake_shp, join = st_nearest_feature) %>% # join points and AUs
  select(MonitoringLocationIdentifier, MonitoringLocationName
         , MonitoringLocationTypeName, AUID_ATTNS, Name_AU, HUC10_ID) # trim unneccessary columns

### determine distance (m) between points and nearest feature
near_feat <- sf::st_nearest_feature(lake_pts, lake_shp)
dist_to_AU_m <- sf::st_distance(lake_pts, lake_shp[near_feat,], by_element = TRUE)

### join distance measurements to join results
lake_SpatJoin2 <- cbind(lake_SpatJoin, dist_to_AU_m)

### results and export data
miss_ML_lake_results <- lake_SpatJoin2 %>%
  sf::st_transform(4326) %>% 
  mutate(Longitude = unlist(map(geometry,1)),
         Latitude = unlist(map(geometry,2))) %>% 
  sf::st_drop_geometry()

write_csv(miss_ML_lake_results, file = file.path('Output/data_processing'
                                                  , paste0("Missing_MonLoc_Lakes_SpatJoin_"
                                                           ,myDate, ".csv"))
          , na = "")

### Clean up environment
rm(num_sites, lake_pts, lake_SpatJoin, lake_SpatJoin2, miss_ML_lake_results
   , miss_ML_lakes, near_feat, dist_to_AU_m)

######20d. Marine #####
miss_ML_marine <- missing_ML %>%
  filter(MonitoringLocationTypeName == "Estuary"
         |MonitoringLocationTypeName == "Ocean")

### QC check
num_sites <- nrow(miss_ML_marine)

if(num_sites == 0){
  print(paste("There are NO marine monitoring locations missing AU data."
              ,"Skip to the next section."))
} else {
  print(paste("There ARE", num_sites, " marine monitoring locations missing AU data."
              ,"Continue to assign MLs to AUs using spatial joins."))
}# end if/else statement

### convert to geospatial layer (sf object)
marine_pts <- sf::st_as_sf(x = miss_ML_marine, coords = c("TADA.LongitudeMeasure"
                                                       ,"TADA.LatitudeMeasure")
                         , crs = "+proj=longlat +datum=WGS84")%>% 
  sf::st_transform(st_crs(marine_shp))

### plot to see how they relate
ggplot() + # takes ~15 seconds to load all the marine locations
  geom_sf(data = AK_shp)+
  geom_sf(data = marine_shp, color = "blue") +
  geom_sf(data = marine_pts, color = "red") +
  theme_minimal()

### spatial join
marine_SpatJoin <- sf::st_join(marine_pts, marine_shp, join = st_nearest_feature) %>% # join points and AUs
  select(MonitoringLocationIdentifier, MonitoringLocationName
         , MonitoringLocationTypeName, AUID_ATTNS, HUC10_ID) # trim unneccessary columns

### determine distance (m) between points and nearest feature
near_feat <- sf::st_nearest_feature(marine_pts, marine_shp)
dist_to_AU_m <- sf::st_distance(marine_pts, marine_shp[near_feat,], by_element = TRUE)

### join distance measurements to join results
marine_SpatJoin2 <- cbind(marine_SpatJoin, dist_to_AU_m)

### results and export data
miss_ML_marine_results <- marine_SpatJoin2 %>%
  sf::st_transform(4326) %>% 
  mutate(Longitude = unlist(map(geometry,1)),
         Latitude = unlist(map(geometry,2))) %>% 
  sf::st_drop_geometry()

write_csv(miss_ML_marine_results, file = file.path('Output/data_processing'
                                                 , paste0("Missing_MonLoc_Marine_SpatJoin_"
                                                          ,myDate, ".csv"))
          , na = "")

### Clean up environment
rm(num_sites, marine_pts, marine_SpatJoin, marine_SpatJoin2, miss_ML_marine_results
   , miss_ML_marine, near_feat, dist_to_AU_m)

######20e. Rivers #####
miss_ML_rivers <- missing_ML %>%
  filter(MonitoringLocationTypeName == "River/Stream"
         |MonitoringLocationTypeName == "Stream")

### QC check
num_sites <- nrow(miss_ML_rivers)

if(num_sites == 0){
  print(paste("There are NO river monitoring locations missing AU data."
              ,"Skip to the next section."))
} else {
  print(paste("There ARE", num_sites, "river monitoring locations missing AU data."
              ,"Continue to assign MLs to AUs using spatial joins."))
}# end if/else statement

### convert to geospatial layer (sf object)
river_pts <- sf::st_as_sf(x = miss_ML_rivers, coords = c("TADA.LongitudeMeasure"
                                                          ,"TADA.LatitudeMeasure")
                           , crs = "+proj=longlat +datum=WGS84")%>% 
  sf::st_transform(st_crs(river_shp))

### plot to see how they relate
ggplot() + # WARNING::takes ~1 minute to load all the rivers
  geom_sf(data = AK_shp)+
  geom_sf(data = river_shp, color = "blue") +
  geom_sf(data = river_pts, color = "red") +
  theme_minimal()

### spatial join
river_SpatJoin <- sf::st_join(river_pts, river_shp, join = st_nearest_feature) %>% # join points and AUs
  select(MonitoringLocationIdentifier, MonitoringLocationName
         , MonitoringLocationTypeName, AUID_ATTNS, Name_AU, HUC10_ID) # trim unneccessary columns

### determine distance (m) between points and nearest feature
near_feat <- sf::st_nearest_feature(river_pts, river_shp)
dist_to_AU_m <- sf::st_distance(river_pts, river_shp[near_feat,], by_element = TRUE)

### join distance measurements to join results
river_SpatJoin2 <- cbind(river_SpatJoin, dist_to_AU_m)

### results and export data
miss_ML_rivers_results <- river_SpatJoin2 %>%
  sf::st_transform(4326) %>% 
  mutate(Longitude = unlist(map(geometry,1)),
         Latitude = unlist(map(geometry,2))) %>% 
  sf::st_drop_geometry()

write_csv(miss_ML_rivers_results, file = file.path('Output/data_processing'
                                                   , paste0("Missing_MonLoc_River_SpatJoin_"
                                                            ,myDate, ".csv"))
          , na = "")

### Clean up environment
rm(num_sites, river_pts, river_SpatJoin, river_SpatJoin2, miss_ML_rivers_results
   , miss_ML_rivers, near_feat, dist_to_AU_m)

######20f. Mapping #####
# interactive map for missing monitoring locations
# reconfigure shapefiles
beach_shp2 <- beach_shp %>%
  sf::st_transform('+proj=longlat +datum=WGS84')
  
lake_shp2 <- lake_shp %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

marine_shp2 <- marine_shp %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

river_shp2 <- river_shp %>%
  sf::st_transform('+proj=longlat +datum=WGS84')

# setup palette
ML_Type <- factor(levels = c("Lake, Reservoir, Impoundment"
                    , "Lake"
                    , "BEACH Program Site-Ocean"
                    , "Estuary"
                    , "Ocean"
                    , "Stream"
                    , "River/Stream"))

pal <- colorFactor(
  palette = c("#a6cee3", "#a6cee3", "#1f78b4", "#b2df8a", "#b2df8a"
              , "#33a02c", "#33a02c"),
  domain = ML_Type,
  ordered = TRUE)

# make map [WARNING! MAP TAKES TWO MINUTES TO LOAD]
missing_ML_map <- leaflet() %>%
  fitBounds(-180, 50, -120, 70) %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(data = missing_ML, lat = ~TADA.LatitudeMeasure
                   , lng = ~TADA.LongitudeMeasure
                   , popup = paste("MonitoringLocationIdentifier:", missing_ML$MonitoringLocationIdentifier, "<br>"
                                   ,"MonitoringLocationTypeName:", missing_ML$MonitoringLocationTypeName, "<br>"
                                   , "TADA.LatitudeMeasure:", missing_ML$TADA.LatitudeMeasure, "<br>"
                                   , "TADA.LongitudeMeasure:", missing_ML$TADA.LongitudeMeasure)
                   , color = "black", fillColor = ~pal(MonitoringLocationTypeName), fillOpacity = 1, stroke = TRUE
                   ) %>%
  addPolygons(data = beach_shp2, color = "black", weight = 1, opacity = 1
              , popup = paste("AUID_ATTNS:", beach_shp2$AUID_ATTNS, "<br>"
                              , "Name_AU:", beach_shp2$Name_AU)
              , fillColor = "#1f78b4", fillOpacity = 0.5, group = "Beaches"
              ) %>%
  addPolygons(data = lake_shp2, color = "black", weight = 1, opacity = 1
              , popup = paste("AUID_ATTNS:", lake_shp2$AUID_ATTNS, "<br>"
                              , "Name_AU:", lake_shp2$Name_AU)
              , fillColor = "#a6cee3", fillOpacity = 0.5, group = "Lakes"
  ) %>%
  addPolygons(data = marine_shp2, color = "black", weight = 1, opacity = 1
              , popup = paste("AUID_ATTNS:", marine_shp2$AUID_ATTNS)
              , fillColor = "#b2df8a", fillOpacity = 0.5, group = "Marine"
  ) %>%
  addPolylines(data = river_shp2, color = "#33a02c", weight = 3
               , label = river_shp2$AUID_ATTNS, group = "Rivers"
               , popup = paste("AUID_ATTNS:", river_shp2$AUID_ATTNS, "<br>"
                               , "Name_AU:", river_shp2$Name_AU)) %>%
  addLegend("bottomright", pal = pal
            , values = missing_ML$MonitoringLocationTypeName, title = "ML Type"
            , opacity = 1) %>%
  addLayersControl(overlayGroups = c("Lakes", "Rivers", "Beaches", "Marine")
                   ,options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("Lakes", "Rivers", "Beaches", "Marine"))

missing_ML_map # view map [WARNING! MAP TAKES TWO MINUTES TO LOAD]

#Clean up environment
rm(data_18, df_ML, df_ML_AU_Crosswalk, map, missing_ML, missing_ML_map
   , ML_in_crosswalk, ML_Type, pal, AK_shp, beach_shp, beach_shp2, lake_shp
   , lake_shp2, marine_shp, marine_shp2, river_shp, river_shp2, USA_shp
   , fn_shp)
#### Organize data by AUs####
##### 21. AU data summary #####
data_21 <- data_19 %>% 
  filter(!is.na(AUID_ATTNS))

# Number of monitoring locations per AU
df_AU_summary1 <- data_21 %>% 
  select(AUID_ATTNS, MonitoringLocationIdentifier) %>% 
  distinct() %>% 
  count(AUID_ATTNS) %>% 
  rename(n_MonitoringLocations = n)

ggplot(data = df_AU_summary1, aes(x = n_MonitoringLocations))+
  geom_bar(fill="#69b3a2", color="#e9ecef", alpha=0.8)+ 
  scale_y_continuous(breaks= pretty_breaks())+
  scale_x_continuous(breaks= pretty_breaks())+
  labs(title = "Distribution of the # of monitoring locations per AU"
       , x = "Number of Monitoring Locations", y = "Count")+
  theme_classic()

# Summary of WQ data by AU and pollutant
df_AU_summary2 <- data_21 %>% 
  group_by(AUID_ATTNS, TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode) %>% 
  summarize(n_Samples = n()
            , min = round(min(TADA.ResultMeasureValue),3)
            , q25 = round(quantile(TADA.ResultMeasureValue, 0.25, na.rm = TRUE),3)
            , med = round(quantile(TADA.ResultMeasureValue, 0.50, na.rm = TRUE),3)
            , q75 = round(quantile(TADA.ResultMeasureValue, 0.75, na.rm = TRUE),3)
            , max = round(max(TADA.ResultMeasureValue),3))

# Multiple results for a given date
df_AU_summary3 <- data_21 %>% 
  count(AUID_ATTNS, ActivityStartDate, TADA.CharacteristicName) %>% 
  filter(n >1)

#Clean up environment
rm(df_AU_summary1, df_AU_summary2, df_AU_summary3, data_19)

#### Data sufficiency ####
##### 22. AU/pollutant data sufficiency #####
# Match using Data/data_processing/ML_AU_Crosswalk.CSV
df_data_sufficiency <- read_csv("Data/data_processing/AK_DataSufficiency_Crosswalk_20231012.csv")
df_data_sufficiency2 <- df_data_sufficiency %>% 
  select(-c(`Constituent Group`, Constituent, `Use Description`, `Other Requirements`
            , `Listing methodology`, Notes))

# test for missing constituents in df_data_sufficiency
constituents <- unique(df_data_sufficiency2$TADA.Constituent)
WQ_CharacteristicNames <- unique(data_21$TADA.CharacteristicName)

(missing_constituents <- WQ_CharacteristicNames[!(WQ_CharacteristicNames %in% constituents)])

# export data
df_missing_constituents <- as.data.frame(cbind(missing_constituents, "Not in data sufficiency table"))
write_csv(df_missing_constituents
          , file = file.path('Output/data_processing'
                             , paste0("Missing_constituents_in_data_sufficiency_table_"
                                      ,myDate, ".csv")), na = "")

# clean environment
rm(df_data_sufficiency, constituents, WQ_CharacteristicNames, df_missing_constituents)

# join data sufficency data by:
# TADA.Constituent (TADA.CharacteristicName)
# Waterbody Type
data_22 <- data_21 %>% 
  mutate(ActivityStartYear = year(ActivityStartDate)) %>% 
  select(AUID_ATTNS, MonitoringLocationTypeName, AU_Type, ActivityStartYear
         , TADA.CharacteristicName, TADA.ResultMeasureValue
         , TADA.ResultMeasure.MeasureUnitCode)%>% 
  group_by(AUID_ATTNS, MonitoringLocationTypeName, AU_Type
           , TADA.CharacteristicName, TADA.ResultMeasure.MeasureUnitCode) %>% 
  summarize(n_Samples = n()
            , n_Years = n_distinct(ActivityStartYear)) %>% 
  filter(!TADA.CharacteristicName %in% missing_constituents) %>% 
  ungroup()

# loop for data sufficiency for each AU
Unique_AUIDs <- unique(data_22$AUID_ATTNS)
result_list <- list()
counter <- 0

for(i in Unique_AUIDs){
  print(i) # print name of current AU
  counter <- counter + 1
  
  # filter data
  df_subset <- data_22 %>% 
    filter(AUID_ATTNS == i)
  
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
  
  # trim data sufficiency table to only relevant information
  my_data_sufficiency <- df_data_sufficiency2 %>% 
    filter(TADA.Constituent %in% my_constituents) %>% 
    filter(`Waterbody Type` %in% my_WtrBdy_Type)
    # filter(!is.na(`Minimum Assessment Period`))
  
  # join trimmed data sufficiency table to AU data
  df_join <- left_join(df_subset, my_data_sufficiency
                     , by = c("TADA.CharacteristicName" = "TADA.Constituent")
                     , relationship = "many-to-many")
  
  # determine data sufficiency based on # years and # samples of data for AU
  # Note: at this time, all uses and types are applied for every AU.
  results <- df_join %>% 
    mutate(Min_Period_Pass = case_when((n_Years >= Min_Assess_Period_Yrs) ~ "Yes"
                                       , (n_Years < Min_Assess_Period_Yrs) ~ "No")
           , Min_Data_Pass = case_when((n_Samples >= Min_Num_Pts)~ "Yes"
                                       , (n_Samples < Min_Num_Pts)~ "No")
           , Data_Sufficient = case_when((Min_Period_Pass == "Yes"
                                          & Min_Data_Pass == "Yes")~"Yes"
                                         , TRUE ~ "No"))
  
  result_list[[counter]] <- results
} # end of for loop
df_loop_results <- do.call("rbind", result_list) # combine results from for loop
df_AU_data_sufficiency <- as.data.frame(df_loop_results) # convert to data frame
df_AU_data_sufficiency <- df_AU_data_sufficiency %>% 
  distinct()

# clean environment
rm(data_22, df_data_sufficiency2, df_join, df_loop_results, df_subset,
   my_data_sufficiency, result_list, results, counter, i, missing_constituents
   , my_WtrBdy_Type, my_AU_Type, my_constituents, Unique_AUIDs)

#Export data summary
write_csv(df_AU_data_sufficiency
          , file = file.path('Output/data_processing'
                             , paste0("WQ_metadata_trimmed_with_data_sufficiency_"
                                      ,myDate, ".csv")), na = "")

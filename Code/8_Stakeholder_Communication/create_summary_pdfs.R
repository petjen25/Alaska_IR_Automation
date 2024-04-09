library(readr)  # for read_csv()
library(dplyr)
library(sf)
library(AKDECtools)
library(rmarkdown)  # for render
library(ggplot2)

output_df <- read_csv(file = "Output/results/categorized_aus_20240222.csv")  # path to file
wqs_table <- read_csv(file = 'Data/data_analysis/AK_WQS_Crosswalk_20240131.csv')
output_samples <- read_csv(file = 'Output/data_processing/WQ_data_trimmed_long_withAU20240117.csv')
ml_au_crosswalk <- read_csv(file = 'Data/data_processing/ML_AU_Crosswalk.csv')
au_shape_crs <- st_read('Data/data_GIS/Marine/MAUs_FINAL_2023.shp')

ak <- st_read('Data/data_GIS/cb_2018_us_state_500k/cb_2018_us_state_500k.shp') %>%
  filter(STUSPS == 'AK') %>%
  st_transform(crs = st_crs(au_shape_crs))


unique_AU <- output_df %>%
  select(AUID_ATTNS) %>%
  unique() %>%
  head()

au_num_mls <- output_samples %>%
  select(AUID_ATTNS, MonitoringLocationName) %>%
  unique() %>%
  group_by(AUID_ATTNS) %>%
  reframe(AUID_ATTNS = AUID_ATTNS,
          n = n()) %>%
  unique()


unique_AU <- output_df %>%
  select(AUID_ATTNS) %>%
  unique()

# Loop --------------------------------------------------------------------
au_loop <- unique_AU$AUID_ATTNS[[2]]

for (au_loop in unique_AU$AUID_ATTNS){  # for each unique episode
  
  # Isolate that episode from the dataset
  
  output_df_au <- output_df[output_df$AUID_ATTNS == au_loop, ]  
  au_type <- unique(output_df_au$AU_Type) %>%
    na.omit()
  
  if(au_type == 'Marine') {
    
    au_shape <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/marine.shp') %>%
      filter(AUID_ATTNS == au_loop)
    
  } else if(au_type == 'River') {
    
    au_shape <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/rivers.shp') %>%
      filter(AUID_ATTNS == au_loop) %>%
      st_transform(crs = st_crs(au_shape_crs))
    
  } else if(au_type == 'Beach') {
    
    au_shape <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/beaches.shp') %>%
      filter(AUID_ATTNS == au_loop) %>%
      st_transform(crs = st_crs(au_shape_crs))
    
  } else if(au_type == 'Lake') {
    
    au_shape <- st_read('Data/data_GIS/AU_Shapefiles_Corrected_20240328/lakes.shp') %>%
      filter(AUID_ATTNS == au_loop) %>%
      st_transform(crs = st_crs(au_shape_crs))
    
  } else(
    print("No AU Type Detected")
  )
  
  
  sites <- ml_au_crosswalk %>%
    filter(AUID_ATTNS == au_loop) %>%
    select(MonitoringLocationIdentifier, Latitude, Longitude) %>%
    unique() %>%
    st_as_sf(coords = c('Longitude', 'Latitude'), remove = F) %>%
    st_set_crs(4617)  %>% #NAD83 EPSG
    st_transform(st_crs(au_shape))
    
  if(dim(sites)[1] == 0) { #If no sites in AU/ML crosswalk, move on
    next
  }
  
  if(dim(au_shape)[1] == 0) { #If no sites in AU/ML crosswalk, move on
    next
  }
  
  # Now render ('knit') the R Markdown file to html format, name it and save it
  
  render(
    input = "Code/8_Stakeholder_Communication/summary_html_template.rmd",  # path to the template
    output_file = paste0("au_", au_loop, ".html"),  # name the output
    output_dir = "Output/results/summary_htmls"  # folder in which to put the output file
  )
  
}  # end of loop

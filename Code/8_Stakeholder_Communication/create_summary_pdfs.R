# Read data ---------------------------------------------------------------


# Read in the CSV data as a dataframe object
# Each row is an episode of the first season of Dawson's Creek

library(readr)  # for read_csv()
library(dplyr)
library(sf)
library(AKDECtools)
library(ggplot2)
output_df <- read_csv(file = "Output/results/categorized_aus_20240222.csv")  # path to file
wqs_table <- read_csv(file = 'Data/data_analysis/AK_WQS_Crosswalk_20240131.csv')
output_samples <- read_csv(file = 'Output/data_processing/WQ_data_trimmed_long_withAU20240117.csv')

au_shape_crs <- st_read('Data/data_GIS/Marine/MAUs_FINAL_2023.shp')

ak <- st_read('Data/data_GIS/cb_2018_us_state_500k/cb_2018_us_state_500k.shp') %>%
  filter(STUSPS == 'AK') %>%
  st_transform(crs = st_crs(au_shape_crs))


# unique_AU <- output_df %>%
#   select(AUID_ATTNS) %>%
#   unique() %>%
#   head()

unique_AU <- output_df %>%
  select(AUID_ATTNS) %>%
  unique()

# Loop --------------------------------------------------------------------
au_loop <- unique_AU$AUID_ATTNS[[2]]

library(rmarkdown)  # for render

for (au_loop in unique_AU$AUID_ATTNS){  # for each unique episode
  
  # Isolate that episode from the dataset
  
  output_df_au <- output_df[output_df$AUID_ATTNS == au_loop, ]  
  
  
  if(unique(output_df_au$AU_Type) == 'Marine') {
    
    au_shape <- st_read('Data/data_GIS/Marine/MAUs_FINAL_2023.shp') %>%
      filter(AUID_ATTNS == au_loop)
    
  } else if(unique(output_df_au$AU_Type) == 'River') {
    
    au_shape <- st_read('Data/data_GIS/Rivers/Rivers.shp') %>%
      filter(AUID_ATTNS == au_loop) %>%
      st_transform(crs = st_crs(au_shape_crs))
    
  } else if(unique(output_df_au$AU_Type) == 'Beach') {
    
    au_shape <- st_read('Data/data_GIS/Beaches/Beaches.shp') %>%
      filter(AUID_ATTNS == au_loop) %>%
      st_transform(crs = st_crs(au_shape_crs))
    
  } else if(unique(output_df_au$AU_Type) == 'Lake') {
    
    au_shape <- st_read('Data/data_GIS/Lakes/Lakes.shp') %>%
      filter(AUID_ATTNS == au_loop) %>%
      st_transform(crs = st_crs(au_shape_crs))
    
  } else(
    print("No AU Type Detected")
  )
  
  # Now render ('knit') the R Markdown file to pdf format, name it and save it
  
  render(
    input = "Code/8_Stakeholder_Communication/summary_pdfs_template.rmd",  # path to the template
    output_file = paste0("au_", au_loop, ".pdf"),  # name the output
    output_dir = "Output/results/summary_pdfs"  # folder in which to put the output file
  )
  
}  # end of loop

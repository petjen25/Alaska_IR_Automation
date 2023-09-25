##WQP data pull for Alaska 2018-2023 using TADA

##Written by: Hannah Ferriby
##Date created: 9-22-2023
##Date updated: 9-25-2023

####Set Up####
library(TADA)
library(tidyverse)


####Data Download####
#Download by water body type
#Aggregate water-use establishment
awue_data_pull <- TADA_BigDataRetrieval(startDate = '2018-01-01',
                                        siteType = 'Aggregate water-use establishment',
                                        statecode = 'AK')

#Estuary
est_data_pull <- TADA_BigDataRetrieval(startDate = '2018-01-01',
                                       siteType = 'Estuary',
                                       statecode = 'AK')


#Lake, Reservoir, Impoundment
lri_data_pull <- TADA_BigDataRetrieval(startDate = '2018-01-01',
                                       siteType = 'Lake, Reservoir, Impoundment',
                                       statecode = 'AK')


#Ocean
ocean_data_pull <- TADA_BigDataRetrieval(startDate = '2018-01-01',
                                         siteType = 'Ocean',
                                         statecode = 'AK')


#Stream
stream_data_pull <- TADA_BigDataRetrieval(startDate = '2018-01-01',
                                          siteType = 'Stream',
                                          statecode = 'AK')


####Combine####
all <- est_data_pull %>% rbind(lri_data_pull) %>%
  rbind(ocean_data_pull) %>% rbind(stream_data_pull)


####Export Data####
write_csv(est_data_pull, 'Data/data_pull_estuary.csv')
write_csv(lri_data_pull, 'Data/data_pull_lri.csv')
write_csv(ocean_data_pull, 'Data/data_pull_ocean.csv')
write_csv(stream_data_pull, 'Data/data_pull_stream.csv')
write_csv(all, 'Data/data_pull_all.csv')

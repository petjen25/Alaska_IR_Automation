##WQP data pull for Alaska 2018-2023 using TADA

##Written by: Hannah Ferriby
##Date created: 9-22-2023
##Date updated: 10-2-2023

####Set Up####
library(TADA)
library(tidyverse)

startDate <- '2018-02-01'
endDate <- '2023-02-01'

####Data Download####
#Download by water body type
#Aggregate water-use establishment
awue_data_pull <- TADA_BigDataRetrieval(startDate = startDate,
                                        endDate = endDate,
                                        siteType = 'Aggregate water-use establishment',
                                        statecode = 'AK',
                                        applyautoclean = T)

#Estuary
est_data_pull <- TADA_BigDataRetrieval(startDate = startDate,
                                       endDate = endDate,
                                       siteType = 'Estuary',
                                       statecode = 'AK',
                                       applyautoclean = T)


#Lake, Reservoir, Impoundment
lri_data_pull <- TADA_BigDataRetrieval(startDate = startDate,
                                       endDate = endDate,
                                       siteType = 'Lake, Reservoir, Impoundment',
                                       statecode = 'AK',
                                       applyautoclean = T)


#Ocean
ocean_data_pull <- TADA_BigDataRetrieval(startDate = startDate,
                                         endDate = endDate,
                                         siteType = 'Ocean',
                                         statecode = 'AK',
                                         applyautoclean = T)


#Stream
stream_data_pull <- TADA_BigDataRetrieval(startDate = startDate,
                                          endDate = endDate,
                                          siteType = 'Stream',
                                          statecode = 'AK',
                                          applyautoclean = T)


####Combine####
all <- awue_data_pull %>% 
  rbind(est_data_pull) %>%
  rbind(lri_data_pull) %>%
  rbind(ocean_data_pull) %>%
  rbind(stream_data_pull)


####Export Data####
write_csv(est_data_pull, 'Data/data_pull/data_pull_estuary.csv')
write_csv(lri_data_pull, 'Data/data_pull/data_pull_lri.csv')
write_csv(ocean_data_pull, 'Data/data_pull/data_pull_ocean.csv')
write_csv(stream_data_pull, 'Data/data_pull/data_pull_stream.csv')
write_csv(all, 'Data/data_pull/data_pull_all.csv')

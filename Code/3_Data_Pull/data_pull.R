##WQP data pull for Alaska 2018-2023 using TADA

##Written by: Hannah Ferriby
##Date created: 9-22-2023
##Date updated: 4-29-2025

####Set Up####
library(EPATADA)
library(tidyverse)

startDate <- '2017-10-01'
endDate <- '2023-09-30'

####Data Download####
#Download by water body type
#Aggregate surface-water-use
aswu_data_pull <- TADA_DataRetrieval(startDate = startDate,
                                     endDate = endDate,
                                     siteType = 'Aggregate surface-water-use',
                                     statecode = 'AK',
                                     applyautoclean = T)

#Estuary
est_data_pull <- TADA_DataRetrieval(startDate = startDate,
                                    endDate = endDate,
                                    siteType = 'Estuary',
                                    statecode = 'AK',
                                    applyautoclean = T)


#Lake, Reservoir, Impoundment
lri_data_pull <- TADA_DataRetrieval(startDate = startDate,
                                    endDate = endDate,
                                    siteType = 'Lake, Reservoir, Impoundment',
                                    statecode = 'AK',
                                    applyautoclean = T)


#Ocean
ocean_data_pull <- TADA_DataRetrieval(startDate = startDate,
                                      endDate = endDate,
                                      siteType = 'Ocean',
                                      statecode = 'AK',
                                      applyautoclean = T)


#Stream
stream_data_pull <- TADA_DataRetrieval(startDate = startDate,
                                       endDate = endDate,
                                       siteType = 'Stream',
                                       statecode = 'AK',
                                       applyautoclean = T)

all <- est_data_pull %>%
  rbind(lri_data_pull) %>%
  rbind(ocean_data_pull) %>%
  rbind(stream_data_pull)

####Export Data####
write_csv(est_data_pull, 'Data/data_pull/data_pull_estuary.csv')
write_csv(lri_data_pull, 'Data/data_pull/data_pull_lri.csv')
write_csv(ocean_data_pull, 'Data/data_pull/data_pull_ocean.csv')
write_csv(stream_data_pull, 'Data/data_pull/data_pull_stream.csv') #Too big for GitHub
write_csv(all, 'Data/data_pull/data_pull_all.csv') #Too big for GitHub

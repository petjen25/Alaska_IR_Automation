#grab updated base_au shapefiles from AGOL

#install packages
#remotes::install_github("R-ArcGIS/arcgis")
#install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
#install.packages("sf")

#load packages
library(arcgisbinding)
arc.check_product()
arc.check_portal()
library(arcgis)
library(sf)

#get access token
token <- auth_binding()
set_arc_token(token)
token$access_token

#get rivers layer
feature_layer_url_riv <- paste("https://services.arcgis.com/8MMg7skvEbOESlSM/ArcGIS/rest/services/rivers/FeatureServer/1",
                               "?token=",
                               token$access_token,sep="")
base_au_rivers <- arc.open(feature_layer_url_riv)
rivers <- arc.select(base_au_rivers)
rivers2 <- arc.shape(base_au_rivers)
#get lakes layer
feature_layer_url_lak <- paste("https://services.arcgis.com/8MMg7skvEbOESlSM/ArcGIS/rest/services/lakes/FeatureServer/1",
                               "?token=",
                               token$access_token,sep="")
base_au_lakes <- arc.open(feature_layer_url_lak)
lakes <- arc.select(base_au_lakes)

#get marine layer
feature_layer_url_mar <- paste("https://services.arcgis.com/8MMg7skvEbOESlSM/ArcGIS/rest/services/marine/FeatureServer/1",
                               "?token=",
                               token$access_token,sep="")
base_au_marine <- arc.open(feature_layer_url_mar)
marine <- arc.select(base_au_marine)

#get beaches layer
feature_layer_url_bea <- paste("https://services.arcgis.com/8MMg7skvEbOESlSM/arcgis/rest/services/beaches/FeatureServer/1",
                               "?token=",
                               token$access_token,sep="")
base_au_beaches <- arc.open(feature_layer_url_bea)
beaches <- arc.select(base_au_beaches)

#write base_au layers to file
file_path_rivers <- "Data/data_GIS/Rivers/rivers.shp"
arc.write(file_path_rivers, rivers, overwrite = T)

file_path_lake <- "Data/data_GIS/Lakes/lakes.shp"
arc.write(file_path_lake, lakes, overwrite = T)

file_path_marine <- "Data/data_GIS/Marine/marine.shp"
arc.write(file_path_marine, marine, overwrite = T)

file_path_beaches <- "Data/data_GIS/Beaches/beaches.shp"
arc.write(file_path_beaches, beaches, overwrite = T)


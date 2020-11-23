#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Utility function for data processing
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: This script processes raw shapefiles and puts them into application_shapefiles
#-------- this process is driven by "data_source_list" excel file
#-------- it requires there be a "shapefiles_raw_cache" folder for shapefiles to be processed
#-------- script performs applies filtering to keep or drop files
#-------- spatially filters sf objects and selects columns to keep
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(magrittr)

# if (!exists("BEING_SOURCED_FROM_SOMEWHERE")){
#   setwd("~/")
#   rstudioapi::getSourceEditorContext()$path %>%
#     as.character() %>%
#     gsub("/R.*","\\1", .) %>%
#     path.expand() %>%
#     setwd()
# }


#sourcing utility script~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("global.R")

#data import~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#shapefile sources==============================================================
#===============================================================================

#file path DF===================================================================
mapping_files = read_xlsx('data_source_list.xlsx', 
                          sheet = "manual") %>%  
  na_if("NA") %>% 
  remove_empty(c("cols", "rows")) %>%
  data.table() %>% 
  .[,`:=`(folder = paste0("./shapefiles_raw_cache/", raw_name))] %>%  
  filter(to_application_shp == "Y")

#shapefile_extract=============================================================
#section filters, uploads, and spatially filters data 
#resulting data is written to intermediary location 'apllication_shapfiles'
#before they are made 'map_ready'

map_me = mapping_files 

sf_objects_list = list(map_me$folder,
                       map_me$raw_layer_name,
                       map_me$raw_name, 
                       map_me$selection) %>%  
  pmap(purrr_read_ogr_shapefile) 

sf_objects_list_buffered = sf_objects_list %>%   
  modify(function(x) x %>%  
           st_filter(corrdior_buffer) %>% 
           st_transform(2163) %>%
           st_simplify(dTolerance = 100) %>%
           st_transform(4326)
           )

shapefile_writeout(sf_objects_list_buffered, map_me$processed_name, "application_shapefiles")

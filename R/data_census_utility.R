#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Utility script for all US_Census layers 
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: this script gets us census data 
#-------- it uses tidycensus and tigris packages
#-------- it is not robust enough to full automate the process
#-------- seperate opertions are perfromed for layers with/without state level fidelity
#-------- also seperate write-out for Tribal lands since only produces tabular data which needs to be merged with spatail
#-------- puts files in application_shapefiles so that they can be processed to map_ready
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#SETUP==========================================================================
#library 
library(tigris)
library(tidycensus)
library(cancensus)

#path
 # # library(magrittr)
 # # 
 # if (!exists("BEING_SOURCED_FROM_SOMEWHERE")){
 # setwd("~/")
 # rstudioapi::getSourceEditorContext()$path %>%
 #   as.character() %>%
 #   gsub("/R.*","\\1", .) %>%
 #   path.expand() %>%
 #   setwd()
 # }
 # 
#global environemnt variables
# census_api_key("242ce7c9a4b28df96a99abda6972ad638d5d5afc", install = TRUE)

#functions
get_acs_clean = function(selection, states){
  get_acs(geography = unique(selection$boundary), 
          variables = selection$variable,
          state = states,
          geometry = TRUE, 
          year = 2018) %>%  
    st_transform(crs = 4326) %>% 
    select(-GEOID, -moe) %>%  
    merge(., selection[, c("variable", "var_name")]) %>%  
    select(-variable) %>%  
    spread(var_name, estimate) %>%  
    select(NAME, selection$var_name) %>%
    st_filter(corrdior_buffer) 
}
 
#DATA IMPORT====================================================================
#state level data and manual operations
tmp = readxl::read_xlsx("data_source_list.xlsx", sheet = "tidycensus") %>%  
  janitor::remove_empty(c("cols", "rows")) %>% 
  mutate(unique_id = rownames(.)) %>%
  group_by(processed_name) %>%  
  nest() %>% 
  .[which(.$processed_name %in% c("US_Congressional_Districts", "US_Census")),] %>% 
  mutate(data_spatial = map(data, get_acs_clean, c("WA", "OR")))

tmp[which(tmp$processed_name %in% "US_Census"), "data_spatial"] = 
  tmp[which(tmp$processed_name %in% "US_Census"), "data_spatial"] %>%  
  unnest(cols = data_spatial) %>% 
  mutate(`Population at or Below Poverty` = round(100*`Population at or Below Poverty`/`Total Population (20-64yrs)`, 1)) %>% 
  mutate_at(vars(contains("alone")), list((~(100*./`Total Population`) %>%  
                                             round(1)))) %>%
  separate(col = "NAME", sep = ", ", into = c("Tract", "County", "State")) %>%
  group_by(County) %>%  
  mutate(`County Median Income (dollars)` = median(`Median income (dollars)`, na.rm = T)) %>%  
  ungroup() %>%  
  mutate(`Median income status` = ifelse(`Median income (dollars)`<`County Median Income (dollars)`,
                                         "Below County Median", "Above County Median"))  %>% 
  nest(cols = everything()) 

#national level data and manual operations
tmp_sep = readxl::read_xlsx("data_source_list.xlsx", sheet = "tidycensus") %>%  
  janitor::remove_empty(c("cols", "rows")) %>% 
  filter(processed_name != "US_First_Peoples") %>% 
  mutate(unique_id = rownames(.)) %>%
  group_by(processed_name) %>%  
  nest() %>% 
  .[which(.$processed_name %nin% c("US_Congressional_Districts", "US_Census")),] %>% 
  mutate(data_spatial_or = map(data, get_acs_clean, c("OR")), 
         data_spatial_wa = map(data, get_acs_clean, c("WA")),
         data_spatial = list(data_spatial_or, data_spatial_wa) %>% 
           pmap(function(x,y)
             rbindlist(list(x,y)) 
             ))

wirte_out_file_names = list(c(tmp$processed_name, tmp_sep$processed_name),
                            c(tmp$data_spatial, tmp_sep$data_spatial)) 

path = "application_shapefiles/"

wirte_out_file_names[[1]] %>% 
  lapply(function(x) paste0(path, x) %>%
           unlink(recursive = T))

wirte_out_file_names[[1]]  %>% 
  unlist() %>% 
  lapply(function(x) paste0(path, x) %>% 
           dir.create())

list(wirte_out_file_names[[1]], 
     wirte_out_file_names[[2]]) %>%  
  pmap(function(x, y) 
    y %>% st_as_sf() %>%  
      st_write(., paste0(path, x, "/", x, ".shp"))
         )

#Tribal Lands===================================================================
#tidycensus does not have first peoples layer so we have to use 'tigris'

#data import 
data_first_people = readxl::read_xlsx("data_source_list.xlsx", sheet = "tidycensus") %>%  
  janitor::remove_empty(c("cols", "rows")) %>% 
  filter(processed_name == "US_First_Peoples") 

#gey tabular data from tidy census 
first_peoples_metrics = data_first_people %>%  
  list(.$boundary, .$variable) %>%  
  .[-1] %>%  
  pmap(function(x,y)  get_acs(geography = x,
                              variables = y,
                              geometry = F) )  
  
#get tigerlines and merge with tabular data
first_peoples_metrics_sf = first_peoples_metrics %>%  
  rbindlist() %>% 
  # unique() %>% 
  mutate(variable = fct_inorder(variable)) %>%   
  pivot_wider(id_cols = -moe, 
              names_from = variable, values_from = estimate) %>%
  merge(native_areas() %>%  
          select(GEOID), ., by = "GEOID") %>%  
  st_transform(crs = 4326) %>%
  st_filter(corrdior_buffer) %>%  
  select(-GEOID) %>%  
  set_names(c("Name", data_first_people$var_name, "geometry"))

data_first_people$variable %>%  unique()

location = "application_shapefiles"
names = "US_First_Peoples"
names %>%
  map(function(x)
    paste0(location, "/", x) %>%
      unlink(recursive = T))

names %>%
  map(function(x)
    paste0(location, "/", x) %>%
      dir.create())

st_write(first_peoples_metrics_sf,
         paste0(location, "/", names, "/", names, ".shp"),
         # "application_shapefiles/US_First_Peoples/yolo2.shp",
         append=FALSE,
         )

#Canada Census Layers===========================================================
options(cancensus.cache_path = 'ca_census_cache')
options(cancensus.api_key = 'CensusMapper_a81cdd3e133a13fa5dd3e8b67652ad70')
data_ca_census = readxl::read_xlsx("data_source_list.xlsx", sheet = "ca_census") %>%  
  janitor::remove_empty(c("cols", "rows"))

#pulls census level data for vancouver and abbotsford CMAs
census_data_CT <- get_census(dataset='CA16', 
                             regions = list(CMA = c("59933", "59932")),
                             vectors = data_ca_census$variable,
                             level='CT', 
                             geo_format = 'sf')

census_data_CT_final = census_data_CT %>%  
  select(`Region Name`, `Area (sq km)`, contains("v_CA16"))  %>%  
  rename_all(
    list(
        str_remove_all(., '.*:') %>%  
          str_trim()
    )
  )

location = "application_shapefiles"
names = "CA_Census"
names %>%
  map(function(x)
    paste0(location, "/", x) %>%
      unlink(recursive = T))

names %>%
  map(function(x)
    paste0(location, "/", x) %>%
      dir.create())

st_write(census_data_CT_final,
         paste0(location, "/",  names, ".shp"),
         append=FALSE,
)

getwd()

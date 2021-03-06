#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Utility function for data processing
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: This script processes raw shapefiles in application_shapefiles and makes them ready for mapping
#-------- this process is driven by "data_source_list" excel file
#-------- perfroms manual process to clean and add items to sf objects 
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# library(magrittr)

# if (!exists("BEING_SOURCED_FROM_SOMEWHERE")){
  # setwd("~/")
  # rstudioapi::getSourceEditorContext()$path %>%
  #   as.character() %>%
  #   gsub("/R.*","\\1", .) %>%
  #   path.expand() %>%
  #   setwd()
# }

#shapefile sources==============================================================
#===============================================================================

#manually cleans and writes shapefiles==========================================
#this section inports data from shapefiles_rawa_cache
#this section shoudld be performed in its entirety
#do not try to augment only a single layer

#gets data from 'data source list' 
{
#bind_rows code to build processed_shape_files hard codes layers
#these layers are either census or layers used to make other layers that are not included 
#new filter(to_map_ready == "Y") added to fit process better
processed_shape_files = read_xlsx('data_source_list.xlsx',
                                  sheet = "manual") %>%
  remove_empty(c("cols", "rows")) %>%
  mutate(data_layer_info = paste0('<a href = "', src_url, '"> Link to source </a>') %>% 
           ifelse(str_detect(., '"NA'),
                  NA, .)) %>% 
  filter(to_map_ready == "Y") %>%
  select(processed_name, data_layer_info) %>%
  bind_rows(.,  data.frame(processed_name = c("CA_Census", "US_Census"), 
                           data_layer_info = NA)) %>%  
  filter(processed_name != "Regional_Planning", 
         processed_name != "US_Cities") %>%  #these have to be removed since they are in the list but are made from the different state layers
  arrange(processed_name)
  
# processed_shape_files$processed_name
# processed_shape_files = list.files("application_shapefiles") %>%
#   data.table(processed_name = .) %>%
#   merge.data.table(., processed_shape_files,
#                    by = "processed_name") %>%
#   .[!is.na(src_url),`:=`(data_layer_info = paste0('<a href = "', src_url, '"> Link to source </a>'))]

half_polished = list(processed_shape_files$processed_name,
                     processed_shape_files$data_layer_info) %>%
  pmap(function(x, y)
    paste0("application_shapefiles/", x, "/", x, ".shp") %>%
      st_read() %>%
      mutate(`Data Source` = y))
}
 
#Manual Processing
{
#US Census======================================================================
# tmp_col_list = c('T.P1.', "Wrkdt", "WrkdC", "Nfxdw", "Wrkdp", "Pblct", "T.M1.", "Ctv.d", "Ctv.p",  "Walkd", "Bcycl", "Othrm")
tmp_index = which(processed_shape_files$processed_name == "CA_Census")
half_polished[tmp_index][[1]] = half_polished[tmp_index][[1]] %>%
  # select(-tmp_col_list, tmp_col_list) %>% 
  quick_col_arrange() 

#CA Census======================================================================
tmp_index = which(processed_shape_files$processed_name == "US_Census")
half_polished[tmp_index][[1]] = half_polished[tmp_index][[1]] %>%
  quick_col_arrange() 

#US Railroads======================================================================
tmp_index = which(processed_shape_files$processed_name == "US_Railroads")
half_polished[tmp_index][[1]] = half_polished[tmp_index][[1]] %>% 
  filter(is.na(yardname)) %>%  
  filter(direction > 0) %>%
  quick_col_arrange() 

#CA Railroads======================================================================
tmp_index = which(processed_shape_files$processed_name == "CA_Rail")
half_polished[tmp_index][[1]] = half_polished[tmp_index][[1]] %>%
  st_transform(4326) %>%
  st_filter(corrdior_buffer) %>%
  filter(TRCK_CLSSN != "Spur" & 
           TRCK_CLSSN != "Siding" &
           TRCK_CLSSN != "Yard" &
           TRCK_CLSSN != "Crossover") %>% 
  quick_col_arrange() 

#universities===================================================================
tmp_index = which(processed_shape_files$processed_name == "US_Selected Universities (R1)")
half_polished[tmp_index][[1]] = half_polished[tmp_index][[1]] %>%
  filter(TYPE == 1 &
           STATUS == "A") %>%
  unite(ADDRESS, col = "Address", sep = ", ") %>%
  mutate(WEBSITE = paste0('<a href = "', WEBSITE, '">', WEBSITE ,'</a>')) %>%
  quick_col_arrange()

#fed house information============================================================
tmp_file = read_xlsx("US_House_Reps.xlsx", sheet = "altered") %>%
  remove_empty("rows") %>%
  fill(names(.), .direction = "down") %>%
  select(-`Committee Assignment`) %>%
  unique() %>%
  mutate(District = str_remove_all(District, "[:alpha:]") %>%
           paste0("Congressional District ",.)) %>%
  data.frame()

tmp_index = which(processed_shape_files$processed_name == "US_Congressional_Districts")
half_polished[tmp_index][[1]] = half_polished[tmp_index][[1]] %>%
  tidyr::separate(col = "NAME", into = c("NAME", "State"), sep = ",") %>%
  tidyr::separate(col = "NAME", into = c("District", "Congress"), sep = "\\(") %>%
  mutate(Congress = str_remove_all(Congress, "[:punct:]"),
         District = str_trim(District),
         State = str_trim(State)) %>%
  left_join(tmp_file, by = c("District", "State")) %>%
  mutate(Party =  Party %>%
           fct_recode(Democrat = "D", Republican = "R") %>%
           fct_relevel(c("Democrat", "Republican")),
         `Data Source` = Source) %>%
  quick_col_arrange() %>%
  select(District, Congress, Name, Party, Phone, `Data Source`)

#state house information============================================================
tmp_file = read_xlsx("US_House_Reps.xlsx", sheet = "wa_house") %>%
  separate(col = "Name", into = c("Name", "Party"), sep = "\\(") %>%
  mutate(Party = str_remove_all(Party, "[:punct:]") %>%
           forcats::fct_recode(Democrat = "D", Republican = "R"),
         District = paste0("State House District ", District),
         State = "Washington") %>%
  filter(Position == 1)

tmp_file_1 = read_xlsx("US_House_Reps.xlsx", sheet = "or_house") %>%
  na.omit() %>%
  mutate(list = str_replace(list, "Representative", "Representative:") %>%
           str_remove("http://")) %>%
  separate(col = "list", into = c("columns", "data"), sep = ":") %>%
  mutate(data = str_trim(data)) %>%
  data.table() %>%
  .[str_detect(columns, "Represe"),`:=`(Name = data)] %>%
  fill(Name, .direction = "down") %>%
  filter(!str_detect(columns, "Represe")) %>%
  pivot_wider(names_from = columns,
              values_from = (data)) %>%
  mutate(District = paste0("State House District ", District) %>%
           str_remove("<U+200B>"),
         State = "Oregon",
         Party = Party %>%
           fct_relevel(c("Democrat", "Republican")))

tmp_file = tmp_file %>%  bind_rows(tmp_file_1)

tmp_index = which(processed_shape_files$processed_name == "US_State_Legislature_(lower)")
half_polished[tmp_index][[1]] = half_polished[tmp_index][[1]] %>%
  tidyr::separate(col = "NAME", into = c("NAME", "State"), sep = ",") %>%
  tidyr::separate(col = "NAME", into = c("District", "Congress"), sep = "\\(") %>%
  mutate(Congress = str_remove_all(Congress, "[:punct:]"),
         District = str_trim(District),
         State = str_trim(State)) %>%
  left_join(tmp_file, by = c("District", "State")) %>%
  select(District, Name, Party, Congress, State) %>%
  mutate(Party = fct_relevel(Party, c("Democrat", "Republican")),
         `Data Source` = NA) %>%
  quick_col_arrange()

#state senate information============================================================
tmp_file = read_xlsx("US_House_Reps.xlsx", sheet = "wa_senate") %>%
  separate(col = "Name", into = c("Name", "Party"), sep = "\\(") %>%
  mutate(Party = str_remove_all(Party, "[:punct:]") %>%
           forcats::fct_recode(Democrat = "D", Republican = "R"),
         District = paste0("State Senate District ", District),
         State = "Washington")

tmp_file_1 = read_xlsx("US_House_Reps.xlsx", sheet = "or_senate") %>%
  na.omit() %>%
  mutate(list = str_replace(list, "Senator", "Senator:") %>%
           str_remove("http://")) %>%
  separate(col = "list", into = c("columns", "data"), sep = ":") %>%
  mutate(data = str_trim(data)) %>%
  data.table() %>%
  .[str_detect(columns, "Senat"),`:=`(Name = data)] %>%
  fill(Name, .direction = "down") %>%
  filter(!str_detect(columns, "Represe")) %>%
  pivot_wider(names_from = columns,
              values_from = (data)) %>%
  mutate(District = paste0("State Senate District ", District) %>%
           str_remove("<U+200B>"),
         State = "Oregon")

tmp_file = tmp_file %>%  bind_rows(tmp_file_1)

tmp_index = which(processed_shape_files$processed_name == "US_State_Legislature_(upper)")
half_polished[tmp_index][[1]] = half_polished[tmp_index][[1]] %>%
  tidyr::separate(col = "NAME", into = c("NAME", "State"), sep = ",") %>%
  tidyr::separate(col = "NAME", into = c("District", "Congress"), sep = "\\(") %>%
  mutate(Congress = str_remove_all(Congress, "[:punct:]"),
         District = str_trim(District),
         State = str_trim(State)) %>%
  left_join(tmp_file, by = c("District", "State")) %>%
  select(District, Name, Party, Congress, State) %>%
  mutate(Party = Party %>%
           fct_relevel(c("Democrat", "Republican")),
         `Data Source` = NA) %>%
  quick_col_arrange()

#Clean up intermodal locations tomorrow morning!!!!=====================================
tmp_index = which(processed_shape_files$processed_name == "US_Multimodal_Stations")
half_polished[tmp_index][[1]] = half_polished[tmp_index][[1]] %>%  
  modify_at(c("mode_bus",   "mode_air", "mode_rail",
              "mode_ferry", "mode_bike"), as.character) %>%
  modify_at(c("mode_bus",   "mode_air", "mode_rail",
              "mode_ferry", "mode_bike"), ~fct_recode(., Y = "1", N = "0")) %>%
  mutate(`Data Source` = '<a href = "https://hifld-geoplatform.opendata.arcgis.com/datasets/intermodal-passenger-connectivity-database-ipcd?geometry=-123.369%2C47.512%2C-120.360%2C47.836"> Link to source </a>') %>%
  select(-geometry, geometry) %>%
  quick_col_arrange()

#combining cities===============================================================
tmp_index = which(processed_shape_files$processed_name == "OR_Cities")
tmp_index_1 = which(processed_shape_files$processed_name == "WA_Cities")
tmp = half_polished[tmp_index][[1]] %>%
  rename(City = "CITY_NAME") %>%
  data.frame() %>%
  rbind(., half_polished[tmp_index_1][[1]] %>%
          rename(City = "CityName") %>%
          select(City, `Data Source`) %>%
          data.frame()) %>%
  st_as_sf()

merged_cities = tmp %>% 
  # st_join(tmp, tmp_pop) %>%
  # group_by(City) %>%
  # summarise(`Population Estimate` = sum(estimate)) %>%
  # st_set_geometry(NULL) %>%
  # merge(tmp, .) %>%
  # select(City, `Population Estimate`, Data.Source, geometry) %>%
  rename(`Data Source` = "Data.Source")

#combining mpos=================================================================
tmp_file = read_xlsx("MPO_resource_table.xlsx") %>%
  remove_empty("cols") %>%
  na_if("NA") %>%
  filter(!is.na(Key)) %>%
  mutate(`Pub. Date` = as.character(Date) %>%
           na_if("NA") %>%  
           convert_to_date() %>% 
           format(.,"%Y-%m"),
         # `Exp. Update Year` = (`Pub. Date`+months(12*as.numeric(Update_time))) %>% 
           # format(.,"%Y"),
         Web = paste0('<a href = "', Web, '"> Link to website </a>'),
         Pub_Web = paste0('<a href = "', Pub_Web, '"> ', Publication, ' </a>')) %>%
  filter(Include == "T") %>%
  select(Key, Acronym, Type, `Pub. Date`, Pub_Web, Web) %>% 
  pivot_wider(names_from = "Type",
              # id_cols = c("Key", "Acronym", "Web"),
              values_from = c("Pub_Web", "Pub. Date")) %>%
  rename_all(~ str_remove(., "Pub_Web_")) %>%  
  select(!contains("TIP")) 

tmp_index = which(processed_shape_files$processed_name == "WA_MPO-RTPO")
tmp_index_1 = which(processed_shape_files$processed_name == "OR_MPO-RTPO")
tmp_index_2 = which(processed_shape_files$processed_name == "CA_Regional_District")

merged_mpos = half_polished[tmp_index][[1]] %>%
  select(Name, `Data Source`) %>%
  clean_names() %>%
  st_cast("MULTIPOLYGON") %>%
  list(., half_polished[tmp_index_1][[1]] %>%
         clean_names() %>%
         select(-county) %>%
         setnames(old = "Data Source", "data_source")) %>%
  shapefile_pair_collapser() %>% 
  list(., half_polished[tmp_index_2][[1]] %>%
         select(RGN_NM , `Data Source`) %>%  
         rename(Name = "RGN_NM") %>%  
         clean_names() %>% 
         st_cast("MULTIPOLYGON")) %>% 
  shapefile_pair_collapser() %>% 
  mutate(name = ifelse(name == "Southwest Washington RTPO",
                       "Cowlitz-Wahkiakum Council of Governments", 
                       as.character(name))) %>% 
  merge(., tmp_file, by.x = "name", by.y = "Key") %>%
  mutate(name = str_glue('{name} ({Acronym})'),
         Area = st_area(.) %>%
           units::set_units(mile^2)) %>%  
  rename(`Data Source` = "data_source") %>%
  select(name, Web, RTP, CEDS, `Pub. Date_RTP`, `Pub. Date_CEDS`, Area, `Data Source`)

# merged_mpos = st_join(merged_mpos, tmp_pop) %>%
#     group_by(name) %>%
#     summarise(`Population Estimate` = sum(estimate)) %>%
#     st_set_geometry(NULL) %>%
#     merge(merged_mpos, .) %>%
#   select(name, Web, TIP, RTP, CEDS, Date_TIP, Date_RTP, Date_CEDS, `Population Estimate`, Area, data_source, geometry)
}



#DATA WRTIE-OUT
#this writes out to "map_ready" folder
{
index_include = which(processed_shape_files$processed_name %in% c("US_Railroads"))
wirte_out_shapefiles = half_polished
wirte_out_file_names = processed_shape_files$processed_name

index_exclude = which(processed_shape_files$processed_name %in% c("WA_MPO-RTPO", "OR_MPO-RTPO", "US Tribal Subdivisions", "OR_Highways",
                                                                  "OR_Cities", "WA_Cities"))
wirte_out_shapefiles = half_polished[-index_exclude] %>% c(list(merged_mpos, merged_cities))
wirte_out_file_names = processed_shape_files$processed_name[-index_exclude] %>% c("Regional_Planning", "US_Cities")

shapefile_writeout(wirte_out_shapefiles, wirte_out_file_names, "map_ready")
}

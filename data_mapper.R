#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Utility function for mapping.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: script pulls from multiple data streams
#-------- script performs applies filtering to keep or drop files
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#path and data set-up~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if (!exists("BEING_SOURCED_FROM_SOMEWHERE")){
# setwd("~/")
# rstudioapi::getSourceEditorContext()$path %>%
#   as.character() %>%
#   gsub("/R.*","\\1", .) %>%
#   path.expand() %>%
#   setwd()
# }

#DATA Import====================================================================
#gets files from source list~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
processed_shape_files = read_xlsx('data_source_list.xlsx', 
                                  sheet = "manual") %>%  
  data.table() %>%  
  .[, .(processed_name, selection, selected_new, zcol, notes, src_url, group)] %>%  
  bind_rows(., 
            read_xlsx('data_source_list.xlsx', 
                      sheet = "tidycensus") %>%  
              data.table() %>%  
              .[,.(processed_name, selection, selected_new, zcol, notes, group, name, var_name, src_url)]) %>% 
  bind_rows(., 
            read_xlsx('data_source_list.xlsx', 
                      sheet = "ca_census") %>%  
              data.table() %>%  
              .[,.(processed_name, selection, selected_new, zcol, notes, name, var_name, src_url)]) %>% 
  unique() %>%  
  remove_empty("rows")

#makes census index object - only takes from the "US_Census" group
census_selection_index = processed_shape_files %>% 
  .[processed_name == "US_Census",.(processed_name, name, var_name)] %>%  
  unique() %>% 
  bind_rows(., data.frame(processed_name = c("US_Census", "US_Census"),
                          name = c("US_Income", "US_Income"),
                          var_name = c("County Median Income (dollars)", "Median income status"))) %>% 
  bind_rows(., .) %>%  
  .[duplicated(.),`:=`(name = "All")] %>% 
  mutate(var_name = var_name %>% 
           str_remove_all(",")  %>% 
           str_trim() %>%  
           str_to_title() %>% 
           replace_non_ascii()) %>%  
  data.table()

#makes census index object - only takes from the "CA_Census" group
ca_census_selection_index = processed_shape_files %>% 
  .[processed_name == "CA_Census",.(processed_name, name)] %>% 
  mutate(var_name = processed_shape_files %>%  
           filter(processed_name == "CA_Census") %>%  
           select(selected_new) %>%  
           .[[1]] %>%  
           .[1] %>%  
           str_split(", ") %>%  
           .[[1]] %>%  
           .[-c(1, 2)]) %>% 
  unique() %>%
  bind_rows(., .) %>%
  data.table() %>% 
  .[duplicated(.),`:=`(name = "All")]

#cleans object of unneeded columns after census object creation (above)
processed_shape_files = processed_shape_files %>%  
  select(-name, -var_name) %>%  
  unique()

#MAP DATA=======================================================================
#checks files that have been processed and put into "map_ready" folder
#merges with what is in "data_source" this is unneeded if info matches
#essentially a holdover from an earlier version that was less stable
#this operation formats and adds some new columns 
map_ready = list.files("map_ready") %>%  
  data.table(processed_name = .) %>% 
  merge.data.table(., processed_shape_files , 
                   by = "processed_name", all.x = T) %>% 
  na_if("NA") %>% 
  .[is.na(zcol), `:=`(zcol = "plain")] %>% 
  mutate(notes = str_remove_all(notes, "'") #%>%  
           # str_replace_all(notes, "\\\r\\\n\\\r\\\n")
           ) %>%
  mutate(notes = ifelse(!is.na(notes), 
                        str_glue('<a href="#" onclick="alert(\'{notes}\');">Click for Description</a>'),
                        "No Data"),
         src_url = ifelse(!is.na(src_url), 
                          str_glue('<a href="{src_url}" target="popup" onclick="window.open("{src_url}","name","width=600,height=400")">Link to data source</a>'),
                          "No Data")) %>%  
  unique() 

#extracts all the shapefiles and performs mapping operation 
map_files = list(map_ready$processed_name,
                 map_ready$selected_new,
                 map_ready$zcol %>%  str_to_title()) %>%
  pmap(function(x, y, z)
    paste0("map_ready/", x, "/", x, ".shp") %>%
      st_read() %>% 
      rename_all(str_to_title) %>% 
      {if(!is.na(y[1])) set_names(.,
                                  c(y %>%
                                      str_split(",") %>%
                                      lapply(str_trim) %>% 
                                      lapply(str_to_title) %>% 
                                      .[[1]], "Data Source", "Geometry")) else .} %>%
      mutate(Plain = "no_color") %>%
      select(-Plain, Plain, -Geometry, Geometry) %>%
      mapview(layer.name = x %>%
                str_replace_all("_", " "),
              zcol = z,
              legend = ifelse(z == "Plain", F, T),
              homebutton = F,
              popup = popupTable(., zcol = -c(ncol(.)-1, ncol(.))))
  )

#extracts all the shapefiles and keeps them in tabular form
map_files_dfs = list(map_ready$processed_name,
                     map_ready$selected_new) %>%
  pmap(function(x, y)
    paste0("map_ready/", x, "/", x, ".shp") %>%
      st_read() %>%
      rename_all(str_to_title) %>% 
      {if(!is.na(y[1])) set_names(.,
                                  c(y %>%
                                      str_split(",") %>%
                                      lapply(str_trim) %>% 
                                      lapply(str_to_title) %>% 
                                      .[[1]], "Data Source", "Geometry")) else .} %>%
      mutate(Plain = "no_color") %>%
      select(-Plain, Plain, -Geometry, Geometry)
  )

#SECTION: CENSUS_SUBSET_METRICS===============================================

tmp_ca = which(map_ready$processed_name %in% "US_Census")
tmp_us = which(map_ready$processed_name %in% "CA_Census")
tmp_reg = which(map_ready$processed_name %in% "Regional_Planning")

#us_census====
index_clmn_rmv = c("Data Source", "Plain", "Geometry", "selected_")

#used for a variety of widgets in the code - this is just the US Census 
notshared_census = which(map_ready$processed_name %in% "US_Census") %>%
  map_files_dfs[[.]]

#same object as above but with an additional popup column 
#this as created as to not mess with any objects that called "notshared_census"
#used int leaflet proxy
notshared_census_lfpopup = leaflet_popup_maker(notshared_census, index_clmn_rmv)

notshared_gg_tmp = notshared_census %>%
  data.frame() %>%
  set_names(c(colnames(notshared_census))) %>%
  select(!all_of(index_clmn_rmv[-length(index_clmn_rmv)])) %>%
  data.table()

index_numeric_columns_gg = notshared_gg_tmp %>%
  modify(as.character) %>%
  modify(as.numeric) %>%
  remove_empty("cols") %>%
  colnames()

notshared_gg_tmp = melt(notshared_gg_tmp,
                        id.vars = c("Tract", "County", "State"),
                        measure.vars = index_numeric_columns_gg) %>%
  mutate(type = "Corridor")

#ca_census====
notshared_census_ca = which(map_ready$processed_name %in% "CA_Census") %>%
  map_files_dfs[[.]]

notshared_census_ca_lfpopup = leaflet_popup_maker(notshared_census_ca, index_clmn_rmv)

notshared_gg_tmp_ca = notshared_census_ca %>%
  data.frame() %>%
  set_names(c(colnames(notshared_census_ca))) %>%
  select(!all_of(index_clmn_rmv[-length(index_clmn_rmv)])) %>%
  data.table()

index_numeric_columns_gg_ca = notshared_gg_tmp_ca %>%
  modify(as.character) %>%
  modify(as.numeric) %>%
  remove_empty("cols") %>%
  colnames()

notshared_gg_tmp_ca = melt(notshared_gg_tmp_ca,
                        id.vars = c("Name"),
                        measure.vars = index_numeric_columns_gg_ca) %>%
  mutate(type = "Corridor")

#MPO PLOTS======================================================================
mpo_spatial = which(map_ready$processed_name %in% "Regional_Planning") %>%  
  map_files_dfs[.] %>%  
  .[[1]]

mpo_census_merge = sf:::aggregate.sf(notshared_census[, index_numeric_columns_gg], 
                         mpo_spatial, 
          FUN = "mean", na.rm = TRUE)

mpo_census_merge_df = mpo_census_merge %>% 
  mutate(`Area` = st_area(geometry) %>%  
           set_units(km^2) %>% 
           round(2)) %>%  
  merge(mpo_spatial %>%  
          mutate(`Area` = st_area(Geometry) %>%  
                   set_units(km^2) %>% 
                   round(2)) %>%  
          select(Name, `Area`) %>% 
          st_set_geometry(NULL), by = "Area") %>%  
  select(Name, everything()) %>%  
  st_set_geometry(NULL) %>%  
  rename(`Area (sq. km)` = "Area") %>% 
  mutate_at(index_numeric_columns_gg, round, 2) %>%  
  filter(Name != "Metro Vancouver (Metro)")

mpo_df_alt = read_xlsx("MPO_resource_table.xlsx") %>%  
  remove_empty(c("cols", "rows")) %>%  
  filter(Type != "TIP") %>% 
  na_if("NA") %>% 
  na_if("-") %>% 
  mutate(Date = convert_to_date(Date), 
         Expected_update = (Date+months(12*as.numeric(Update_time))) %>% 
           format(.,"%Y"),
         Name = paste0(Key, " (", Acronym, ")")) %>% 
  data.table() %>%  
  mutate(html_link = ifelse(is.na(Pub_Web), 
                            "No link",
                            str_glue("<a href = \"{Pub_Web}\"> Link to document </a>")))

mpo_df_alt_soonest_10 = mpo_df_alt %>% 
  filter(Type == "RTP") %>%  
  arrange(Expected_update) %>%  
  mutate(Date = Date %>% 
           format(.,"%Y-%m")) %>% 
  rename(`MPO/RTPO` = "Acronym", 
         `Pub. Date` = "Date",
         `Exp. Update` = "Expected_update" ) %>% 
    select(`MPO/RTPO`, `Pub. Date`, `Exp. Update`,  html_link)  

# mpo_df = read_xlsx("MPO_resource_table.xlsx") %>%
#   remove_empty(c("cols", "rows")) %>%
#   na_if("NA") %>%
#   mutate(Date = convert_to_date(Date),
#          Name = paste0(Key, " (", Acronym, ")")) %>%
#   merge(., data.frame(mpo_spatial) , by = "Name") %>%
#   rename(#`Population Estimate` = Population.Estimate,
#          `Area (mi^2)` = Area...Mi.2.) %>%
#   mutate(`Area (mi^2)` = round(`Area (mi^2)`,0),
#          `Document\nStatus:` = ifelse(is.na(Date), "Missing", "Obtained")) %>% 
#   filter(Type != "LRP") 

print("END")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Scratch file for ad hoc code or for unit-testing code
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: NA
#-------- NA
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

# read_excel("ca_parknride_raw.xlsx") %>%  
#   fill(lat_long, .direction = "down") %>%  
#   separate(Vars, sep = ":", into = c("Var", "Value")) %>% 
#   separate(lat_long, sep = ",", into = c("lat", "long")) %>%  
#   mutate_all(str_trim) %>%  
#   mutate_at(c("lat", "long"), as.numeric) %>% 
#   mutate(Var = str_replace(Var, "Daily Rate", "Dly Rate") %>% 
#            str_replace("Monthly Rate", "Mon Rate") %>% 
#            str_replace("Montly Rate", "Mon Rate") %>% 
#            str_replace("TransLink Park&Go Zone Number", "Zone") %>% 
#            str_replace("Impark Lot Number", "Lot")) %>% 
#   pivot_wider(names_from = "Var", 
#               values_from = "Value") %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
#   st_write("shapefiles_raw_cache/CA_Park and Ride/CA_Park and Ride.shp")
#   
# map = readOGR('shapefiles_raw_cache/EBC_LOCAL_GOVERNMENTS_SP', 
#         'EBC_LOC_GO_polygon') %>% 
#   st_as_sf() 
# 
# map %>%  
#   filter(RGN_TYP == "Regional District") %>%  
#   st_write("shapefiles_raw_cache/CA_Regional_District/CA_Regional_District.shp")
# 
# map %>%  
#   filter(RGN_TYP == "District Municipality") %>%  
#   st_write("shapefiles_raw_cache/CA_District_Municipality/CA_District_Municipality.shp")
# 
# map %>%  
#   filter(RGN_TYP == "IR") %>%  
#   st_write("shapefiles_raw_cache/CA_First_Peoples/CA_First_Peoples.shp")
# 
# readOGR('corrdior_buffer', 
#         'usr_geometry_201117_1346') %>%
#   st_as_sf() %>% buffer_cleaner(20*1609.34) %>%  
#   st_write("corrdior_buffer/corrdior_buffer.shp", 
#            append = F)
#   mapview()
# readOGR('corrdior_buffer', 
#         'corrdior_buffer') %>%
#   st_as_sf() %>%   
#   mapview()

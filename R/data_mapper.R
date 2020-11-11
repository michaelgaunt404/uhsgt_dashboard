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
              .[,.(processed_name, selection, selected_new, zcol, notes, group, name, var_name)],
            read_xlsx('data_source_list.xlsx', 
                       sheet = "ca_census") %>%  
              data.table() %>%  
              .[,.(processed_name, selected_new, zcol, notes)]) %>% 
  unique() %>%  
  remove_empty("rows")
# 
# yolo = readOGR("map_ready/CA_Census", 
#         "CA_Census") %>%  
#   st_as_sf()
# 
# yolo =read_xlsx('data_source_list.xlsx', 
#           sheet = "ca_census") %>%
#   select(var_name) %>% 
#   data.table() %>%  
#   .[,.(processed_name, selection, selected_new, zcol, notes, group, name, var_name)]

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
           str_to_title())  %>%  
  data.table()

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
  mutate(notes = str_glue('<a href="#" onclick="alert(\'{notes}\');">Click for Description</a>'),
         src_url = str_glue('<a href="{src_url}">Link to data source</a>')) %>%  
  unique() 
# map_ready$processed_name
# map_ready$processed_name
# x1 = map_ready[str_detect(map_ready$processed_name, "US_First_Peoples"),]
# x = "US_First_Peoples"
# x$selection %>%  str_split(",") %>% .[[1]] %>%   length()
# x1$selected_new %>%  str_split(",") %>% .[[1]] %>%   length()
# paste0("map_ready/", x, "/", x, ".shp") %>%
#   st_read() %>%  colnames()

# yolo = map_ready
# map_ready = yolo[1,]
# x = map_ready$processed_name
# y = map_ready$selected_new 
# z = map_ready$zcol %>%  str_to_title()

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

#MPO PLOTS======================================================================
mpo_spatial = which(map_ready$processed_name %in% "MPO_RTPOs") %>%  
  map_files_dfs[.] %>%  
  .[[1]]

mpo_df = read_xlsx("MPO_resource_table.xlsx") %>%  
  remove_empty(c("cols", "rows")) %>%  
  na_if("NA") %>% 
  mutate(Date = convert_to_date(Date), 
         Name = paste0(Key, " (", Acronym, ")")) %>%  
  merge(., data.frame(mpo_spatial) , by = "Name") %>%
  rename(#`Population Estimate` = Population.Estimate,
         `Area (mi^2)` = Area...Mi.2.) %>%  
  mutate(`Area (mi^2)` = round(`Area (mi^2)`,0), 
         `Document\nStatus:` = ifelse(is.na(Date), "Missing", "Obtained"))

mpo_timeline = mpo_df %>%
  mutate(id = as.numeric(rownames(.)), 
         end = NA, 
         Date = Date + months(48),
         content = paste(Acronym, Type, sep = "-")) %>% 
  select(id, content, Date, end, Type) %>%
  mutate(qtr = Date %>%  floor_date("quarter")) %>%
  arrange(qtr, content) %>%
  mutate(order = 1) %>%
  group_by(qtr) %>%
  mutate(order = cumsum(order),
         text = str_glue("{content} {Date}")) %>%
  filter(Type != "LRP") %>% 
  filter(!is.na(Type)) %>% 
  ggplot() +
  geom_point(aes(qtr, order, color = as.factor(Type),
                 text = text),
             size = 6) +
  ylim(0,5) +
  labs(x = "", y = "", color = "Publication") +
  theme +
  theme(legend.position="bottom")

mpo_timeline = mpo_timeline %>%
  ggplotly(tooltip = "text") %>%
  layout(legend = list(
    orientation = "h")
  )

#FINAL MAPPING==================================================================
#i dont believe these area used at all
# mmap = map_files %>%  
#   reduce(`+`)
# 
# mmap@map = mmap@map %>%
#   leaflet::hideGroup(map_ready$processed_name[-2] %>%  str_replace_all("_", " ")) %>%  
#   setView(-116.207, 46,223, zoom = 7) 


# map_files_dfs[[4]]  %>% mapview()
  # st_transform(2163) %>%
  # st_simplify(dTolerance = 100) %>%
  # st_transform(4326) %>%  mapview()

  
#mpaviles filtered by buffer ===
# tmppp = map_files_dfs %>% 
#   map(st_filter, buffer_filter) 
# 
# #index of resulting empty files are here 
# index_bffr_fltrd_map = tmppp %>%  map( function(x) x %>% 
#                           st_dimension() %>%
#                           length()) %>% 
#   unlist() %>%  
#   data.table(empty_list = .) %>%
#   .[,`:=`(rwnms = rownames(.) %>%  as.numeric())] %>%
#   .[which(empty_list == 0),rwnms]
# 
# empty_layers = map_ready[index_bffr_fltrd_map, "processed_name"]
#   
# map_files_dfs_fltrd = tmppp[-index_bffr_fltrd_map]
# 
# map_ready_fltrd = map_ready[-index_bffr_fltrd_map,]
# 
# yolo = list(map_files_dfs_fltrd,
#             map_ready_fltrd$zcol %>%  str_to_title(),
#             map_ready_fltrd$processed_name) %>%
#   pmap(function(x, y, m) x %>%
#       mapview(
#         layer.name = m %>%
#           str_replace_all("_", " "),
#         zcol = y,
#         legend = F,
#         homebutton = F,
#         popup = popupTable(., zcol = -c(ncol(.)-1, ncol(.)))
#       ))
# 
# 
# tmp_map = yolo  %>%  
#   reduce(`+`)
# 
# tmp_map@map = tmp_map@map %>%
#   leaflet::hideGroup(map_ready_fltrd$processed_name[-1] %>%  str_replace_all("_", " "))


# 
# yolo = list(map_files,
#             map_ready$zcol %>%  str_to_title(),
#             map_ready$processed_name) %>%
#   pmap(function(x, y, m) 
#     x %>%
#          st_filter(buffer_filter)) %>%
#   mapview(
#     layer.name = m %>%
#             str_replace_all("_", " "),
#           zcol = y,
#           legend = F,
#           homebutton = F,
#           popup = popupTable(., zcol = -c(ncol(.)-1, ncol(.)))
#   )
# 
# 
# input = data.frame(longitude = -122.19543,
#                    latitude = 47.37044,
#                    distance = 10000)
# 
# #
# buffer_filter = st_as_sf(input, coords = c("longitude", "latitude"), crs = 4326) %>%
#     st_transform(crs = 3488) %>%
#     st_buffer(dist = input$distance) %>%
#     st_transform(crs = 4326)
# # 
# buffer_filter = st_as_sf(coords = c(input$longitude, input$latitude), crs = 4326) %>%
#   st_transform(crs = 3488) %>%
#   st_buffer(dist = input$distance) %>%
#   st_transform(crs = 4326)



# 
# mapview(franconia, popup = popupTable(franconia, zcol = 1:5))
# 
# install.packages("htmltools")
# library(htmltools)
# 
# bingPlugin <- htmlDependency(
#   "leaflet.plugins", "2.0.0",
#   src = normalizePath("./js"),
#   script = "Bing.min.js"
# )
# 
# registerPlugin <- function(map, plugin) {
#   map$dependencies <- c(map$dependencies, list(plugin))
#   map
# }
# 
# mmap@map %>%
#   registerPlugin(bingPlugin) %>%
#   onRender("function(el, x) {
#     var imagerySet = 'Aerial';
#     var bing = new L.BingLayer('LfO3DMI9S6GnXD7d0WGs~bq2DRVkmIAzSOFdodzZLvw~Arx8dclDxmZA0Y38tHIJlJfnMbGq5GXeYmrGOUIbS2VLFzRKCK0Yv_bAl6oe-DOc',
#          {type: imagerySet});
#      this.addLayer(bing);
#  }")
# 
# 
# esriPlugin <- htmlDependency("leaflet.esri", "1.0.3",
#                              src = c(href = "https://cdn.jsdelivr.net/leaflet.esri/1.0.3/"),
#                              script = "esri-leaflet.js"
# )
# 
# esriPlugin <- htmlDependency("leaflet-groupedlayercontrol", "0.6.1",
#                              src = c(href = "https://github.com/ismyrnow/leaflet-groupedlayercontrol/blob/gh-pages/src/"),
#                              script = "leaflet.groupedlayercontrol.js"
# )
# 
# # https://github.com/ismyrnow/leaflet-groupedlayercontrol/blob/gh-pages/src/leaflet.groupedlayercontrol.js
# # 
# # registerPlugin <- function(map, plugin) {
# #   map$dependencies <- c(map$dependencies, list(plugin))
# #   map
# # }
# 
# 
# leaflet() %>% setView(-122.23, 37.75, zoom = 10) %>%
#   # Register ESRI plugin on this map instance
#   registerPlugin(esriPlugin) %>%
#   # Add your custom JS logic here. The `this` keyword
#   # refers to the Leaflet (JS) map object.
#   onRender("function(el, x) {
#     L.esri.basemapLayer('Topographic').addTo(this);
#   }")
# 
# 
# mmap@map %>%
#   registerPlugin(ctrlGrouped) %>%
#   onRender("function(el, x) {
#     var groupedOverlays = {
#   'Landmarks': {
#     'ROW': `Utility ROWs`
#   },
#   'Points of Interest': {
#     'Restaurants': `Selected Universities (R1)`,
#     'Restaurants_1': `Electric Planning Areas`
#   }
# };
# 
# var options = {
#       exclusiveGroups: ['Landmarks'],  // use radio inputs
#       groupCheckboxes: true
# 		};
# 
# L.control.groupedLayers(baseLayers, groupedOverlays).addTo(this);
#   }")
# 
# 
# leaflet() %>%
#   setView(-104.99, 39.73, zoom = 10) %>% registerPlugin(ctrlGrouped) %>% 
#   onRender("function(el, x) {
#     var basemaps = {
#       Grayscale: L.tileLayer('http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png', {
#         maxZoom: 18,
#         attribution: '&copy; <a href=http://www.openstreetmap.org/copyright>OpenStreetMap</a>'
#       }),
#       Streets: L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
#         maxZoom: 19,
#         attribution: '&copy; <a href=http://www.openstreetmap.org/copyright>OpenStreetMap</a>'
#       })
#     };
#     basemaps.Grayscale.addTo(this);   // default base layer
#     var groups = {
#       cities: new L.LayerGroup(),
#       restaurants: new L.LayerGroup(),
#       dogs: new L.LayerGroup(),
#       cats: new L.LayerGroup()
#     };
#     L.marker([39.61, -105.02]).bindPopup('Littleton, CO.').addTo(groups.cities);
#     L.marker([39.74, -104.99]).bindPopup('Denver, CO.').addTo(groups.cities);
#     L.marker([39.73, -104.8 ]).bindPopup('Aurora, CO.').addTo(groups.cities);
#     L.marker([39.77, -105.23]).bindPopup('Golden, CO.').addTo(groups.cities);
#   
#     L.marker([39.69, -104.85]).bindPopup('A restaurant').addTo(groups.restaurants);
#     L.marker([39.69, -105.12]).bindPopup('B restaurant').addTo(groups.restaurants);
#   
#     L.marker([39.79, -104.95]).bindPopup('A dog').addTo(groups.dogs);
#     L.marker([39.79, -105.22]).bindPopup('B dog').addTo(groups.dogs);
#   
#     L.marker([39.59, -104.75]).bindPopup('A cat').addTo(groups.cats);
#     L.marker([39.59, -105.02]).bindPopup('B cat').addTo(groups.cats);
#     // Overlay layers are grouped
#     var groupedOverlays = {
#       'Landmarks': {
#         'Cities': groups.cities,
#         'Restaurants': groups.restaurants
#       },
#       'Random': {
#         'Dogs': groups.dogs,
#         'Cats': groups.cats
#       }
#     };
#     
# 		var options = {
#       exclusiveGroups: ['Landmarks'],  // use radio inputs
#       groupCheckboxes: true
# 		};
# 		L.control.groupedLayers(basemaps, groupedOverlays, options).addTo(this);
# 	}") 
# 
# 
# 
# urlf <- 'https://raw.githubusercontent.com/ismyrnow/leaflet-groupedlayercontrol/gh-pages/dist/%s'
# download.file(sprintf(urlf,'leaflet.groupedlayercontrol.min.js'), './java/L.Control.groupedlayer.js', mode="wb")
# download.file(sprintf(urlf,'leaflet.groupedlayercontrol.min.css'), './java/L.Control.groupedlayer.css', mode="wb")
# 
# ctrlGrouped <- htmltools::htmlDependency(
#   name = 'ctrlGrouped',
#   version = "1.0.0",
#   # works in R and Shiny - download js/css files, then use this:
#   src = c(file = normalizePath('./java')),
#   script = "L.Control.groupedlayer.js",
#   stylesheet = "L.Control.groupedlayer.css"
# )

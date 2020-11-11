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
library(magrittr)

if (!exists("BEING_SOURCED_FROM_SOMEWHERE")){
  setwd("~/")
  rstudioapi::getSourceEditorContext()$path %>%
    as.character() %>%
    gsub("/R.*","\\1", .) %>%
    path.expand() %>%
    setwd()
}

#data import~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#tabular sources================================================================
#===============================================================================

#shapefile sources==============================================================
#===============================================================================

#individual layers==============================================================
#===============================================================================

#file to ===================================================================
processed_shape_files = read_xlsx('./data/data_source_list.xlsx', 
                                  sheet = "manual") %>%  
  data.table() %>%  
  .[,.(processed_name, selection, zcol, notes)] %>%  
  bind_rows(., 
            read_xlsx('./data/data_source_list.xlsx', 
                      sheet = "tidycensus") %>%  
              data.table() %>%  
  .[,.(processed_name, selection, zcol, notes)]) %>% 
  unique() %>%  
  remove_empty("rows")

map_ready = list.files("./data/map_ready") %>%  
  data.table(processed_name = .) %>% 
  merge.data.table(., processed_shape_files, 
                   by = "processed_name", all.x = T) %>% 
  .[is.na(zcol), `:=`(zcol = "plain")]

# half_polished = list(processed_shape_files$processed_name, 
#                      processed_shape_files$data_layer_info) %>%
#   pmap(function(x, y)
#     paste0("./data/application_shapefiles/", x, "/", x, ".shp") %>%
#       st_read() %>%
#       mutate(`Data Source` = y))


# processed_shape_files = read_xlsx('./data/data_source_list.xlsx', 
#                           sheet = "manual") %>%  
#   na_if("NA") %>% 
#   remove_empty(c("cols", "rows")) %>%
#   data.table() %>% 
#   .[import == "Y" &
#       processed == "Y" & 
#       Map == "X", ] %>% 
#   .[!is.na(src_url),`:=`(data_layer_info = paste0('<a href = "', src_url, '"> Link to source </a>'))] %>% 
#   .[,`:=`(selected = str_to_title(selection))]
# 
# processed_census_files = read_xlsx('./data/data_source_list.xlsx', 
#                             sheet = "tidycensus") %>%  
#   na_if("NA") %>% 
#   remove_empty(c("cols", "rows")) %>%
#   data.table() %>% 
#   .[processed == "Y" & 
#       Map == "X", ]  %>% 
#   .[,`:=`(selected = str_to_title(selection))] %>%  
#   .[,-c(1:5)] %>%  
#   unique()

processed_files = bind_rows(processed_shape_files, processed_census_files) %>%  
  .[is.na(zcol), `:=`(zcol = "plain")]

corridor_files = list(processed_files$processed_name, 
                 processed_files$data_layer_info) %>%
  pmap(function(x, y, z, m)
    paste0("./data/application_shapefiles/", x, "/", x, ".shp") %>%
      st_read() %>%
      rename_all(str_to_title) %>%
      mutate(`Data Source` = y) %>% 
      data.table() %>%  list(x)
  ) #$%>%  list(processed_files$processed_name)

corridor_files  

map_files = list(processed_files$processed_name, 
                 processed_files$selected,
                 processed_files$data_layer_info, 
                 processed_files$zcol %>%  str_to_title()) %>%
  pmap(function(x, y, z, m)
    paste0("./data/application_shapefiles/", x, "/", x, ".shp") %>%
      st_read() %>%
      rename_all(str_to_title) %>%
      mutate(Plain = "no_color",
             `Data Source` = z) %>% 
      select(-Plain, Plain, -Geometry, Geometry) %>% 
      mapview(layer.name = x %>%
                str_replace_all("_", " "),
              zcol = m,
              legend = F,
              homebutton = F,
              popup = popupTable(., zcol = -c(ncol(.)-1, ncol(.))))
  )

mmap = map_files %>%  
  reduce(`+`)

mmap@map = mmap@map %>%
  # leaflet::hideGroup((processed_files$processed_name[c(1:3)])) %>%
  leaflet::hideGroup(processed_files$processed_name[c(8:13)])
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

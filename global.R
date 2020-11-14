#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Loads Library and custom funcitons
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: limiting the length of mapping_utiliy.R script
#-------- defines environment varaibles
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#package install and load~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(rgdal) #for inport/outport
# library(maptools) #provides mapping functions
library(sf)
library(mapview)
library(leaflet)
library(leafpop)
library(units)

library(data.table)
library(tidyverse)
library(lubridate)
library(tidycensus)
library(tidyr)
library(magrittr)
library(furrr)
library(skimr)

library(readxl)
library(stringr)
library(janitor)
library(forcats)

library(plotly)
library(ggplot2)
library(viridis)
library(DT)
library(ggdark)

library(crosstalk)
library(shiny)
library(shinycssloaders)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(rintrojs)
library(htmlwidgets)
library(dashboardthemes)

library(timevis)
#maynotwantthese 
library(mapedit)



#global environement variables~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plan(multisession)

corrdior_buffer = readOGR("corrdior_buffer",
                          "corrdior_buffer",
                          verbose = F) %>% st_as_sf()

#initializes custom funcitons~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
operation_manual = "https://github.com/michaelgaunt404/uhsgt_dashboard/blob/main/Operation_Manual.html"

inline = function (x) {
  tags$div(style="display:inline-block;", x)
}

purrr_read_ogr_corridor = function(dsnn, layerr, namee){
  readOGR(as.character(dsnn),
                 as.character(layerr),
                 verbose = F) %>%
    st_as_sf() 
}

#spatially reads layers rapidly
purrr_read_ogr_shapefile = function(dsnn, layerr, namee, selection = NA){
    tmp = readOGR(as.character(dsnn),
                  as.character(layerr),
                  verbose = F) %>% 
      st_as_sf() %>% 
      st_transform(crs = 4326) %>% 
      .[st_intersects(., corrdior_buffer, sparse = F)[,1],]
    
    tmp2 = selection %>%
      str_remove_all(" ") %>%
      str_split(",") %>%
      .[[1]]

    if(!is.na(selection[1])){
        tmp %>%
        select(all_of(tmp2))
    }
}

shapefile_writeout = function(data, names, location){
  names %>%
    map(function(x)
      paste0(location, "/", x) %>%
        unlink(recursive = T))
  
  names %>%
    map(function(x)
      paste0(location, "/", x) %>%
        dir.create())
  
  list(data,
       names) %>%
    pmap(function(x,y)
      st_write(x, paste0(location, "/", y, "/", y, ".shp")))
}

# purrr_read_ogr_shapefile = function(dsnn, layerr, namee){
#   readOGR(as.character(dsnn),
#                 as.character(layerr),
#                 verbose = F) %>%
#     st_as_sf() %>% 
#     st_transform(crs = 4326) %>% 
#     st_filter(corrdior_buffer)
# }

buffer_cleaner = function(geo_data, radius){
  geo_data %>% 
    st_geometry() %>% 
    st_transform(crs = 3488) %>% 
    st_buffer(dist = radius) %>% 
    st_union() %>% 
    st_transform(crs = 4326)
}

safe_purrr_read_ogr_corrdior = safely(purrr_read_ogr_corridor)
safe_purrr_read_ogr_shapefile = safely(purrr_read_ogr_shapefile)

shapefile_pair_detector = function(name){
  layer_names %>%  
    str_detect(name) %>% 
    sf_objects_list_pos_nonempty[.] %>%  
    map(clean_names)
}

shapefile_pair_collapser = function(geo_data_list){
  geo_data_list %>% 
    rbindlist() %>%  
    st_as_sf() 
}

shapefile_pair_collapser_WEIRD_POLY = function(geo_data_list){
  geo_data_list %>% 
    map(st_cast, "MULTIPOLYGON") %>% 
    rbindlist() %>%  
    st_as_sf() 
}

color_previewer = function(n){
  image(
    1:n, 1, as.matrix(1:n),
    col = viridis(n, option = "D"),
    xlab = "viridis n", ylab = "", xaxt = "n", yaxt = "n", bty = "n"
  )
}

data_extractor = function(data, name){
  filepath_tmp = paste0("C:/Users/USMG687637/Documents/040_projects/UHSR_jursidcition_map/output/", name, ".csv") 
  data %>%
    data.table() %>% 
    .[,-c("geometry")] %>% 
    fwrite(filepath_tmp)
}

quick_col_arrange = function(data) {
  data %>%
    select(-`Data Source`, `Data Source`, -geometry, geometry)
}

`%nin%` = Negate(`%in%`)

color = "#1e282d"
color_2 = "#222d32"

theme = dark_theme_gray() +
  theme(plot.background = element_rect(fill = color),
        legend.background = element_rect(fill= color, size=.5, linetype="dotted"),
        panel.background = element_rect(fill = color, colour = color), 
        axis.line = element_line(color = "black"))


# box_styling = ".box.box-solid.box-primary>.box-header {
# color:#fff;background:#0d1214}
# .box.box-solid.box-primary{
# border-bottom-color:#0d1214; border-left-color:#0d1214; 
# border-right-color:#0d1214;border-top-color:#0d1214;
# background:#0d1214}"

box_styling = ".box.box-solid.box-primary>.box-header {
color:#fff;background:#1e282d}
.box.box-solid.box-primary{
border-bottom-color:#1e282d; border-left-color:#1e282d; 
border-right-color:#1e282d;border-top-color:#1e282d;
background:#1e282d}"

simplify_shapefile = function(data, meters = 100){
  st_transform(2163) %>%
    st_simplify(dTolerance = meters) %>%
    st_transform(4326)
} 

base_map_options = function(map){
  map %>%  
    setView(-120.207, 46.223, zoom = 7) %>% 
    addMeasure(
      position = "bottomleft",
      activeColor = "#3D535D",
      completedColor = "#7D4479") %>%
    addMiniMap(
      tiles = providers$CartoDB.PositronEsri.WorldStreetMap,
      toggleDisplay = TRUE)
}


tab_mtrcs_tpbx_hght = 400
tab_mtrcs_plttab_hght = 400

data_table_fromat = '"function(settings, json) {",
"$(\'body\').css({\'font-family\': \'Calibri\'});",
"}"'

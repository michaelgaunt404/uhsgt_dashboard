# Define server logic to read selected file ----
server <- function(input, output) {
  #SECTION: Utility Setup======================================================================
  
  #timer for mapedit ping
  autoInvalidate <- reactiveTimer(2000)
  
  #this is used as an index in number of places below
  map_selectors = map_ready %>%
    select(group, processed_name) %>% 
    group_by(group) %>%  
    nest()
  
  #intro modal
  observeEvent("", {
    showModal(modalDialog(
      flipBox(
        id = 1,
        main_img = "trains.svg",
        header_img = "flatart_long.jpg",
        intro_modal_front,
        back_content = tagList(
          intro_modal_back
        )
      ),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$into_button, {
    showModal(modalDialog(
      flipBox(
        id = 1,
        main_img = "trains.svg",
        header_img = "flatart_long.jpg",
        intro_modal_front,
        back_content = tagList(
          intro_modal_back
        )
      ),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  #Map dashboard modal
  observeEvent(input$map_btn, {
    showModal(modalDialog(
      wellPanel(
        HTML(map_intro)),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  #Mapedit modal
  observeEvent(input$map_btn_info, {
    showModal(modalDialog(
      wellPanel(map_edit_tab_info),
      size = "l",
      easyClose = TRUE
    ))
  })

    #Mapedit input received
  observeEvent(input$map_btn_filter, {
    shinyalert(title = "Input Received!", type = "success",
               showConfirmButton = T, closeOnEsc = T,
               closeOnClickOutside = T)
  })
  
  #get leaflet shown features setup and observers
  listtt = 0
  show = "Regional Planning"
  layer_list = map_ready$processed_name %>%  str_replace_all("_", " ")
  hide_layer_list =  layer_list[layer_list %nin% show]
  hide_layer_list_edit =  layer_list[layer_list %nin% show]
  
  observe({
    req(input$`editor-map_groups`)
    tmp_list = input$`editor-map_groups`
    tmp_list = tmp_list[-c(1, length(tmp_list))] %>%  
      gsub(" - .*", "\\1", .) %>%  
      str_trim()
    
    hide_layer_list_edit <<-  layer_list[layer_list %nin% tmp_list]
  })
  
  observe({
    req(input$full_map_groups)
    tmp_list = input$full_map_groups
    
    hide_layer_list <<-  layer_list[layer_list %nin% tmp_list]
  })
  
  #SECTION: MAP_EDIT============================================================
  geom = NULL
  
  #us_census inputs=====
  index_census_select_columns = reactive({
    index_census_select_columns = census_selection_index %>%  
      filter(var_name != "Median Income Status") %>% 
      .[name %in% input$census_column_filter, var_name]
  })
  
  output$layer_color <- renderUI({
    awesomeRadio("census_color", 
                 "Census Layer Color Metric", 
                 selected = index_census_select_columns()[1],
                 choices = index_census_select_columns(), inline = T
    )
  })
  
  census_color = reactive({
    input$census_color
  })
  
  census_color_ca = reactive({
    input$census_color_ca
  })
  
  census_map_data_zcol = "Total Population (20-64yrs)"
  
  #creates new census map based on color input
  census_map = reactive({
    if(is.null(census_color())) {
      tmp_color = index_census_select_columns()[1]  
    } else {
      if (census_color() %in% index_census_select_columns()){
        tmp_color = census_color()
      } else {
        tmp_color = index_census_select_columns()[1] 
      }
    }
    
    #this perfroms very hard indexing operation 
    #issues arose with str_matching for the column indexes
    tmp_index = c("Tract", "County", "State", index_census_select_columns())
    tmp_map_ready = which(map_ready$processed_name %in% "US_Census")
    tmp_map_file = map_files_dfs[[tmp_map_ready]] 
    tmp_index = which(c("Tract", "County", "State", census_selection_index[name == "All", var_name]) %in% tmp_index)
    tmp_zcol = which(c("Tract", "County", "State", census_selection_index[name == "All", var_name]) %in% tmp_color) %>%  
      colnames(tmp_map_file)[.]
    census_map_data_zcol <<- tmp_zcol
    census_map_data <<- tmp_map_file %>%
      .[,tmp_index]
    
    tmp_map_file %>%
      .[,tmp_index] %>%  
      mapview(layer.name = paste0("US_Census - ", tmp_color) %>%
                str_replace_all("_", " "),
              zcol = tmp_zcol,
              legend = T,
              homebutton = F,
              alpha = .4,
              popup = popupTable(., zcol = -c(ncol(.)-1, ncol(.))))  
  })
  
  #creates front display plots based on click and zoom
  observe({
    req(census_color())
    
    tmp_click = input$full_map_shape_click
    # tmp_bounds = bbox_reactive_d()
    
    centileBreaks <- hist(plot = F, notshared_census %>%
                            select(census_color()) %>%
                            .[[1]], breaks = 20)$breaks
    
    if (is.null(tmp_click)) {
      tmp_vert = notshared_census %>% 
        st_set_geometry(NULL) %>% 
        select(census_color()) %>% 
        .[[1]] %>%  
        mean(na.rm = T)
    } else {
      tmp_vert = st_sfc(st_point(c(tmp_click$lng, tmp_click$lat)), crs = 4326) %>%
        st_filter(notshared_census, .) %>%
        select(census_color()) %>%
        .[[1]]
    }

    output$map_var_plot = renderPlot({
      notshared_census %>%
        st_set_geometry(NULL) %>%
        select(census_color()) %>%
        set_names("data") %>%
        ggplot(aes(x = data)) +
        geom_histogram(color = "black", bins = 20) +
        geom_vline(xintercept = tmp_vert) +
        theme +
        xlim(c(min(centileBreaks), max(centileBreaks))) +
        labs(y = "Count", title = "Entire Corridor")
    })
    
  })
  
  #us_census inputs=====
  ca_index_census_select_columns = reactive({
    ca_index_census_select_columns = ca_census_selection_index[name %in% input$ca_census_column_filter, var_name] %>%  
      str_to_title()
  })

    output$layer_color_ca <- renderUI({
    awesomeRadio("census_color_ca", 
                 "Census Layer Color Metric", 
                 selected = ca_index_census_select_columns()[1],
                 choices = ca_index_census_select_columns(), inline = T
    )
  })
  
  census_map_data_zcol = "Population (2016)"
  
  #creates new census map based on color input
  census_map_ca = reactive({
    if(is.null(census_color_ca())) {
      tmp_color = ca_index_census_select_columns()[1]  
    } else {
      if (census_color_ca() %in% ca_index_census_select_columns()){
        tmp_color = census_color_ca()
      } else {
        tmp_color = ca_index_census_select_columns()[1] 
      }
    }
    
    #this perfroms very hard indexing operation 
    #issues arose with str_matching for the column indexes
    tmp_index = c("Name", "Area",  ca_index_census_select_columns())
    tmp_map_ready = which(map_ready$processed_name %in% "CA_Census")
    tmp_map_file = map_files_dfs[[tmp_map_ready]] 
    tmp_index = which(c("Name", "Area", ca_census_selection_index[name == "All", var_name]) %in% tmp_index)
    tmp_zcol = which(c("Name", "Area", ca_census_selection_index[name == "All", var_name]) %in% tmp_color) %>%  
      colnames(tmp_map_file)[.]
    census_map_data_zcol_ca <<- tmp_zcol
    census_map_data_ca <<- tmp_map_file %>%
      .[,tmp_index]
    
    tmp_map_file %>%
      .[,tmp_index] %>%  
      mapview(layer.name = paste0("CA_Census - ", tmp_color) %>%
                str_replace_all("_", " "),
              zcol = tmp_zcol,
              legend = T,
              homebutton = F,
              alpha = .4,
              popup = popupTable(., zcol = -c(ncol(.)-1, ncol(.))))  
  })
  
  
  # bbox_reactive <- reactive({
  #   input$full_map_bounds
  # })
  
  # bbox_reactive_d <- bbox_reactive %>% debounce(2000)
  
  #creates front display plots based on click and zoom
  observe({
    req(census_color_ca())
    tmp_click = input$full_map_shape_click
    # tmp_bounds = bbox_reactive_d()
    
    centileBreaks <- hist(plot = F, notshared_census_ca %>%
                            select(census_color_ca()) %>%
                            .[[1]], breaks = 20)$breaks
    
    if (is.null(tmp_click)) {
      tmp_vert = notshared_census_ca %>% 
        st_set_geometry(NULL) %>% 
        select(census_color_ca()) %>% 
        .[[1]] %>%  
        mean(na.rm = T)
    } else {
      tmp_vert = st_sfc(st_point(c(tmp_click$lng, tmp_click$lat)), crs = 4326) %>%
        st_filter(notshared_census_ca, .) %>%
        select(census_color_ca()) %>%
        .[[1]]
    }
    
    output$map_var_plot_ca = renderPlot({
      notshared_census_ca %>%
        st_set_geometry(NULL) %>%
        select(census_color_ca()) %>%
        set_names("data") %>%
        ggplot(aes(x = data)) +
        geom_histogram(color = "black", bins = 20) +
        geom_vline(xintercept = tmp_vert) +
        theme +
        xlim(c(min(centileBreaks), max(centileBreaks))) +
        labs(y = "Count", title = "Entire Corridor")
    })
    
  })
  
  #map_object creation====
  # census_map_data = which(map_ready$processed_name %in% "US_Census") %>% 
  #   map_files_dfs[[.]]
  
  # {
  #   
  # 
  #   # leaflet() %>% 
  #   #   addTiles() %>% 
  #   #   addPolygons(data = bobo, 
  #   #               # color = ~pal(tmp),
  #   #               popup = ~label, 
  #   #               group = "census") %>% 
  #   #   addLayersControl(
  #   #     position = "topleft",
  #   #     baseGroups = c('CartoDB.Positron', 'CartoDB.DarkMatter', 'OpenStreetMap', 'Esri.WorldImagery', "OpenTopoMap"),
  #   #     overlayGroups = c("heads", "tails", "census"))
  #   
  # }
  
  # ca_census_map_data = which(map_ready$processed_name %in% "US_Census") %>% 
  #   map_files_dfs[[.]]
  
  #makes the reactive map
  # reactive_mappp = reactive({
  #   map_files_used = map_ready
  #   tmp = which(map_ready$processed_name %in% "US_Census")
  #   map_files[[tmp]] = census_map()
  #   
  #   tmp = which(map_ready$processed_name %in% "CA_Census")
  #   map_files[[tmp]] = census_map_ca()
  #   
  #   map_to_be_shown = map_files %>%
  #     reduce(`+`)
  #   
  #   map_to_be_shown@map
  # })
  
  #takes reactive map and makes main default map
  # output$full_map = renderLeaflet({
  #   req(census_color())
  #   reactive_mappp() %>% 
  #     hideGroup(hide_layer_list) %>% 
  #     base_map_options()
  # })
  # 
  #test_area============
  output$full_map = renderLeaflet({
    map_to_be_shown = map_files[-c(tmp_ca, tmp_us)] %>%
      reduce(`+`) 
    
    map_to_be_shown@map %>% 
      hideGroup(hide_layer_list) %>% 
      base_map_options()
  })
  
  
  observe({
    req(input$census_color)
    req(input$census_color_ca)
    
    tmp_col = which(census_selection_index[name != "All", var_name] %in% input$census_color) + 3
    tmp_us_index = notshared_census_lfpopup[,tmp_col] %>%  
      st_set_geometry(NULL) %>% 
      .[[1]]
    pal = colorNumeric(palette = viridis(10),
                       domain = tmp_us_index)
    
    tmp_col_ca = which(ca_census_selection_index[name != "All", var_name] %>%  str_to_title() %in% input$census_color_ca) + 2
    tmp_ca_index = notshared_census_ca_lfpopup[,tmp_col_ca] %>%  
      st_set_geometry(NULL) %>% 
      .[[1]]
    pal_ca = colorNumeric(palette = viridis(10),
                       domain = tmp_ca_index)
    
    leafletProxy("full_map") %>%
      clearControls() %>%
      addPolygons(data = notshared_census_lfpopup, fillColor = ~pal(tmp_us_index),
                  color = "black",
                  opacity = 1,
                  fillOpacity = .5,
                  weight = 1, 
                  label = tmp_us_index,
                  popup = ~notshared_census_lfpopup$label, 
                  group = paste0("US Census: ", input$census_color)) %>%
      addLegend(position = "topright",
                pal = pal, tmp_us_index, title = paste0("US Census: ", input$census_color)) %>% 
      addPolygons(data = notshared_census_ca_lfpopup, fillColor = ~pal_ca(tmp_ca_index),
                  color = "black",
                  opacity = 1,
                  fillOpacity = .5,
                  weight = 1, 
                  label = tmp_ca_index,
                  popup = ~notshared_census_ca_lfpopup$label, 
                  group = paste0("CA Census: ", input$census_color_ca)) %>%
      addLegend(position = "topright",
                pal = pal_ca, tmp_ca_index, title = paste0("CA Census: ", input$census_color_ca)) %>% 
      addLayersControl(
              position = "topleft",
              baseGroups = c('CartoDB.Positron', 'CartoDB.DarkMatter', 'OpenStreetMap', 'Esri.WorldImagery', "OpenTopoMap"),
              overlayGroups = c(paste0("US Census: ", input$census_color), 
                                paste0("CA Census: ", input$census_color_ca),
                                map_ready$processed_name[-c(tmp_ca, tmp_us)] %>%  
                                  str_replace_all("_", " ")) %>% 
                str_sort())
  })

  #makes mapedit map
  edits <- callModule(editMod, "editor", 
                      isolate(map_files %>%
                                reduce(`+`) %>%  
                                .@map %>% 
                                hideGroup(hide_layer_list) %>% 
                                base_map_options()))
  
  #ping_mapedit====
  #this observe pings if edits() have been made to map
  #prints map as well on save button
  observe({
    autoInvalidate()
    
    if(!identical(geom, edits()$finished)){
      geom <<- edits()$finished 
        
      observeEvent(input$map_btn_filter, {
        if (!is.null(geom)) {
          
          # assign('tmp_shapefile', geom, envir = .GlobalEnv)
          # st_write(geom, './mapedit_tmp/tmp_shapefile.shp', 
          #          delete_layer = TRUE, delete_dsn = TRUE)
  
          # tmp_buffer = read_sf('./mapedit_tmp/tmp_shapefile.shp') %>%
          #     buffer_cleaner((input$rad_input)*1609.34)
          
          tmp_buffer = geom %>%
            buffer_cleaner((input$rad_input)*1609.34)
          
          #us_census_subset_calculations=========
          data_for_plot = notshared_census %>%  
            st_filter(., tmp_buffer) %>% 
            data.frame() %>% 
            set_names(c(colnames(notshared_census))) %>% 
            select(!all_of(index_clmn_rmv[-length(index_clmn_rmv)])) %>%  
            data.table()  %>%  
            melt(., 
                 id.vars = c("Tract", "County", "State"), 
                 measure.vars = index_numeric_columns_gg) %>% 
            mutate(type = "Subset") %>% 
            bind_rows(notshared_gg_tmp,
                      .) %>%  
            data.table() %>% 
            .[,`:=`(variable_raw = variable %>% 
                      str_remove_all("[^a-zA-Z0-9]"))]
          
          output$census_aggregate_subset = renderDataTable({
            data_tables = data_for_plot %>%
              data.table() %>% 
              .[,.(mean = mean(value, na.rm = T) %>%  round(2),
                   median = median(value, na.rm = T)%>%  round(2),
                   min = min(value, na.rm = T)%>%  round(2),
                   max = max(value, na.rm = T)%>%  round(2),
                   sd = sd(value, na.rm = T)%>%  round(2)), by = .(variable, type)] %>% 
              .[,`:=`(variable = fct_inorder(variable))]
            
            merge(data_tables %>% 
                                  filter(type == "Subset"), 
                                data_tables %>%  
                                  filter(type != "Subset"), 
                                by = "variable", 
                                suffixes = c(".sub", ".cor")
            )  %>%  
              arrange(variable) %>%  
              select(!contains("type")) %>% 
              datatable(escape = F, 
                        fillContainer = T,
                        extensions = c('Buttons'),
                        options = list(
                          scrollY = 700, scrollX = T,
                          pageLength = 900, dom = 'Brt',
                          initComplete = JS(
                            "function(settings, json) {",
                            "$('body').css({'font-family': 'Calibri'});",
                            "}"),
                          buttons = list("csv", "excel", "copy")
                        ))
          })
          
          output$census_subset = renderDataTable({
            notshared_census %>%  
              st_filter(., tmp_buffer) %>% 
              data.frame() %>% 
              set_names(c(colnames(notshared_census))) %>% 
              select(!all_of(index_clmn_rmv[-length(index_clmn_rmv)])) %>%  
              data.table()  %>% 
              datatable(escape = F, 
                        fillContainer = T,
                        extensions = c('Buttons', "FixedColumns"),
                        options = list(
                          scrollY = 650, scrollX = 800,
                          pageLength = 900, dom = 'Brt',
                          fixedColumns = T,
                          initComplete = JS(
                            "function(settings, json) {",
                            "$('body').css({'font-family': 'Calibri'});",
                            "}"),
                          buttons = list("csv", "excel", "copy")
                        ))
          })
          
          output$smmry_brplts <- renderPlotly({
            smmry_brplts = data_for_plot %>%
              merge.data.table(., 
                               census_selection_index[name != "All", .(name, var_name)] %>% 
                                 mutate(variable_raw = var_name %>% 
                                          str_remove_all("[^a-zA-Z0-9]")), 
                               by = "variable_raw") %>% 
              rename(var_type = name) %>% 
              select(variable, type, var_type, value) %>% 
              data.table() %>% 
              .[,.(mean = mean(value, na.rm = T),
                   median = median(value, na.rm = T),
                   min = min(value, na.rm = T)%>%  round(2),
                   max = max(value, na.rm = T)%>%  round(2),
                   sd = sd(value, na.rm = T)), by = .(variable, type, var_type)] %>%
              pivot_longer(cols = !c(variable, type, var_type)) %>%
              pivot_wider(names_from = type, values_from = value) %>%
              mutate(Error = 100*(Subset-Corridor)/Corridor,
                     variable = fct_relevel(variable, 
                                            census_selection_index[name != "All", var_name]),
                     text = str_glue("{variable} 
                                     \n Corridor: {round(Corridor,2)} 
                                     \n Subset: {round(Subset, 2)}
                                     \n Deviation: {round(Error, 2)}%")) %>% 
              arrange(variable) %>%  
              mutate(names_smll = variable %>%  
                       gsub("\\(.*","\\1", .) %>%  
                       str_trim() %>% 
                       str_replace_all("000 ", "K ") %>% 
                       ifelse(str_count(.)>18, 
                              abbreviate(., minlength = 10) %>%  
                                as.character(), 
                              .) %>%  
                       fct_inorder()) %>% 
              filter(name == input$smmry_brplot_slct) %>%
              ggplot() +
              geom_col(aes(x = names_smll, y = Error, fill = Error>0, text = text) ) + 
              facet_wrap(~var_type, scales = "free_x") +
              labs(y = "Percent Difference", x = "") +
              theme +
              theme(legend.position='none',
                    axis.text.x = element_text(angle = 45, hjust = 1)) 
            
            ggplotly(smmry_brplts, tooltip = c("text")) %>%  
              hide_legend()
          })

          output$smmry_bxplts = renderPlotly({
            req(input$table_select_rows_selected)
            
            index_plt_clmns = census_selection_index[, var_name] %>%
                  str_remove_all("[^a-zA-Z0-9]") %>%  
              .[input$table_select_rows_selected]
              
            smmry_bxplts = data_for_plot %>% 
              .[variable_raw %in% index_plt_clmns,] %>%
              # .[variable %in% table_select_rows_selected,] %>% 
              ggplot() + 
              geom_boxplot(aes(x = as.factor(type), 
                               y = value, 
                               fill = as.factor(variable)), 
                           width = 1, color = "black") +      
              facet_wrap(~variable, #nrow = 2, 
                         scales = "free_y") +
              # scale_x_discrete(expand=c(0.8,0)) +
              labs(y = "", x = "") +
              theme +
              theme(legend.position='none') 
            
            ggplotly(smmry_bxplts, tooltip = c("text")) %>%  
              hide_legend()
          })
         
          output$leaflet_map_edit =  renderLeaflet({
            tmp = which(map_ready$processed_name %in% "US_Census")
            data = map_files_dfs
            map_ready_fltrd = map_ready
            data[[tmp]] = census_map_data
            map_ready_fltrd[tmp, "zcol"] = census_map_data_zcol
            map_files_dfs_fltrd = data %>%
              map(st_filter, tmp_buffer)

            index_bffr_fltrd_map = map_files_dfs_fltrd %>%  map( function(x) x %>%
                                                                   st_dimension() %>%
                                                                   length()) %>%
              unlist() %>%
              data.table(empty_list = .) %>%
              .[,`:=`(rwnms = rownames(.) %>%  as.numeric())] %>%
              .[which(empty_list == 0),rwnms]

            map_files_dfs_fltrd = map_files_dfs_fltrd[-index_bffr_fltrd_map]
            map_ready_fltrd = map_ready_fltrd[-index_bffr_fltrd_map,]

            map_edit_maps = list(map_files_dfs_fltrd,
                                 map_ready_fltrd$zcol %>%  str_to_title(),
                                 map_ready_fltrd$processed_name) %>%
              pmap(function(x, y, m) x %>%
                     mapview(
                       layer.name = m %>%
                         str_replace_all("_", " "),
                       zcol = y,
                       legend = F,
                       homebutton = F,
                       popup = popupTable(., zcol = -c(ncol(.)-1, ncol(.)))
                     ))

            map_edit_map = map_edit_maps  %>%
              reduce(`+`)

            map_edit_map@map %>%
              hideGroup(hide_layer_list_edit) %>%
              base_map_options()
          })

          #ca_census_subset_calculations=========
          data_for_plot_ca = notshared_census_ca %>%
            st_filter(., tmp_buffer) %>%
            data.frame() %>%
            set_names(c(colnames(notshared_census_ca))) %>%
            select(!all_of(index_clmn_rmv[-length(index_clmn_rmv)])) %>%
            data.table()  %>%
            melt(.,
                 id.vars = c("Name"),
                 measure.vars = index_numeric_columns_gg_ca) %>%
            mutate(type = "Subset") %>%
            bind_rows(notshared_gg_tmp_ca,
                      .) %>%
            data.table() %>%
            .[,`:=`(variable_raw = variable %>%
                      str_remove_all("[^a-zA-Z0-9]"))]

          output$census_aggregate_subset_ca = renderDataTable({
            data_tables = data_for_plot_ca %>%
              data.table() %>%
              .[,.(mean = mean(value, na.rm = T) %>%  round(2),
                   median = median(value, na.rm = T)%>%  round(2),
                   min = min(value, na.rm = T)%>%  round(2),
                   max = max(value, na.rm = T)%>%  round(2),
                   sd = sd(value, na.rm = T)%>%  round(2)), by = .(variable, type)] %>%
              .[,`:=`(variable = fct_inorder(variable))]

            merge(data_tables %>%
                    filter(type == "Subset"),
                  data_tables %>%
                    filter(type != "Subset"),
                  by = "variable",
                  suffixes = c(".sub", ".cor")
            )  %>%
              arrange(variable) %>%
              select(!contains("type")) %>%
              datatable(escape = F,
                        fillContainer = T,
                        extensions = c('Buttons'),
                        options = list(
                          scrollY = 700, scrollX = T,
                          pageLength = 900, dom = 'Brt',
                          initComplete = JS(
                            "function(settings, json) {",
                            "$('body').css({'font-family': 'Calibri'});",
                            "}"),
                          buttons = list("csv", "excel", "copy")
                        ))
          })

          output$census_subset_ca = renderDataTable({
            notshared_census_ca %>%
              st_filter(., tmp_buffer) %>%
              data.frame() %>%
              set_names(c(colnames(notshared_census_ca))) %>%
              select(!all_of(index_clmn_rmv[-length(index_clmn_rmv)])) %>%
              data.table()  %>%
              datatable(escape = F,
                        fillContainer = T,
                        extensions = c('Buttons', "FixedColumns"),
                        options = list(
                          scrollY = 650, scrollX = 800,
                          pageLength = 900, dom = 'Brt',
                          fixedColumns = T,
                          initComplete = JS(
                            "function(settings, json) {",
                            "$('body').css({'font-family': 'Calibri'});",
                            "}"),
                          buttons = list("csv", "excel", "copy")
                        ))
          })

          output$smmry_brplts_ca <- renderPlotly({
            smmry_brplts = data_for_plot_ca %>% 
              merge.data.table(.,
                               ca_census_selection_index[name != "All", .(name, var_name)] %>%
                                 mutate(var_name = var_name %>%  str_to_title(),
                                        variable_raw = var_name %>%
                                          str_remove_all("[^a-zA-Z0-9]")),
                               by.x = "variable", 
                               by.y = "var_name") %>%
              rename(var_type = name) %>%
              select(variable, type, var_type, value) %>%
              data.table() %>%
              .[,.(mean = mean(value, na.rm = T),
                   median = median(value, na.rm = T),
                   min = min(value, na.rm = T)%>%  round(2),
                   max = max(value, na.rm = T)%>%  round(2),
                   sd = sd(value, na.rm = T)), by = .(variable, type, var_type)] %>%
              pivot_longer(cols = !c(variable, type, var_type)) %>%
              pivot_wider(names_from = type, values_from = value) %>%
              mutate(Error = 100*(Subset-Corridor)/Corridor,
                     variable = fct_relevel(variable,
                                            ca_census_selection_index[name != "All", var_name]),
                     text = str_glue("{variable}
                                     \n Corridor: {round(Corridor,2)}
                                     \n Subset: {round(Subset, 2)}
                                     \n Deviation: {round(Error, 2)}%")) %>%
              arrange(variable) %>%
              mutate(names_smll = variable %>%
                       gsub("\\(.*","\\1", .) %>%
                       str_trim() %>%
                       str_replace_all("000 ", "K ") %>%
                       ifelse(str_count(.)>18,
                              abbreviate(., minlength = 10) %>%
                                as.character(),
                              .) %>%
                       fct_inorder()) %>%
              filter(name == input$smmry_brplot_slct) %>%
              ggplot() +
              geom_col(aes(x = names_smll, y = Error, fill = Error>0, text = text) ) +
              facet_grid(.~var_type, scales = "free_x", space='free_x') +
              labs(y = "Percent Difference", x = "") +
              theme +
              theme(legend.position='none',
                    axis.text.x = element_text(angle = 45, hjust = 1))

            ggplotly(smmry_brplts, tooltip = c("text")) %>%
              hide_legend()
          })

          output$smmry_bxplts_ca = renderPlotly({
            req(input$table_select_ca_rows_selected)

            index_plt_clmns = ca_census_selection_index[, var_name] %>%
              unique() %>% 
              str_to_title() %>% 
              .[input$table_select_ca_rows_selected]

            smmry_bxplts = data_for_plot_ca %>%
              # .[variable_raw %in% index_plt_clmns,] %>%
              .[variable %in% index_plt_clmns,] %>%
              ggplot() +
              geom_boxplot(aes(x = as.factor(type),
                               y = value,
                               fill = as.factor(variable)),
                           width = 1, color = "black") +
              facet_wrap(~variable, #nrow = 2,
                         scales = "free_y") +
              # scale_x_discrete(expand=c(0.8,0)) +
              labs(y = "", x = "") +
              theme +
              theme(legend.position='none')

            ggplotly(smmry_bxplts, tooltip = c("text")) %>%
              hide_legend()
          })

          #user_shapfile_zip _and_write=========
          geom = geom %>%  
            mutate(feature_type = feature_type %>%  
                     str_replace("rectangle", "polygon") %>% 
                     str_remove("circle"))
          
          if ((geom$feature_type %>% unique() %>%  length()) > 1) {
            tmp_data_sf_collection = data.frame(feature_types = c("POLYGON", "POINT", "LINESTRING"), 
                                                stringsAsFactors = F) %>%  
              group_by(feature_types) %>%  
              nest() %>%  
              mutate(data = map(feature_types, function(x) x %>%  
                                  st_collection_extract(geom, .)),
                     nrow = map(data, nrow)
              ) %>%  
              unnest(cols = nrow) %>%  
              filter(nrow > 0) 
            
            unlink("./mapedit_tmp/*")
            
            list(tmp_data_sf_collection$feature_types, 
                 tmp_data_sf_collection[["data"]]) %>%  
              pmap(function(x, y) y %>%  
                     st_write(str_glue('./mapedit_tmp/usr_geometry_{format(Sys.time(), "%y%m%d_%H%M")}_{x}.shp')))
          } else {
            unlink("./mapedit_tmp/*")
            
            geom %>%  
              st_write(str_glue('./mapedit_tmp/usr_geometry_{format(Sys.time(), "%y%m%d_%H%M")}.shp'))
          }
          
          output$download_shapefile <- downloadHandler(
            filename = function() {
              paste0("usrftr_", Sys.time(), ".tar") %>%
                str_remove_all(":") %>%
                str_remove_all("-") %>%
                str_replace_all(" ", "_")
            },
            content <- function(file) {
              tar(file, "mapedit_tmp")
            }
          )
        }
      })
    }
  })

  
  #SECTION: MPO_OBJECTS=========================================================
  #mpo_summary_table=====
  output$mpo_table_with_census = renderDataTable({
    mpo_census_merge_df %>%
      set_names(str_trunc(names(.), 20, "right")) %>% 
      unique() %>%  
      datatable(
        escape = F,
        selection = "none",
        extensions = c('Buttons', "FixedColumns"), 
        options = list(
          scrollY = 370, pageLength = 900, fixedColumns = TRUE,
          dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'),
          initComplete = JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Calibri'});",
            "}")
        )
      ) 
  })
  
  #mpo_upcoming_publish_table=====
  output$mpo_df_alt_soonest_10 = renderDataTable({
    mpo_df_alt_soonest_10 %>%
      datatable(
        escape = F,
        selection = "single",
        options = list( 
          pageLength = 20,
          initComplete = JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Calibri'});",
            "}")
        )
      )
  })
  
  #mpo_tracking_plot=====
  output$mpo_plot_status = renderPlot({
    mpo_df %>% 
      filter(Type != "LRP") %>% 
      ggplot() + 
      geom_tile(aes(Type, Acronym, fill = `Document\nStatus:`), color = "black") + 
      labs(x = "", y = "", fill = "Document Status:") + 
      scale_fill_viridis_d() +
      theme +
      theme(legend.position="bottom")
  })
  
  #mpo_timeline_plot=====
  output$mpo_plot_timeline = renderTimevis({
    mpo_timeline = mpo_df %>%
      mutate(id = as.numeric(rownames(.)), 
             end = NA, 
             Date = Date + months(48),
             qtr = Date %>%  floor_date("quarter"),
             group = Type,
             start = Date,
             content = paste(Acronym, Type, sep = "-"),
             order = 1,
             text = str_glue("{content} {Date}")) %>% 
      arrange(qtr, content) %>%
      group_by(qtr) %>%
      mutate(order = cumsum(order)) %>%
      filter(Type != "LRP") %>% 
      filter(!is.na(Type)) %>%  
      filter(!is.na(start)) %>%  
      select(end, qtr, group, content, order, text, start)
    
    groups = unique(mpo_timeline$group) %>%  
      data.frame(id = ., 
                 content = .)
    mpo_timeline %>%
      timevis(., groups, height = 500)
  })

  output$layer_select <- renderUI({
    lapply(map_selectors$group, function(x) {
      boxPlus(title = paste(x), width = "100%",
              closable = F, collapsible = T, collapsed = T, 
              status = "primary", solidHeader = T,
              list(prettyCheckboxGroup(paste0("dynamic", x), label = NA,
                                       choices =  map_selectors[which(map_selectors$group %in% x),2] %>%
                                         unnest(cols = data) %>% 
                                         data.table() %>% 
                                         .[, processed_name], 
                                       animation = "pulse",
                                       shape = "round",
                                       status = "danger",
                                       fill = TRUE,
                                       inline = T)))
    })
  })
  
  #SECTION: MISC OBJECTS========================================================
  output$data_overview = renderDataTable({
    map_ready %>%
      select(processed_name, group, notes, src_url) %>%
      set_names(c("Layer", "Layer Group", "Notes", "Source URL")) %>%
      datatable(
        escape = F,
        fillContainer = T,
        selection = "single",
        extensions = 'Buttons', 
        options = list(
          scrollY = T, pageLength = 900,
          dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'),
          initComplete = JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Calibri'});",
            "}")
        )
      )

  })

  output$data_select = renderDataTable({
    req(input$data_overview_rows_selected)
    map_files_dfs[[input$data_overview_rows_selected]] %>%  
      data.table() %>% 
      .[, -c("Plain", "Geometry")] %>%
      datatable(
        escape = F,
        selection = "single",
        extensions = 'Buttons', 
        options = list(
          scrollY = 700, pageLength = 900,
          dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'),
          initComplete = JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Calibri'});",
            "}")
        )
      )
  })
  
  output$table_select = renderDataTable({
    census_selection_index[, var_name] %>%  
      unique() %>%  
      data.table(`Box Plot Metric Select` = .) %>%  
      datatable(
        escape = F,
        fillContainer = T,
        options = list(
          searching = "FALSE",
          scrollY = 350, pageLength = 900,
          dom = 't', 
          initComplete = JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Calibri'});",
            "}")
        )
      )
  })
  
  output$table_select_ca = renderDataTable({
    ca_census_selection_index[, var_name] %>%  
      unique() %>%  
      data.table(`Box Plot Metric Select` = .) %>%  
      datatable(
        escape = F,
        fillContainer = T,
        options = list(
          searching = "FALSE",
          scrollY = 350, pageLength = 900,
          dom = 't', 
          initComplete = JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Calibri'});",
            "}")
        )
      )
  })
  
}




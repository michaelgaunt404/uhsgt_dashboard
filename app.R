source("global.R")
source("./R/data_mapper.R")
source("./R/data_scripts.R")

# Define UI for data upload app ----

ui = dashboardPagePlus(
  useShinyalert(),
  #Header==============================================================================================================================================================================
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  header = dashboardHeaderPlus(
    title = tagList(
      span(class = "logo-lg", "UHSGT Map Dashboard"), 
      img(src = "trains.svg")),
    fixed = TRUE,
    enable_rightsidebar = F
  ),
  
  #Sidebar==============================================================================================================================================================================
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  sidebar = dashboardSidebar(width  = 200,
                             sidebarMenu(id = "tabs",
                                         
                                         tags$style(HTML(box_styling)),
                                         # menuItem("Map Dashboard", tabName = "map_dahsboard", icon = icon("map"), 
                                         #          startExpanded = F),
                                         menuItem("Map Dashboard", icon = icon("map"),
                                                  menuSubItem("Base Map", tabName = "map_dahsboard"),
                                                  menuSubItem("Filtered Map", tabName = "filtered_map")
                                         ),
                                         # menuItem("Census Metrics", tabName = "metrics", icon = icon("calculator")),
                                         menuItem("Regional Planning", tabName = "mpo", icon = icon("calculator")),
                                         menuItem("Data Center", tabName = "data_center", icon = icon("table")),
                                         menuItem("Help Center", tabName = "help_center", icon = icon("table"),
                                                  actionButton(inputId = "into_button", label = "Show Into Msg",
                                                             icon("info")
                                                  ),
                                                  actionButton(inputId = "learn_more", label = "Operation Manual",
                                                             icon("th"), 
                                                             onclick = "window.open('https://github.com/michaelgaunt404/uhsgt_dashboard/blob/main/Operation_Manual.pdf', '_blank')"
                                                  ),
                                                  actionButton(inputId = "repo", label = "To Project Repo",
                                                               icon("github"), 
                                                               onclick = "window.open('https://github.com/michaelgaunt404/uhsgt_dashboard/', '_blank')"
                                                  )
                                         )
                             )
  ),
  #Body=================================================================================================================================================================================
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  body = dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tags$head(tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 90;

        $("#map_container").height(boxHeight);
        $("#map").height(boxHeight - 20);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
    tags$style(type = "text/css", "#full_map {height: calc(100vh - 90px) !important;}"),
    tags$style(type = "text/css", "#data_overview {height: calc(100vh - 90px) !important;}"),
    tabItems(
      #TAB: MAP_DB====================================================================
      tabItem("map_dahsboard",
              setShadow(class = "dropdown-menu"),
              fluidRow(box(height = 20)),
              fluidRow(
                column(width = 9,
                       box(width = "100%",
                           id = "map_container",
                           leafletOutput("full_map") %>%  withSpinner(size = 3))
                ),
                column(width = 3,
                       tabBox(width = "100%",
                              tabPanel("Mapping Options",
                                       actionButton("map_btn","Base Map Page Info", icon("info")),
                                       br(),
                                       # ),
                                       boxPlus(title = "US Census Filters", width = "100%",
                                               closable = F, collapsible = T, collapsed = F, 
                                               status = "primary", solidHeader = T,
                                               awesomeRadio("census_column_filter", label = "Filter Census Data (by group)",
                                                            choices =  unique(census_selection_index$name), 
                                                            selected = "All", inline = T
                                               ),
                                               boxPlus(width = "100%", title = "Color Select",
                                                       closable = F, collapsible = T, collapsed = F, 
                                                       status = "primary", solidHeader = T,
                                                       uiOutput("layer_color")), 
                                               boxPlus(width = "100%", title = "Metric Plots",
                                                       closable = F, collapsible = T, collapsed = F, 
                                                       status = "primary", solidHeader = T,
                                                       plotOutput("map_var_plot", height = 150) %>%  withSpinner(),
                                                       br(),
                                                       plotOutput("map_var_plot_sub", height = 150) %>%  withSpinner())
                                       )
                              )
                       )
                )
              )
      ),
      #TAB: FILTERED_MAP========================================================
      tabItem("filtered_map", 
              setShadow(class = "dropdown-menu"),
              fluidRow(box(height = 20)),
              fluidRow(
                column(width = 2, actionButton("map_btn_info","Map Editing Info", icon("question-circle"), block = T),
                       actionButton("map_btn_filter", "Filter Default Map", icon("filter")),
                       downloadButton("download_shapefile", "Download User Geometry")
                ),
                # column(width = 1, actionButton("save", "Filter Map", icon("filter"))),
                column(width = 2, sliderInput('rad_input', "Filter Radius (miles)", min = 1, max = 15, value = 2)),
                # column(width = 2, selectInput("ggplt_slct", label = "Select Plot Filter",
                #                               choices = unique(census_selection_index$name),
                #                               selected = unique(census_selection_index$name)[1])),
                column(width = 2, selectInput("smmry_brplot_slct", label = "Select Percnet Difference Plot Metric",
                                              choices = c("sd", "mean", "median", "min", "max"),
                                              selected = "mean"))
              ),
              fluidRow(
                tabBox(
                  width = "100%", height = 920,
                  tabPanel("Spatially Filtered Map", 
                           column(width = 6,
                                  editModUI("editor", height = 850) %>%  
                                    withSpinner(size = 3)),
                           column(width = 6,
                                  leafletOutput("leaflet_map_edit", height = 850) %>%  withSpinner(size = 3))
                  ),
                  tabPanel("Metric Plots",
                           box(width = "100%",
                               fluidRow(
                                 column(width = 12,
                                        box(width = "100%",
                                            height = tab_mtrcs_tpbx_hght,
                                            plotlyOutput("smmry_brplts", height = (tab_mtrcs_tpbx_hght)
                                            ) %>%  withSpinner()
                                        )
                                 )
                               ),
                               fluidRow(
                                 column(width = 2, 
                                        dataTableOutput("table_select") %>% withSpinner()
                                 ),
                                 column(width = 10,
                                        box(width = "100%",
                                            height = tab_mtrcs_tpbx_hght,
                                            plotlyOutput("smmry_bxplts", height = (tab_mtrcs_tpbx_hght)
                                            ) %>%  withSpinner()
                                        )
                                 )
                               ))
                           
                           
                           # plotlyOutput("smmry_brplts", height = (tab_mtrcs_tpbx_hght-30)) %>%  withSpinner(),
                           # plotlyOutput("smmry_bxplts", height = (tab_mtrcs_tpbx_hght-30)) %>%  withSpinner()
                           
                           # tabBox(
                           #   width = "100%", 
                           #   tabPanel("Percent Difference Plot",
                           #            plotlyOutput("smmry_brplts", height = 750 #(tab_mtrcs_tpbx_hght-30)
                           #                         ) %>%  withSpinner()),
                           #   tabPanel("Box Plots",
                           #            column(width = 3, 
                           #                   dataTableOutput("table_select") %>% withSpinner()
                           #            ),
                           #            column(width = 9,
                           #                   plotlyOutput("smmry_bxplts", height = 750 #(tab_mtrcs_tpbx_hght-30)
                           #                                ) %>%  withSpinner())
                           #   )
                           #   
                           # )
                           
                  ),
                  tabPanel("Metric Tabular Data", 
                           dataTableOutput("census_aggregate_subset") %>%  withSpinner(size = 3)),
                  tabPanel("Census Tract Subset", 
                           dataTableOutput("census_subset") %>%  withSpinner(size = 3))
                )
              )    
      ),
      #TAB: Regional Planning===================================================================
      tabItem("mpo",
              setShadow(class = "dropdown-menu"),
              fluidRow(box(height = 20)),
              # fluidRow(
              boxPlus(width = "100%", height = 600, title = "Regional Planning Statistics",
                      closable = F, collapsible = F, collapsed = F, 
                      status = "primary", solidHeader = T,
                      enable_sidebar = T, sidebar_content = mpo_drop_down,
                      box(width = "95%",
                          dataTableOutput("mpo_table_with_census") %>%  withSpinner())),
              # ),
              fluidRow(
                column(width = 4,
                       boxPlus(closable = F, collapsible = F, collapsed = F, 
                               width = "100%", solidHeader = F, status = "primary",
                               boxPlus(closable = F, collapsible = T, collapsed = T, 
                                       width = "100%", solidHeader = T, status = "primary",
                                       title = "Next 10 Renewed Publications",
                                       dataTableOutput("mpo_df_alt_soonest_10") %>%  withSpinner()),
                               boxPlus(collapsed = T, title = "Tracked Documents", 
                                       closable = FALSE, status = "primary", 
                                       solidHeader = T, collapsible = TRUE,
                                       width = "100%",
                                       plotOutput("mpo_plot_status", width = "100%", height = 300) %>%  
                                         withSpinner())
                       )
                ),
                column(width = 8,
                       fluidRow(
                         wellPanel(height = tab_mtrcs_tpbx_hght+400,
                                   timevisOutput("mpo_plot_timeline") %>%  withSpinner()
                         ), 
                         fluidRow()
                       )
                )
              )
      ),
      #TAB: DATA_CENTER=========================================================
      tabItem("data_center",
              fluidRow(box(height = 20)),
              fluidRow(
                column(width = 5,
                       box(width = "100%",
                           id = "map_container",
                           title = "Default Map Data Layers",
                           dataTableOutput("data_overview") %>%  withSpinner()
                       )
                ), 
                column(width = 7,
                       box(width = "100%",
                           id = "map_container",
                           title = "Raw Data for Selected Layer",
                           dataTableOutput("data_select")  %>%  withSpinner())
                )
              )      
      )
    )
  )
)


#server====================================================================================================================================================================================================
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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
        HTML(intro_modal_front),
        back_content = tagList(
          HTML(intro_modal_back)
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
        HTML(intro_modal_front),
        back_content = tagList(
          HTML(intro_modal_back)
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
      wellPanel(
      map_edit_tab_info),
      size = "l",
      easyClose = TRUE
    ))
  })
  # library(shinyalert)
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
  
  index_census_select_columns = reactive({
    index_census_select_columns = census_selection_index[name %in% input$census_column_filter, var_name]
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
  
  bbox_reactive <- reactive({
    input$full_map_bounds
  })
  
  bbox_reactive_d <- bbox_reactive %>% debounce(2000)

  # output$map_var_plot = renderPlot({
  observe({
    req(census_color())
    # isolate(input$full_map_bounds)
    print("Stop")
    tmp_click = input$full_map_shape_click
    tmp_bounds = bbox_reactive_d()
    
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
      tmp_vert = st_sfc(st_point(c(-122, 48)), crs = 4326) %>%
        st_filter(notshared_census, .) %>%
        select(census_color()) %>%
        .[[1]]
    }
    
    output$map_var_plot_sub = renderPlot({
      filter_sf(notshared_census, 
                xmin = tmp_bounds$west, xmax = tmp_bounds$east,
                ymin = tmp_bounds$south, ymax = tmp_bounds$north) %>% 
        st_set_geometry(NULL) %>%
        select(census_color()) %>%
        set_names("data") %>%
        ggplot(aes(x = data)) +
        geom_histogram(color = "black", bins = 20) +
        geom_vline(xintercept = tmp_vert) +
        theme +
        xlim(c(min(centileBreaks), max(centileBreaks))) +
        labs(y = "Count", title = "Subset (map bounding box)")
    })
    
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
  
  census_map_data_zcol = "Total Population (20-64yrs)"
  
  census_map_data = which(map_ready$processed_name %in% "US_Census") %>% 
    map_files_dfs[[.]]
  
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
  
  #makes the reactive map
  reactive_mappp = reactive({
    map_files_used = map_ready
    tmp = which(map_ready$processed_name %in% "US_Census")
    map_files[[tmp]] = census_map()
    
    map_to_be_shown = map_files %>%
      reduce(`+`)
    
    map_to_be_shown@map
  })
  
  #takes reactive map and makes main default map
  output$full_map = renderLeaflet({
    req(census_color())
    reactive_mappp() %>% 
      hideGroup(hide_layer_list) %>% 
      base_map_options()
  })
  
  #makes mapedit map
  edits <- callModule(editMod, "editor", 
                      isolate(reactive_mappp() %>%
                                hideGroup(hide_layer_list_edit) %>% 
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
                                suffixes = c(".cor", ".sub")
            )  %>%  
              arrange(variable) %>%  
              select(!contains("type")) %>% 
              datatable(escape = F, 
                        fillContainer = T,
                        extensions = c('Buttons'),
                        options = list(
                          scrollY = 750, scrollX = T,
                          pageLength = 900, dom = 'Brt',
                          initComplete = JS(
                            "function(settings, json) {",
                            "$('body').css({'font-family': 'Calibri'});",
                            "}"),
                          buttons = list("csv", "excel", "copy", "pdf")
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
                          scrollY = 750, scrollX = 800,
                          pageLength = 900, dom = 'Brt',
                          fixedColumns = list(leftColumns = 2),
                          initComplete = JS(
                            "function(settings, json) {",
                            "$('body').css({'font-family': 'Calibri'});",
                            "}"),
                          buttons = list("csv", "excel", "copy", "pdf")
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
          scrollY = 370, pageLength = 900, fixedColumns = list(leftColumns = 2),
          dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
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
          dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          initComplete = JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Calibri'});",
            "}")
        )
      )

  })
  
  output$data_select = renderDataTable({
    req(input$data_overview_rows_selected)
    
    map_files_dfs[input$data_overview_rows_selected] %>%  
      data.table() %>% 
      .[, -c("Plain", "Geometry")] %>% 
      datatable(
        escape = F,
        selection = "single",
        extensions = 'Buttons', 
        options = list(
          scrollY = 700, pageLength = 900,
          dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
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
      data.table(`Box Plot Select` = .) %>%  
      datatable(
        escape = F,
        fillContainer = T,
        options = list(
          searching = "FALSE",
          scrollY = 400, pageLength = 900,
          dom = 't', 
          initComplete = JS(
            "function(settings, json) {",
            "$('body').css({'font-family': 'Calibri'});",
            "}")
        )
      )
  })
  
}



# Run the app ----
shinyApp(ui, server)

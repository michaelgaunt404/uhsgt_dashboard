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
                                         menuItem("Help Center", tabName = "help_center", icon = icon("life-ring"),
                                                  actionButton(inputId = "into_button", label = "Show Intro Msg",
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
      #TAB: MAP_DASHBOARD====================================================================
      tabItem("map_dahsboard",
              setShadow(class = "dropdown-menu"),
              fluidRow(box(height = 20)),
              fluidRow(
                column(width = 8,
                       box(width = "100%",
                           id = "map_container",
                           leafletOutput("full_map") %>%  withSpinner(size = 3))
                ),
                column(width = 4,
                       tabBox(width = "100%",
                              tabPanel("Mapping Options",
                                       actionButton("map_btn","Base Map Page Info", icon("info")),
                                       br(),
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
                                                       plotOutput("map_var_plot", height = 150) %>%  withSpinner()
                                                       # ,
                                                       # br(),
                                                       # plotOutput("map_var_plot_sub", height = 150) %>%  withSpinner()
                                                       )
                                       ),
                                       boxPlus(title = "CA Census Filters", width = "100%",
                                               closable = F, collapsible = T, collapsed = F, 
                                               status = "primary", solidHeader = T,
                                               awesomeRadio("ca_census_column_filter", label = "Filter Census Data (by group)",
                                                            choices =  unique(ca_census_selection_index$name), 
                                                            selected = "All", inline = T
                                               ),
                                               boxPlus(width = "100%", title = "Color Select",
                                                       closable = F, collapsible = T, collapsed = F, 
                                                       status = "primary", solidHeader = T,
                                                       uiOutput("layer_color_ca"))
                                               ,
                                               boxPlus(width = "100%", title = "Metric Plots",
                                                       closable = F, collapsible = T, collapsed = F,
                                                       status = "primary", solidHeader = T,
                                                       plotOutput("map_var_plot_ca", height = 150) %>%  withSpinner()
                                                       # ,
                                                       # br(),
                                                       # plotOutput("map_var_plot_sub_ca", height = 150) %>%  withSpinner()
                                                       )
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
                column(width = 2, sliderInput('rad_input', "Filter Radius (miles)", min = 1, max = 15, value = 2)),
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
                                    withSpinner(size = 3))
                           ,
                           column(width = 6,
                                  leafletOutput("leaflet_map_edit", height = 850) %>%  withSpinner(size = 3))
                  ),
                  tabPanel("Metric Plots",
                           tabBox(width = "100%",
                                  tabPanel("US Census",
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
                                                        # height = tab_mtrcs_tpbx_hght,
                                                        plotlyOutput("smmry_bxplts", height = (tab_mtrcs_tpbx_hght)
                                                        ) %>%  withSpinner()
                                                    )
                                             )
                                           )
                                  ),
                                  tabPanel("CA Census",
                                           fluidRow(
                                             column(width = 12,
                                                    box(width = "100%",
                                                    #     height = tab_mtrcs_tpbx_hght,
                                                    plotlyOutput("smmry_brplts_ca", height = (tab_mtrcs_tpbx_hght)
                                                    ) %>%  withSpinner()
                                                    )
                                             )
                                           )
                                           ,
                                           fluidRow(
                                             column(width = 2,
                                                    dataTableOutput("table_select_ca") %>% withSpinner()
                                             ),
                                             column(width = 10,
                                                    # box(width = "100%",
                                                    #     height = tab_mtrcs_tpbx_hght,
                                                    plotlyOutput("smmry_bxplts_ca", height = (tab_mtrcs_tpbx_hght)
                                                    ) %>%  withSpinner()
                                                    # )
                                             )
                                           )
                                  )
                           )
                  ),
                  tabPanel("Metric Tabular Data", 
                           tabBox(width = "90%",
                             tabPanel("US Census",
                                      dataTableOutput("census_aggregate_subset"#, width = "100%"
                                                      ) %>%  withSpinner(size = 3)),
                             tabPanel("CA Census",
                                      dataTableOutput("census_aggregate_subset_ca", width = "100%") %>%  withSpinner(size = 3)))
                  ),
                  tabPanel("Census Tract Subset", 
                           tabBox(width = "100%",
                             tabPanel("US Census",
                                      dataTableOutput("census_subset", width = "100%") %>%  withSpinner(size = 3)),
                             tabPanel("CA Census",
                                      dataTableOutput("census_subset_ca", width = "100%") %>%  withSpinner(size = 3)))
                  )
                )    
              )
      ),
      #TAB: REGIONAL PLANNING===================================================================
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
                                       title = "RTP Update Schedule",
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

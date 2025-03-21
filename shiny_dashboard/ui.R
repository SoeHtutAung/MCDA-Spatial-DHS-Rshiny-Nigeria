#########################
# UI function
# 
##########################

# create UI
ui <- page_navbar(
  title = "Nigeria CHW Expansion",
  bg = "#809e71",
  inverse = TRUE,
  
  ## scenario page
  nav_panel(
    title = "Scenario setting",
    tuning_panel,
    state_panel,
    # instruction, # instruction box
    fluidRow(
      # # left column for controls
      column(width = 3
      #        tuning_panel,
      #        state_panel # states selection
             ),
      # right column for plots
      column(width = 9,  # Right side panel for plots
        fluidRow(
          column(width = 4),
          column(width = 8, card(full_screen = TRUE,
                                 plotlyOutput("plot1", height = "250px"),
                                 bubble_filter,
                                 title = "Bubble plot"))
          ),
        fluidRow(
          column(width = 4),
          column(width = 8, card(plotlyOutput("plot3", height = "200px"), title = "Vulnerability"))
        )
        )
      )
    ),
  
  ## ward adjustment page
  nav_panel(
    title = "Ward selection",
    fluidRow(
      # left column for controls
      column(width = 3, 
             ward_panel
      ),
      # column for instructions
      column(width = 3, 
             summary_card
      ),
      # right column for plots
      column(width = 6,  # Right side panel for plots
             fluidRow(
               card(leafletOutput("map_pg2", height = "300px"),
                    update_map_pg2,
                    title = "ward_vul", full_screen = TRUE),
             ),
             fluidRow(
               table_pg2
               
             )
      ))
      ),
  
  ## detail page
  nav_panel(
    title = "Details",

    ## for pdf rendering, enable shinyjs
    useShinyjs(),
  
    sidebarLayout(
      detail_panel,
      # main panel
      mainPanel(
        fluidRow(
          column(5, leafletOutput("map_pg3", height = "600px")),
          column(7, table2_filter,
                 div(style = "font-size: 12px;",
                 DT::DTOutput("tbl_pg3")
                 ))
        ),
        width = 10
      ))
    ),
  
  ## display page
  nav_panel(
    title = "View map", value = "view_map",
    
    # for map resizing
    # useShinyjs(),
    
    leafletOutput("map_pg4", height = "100%", width = "100%"),
    display_panel,
    # p("Before downloading the dataset, you can specify the state or LGA. Data codes as below:"),
    # tags$ul(
    #   tags$li(tags$b("inc_rate"), " - Projected Malaria incidence rate for 1,000 pupulation (Source: MAP)"),
    #   tags$li(tags$b("mor_rate"), " - Projected Malaria mortality rate for 100,000 pupulation (Source: MAP)"),
    #   tags$li(tags$b("traveltime"), " - Population weighted average travel time for wards (Source: MAP)"),
    #   tags$li(tags$b("pop_above60"), " - Esimtaed population who live beyond 60 minutes travel time to nearest health facility"),
    #   tags$li(tags$b("chw_final"), " - Estimated number of CHW required according to selected algorithm")
    # )
  ),
  
  ## detail page
  nav_panel(
    title = "Optimization",
    
    sidebarLayout(
      optimize_panel,
      # main panel
      mainPanel(
        fluidRow(
          column(7, leafletOutput("map_pg5", height = "600px")),
          column(5, div(style = "font-size: 12px;",
                        DT::DTOutput("tbl_pg5")
                        )
                 )
        ),
        width = 10
      ))
  ),
  
  ## navigation link
  nav_spacer(),
  nav_menu(
    title = HTML(paste0(shiny::icon("circle-question"), " Contact us")),
    align = "right",
    nav_item(tags$a(shiny::icon("headset"), "PATH", href = "https://www.path.org/"))
  )
)

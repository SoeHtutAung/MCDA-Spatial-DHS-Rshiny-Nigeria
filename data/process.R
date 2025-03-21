################################################################################
# To produce datasets for app
# 
################################################################################

# load necessary packages
library(sf)
library(tidyverse)
library(bslib)
library(shiny)
library(shinyWidgets) # slidertextinput
library(leaflet)
library(plotly)
library(viridis) # for palette with no minimum
library(DT)
library(shinydashboard)
library(stringr)
library(shinyjs) # for pdf rendering inside server
library(processx) # for rendering static
library(webshot) # to save as png
library(rmarkdown) # for pdf rendering inside server
library(quarto) # for pdf rendering, make sure to install and load processx
library(rmapshaper) # if ms_simplify is used

# load shape files
adm1_poly <- st_read("data/NGA_states/NGA_states.shp",stringsAsFactors = F)
adm3_poly <- st_read("data/NGA_wards_dashboard/NGA_wards_dashboard_poly.shp",stringsAsFactors = F)
adm3_poly <- ms_simplify(adm3_poly, keep = 0.50) # already 50% reduced with vulnerability shape file from original
hf <- st_read("data/NGA_facilities/NGA_facilities.shp",stringsAsFactors = F)
chw <- st_read("data/chw_optimize/chw_optimize.shp",stringsAsFactors = F)

# clean datasets
## change ESRI abbreviated column names
adm3_final <- adm3_poly %>% dplyr::rename(
  statecode = statecd,
  statename = statenm,
  wardcode = wardcod,
  wardname = wardnam,
  population = pop_24,
  pop_above60 = pp_bv60,
  
  # clean data
  traveltime = travltm,
  inc_rate = inc_rat,
  mor_rate = mor_rat,
  tt_abuja = tt_abuj,
  itnaccess = itnccss,
  inc_score = inc_scr,
  mor_score = mor_scr,
  pf_score = pf_scor,
  itnaccess_score = itnccs_,
  itnuse_score = itns_sc,
  tt_score = tt_scor,
  pop_score = pop_scr,
  conflict_index = cnflct_,
  
  chw_above60 = chw_b60,
  chw_ward = chw_wrd,
  
  chw_final = chw_fnl, 
  vul_final = vul_fnl,
  ) %>% mutate (population = round(population,0))

# prepare chw data 
## change ESRI abbreviated column names
chw <- chw %>% dplyr::rename(
  statecode = statecd,
  statename = statenm,
  wardcode = wardcod,
  wardname = wardnam,
  catchment = ctchmn_,
  coverage_pop = cvrg_pp,
  expected_mal = expctd_,
  ) 

##########################################################################
# create objects to put into UI
# 
##########################################################################
## create a new color pallet
my_palette <- colorRampPalette(c("green", "yellow", "red"))

#################### 
# Map page 
####################
### state parameters
state_panel <- absolutePanel(id = "state_controls", class = "panel panel-default", fixed = TRUE,
                          draggable = FALSE, top = 60, right = "auto", left = 405, bottom = 20,
                          width = 275, height = "auto", #h6("Step 1: State selection"),
                          style = "background-color: rgba(128, 158, 113, 0.5); padding: 10px;font-size: 12px;",
                          tags$style(type='text/css', ".accordion-button {font-size: 14.5px;}"),
                          accordion(
                            div(style = "max-height: 420px; overflow-y: auto;font-size: 12px;",
                                accordion_panel(
                                  "State selection", icon = shiny::icon("filter"),
                                  selectizeInput("state_map", "Select state", choices = unique(adm3_final$statename),
                                                 selected = NULL, multiple = TRUE)
                                )
                            ),
                            open = FALSE
                          ),
                          br(),
                          layout_column_wrap(
                            width = 1/2,
                            actionButton("state_reset", "Reset",
                                                           style = "background-color: #809e71; color: white; border-radius: 10px; padding: 10px 20px; border: none; font-size: 12px;"),
                            actionButton("state_select", "Confirm",
                                         style = "background-color: #809e71; color: white; border-radius: 10px; padding: 10px 20px; border: none; font-size: 12px;"),
                            ),
                          card(card_header("Selected states"),
                               leafletOutput("map_pg1", height = "200px")
                               ),
                          card(card_header("Algorithm weights"),
                               plotlyOutput("plot2", height = "130px")
                          )
                          )

### algorithm tuning parameters
tuning_panel <<- absolutePanel(id = "algo_controls", class = "panel panel-default", fixed = TRUE,
                        draggable = FALSE, top = 60, left = 10, right = "auto", bottom = 20,
                        width = 380, height = "auto", #h6("Step 1: Algorithm tuning"),
                        style = "background-color: rgba(128, 158, 113, 0.5); padding: 10px;font-size: 12px;",
                        # alignment of awesomeradio
                        tags$style(type='text/css', 
                        ".radio-inline, .radio label {display: block; white-space: normal;}
                        .awesome-radio label {display: flex; align-items: left;}"),
                        # Vulnerability scores
                        navset_card_underline(
                          title = "Adjust parameters of vulnerability score",
                          nav_panel("Malaria",
                                    tags$p(strong("Note:"), "Select the importance level"),
                                    awesomeRadio("pf_weight", "Proportion with PF parasite in 2021 (MAP)", choices = c("No", "Low", "Medium", "High"), selected = "Low", inline = T),
                                    awesomeRadio("inc_weight", "Malaria incidence rate in 2022 (MAP)", choices = c("No", "Low", "Medium", "High"), selected = "Low", inline = T),
                                    awesomeRadio("mor_weight", "Malaria mortality rate in 2022 (MAP)", choices = c("No", "Low", "Medium", "High"), selected = "Low", inline = T)
                          ),
                          nav_panel("Intervention",
                                    selectInput("itn_coverage", "Choose ITN coverage indicator", choices = c("Proportion with ITN access", "Proportion of ITN use"), selected = "Proportion with ITN access"),
                                    awesomeRadio("itn_weight", "Select importance level of ITN coverage", choices = c("No", "Low", "Medium", "High"), selected = "Low", inline = T)
                                    # awesomeRadio("irs_weight", "Select importance level of IRS coverage", choices = c("No", "Low", "Medium", "High"), selected = "Low", inline = T),
                                    # awesomeRadio("amt_weight", "Select importance level of Tx coverage", choices = c("No", "Low", "Medium", "High"), selected = "Low", inline = T)
                          ),
                          nav_panel("Accessibility", 
                                    selectInput("remote", "Choose one indicator", choices = c("Proportion of population beyond 1 hour distance from nearest facility", "Average travel time to nearest facility"),
                                                 selected = "Proportion of population beyond 1 hour distance from nearest facility"),
                                    awesomeRadio("remote_weight", "Select importance level of accessibility", choices = c("No", "Low", "Medium", "High"), selected = "Low", inline = T)
                          )
                        ),
                        # CHW strategy
                        card(card_header("CHW expansion strategy"),
                             awesomeRadio("chw_method", "How many CHWs will be recruited?", choices = c("1 CHW per 1,000 population for total population in each ward",
                                                                                             "1 CHW per 1,000 population for population beyond 1hr distance from facility",
                                                                                             "10 CHWs in each ward"),
                                          selected = "1 CHW per 1,000 population for population beyond 1hr distance from facility", inline = F)
                          ),
                        layout_column_wrap(
                          width = 1/2,
                          actionButton("algo_update", "Update algorithm",
                                       style = "background-color: #809e71; color: white; border-radius: 12px; padding: 10px 20px; border: none; font-size: 12px;"),
                          actionButton("showInfo", "Information",
                                       style = "background-color: #FFA500; color: white; border-radius: 12px; padding: 10px 20px; border: none; font-size: 12px;"),
                          ),
                        )

## filter rows at bubble plot
bubble_filter <- fluidRow(
  div(style = "font-size: 12px; display: flex; align-items: center;",  # Reduced font size and aligned items in a row
      div(style = "margin-right: 5px;", p("X-axis: ")),  # Added margin for spacing
      tags$style(type='text/css', ".selectize-input { font-size: 12px;} .selectize-dropdown { font-size: 12px;}"),
      selectInput("bubble_x", "",
                  choices = c("Parasitemia (via microscopy) in children 6-59 months (MIS 2021)", # disease burden
                              "Percent of household owns at least one ITN (MIS 2021)",
                              "Fever symptoms in the 2 weeks before the survey (MIS 2021)",
                              "Advice or treatment sought for fever symptoms on the same or next day (MIS 2021)",
                              
                              "Percent of population living beyond 1 hr distance", # accessibility
                              "Percent of wards where more than half of population living beyond 1 hr distance"), # accessibility
                  selected = "Parasitemia (via microscopy) in children 6-59 months (MIS 2021)"),
      div(style = "margin-right: 5px;", p("Y-axis: ")),  # Added margin for spacing
      selectInput("bubble_y", "",
                  choices = c("Median vulnerability score",
                              "Parasitemia (via microscopy) in children 6-59 months (MIS 2021)", # disease burden
                              "Percent of household owns at least one ITN (MIS 2021)",
                              "Fever symptoms in the 2 weeks before the survey (MIS 2021)",
                              "Advice or treatment sought for fever symptoms on the same or next day (MIS 2021)",
                              
                              
                              "Percent of population living beyond 1 hr distance", # accessibility
                              "Percent of wards where more than half of population living beyond 1 hr distance"), # interventions
                  selected = "Median vulnerability score")
  )
)


####################### 
# ward adjustment page 
#######################
## summary table for states
table_pg2 <- absolutePanel(id = "tbl_pg2", draggable = FALSE, fixed = TRUE, 
                        bottom = 10, top = 420, right = 5, width = 690, 
                        h6("Summary information", style = "font-size: 14.5px;"),
                        style = "background-color: rgba(128, 158, 113, 0.5); padding: 10px;font-size: 12px;",
                        div(style = "max-height: 250px; overflow-y: auto;", # scroll down
                            tags$strong("Summary information for Nigeria"),br(),
                            tableOutput("tbl_pg2_1"),
                            tags$strong("Summary information for selected states"),br(),
                            tableOutput("tbl_pg2"),
                            tags$strong("Selected CHW expansion strategy"),br(),
                            textOutput("chw_method")
                        )
                        )

### ward parameters                    
ward_panel <- absolutePanel(id = "ward_controls", class = "panel panel-default", fixed = TRUE,
                            draggable = FALSE, top = 60, left = 10, right = "auto", bottom = 10,
                            width = 340, height = "auto", #h6("Ward parameters"),
                            style = "background-color: rgba(128, 158, 113, 0.5); padding: 10px;font-size: 12px;",
                            card(card_header("Ward parameters", icon = shiny::icon("sliders")),
                                 sliderInput("vulscore_map","Vulnerability score", min = 0, max = 84, value = c(0,84), step = 2), # 2.577 - 24 before weighting
                                 # sliderInput("vulpop_map", "Min. pop beyond 1hr distance", min = 0, max = 140000, value = 0, step = 10000), # 0 - 145095.0
                                 sliderInput("vulpop_map", "Proportion of population beyond 1hr distance", min = 0, max = 1, value = 0, step = 0.25), # 25% steps
                                 # sliderInput("tt_map", "Select wards with average travel time to PHC", min = 0, max = 690, value = 0, step = 30), # 0 - 699.10 
                                 selectInput("tt_map", "Select wards with travel time to PHC",
                                             choices = c("No minimum", "30 minutes or greater", "1 hour or greater", "2 hours or greater"),
                                             selected = "No minimum")),
                            card(tags$strong("Contexual parameters"),
                                 sliderInput("conflict_map", "Maximum allowable conflict index", min = 0, max = 8, value = 8, step = 1), # 0 - 8
                                 sliderInput("abuja_map", "Maximum driving hours from Abuja", min = 0, max = 20, value = 20, step = 2)) # 0 - 18.3
                            )

## card for summary information
summary_card <- card(card_header("Information", icon = shiny::icon("info-circle")),
                     card_body(
                       style = "height: 510px; overflow-y: auto;font-size: 12px;", # fixed card height as table beside
                       "In the previous page, selection is done at state-level and thus all wards in selected states are included. This page is to further exclude wards according to ward-level parameters.",
                       
                         tags$strong("Ward parameters"),
                         "1. Vulnerability scores: To exclude wards according to their vulnerability scores as per set parameters.", br(),
                         "2. Proportion of population beyond 1hr distance: To exclude wards with fewer proportion of population in remote area.", br(),
                         "3. Travel time to PHC: To exclude wards which are less than the defined median travel time to nearest PHC.", br(),
                         tags$strong("Contextual parameters"),
                       tags$p(
                         "1. Maximum allowable conflict index: To exclude wards where conflict index is high according to number of conflict events and fatalities.", tags$sup("1"), br(),
                         "2. Maximum driving hours from Abuja: To exclude wards according to their driving distance (in hours) from Abuja", tags$sup("2"),
                         )
                     ),
                     card_footer(
                       style = "font-size: 12px;",
                       tags$strong("References"), br(),
                       "1. The Armed Conflict Location & Event Data Project (ACLED)", tags$a(href = "https://acleddata.com/data/", "Link"),br(),
                       "2. Malaria Atlas Project (MAP)", tags$a(href = "https://data.malariaatlas.org/maps", "Link"),
                     )
                     )

## update button for map
update_map_pg2 <- absolutePanel(bottom = 20, right = 20, draggable = FALSE,
                            actionButton("update_map_pg2", "Click before viewing!",
                                         style = "background-color: #FFA500; color: white; border-radius: 12px; padding: 10px 20px; border: none; font-size: 12px;"))

####################### 
# Details page 
#######################
## create side bar for input of state

detail_panel <- sidebarPanel(
  tags$style(type = 'text/css', "#download_option label { font-size: 12px; }"),
  actionButton("update_pg3", "Click to update map and table!",
               style = "background-color: #FFA500; color: white; border-radius: 12px; padding: 10px 20px; border: none; font-size: 12px;"),
  br(), hr(),
  radioButtons("download_option", "Select content", choices = c("Dataset (CSV)", "Summary report (PDF)"), selected = "Dataset (CSV)"),
  disabled(
    downloadButton("download_report", "Download",
                   style = "background-color: #809e71; color: white; border-radius: 12px; padding: 10px 20px; border: none; font-size: 12px;")),
  width = 2
  )

## filter row
table2_filter <- fluidRow(
  div(style = "font-size: 12px;",
  column(4, selectInput("tbl2state", "Choose states to download",
                        c("All", unique(as.character(adm3_final$statename)))))
  # column(4, downloadButton("downloadcsv", "Download CSV",
  #                                style = "background-color: #809e71; color: white; border-radius: 12px; padding: 10px 20px; border: none; font-size: 12px;"))
  )
)

# update button for table and map
# update_pg3 <- absolutePanel(bottom = 20, right = 20, draggable = FALSE,
#                                 actionButton("update_pg3", "Click to update map and table!",
#                                              style = "background-color: #FFA500; color: white; border-radius: 12px; padding: 10px 20px; border: none; font-size: 12px;"))

#################### 
# Map view page 
####################
## display panel
display_panel <- absolutePanel(id = "display_panel", draggable = TRUE, fixed = TRUE,
                               bottom = "auto", top = 60, left = "auto", right = 20,
                               width = 300, h6("Choose the display view of map"),
                               style = "background-color: rgba(255, 255, 255, 0.8); padding: 10px;font-size: 12px;",
                               radioButtons("display_map", "Select view", choices = c(
                                 "Vulnerability", "Incidence", "Mortality", "Travel time to PHC", "Driving distance to Abuja","Conflict index"), selected = "Vulnerability"),
                               actionButton("update_map", "Update map",
                                            style = "background-color: #809e71; color: white; border-radius: 12px; padding: 10px 20px; border: none; font-size: 12px;")
                               )

#################### 
# Optimize page 
####################
## create side bar for input of state
optimize_panel <- sidebarPanel(
  actionButton("update_pg5", "Click to update map and table!",
               style = "background-color: #FFA500; color: white; border-radius: 12px; padding: 10px 20px; border: none; font-size: 12px;"),
  br(), hr(),
  div(style = "font-size: 12px;",
  selectInput("optimize_state", "Choose state to optimize CHW placement",
              unique(as.character(adm3_final$statename)), selected = "Delta"),
  ),
  br(), hr(),
  p(
    strong("Optimization parameters:"),
    tags$ul(
      tags$li(tags$b("PHC coverage area"), " - 1 hour walking distance from PHC"),
      tags$li(tags$b("Catchment area by CHW"), " - 1 hour walking distance from CHW location"),
      tags$li(tags$b("CHW in urban area"), " - 1 CHW for 2,500 urban population"),
      tags$li(tags$b("CHW in rural area"), " - 1 CHW for 1,000 rural population"),
      style = "font-size: 12px;"
    ),
    style = "font-size: 12px;"
  ),
  width = 2
)


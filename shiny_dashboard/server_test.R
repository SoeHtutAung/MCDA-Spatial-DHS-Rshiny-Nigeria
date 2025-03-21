#########################
# Server function
# 
##########################

# create server
server <- function(input, output, session) {
  
  ################## Initial objects #####################
  
  ## render initial maps on app loading
  # Render an initial map at scenario page
  output$map_pg1 <- renderLeaflet({
    leaflet(adm3_final) %>%
      addProviderTiles(providers$Stadia.StamenTonerBackground) %>%
      # Add boundary for states
      addPolygons(data = adm1_poly, color = "#48494B", weight = 0.5,
                  fillColor = NA, fillOpacity = 0,
                  layerId = ~statename) # to connect with click ID
  })
  # Render an initial at ward adjustment page
  output$map_pg2 <- renderLeaflet({
    leaflet(adm3_final) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # Add boundary for states
      addPolygons(data = adm1_poly, fillColor = NA, color = "#48494B", weight = 1, fillOpacity = 0) 
  })
  # Render an initial map at details page
  output$map_pg3 <- renderLeaflet({
    leaflet(adm1_poly) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      # Add boundary for states
      addPolygons(data = adm1_poly, fillColor = NA, color = "orange", weight = 1, fillOpacity = 0) 
  })
  # Render an initial map at view map page
  output$map_pg4 <- renderLeaflet({
    leaflet(adm3_final) %>%
      addProviderTiles(providers$Stadia.Outdoors) %>%
      # Add boundary for states
      addPolygons(data = adm1_poly, fillColor = NA, color = "#48494B", weight = 1, fillOpacity = 0) %>%
      addMiniMap(width = 150, height = 150)
  })
  
  # suspend the initial map at other pages for efficiency
  observe({
    outputOptions(output, "map_pg2", suspendWhenHidden = TRUE)
    outputOptions(output, "map_pg3", suspendWhenHidden = TRUE)
    outputOptions(output, "map_pg4", suspendWhenHidden = TRUE)
  })

  ################## Page 1: Scenario #####################
  
  # information button
  observeEvent(input$showInfo, {
    showModal(modalDialog(
      title = "Information",
      style = "height: 300px; overflow-y: auto; font-size: 12px;", # fixed card height as table beside
      "This page is to adjust parameters for vulnerability score calculation, to define CHW expansion strategy, and to select states.",
      "Importance level means whether a parameter is important to consider when prioritizing the CHW recruitment.", br(), br(),
      tags$strong("Step 1: Adjust importance level of different parameters to calculate vulnerability score"), br(),
      tags$p(
        tags$em("1. Malaria: "), "Define importance level of parasite prevalence rate, incidence rate and mortality rate of malaria", br(),
        tags$em("2. Intervention: "), "Choose one indicator among 'proportion of household with ITN'/'ITN usage', then define importance level",br(),
        tags$em("3. Accessibility: "), "Choose one indicator among 'proportion of people in remote area'/'median travel time to nearest facility', then define importance level "), br(),
      tags$strong("Step 2: Choose a CHW expansion strategy"), br(),
      "3 different options are available to choose on how to quantify number of CHWs needed.",br(), br(),
      tags$strong("Step 3: Select states"), br(),
      "Either choosing input or clicking on map can be used to select states, then press 'Confirm'",br(), br(),
      tags$strong("Step 4: Press on 'Update Algorithm'"), hr(),
      tags$strong("Note: It is important to wait until the calaulation is finished. You can notice by the bubble chart being appeared"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  ## state selection buttons
  # Reactive value to store the selected states
  temp_selected_states <- reactiveVal(character(0))
  
  # Update selected states based on selectizeInput
  observeEvent(input$state_map, {
    temp_selected_states(input$state_map)
    
    # # debugging as needed
    # print(paste("SelectizeInput updated:", paste(input$state_map, collapse = ", ")))
  })
  
  # Click event on the map
  observeEvent(input$map_pg1_shape_click, {
    clicked_state <- input$map_pg1_shape_click$id
    current_selection <- temp_selected_states()
    
    # # debugging as needed
    # print("Shape click event triggered")
    # print(str(input$map_pg1_shape_click))  # Print structure of the click event
    # print(paste("Clicked state:", clicked_state))
    # print(paste("Current selection before update:", paste(current_selection, collapse = ", ")))
    
    if (!is.null(clicked_state) && clicked_state != "") {
      if (clicked_state %in% current_selection) {
        # If the state is already selected, remove it
        new_selection <- setdiff(current_selection, clicked_state)
      } else {
        # If the state is not selected, add it
        new_selection <- c(current_selection, clicked_state)
      }
      
      # # debugging as needed
      # print(paste("New selection after update:", paste(new_selection, collapse = ", ")))
      
      temp_selected_states(new_selection)
      updateSelectizeInput(session, "state_map", selected = new_selection)
    }
  })
  
  # Update leaflet map
  observe({
    leafletProxy("map_pg1") %>%
      clearShapes() %>%
      addProviderTiles(providers$Stadia.StamenTonerBackground) %>%
      addPolygons(data = adm1_poly,
                  color = "#48494B", weight = 1,
                  fillColor = ~ifelse(statename %in% temp_selected_states(), "orange", "transparent"),
                  fillOpacity = 0.5, label = ~statename,
                  layerId = ~statename)
  })
  
  # Reset button
  observeEvent(input$state_reset, {
    # remove selected states
    temp_selected_states(character(0))
    # update selectsizeinput
    updateSelectizeInput(session, "state_map", choices = unique(adm1_poly$statename), selected = character(0))
    # update map
    leafletProxy("map_pg1") %>%
      clearShapes() %>%
      addProviderTiles(providers$Stadia.StamenTonerBackground) %>%
      addPolygons(data = adm1_poly,
                  color = "#48494B", weight = 1,
                  fillColor = "transparent", fillOpacity = 0.5,
                  layerId = ~statename)
  })
  
  # # all button
  # observeEvent(input$state_all, {
  #   updateSelectizeInput(session, "state_map", choices = unique(adm3_final$statename),
  #                        selected = unique(adm3_final$statename))
  # })
  
  # reactive expression that updates only when the state_select button is clicked
  selected_states <- eventReactive(input$state_select, {
    temp_selected_states()
  })
  
  ## Algorithm update
  observeEvent(input$algo_update, {
    
    # create progress object
    progress <- Progress$new(session, min = 0, max = 1)
    progress$set(message = "Algorithm updating process", value = 0)
    on.exit(progress$close()) # to close the progress bar on exit
    
    # progress 1/10
    progress$inc(0.1, detail = "Updating data...")
    
    weights <- reactive({
      list(
        inc = switch(input$inc_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3),
        mor = switch(input$mor_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3),
        pf = switch(input$pf_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3),
        itn = switch(input$itn_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3),
        remote = switch(input$remote_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3),
        tt = switch(input$tt_map, "No minimum" = 0, "30 minutes or greater" = 30, "1 hour or greater" = 60, "2 hours or greater" = 120)
      )
    })
    
    # # switch to number
    # inc_weight <- switch(input$inc_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3)
    # mor_weight <- switch(input$mor_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3)
    # pf_weight <- switch(input$pf_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3)
    # itn_weight <- switch(input$itn_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3)
    # # irs_weight <- switch(input$irs_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3)
    # # amt_weight <- switch(input$amt_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3)
    # remote_weight <- switch(input$remote_weight, "No" = 0, "Low" = 1, "Medium" = 2, "High" = 3)
    # # switch travel time input as reactive dataframe
    # tt_map <- reactive({
    #   switch(input$tt_map, "No minimum" = 0, "30 minutes or greater" = 30, "1 hour or greater" = 60, "2 hours or greater" = 120)
    # })
    
    # extract ITN and Accessibility score according to input
    # itn score
    itn_score <- reactive({
      itn_result <- NULL  # to prevent length zero error
      if (input$itn_coverage == "Proportion with ITN access") {
          itn_result <- adm3_final$itnaccess_score
        } else if (input$itn_coverage == "Proportion of ITN use") {
          itn_result <- adm3_final$itnuse_score
        }
      return(itn_result)
    })
    # accessibility score
    access_score <- reactive({
      access_result <- NULL  # to prevent length zero error
      if (input$remote == "Proportion of population beyond 1 hour distance from nearest facility") {
        access_result <- adm3_final$pop_score
      } else if (input$remote == "Average travel time to nearest facility") {
        access_result <- adm3_final$tt_score
      }
      return(access_result)
    })
    
    # reactive data frame for adm3_final after setting algorithm
    reactive_data <- reactiveVal()
    
    # create a new dataframe for simplicity
    new_data <- adm3_final %>% as.data.frame() %>%
      mutate(
        # disease burden
        inc_weighted = inc_score * weights()$inc,
        mor_weighted = mor_score * weights()$mor,
        pfrate_weighted = pf_score * weights()$pf,
        # combined disease burden
        burden_weighted = inc_weighted + mor_weighted + pfrate_weighted,
        
        # intervention
        itn_weighted = itn_score() * weights()$itn,
        # irs_weighted = irs_score * irs_weight, # TBD
        # amt_weighted = amt_score * amt_weight, # TBD
        # combined intervention score
        # invt_weighted = itn_weighted + irs_weighted + amt_weighted, # TBD and will replace later codes
        
        # accessibility
        remote_weighted = access_score() * weights()$remote,
        
        # final vulnerability score
        vul_final = burden_weighted + itn_weighted + remote_weighted,
        
        # chw strategy
        chw_final = case_when(
          input$chw_method == "1 CHW per 1,000 population for total population in each ward" ~ chw,
          input$chw_method == "1 CHW per 1,000 population for population beyond 1hr distance from facility" ~ chw_above60,
          input$chw_method == "10 CHWs in each ward" ~ chw_ward,
          TRUE ~ NA_real_
        )
      ) %>%
      st_as_sf()
    
    # save as reactive dataframe
    reactive_data(new_data)
    
    # create numeric color palettes
    ## community health workers
    chw_palette <- colorNumeric(palette = "YlOrRd", domain = reactive_data()$chw_final)
    ## custom function to apply grey color to zero values and Blues palette to non-zero values
    chw_pal_custom <- function(value) {
      ifelse(value == 0, "#48494B", chw_palette(value))
    }
    ## vulnerability
    vul_breaks <- quantile(reactive_data()$vul_final, probs = seq(0, 1, 0.25))
    # vul_labels <- paste(lag(vul_breaks), vul_breaks, sep = " - ")[-1] 
    vul_palette <- colorBin(palette = my_palette(4), domain = reactive_data()$vul_final, bins = vul_breaks)
    ## incidence
    inc_breaks <- quantile(reactive_data()$inc_rate, probs = seq(0, 1, 0.25))
    # inc_labels <- paste(lag(inc_breaks), inc_breaks, sep = " - ")[-1] 
    inc_palette <- colorBin(palette = my_palette(4), domain = reactive_data()$inc_rate, bins = inc_breaks)
    ## mortality
    mor_breaks <- quantile(reactive_data()$mor_rate, probs = seq(0, 1, 0.25))
    # mor_labels <- paste(lag(mor_breaks), mor_breaks, sep = " - ")[-1]
    mor_palette <- colorBin(palette = my_palette(4), domain = reactive_data()$mor_rate, bins = mor_breaks)
    ## travel time, tt to abuja and conflict index are preset
    tt_palette <- colorBin(palette = my_palette(4), domain = reactive_data()$traveltime, bins = c(0,30,60,120,Inf))
    # tt_labels <- c("Less than 30 minutes", "30 minutes - 1 hour", "1 - 2 hours", "2 hours and above")
    ttabj_palette <- colorBin(palette = my_palette(4), domain = reactive_data()$tt_abuja, bins = c(0,2,4,6,Inf))
    # ttabj_labels <- c("Less than 2 hours", "2 - 4 hours", "4 - 6 hours", "6 hours and above")
    con_palette <- colorBin (palette = my_palette(4), domain = reactive_data()$conflict_index, bins = c(0, 1, 3,5, 8))
    # con_labels <- c("No conflict", "Low conflict", "Moderate conflict", "High conflict")
    
    ## create reactive dataframe for ward
    ward_react <- reactive({
      
      reactive_data() %>% as.data.frame() %>%
        filter(statename %in% selected_states() &
                 vul_final >= input$vulscore_map[1] &
                 vul_final <= input$vulscore_map[2] &
                 percent >= input$vulpop_map &
                 traveltime >= weights()$tt &
                 conflict_index <= input$conflict_map &
                 tt_abuja <= input$abuja_map)
    })
    
    ## create reactive dataframe for states
    state_react <- reactive({
      reactive_data() %>% as.data.frame() %>% #TBD
      # ward_react() %>% as.data.frame() %>% # replace with reactive data() if want to see all selected states ignoring ward adjustment 
        filter (statename %in% selected_states()) %>%
        group_by (statename, zone) %>% summarize (
          
          # malaria prevalence as per MIS 2021
          malaria = round(first(malaria),1),
          itn = round(first(itn),1),
          fever = round(first(fever),1),
          seek = round(first(seek),1),
          
          # median vulnerability score at state-level
          vulnerability = median(vul_final, na.rm = TRUE), # median rather than mean vulnerability score
          
          # itn_access = round(median(itnaccess, na.rm = TRUE),1), # ITN
          # irs_coverage = round(median(irs, na.rm = TRUE),1), # IRS #TBD
          # amt_coverage = round(median(amt, na.rm = TRUE),1), # AMT #TBD
          
          # population parameters at state-level
          population = sum(population), # total population at state
          percent_pop = round(sum(pop_above60) / sum(population),1), # percent of population living beyond 1hr distance
          percent_ward = round(sum(percent > 0.5) / n(),1), # percent of wards with more than 50% population living beyond 1hr distance
          .groups = 'drop') 
    })
    
    # progress 3/10
    progress$inc(0.2, detail = "Updating page 1...")
    
    ## render plot2 (horizontal stacked bar) of Scenario page
    output$plot2 <- renderPlotly({
      # Create dataframe
      weight_data <- data.frame(
        labels <- c("Malaria burden", "ITN coverage", "Accessibility"),
        weights <- c((weights()$inc+weights()$mor+weights()$pf), weights()$itn, weights()$remote))
      # for percent labelling
      weight_data <- weight_data %>% mutate (pct = (weights / sum(weights)) * 100,
                                             percent = paste0(round(pct, 1), "%"))
      # Create stacked bar
      plot_ly(weight_data, x = ~weights, type = 'bar', # orientation = 'v',
                   color = ~labels, text = ~paste(labels,'<br>',percent), hoverinfo = 'text',
                   marker = list(line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
        # modify layout
        layout(xaxis = list(title = "",
                            showgrid = FALSE,
                            showline = FALSE,
                            showticklabels = FALSE,
                            zeroline = FALSE,
                            domain = c(0, 1)),
               yaxis = list(title = "",
                            showgrid = FALSE,
                            showline = FALSE,
                            showticklabels = FALSE,
                            zeroline = FALSE),
               barmode = 'stack',
               margin = list(l = 0, r = 0, t = 0),
               #showlegend = TRUE,
               legend = list(orientation = "h",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             x = 0.5)) %>%        # put legend in center of x-axis) 
        config(displayModeBar = FALSE) # remove mode bar
    }) # end of plot2
    
  ## render bubble chart
  output$plot1 <- renderPlotly({
    # get dataframe
    state_react <- state_react()
    
    # transform inputs into column names for y axis
    y_input <- reactive({
      y_var <- NULL  # to prevent length zero error
      if (!is.null(input$bubble_y) && input$bubble_y != "") {
        if (input$bubble_y == "Median vulnerability score") {
          y_var <- state_react$vulnerability
        } else if (input$bubble_y == "Percent of population living beyond 1 hr distance") {
          y_var <- state_react$percent_pop
        } else if (input$bubble_y == "Percent of wards where more than half of population living beyond 1 hr distance") {
          y_var <- state_react$percent_ward
        } else if (input$bubble_y == "Parasitemia (via microscopy) in children 6-59 months (MIS 2021)") {
          y_var <- state_react$malaria
        } else if (input$bubble_y == "Percent of household owns at least one ITN (MIS 2021)") {
          y_var <- state_react$itn
        } else if (input$bubble_y == "Fever symptoms in the 2 weeks before the survey (MIS 2021)") {
          y_var <- state_react$fever
        } else if (input$bubble_y == "Advice or treatment sought for fever symptoms on the same or next day (MIS 2021)") {
          y_var <- state_react$seek
        }
        # Excluded as there is no reliable IRS or AMT dataset at ward level
        # else if (input$bubble_y == "Median IRS coverage") {
        #   y_var <- state_react$irs_coverage
        # } 
        # else if (input$bubble_y == "Median Animalaria treatment coverage") {
        #   y_var <- state_react$amt_coverage
        # }
      }
      return(y_var)
      })
    
    # transform inputs into column names for x axis
    x_input <- reactive({
      x_var <- NULL  # to prevent length zero error
      if (!is.null(input$bubble_x) && input$bubble_x != "") {
        if (input$bubble_x == "Parasitemia (via microscopy) in children 6-59 months (MIS 2021)") {
          x_var <- state_react$malaria
        } else if (input$bubble_x == "Percent of household owns at least one ITN (MIS 2021)") {
          x_var <- state_react$itn
        } else if (input$bubble_x == "Fever symptoms in the 2 weeks before the survey (MIS 2021)") {
          x_var <- state_react$fever
        } else if (input$bubble_x == "Advice or treatment sought for fever symptoms on the same or next day (MIS 2021)") {
          x_var <- state_react$seek
        } else if (input$bubble_x == "Percent of population living beyond 1 hr distance") {
          x_var <- state_react$percent_pop
        } else if (input$bubble_x == "Percent of wards where more than half of population living beyond 1 hr distance") {
          x_var <- state_react$percent_ward
        } 
        
        #TBD
        # else if (input$bubble_x == "Median IRS coverage") {
        #   x_var <- state_react$irs_coverage
        # } 
        # else if (input$bubble_x == "Median Animalarial treatment coverage") {
        #   x_var <- state_react$amt_coverage
        # }
      }
      return(x_var)
    })
    
    # transform inputs into yaxis label
    y_titles <- list(
      "Median vulnerability score" = "Median vulnerability score",
      "Parasitemia (via microscopy) in children 6-59 months (MIS 2021)" = "Parasitemia (via microscopy)<br>in children 6-59 months (MIS 2021)",
      "Percent of household owns at least one ITN (MIS 2021)" = "Percent of household owns<br>at least one ITN (MIS 2021)",
      "Fever symptoms in the 2 weeks before the survey (MIS 2021)" = "Fever symptoms in the 2 weeks<br>before the survey (MIS 2021)",
      "Advice or treatment sought for fever symptoms on the same or next day (MIS 2021)" = "Advice or treatment sought for fever<br>symptoms on the same or next day<br>(MIS 2021)",
      
      "Percent of population living beyond 1 hr distance" = "Percent of population living beyond<br>1 hr distance from facility",
      "Percent of wards where more than half of population living beyond 1 hr distance" = "Percent of wards where more than<br>half of population living beyond<br>1 hr distance from facility"
      # "Median IRS coverage" = "Median IRS coverage",
      # "Median Animalarial treatment coverage" = "Median Animalarial treatment coverage"
    )
    
    # transform inputs into yaxis label
    x_titles <- list(
      "Parasitemia (via microscopy) in children 6-59 months (MIS 2021)" = "Parasitemia (via microscopy) in children 6-59 months (MIS 2021)",
      "Percent of household owns at least one ITN (MIS 2021)" = "Percent of household owns at least one ITN (MIS 2021)",
      "Fever symptoms in the 2 weeks before the survey (MIS 2021)" = "Fever symptoms in the 2 weeks before the survey (MIS 2021)",
      "Advice or treatment sought for fever symptoms on the same or next day (MIS 2021)" = "Advice or treatment sought for fever symptoms<br>on the same or next day (MIS 2021)",
      
      "Percent of population living beyond 1 hr distance" = "Percent of population living beyond 1 hr distance from facility",
      "Percent of wards where more than half of population living beyond 1 hr distance" = "Percent of wards where more than half of population<br>living beyond 1 hr distance"
      # "Median IRS coverage" = "Median IRS coverage",
      # "Median Animalarial treatment coverage" = "Median Animalarial treatment coverage"
    )
    
    # function to determine if axis format (tickformat) should be percentage
    format_axis <- function(axis_var) {
      if (axis_var %in% c("Percent of population living beyond 1 hr distance", 
                          "Percent of wards where more than half of population living beyond 1 hr distance",
                          "Parasitemia (via microscopy) in children 6-59 months (MIS 2021)", # disease burden
                          "Percent of household owns at least one ITN (MIS 2021)",
                          "Fever symptoms in the 2 weeks before the survey (MIS 2021)",
                          "Advice or treatment sought for fever symptoms on the same or next day (MIS 2021)"
                          )) {
        return(".0%")
      } else {
        return("")
      }
    }
    
    # plot
    plot1 <- plot_ly(state_react, x = ~x_input(), y = ~y_input(), 
            color = ~zone, colors = viridis(n_distinct(state_react$zone)),
            type = 'scatter', mode = 'markers',
            size = ~population, sizes = c(10, 60),
            hoverinfo = 'text', text = ~paste('State: ', statename,
                                              '<br>Population: ', format(population, big.mark = ",", scientific = FALSE, trim = TRUE),
                                              '<br>Vul. score: ', vulnerability
                                              # '<br>Malaria: ', malaria,
                                              # '<br>Pop > 60min: ', percent_pop, '%',
                                              # '<br>Ward > 50%: ', percent_ward, '%'
                                              ),
            fill = ~'', # for line.width error
            marker = list(sizemode = 'diameter')
            ) %>%
      layout(
        xaxis = list(title = x_titles[[input$bubble_x]], tickformat = format_axis(input$bubble_x), 
                     rangemode = "tozero"),
        yaxis = list(title = y_titles[[input$bubble_y]], tickformat = format_axis(input$bubble_y),
                     rangemode = "tozero"),
        margin = list(b = 10)
      )
    
    # save plot as png file using webshot
    bubble_html <- file.path(tempdir(), "plot1.html")
    bubble <- file.path(tempdir(), "plot1.png")
    saveWidget(plot1, bubble_html) # save plot as html file
    webshot(bubble_html, bubble, , vwidth = 600, vheight = 400) # save plot as png file
    
    # return plot1
    plot1
    
    })
    
    ## render box plot with jitter
    output$plot3 <- renderPlotly({
      ward_react <- reactive_data() %>% as.data.frame() %>% 
        filter (statename %in% selected_states())
      # ward_react <- ward_reac
      # plot
      plot3 <- ward_react %>%
        plot_ly(x = ~statename, y = ~vul_final,
                type = 'box', boxpoints = 'all',
                jitter = 0.3, pointpos = 0, marker = list(size = 3),
                color = ~zone, colors = viridis(n_distinct(ward_react$zone)),
                hoverinfo = 'text', text = ~paste('Ward: ', wardname,
                                                  '<br>Vul. score: ', vul_final),
                line = list(width = 1)
        ) %>%
        layout(
          xaxis = list(title = "", tickangle = -45), # Change x-axis label direction
          yaxis = list(title = "Vulenrability score"),
          margin = list(b = 0) # Adjust margins
        )
      
      # save plot as png file using webshot
      boxplot_html <- file.path(tempdir(), "plot3.html")
      boxplot <- file.path(tempdir(), "plot3.png")
      saveWidget(plot3, boxplot_html) # save plot as html file
      webshot(boxplot_html, boxplot, , vwidth = 600, vheight = 400) # save plot as png file
      
      # return the plot
      plot3
    })

    ################## Page 2: Ward adjustment page #####################
    
    # progress 5/10
    progress$inc(0.2, detail = "Updating page 2...")
    
    ## update map
    observeEvent (input$update_map_pg2,{
      
      # create numeric color palette
      # vul_pal <- colorBin(palette = my_palette(4), domain = ward_react()$vul_final, bins = 4)
      # update map
      leafletProxy("map_pg2", data = st_as_sf(ward_react())) %>%
        clearShapes() %>%
        clearControls() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # add boundary for states
        addPolygons(data = adm1_poly %>% filter(statename %in% ward_react()$statename),
                    fillColor = NA, color = "#48494B", weight = 1, fillOpacity = 0) %>%
        # add vulnerability scores for wards
        addPolygons(fillColor = ~vul_palette(vul_final), fillOpacity = 0.5,
                    stroke = FALSE, smoothFactor = 0, # to clear gaps
                    label = ~ paste("Ward: ", wardname),
                    popup = ~ paste("Ward: ", wardname,
                                    "<br/>Vulnerability score: ", vul_final)
        ) %>%
        # add legend
        addLegend(title = "Vulnerability score", "bottomleft", pal = vul_palette,
                  values = ward_react()$vul_final, opacity = 0.7, labels = NA)
    }) # end of map update in page 2
  
    ## Render SUMMARY table
    # overall
    output$tbl_pg2_1 <- renderTable({
      # Reactive dataframe for table
      ward_tbl_pg2_1 <- ward_react() %>%
        summarise("No. States" = n_distinct(statename),
                  "No. LGAs" = n_distinct(lgacode),
                  "No. Wards" = n(),
                  "No. CHWs" = format(sum(chw_final, na.rm = TRUE), big.mark = ",", scientific = FALSE),
                  "Population" = format(sum(population, na.rm = TRUE), big.mark = ",", scientific = FALSE),
                  "Population (remote)" = format(sum(pop_above60, na.rm = TRUE), big.mark = ",", scientific = FALSE))%>%
        mutate(across(where(is.character), ~str_pad(., width = max(nchar(.)), side = "left")),
               across(where(is.numeric), ~str_pad(., width = max(nchar(format(., big.mark = ",", scientific = FALSE))), side = "left")))
      # return table
      ward_tbl_pg2_1
    })
    
    # for states
    output$tbl_pg2 <- renderTable({
      # Reactive dataframe for table
      ward_tbl_pg2 <- ward_react() %>%
        group_by(statename) %>% rename("State" = statename) %>%
        summarise("No. LGAs" = n_distinct(lgacode),
                  "No. Wards" = n(),
                  "No. CHWs" = format(sum(chw_final, na.rm = TRUE), big.mark = ",", scientific = FALSE),
                  "Population" = format(sum(population, na.rm = TRUE), big.mark = ",", scientific = FALSE),
                  "Population (remote)" = format(sum(pop_above60, na.rm = TRUE), big.mark = ",", scientific = FALSE))%>%
        mutate(across(where(is.character), ~str_pad(., width = max(nchar(.)), side = "left")),
               across(where(is.numeric), ~str_pad(., width = max(nchar(format(., big.mark = ",", scientific = FALSE))), side = "left")))
      # Return table
      ward_tbl_pg2
    }) # end of table in page2 update
    
    # for chw strategy text
    output$chw_method <- renderText({
      input$chw_method
    })
    
    ################## Page 3: Details #####################
    
    # progress 8/10
    progress$inc(0.3, detail = "Updating page 3...")
    
    ## Update only when the update button is pressed
    observeEvent(input$update_pg3, {
      
      ## Update map at detail page
      
      # Update leaflet map
      leafletProxy("map_pg3") %>%
        # clear previous marks and shapes
        clearShapes() %>%
        clearMarkers () %>%
        clearControls() %>%
        # add boundary for states
        addProviderTiles(providers$CartoDB.Positron) %>%
        # add boundary for states
        addPolygons(data = adm1_poly %>% filter(statename %in% ward_react()$statename),
                    fillColor = NA, color = "orange", weight = 1.5, fillOpacity = 0) %>%
        # add CHWs
        addPolygons(data = st_as_sf(ward_react()),
                    fillColor = ~chw_pal_custom(chw_final), fillOpacity = 0.5,
                    stroke = FALSE, smoothFactor = 0, # to clear gaps
                    label = ~ paste("Ward: ", wardname),
                    popup = ~ paste("Ward: ", wardname,
                                    "<br/>No. CHW: ", chw_final)) %>%
        # add legend
        addLegend(title = "Number of CHWs", "bottomleft",
                  pal = chw_palette, values = ward_react()$chw_final, opacity = 0.7, labels = NA) # %>%
        # To zoom into selected state
        # flyToBounds(as.numeric(bounds["xmin"]), as.numeric(bounds["ymin"]), as.numeric(bounds["xmax"]), as.numeric(bounds["ymax"]))
      
      ## render table in details page
      output$tbl_pg3 <- DT::renderDataTable(DT::datatable({
        # only run when button is click
        input$input$update_pg3
        # Reactive dataframe for table
        ward_tbl_pg3 <- ward_react() %>%
          filter (statename %in% ward_react()$statename, # after ward adjustments
                  chw_final > 0) %>%
          select (statename, lganame, wardname, chw_final, population, pop_above60) %>% 
          rename("State" = statename,
                 "LGA" = lganame,
                 "Ward" = wardname,
                 "Population (total)" = population,
                 "Population (remote)" = pop_above60,
                 "No. CHW" = chw_final)
        # return table 2
        ward_tbl_pg3
      })) # end of table
    }) # end of update_pg3
    
    ## Download button
    # enable rendering if states are selected, else, disable everything
    observeEvent(input$state_map,{
      
      if(!is.null(input$state_map)){
        enable("download_report")
      }else{
        disable("download_report")
      }
    }, ignoreNULL = FALSE)
    
    # prepare dataset for csv download
    observe({
      # get unique statename based on ward_react dataframe
      state_names <- c("All", unique(as.character(ward_react()$statename)))
      # Update the selectInput choices
      updateSelectInput(session, "tbl2state", choices = state_names)
    })
    
    # Define download handler for both CSV and PDF
    output$download_report <- downloadHandler(
      filename = function() {
        if (input$download_option == "Dataset (CSV)") {
          paste("dataset", Sys.Date(), ".csv", sep = "")
        } else if (input$download_option == "Summary report (PDF)") {
          paste("report", Sys.Date(), ".pdf", sep = "")  # Use return() explicitly to ensure the correct value is returned
        } else {
          return(NULL)  # fallback if option not recognized
        }
      },
      content = function(file) {
        if (input$download_option == "Dataset (CSV)") {
          # Get the reactive dataframe
          ward_react_tbl1 <- ward_react()
          # Filter the dataframe based on the selected state, if not "All"
          if (input$tbl2state != "All") {
            ward_react_tbl1 <- ward_react_tbl1 %>%
              filter(statename %in% input$tbl2state)
          }
          # Select and rename columns as needed
          ward_react_tbl1 <- ward_react_tbl1 %>%
            select(statename, lgacode, lganame, wardcode, wardname,
                   traveltime, inc_rate, mor_rate, vul_final, conflict_index,
                   population, pop_above60, chw_final) %>%
            rename("State Name" = statename,
                   "LGA Code" = lgacode,
                   "LGA Name" = lganame,
                   "Ward Code" = wardcode,
                   "Ward Name" = wardname,
                   "No. CHW" = chw_final)
          
          # Write to csv
          write.csv(ward_react_tbl1, file, row.names = FALSE)
          
        } else if (input$download_option == "Summary report (PDF)") {
          
          # progress bar pdf report
          withProgress(message = "Generating PDF report", {
            incProgress(0.1, "Preparing data...")
          
          # create report temp file
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("report.Rmd", tempReport, overwrite = TRUE)
          
          # create path for png images
          bubble <- file.path(tempdir(), "plot1.png") # bubble plot of page 1
          boxplot <- file.path(tempdir(), "plot3.png") # boxplot of page 1
          # map_pg1_png <- file.path(tempdir(), "map_pg1.png") # map of page 1
          
          # set up parameters (dataframes and objects) to pass to Rmd
          report <- list(state_map = input$state_map,
                         selected_states = selected_states(),
                         state_react = state_react(),
                         ward_react = ward_react(),
                         adm1_poly = adm1_poly,
                         my_palette = my_palette,
                         boxplot = boxplot,
                         bubble = bubble)
          # map_pg1 = map_pg1_png) 
          
          # progress bar pdf report
          incProgress(0.5, "Rendering report...")
          
          # knit the document
          rmarkdown::render(tempReport, output_file = file,
                            params = report,
                            envir = new.env(parent = globalenv()))
          
          # progress bar pdf report
          incProgress(1.0, "Report generated!")
          })
        }
      }
    )
    
    ############## Page 4: View map #####################
    
    # progress 10/10
    progress$inc(0.2, detail = "Updating page 4...")
    
    observeEvent(input$update_map, {
      
      ## update map
      # create reactive dataframe
      ward_react <- st_as_sf(ward_react())
      # Update Leaflet map
      leafletProxy("map_pg4", data = ward_react) %>%
        clearShapes() %>%
        clearControls() %>%
        # setView(lng = 8.6753, lat = 9.0820, zoom = 6) %>% 
        addProviderTiles(providers$Stadia.Outdoors) %>%
        addMiniMap(width = 150, height = 150) %>%
        # Add boundary for states
        addPolygons(data = adm1_poly %>% filter(statename %in% ward_react$statename), fillColor = NA, color = "#48494B", weight = 1, fillOpacity = 0) %>%
        # Adjust palette for wards according to view input by radio button
        {
          # If "vulnerability scores" is selected
          if (input$display_map == "Vulnerability") {
            addPolygons(., data = ward_react,
                        fillColor = ~vul_palette(vul_final), fillOpacity = 0.5,
                        stroke = FALSE, smoothFactor = 0, # to clear gaps
                        label = ~ paste("Ward: ", wardname),
                        popup = ~ paste("Ward: ", wardname,
                                        "<br/>Total population: ", population,
                                        "<br/>Affected population: ", pop_above60,
                                        "<br/>Vulnerability score: ", vul_final)
            ) %>%
              addLegend(title = "Vulnerability score", "bottomleft", pal = vul_palette, values = ~vul_final, opacity = 0.7, labels = NA)
          }
          # If "Incidence" is selected
          else if (input$display_map == "Incidence") {
            addPolygons(., data = ward_react,
                        fillColor = ~inc_palette(inc_rate), fillOpacity = 0.5,
                        stroke = FALSE, smoothFactor = 0, # to clear gaps
                        label = ~ paste("Ward: ", wardname),
                        popup = ~ paste("Ward: ", wardname,
                                        "<br/>Total population: ", population,
                                        "<br/>Affected population: ", pop_above60,
                                        "<br/>Incidence: ", inc_rate)
            ) %>%
              addLegend(title = "Malaria incidence (per 1,000)", "bottomleft", pal = inc_palette, values = ~inc_rate, opacity = 0.7, labels = NA)
          }
          # if "Mortality" is selected
          else if (input$display_map == "Mortality") {
            addPolygons(., data = ward_react,
                        fillColor = ~mor_palette(mor_rate), fillOpacity = 0.5,
                        stroke = FALSE, smoothFactor = 0, # to clear gaps
                        label = ~ paste("Ward: ", wardname),
                        popup = ~ paste("Ward: ", wardname,
                                        "<br/>Total population: ", population,
                                        "<br/>Affected population: ", pop_above60,
                                        "<br/>Mortality: ", mor_rate)
            ) %>%
              addLegend(title = "Malaria mortality rate (per 100,000)", "bottomleft", pal = mor_palette, values = ~mor_rate, opacity = 0.7, labels = NA)
          }
          # If "Traveltime to PHC" is selected
          else if (input$display_map == "Travel time to PHC") {
            addPolygons(., data = ward_react,
                        fillColor = ~tt_palette(traveltime), fillOpacity = 0.5,
                        stroke = FALSE, smoothFactor = 0, # to clear gaps
                        label = ~ paste("Ward: ", wardname),
                        popup = ~ paste("Ward: ", wardname,
                                        "<br/>Total population: ", population,
                                        "<br/>Affected population: ", pop_above60,
                                        "<br/>Travel time: ", traveltime)
            ) %>%
              addLegend(title = "Travel time to nearest PHC (minutes)", position = "bottomleft",
                        pal = tt_palette, values = ~traveltime, opacity = 0.7,
                        labels = tt_labels
                        )
          }
          # If "Driving distance to Abuja" is selected
          else if (input$display_map == "Driving distance to Abuja") {
      
            # add polygons
            addPolygons(., data = ward_react,
                        fillColor = ~ttabj_palette(tt_abuja), fillOpacity = 0.5,
                        stroke = FALSE, smoothFactor = 0, # to clear gaps
                        label = ~ paste("Ward: ", wardname),
                        popup = ~ paste("Ward: ", wardname,
                                        "<br/>Total population: ", population,
                                        "<br/>Affected population: ", pop_above60,
                                        "<br/>Travel time: ", tt_abuja)
            ) %>%
              addLegend(title = "Driving distance to Abuja (hours)", position = "bottomleft", 
                        pal = ttabj_palette, values = ~tt_abuja, opacity = 0.7, 
                        labels = ttabj_labels
                        )
          }
          # If "Conflict index" is selected
          else if (input$display_map == "Conflict index") {
            addPolygons(., data = ward_react,
                        fillColor = ~con_palette(conflict_index), fillOpacity = 0.5,
                        stroke = FALSE, smoothFactor = 0, # to clear gaps
                        label = ~ paste("Ward: ", wardname),
                        popup = ~ paste("Ward: ", wardname,
                                        "<br/>Total population: ", population,
                                        "<br/>Affected population: ", pop_above60,
                                        "<br/>Conflict index: ", conflict_index)
            ) %>%
              addLegend(title = "Conflict index", "bottomleft", pal = con_palette, values = ~conflict_index, opacity = 0.7, 
                        labels = con_labels
                          )
          }
        }
    }) # end of map update at map view page
    
    ############## Page 5: Optimization #####################
    
    # Create CHW map
    observeEvent(input$update_pg5, {
      
      # update select state button
      observe({
        # get unique statename based on ward_react dataframe
        state_names <- unique(as.character(ward_react()$statename))
        # Update the selectInput choices
        updateSelectInput(session, "optimize_state", choices = state_names)
      })
      
      # create dataframes of the selected wards among inputed states
      ward <- st_as_sf(ward_react()) %>% filter(statename == input$optimize_state) # adm3 data
      hf_filtered <- hf %>% filter(wardcode %in% ward$wardcode)
      chw_filtered <- chw %>% filter(wardcode %in% ward$wardcode)
      
      # map
      output$map_pg5 <- renderLeaflet({
        
        # create leaflet map
        leaflet() %>% 
          # add different provider tiles
          addProviderTiles(
            "CartoDB.Positron",
            group = "CartoDB.Positron"
          ) %>%
          addProviderTiles(
            "Esri.WorldImagery",
            group = "Esri.WorldImagery"
          ) %>%
          # add a layers control
          addLayersControl(
            baseGroups = c(
              "CartoDB.Positron", "Esri.WorldImagery"
            ),
            # position it on the top left
            position = "topleft"
          ) %>% 
          # add ward boundaries
          addPolygons(data = ward, 
                      fillColor = "transparent",
                      color = "black", weight = 1.5,
                      label = ~paste("Ward name: ", wardname)) %>% 
          # add health facilities
          addCircleMarkers(data = hf_filtered, 
                           radius = 3, 
                           color = "red", 
                           label = ~paste("PHC name: ", prmry_name),
                           popup = ~paste("Ward name: ", wardname,
                                          "<br/>PHC name: ", prmry_name,
                                          "<br/>Owned by: ", ownership)) %>%
          # add CHWs
          addCircleMarkers(data = chw_filtered, 
                           radius = 1,
                           label = ~paste("Number of CHWs: ", n_chw),
                           popup = ~paste("Ward name: ", wardname,
                                          "<br/>Number of CHWs: ", n_chw,
                                          "<br/>Catchment type: ", catchment,
                                          "<br/>Coverage population: ", as.integer(coverage_pop),
                                          "<br/>Expected malaria cases: ", as.integer(expected_mal))) %>%
          # add a mini map
          addMiniMap(tiles = providers$Stamen.Toner, toggleDisplay = TRUE)
      }) # end of map
      
      # develop table
      ## render table in details page
      output$tbl_pg5 <- DT::renderDataTable(DT::datatable({

        # Reactive dataframe for table
        ward_tbl_pg5 <- chw_filtered %>% as.data.frame() %>%
          group_by(lganame) %>% 
          summarise("No. wards" = n_distinct(wardcode),
                    "No. CHWs" = sum(n_chw),
                    "Population reached" = as.integer(sum(coverage_pop)),
                    "Expected cases" = as.integer(sum(expected_mal))) %>%
          rename("LGA" = lganame)
        
        # total row
        total <- ward_tbl_pg5 %>%
          summarise(
            LGA = "Total",
            `No. wards` = sum(`No. wards`),
            `No. CHWs` = sum(`No. CHWs`),
            `Population reached` = sum(`Population reached`),
            `Expected cases` = sum(`Expected cases`)
          )
        
        # bind the total row to the original table
        ward_tbl_pg5 <- bind_rows(ward_tbl_pg5, total)
        
        # return table
        ward_tbl_pg5
      })) # end of table
      
    })
    
    
    # # Ensure map renders correctly when the tab is shown
    # observeEvent(input$tabs, {
    #   if (input$tabs == "view_map_tab") {
    #     runjs('
    #     setTimeout(function() {
    #       $("#map_pg4").each(function() {
    #         this._leaflet_map.invalidateSize();
    #       });
    #     }, 10);
    #   ')
    #   }
    # })
    
    # # to make sure that map is appeared when tab is clicked
    # observeEvent(input$main_nav, {
    #   if (input$main_nav == "view_map") {
    #     delay(100, { leafletProxy("map_pg4") %>% invalidateSize() })
    #   }
    # })

  }) # end of algo_update
}

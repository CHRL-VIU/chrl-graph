#### custom graphs ####

output$header2 <- renderUI({
  req(input$custom_site)
  str1 <- paste0("<h2>", station_meta[[input$custom_site]][1], " (", station_meta[[input$custom_site]][2], " m)", "</h2>")
  HTML(paste(str1))
})



# pull data from mysql db based on user station and year input
custom_data_query <- reactive({
  req(input$custom_site)
  req(input$custom_year)

  conn <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn))
  query <- paste0("SELECT * FROM clean_", input$custom_site,  " where WatYr = ", input$custom_year, ";")
  data <- dbGetQuery(conn, query)

})

# reactive element to create year list based on available years for chosen station
observe({
  # need to find the year range of selected sites. finds the max of the two start years as the min.
  start_years <- station_meta[[input$custom_site]][3]
  min_year <- unname(unlist(lapply(start_years, max)))
  max_year <- weatherdash::wtr_yr(Sys.Date(), 10) # return current water year.
  year_range <- seq.int(min_year, max_year, by = 1)
  updateSelectInput(session, "custom_year", "Select Water Year:", year_range, selected = max_year)
})

# get available variables for selected station
output$varSelection <- renderUI({
  # get colnames from reactive dataset
  stnVars <- unname(unlist(station_meta[[input$custom_site]][6]))

  var_subset <- Filter(function(x) any(stnVars %in% x), varsDict)

  checkboxGroupInput(inputId = "custom_var", label = "Select one or two variables:", choices = var_subset, inline = FALSE, selected = "Air_Temp")
})

output$cleanSnowButton <- renderUI({
  req(input$custom_var)
  if("Snow_Depth" %in% input$custom_var){
    radioButtons("cleanSnowCstm", "Preform automated spike correction on Snow Depth?:", inline = T,
                 c("Yes" = "yes",
                   "No" = "no"),
                 selected = "no"
    )
  }

})

# ensure only two variables are selected
observe({
  if(length(input$custom_var) > 2){
    updateCheckboxGroupInput(session, "custom_var", selected = tail(input$custom_var, 2))
  }
})

output$slider <- renderUI({
  req(custom_data_query())
  sliderInput(inputId = "sliderTimeRange", label = "",
              min = min(custom_data_query()$DateTime),
              max = max(custom_data_query()$DateTime),
              value = c(min(custom_data_query()$DateTime),
                        max(custom_data_query()$DateTime)),
              step = 3600,
              width = '85%',
              height )
})

#filter preset data query
customDataFilter <-  reactive({
  req(input$sliderTimeRange)
  df <- custom_data_query()
  df %>%  filter(DateTime >= input$sliderTimeRange[1] & DateTime <= input$sliderTimeRange[2])
})

# final data set

finalData <- reactive({
  req(customDataFilter())
  req(input$custom_var)

  df <- customDataFilter()

  if("Snow_Depth" %in% input$custom_var) {
    req(input$cleanSnowCstm)
    if(input$cleanSnowCstm == "yes"){
      flag  <- ("Snow_Depth" %in% input$custom_var)
      clean <- input$cleanSnow
      df <- spike_clean(data = df, 'DateTime', 'Snow_Depth', spike_th = 10, roc_hi_th = 40, roc_low_th = 75)
    }
    else{return(df)}
  }
  return(df)
})


# plot for custom graphs page
output$plot1 <- renderPlotly({
  req(input$custom_site)
  req(input$custom_year)
  req(input$custom_var)
  req(finalData())

  df <- finalData() %>%
    select(DateTime, input$custom_var)

  varNames <- names(Filter(function(x) unlist(x) %in% input$custom_var, varsDict))

  if(length(input$custom_var) ==  2){
    
    weatherdash::graph_two(
      data = df,
      x = "DateTime",
      y1 = 2,
      y2 = 3, 
      y1_name = varNames[1], 
      y2_name = varNames[2]
    )

  } else {
    
    weatherdash::graph_one(
      data = df,
      x = "DateTime",
      y1 = 2,
      y1_name = varNames[1]
    )
  }

})

#### render partner logo ui ####
output$partnerLogoUI_custom <- renderUI({
  req(input$custom_site)
  cur_stn <- input$custom_site
  station_meta[[cur_stn]]['logos']
})

# create warning for down stations
observe({
  req(preset_data_query())
  req(input$custom_site)
  if(input$custom_site == 'mountcayley'){
    showModal(modalDialog(
      title = "Warning:",
      paste("This station is currently offline."),
      easyClose = T
    ))
  }
})



#### preset graphs ####

output$header <- renderUI({
  req(input$preset_site)
  str1 <- paste0("<h2>", station_meta[[input$preset_site]][1], " (", station_meta[[input$preset_site]][2], " m)", "</h2>")
  if(input$preset_site %in% list_stn_tipping_bucket_errs){
    HTML(paste(str1, p(tetrahedronDisclaimer, style = "color:red")))
  }
  else{HTML(paste(str1))}
})

# pull last 7 days from user selected station
preset_data_query <- reactive({
  req(input$preset_site)

  # query from db based on user selected station
  timeStart <- ((Sys.time() - hours(8)) - days(7)) #have to control for server being in UTC and db data stored as UTC even though is acutally PST
  conn <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn))
  if(input$preset_site == 'mountarrowsmith'){
    query <- paste0("SELECT DateTime, Air_Temp, RH, Snow_Depth, PC_Raw_Pipe, Wind_Speed, Wind_Dir FROM clean_",input$preset_site," WHERE DateTime >= '",timeStart, "'")
    data <- dbGetQuery(conn, query)
  }
  # specific case for tetrehedron while snow depth is out
  else if(input$preset_site == 'tetrahedron'){
    query <- paste0("SELECT DateTime, Air_Temp, RH, SWE, PP_Tipper, Wind_Speed, Wind_Dir FROM clean_",input$preset_site," WHERE DateTime >= '",timeStart, "'")
    data <- dbGetQuery(conn, query)
  }
  else{
    query <- paste0("SELECT DateTime, Air_Temp, RH, Snow_Depth, PP_Tipper, Wind_Speed, Wind_Dir FROM clean_",input$preset_site," WHERE DateTime >= '",timeStart, "'")
    data <- dbGetQuery(conn, query)
  }
})

# create warning for delayed transmission and down stations
observe({
  req(preset_data_query())
  req(input$preset_site)
  if(input$smenu == "wkly_graph"){
    if(length(preset_data_query()$DateTime) == 0){
      showModal(modalDialog(
        title = "Warning:",
        paste("The last transmission was more than 7 days ago. Please see the custom graphs page for latest available data."),
        easyClose = T
      ))
    } else if (as.integer(difftime(Sys.time() - hours(8), 
                                   max(preset_data_query()$DateTime), 
                                   unit = "hours")) > 1){
      showModal(modalDialog(
        title = "Warning:",
        paste("The last transmission was ", 
              as.integer(difftime(Sys.time() - hours(8), 
                                  max(preset_data_query()$DateTime), 
                                  unit = "hours")), "hours ago."),
        easyClose = T
      ))
    } else if (input$preset_site %in% down_stations){
      showModal(modalDialog(
        title = "Warning:",
        paste("This station is currently offline."),
        easyClose = T
      ))
    }
  }
})



# create data summary table
output$precipTable <- renderTable({
  req(preset_data_query())
  req(input$preset_site)
  if(input$preset_site == 'mountarrowsmith'){
    # no rain at tett so have to disable table - precip total too erroneous to show

    precipSummary <- NULL
    return(precipSummary)
  }

  else{
    # filter df and remove erroneous tipper vals
    df <- preset_data_query() %>%
      select(DateTime, PP_Tipper) %>%
      mutate(PP_Tipper = ifelse(PP_Tipper < 0, 0, PP_Tipper)) %>% # remove negatives
      mutate(PP_Tipper = ifelse(PP_Tipper > 30, 0, PP_Tipper)) # filter erroneous tip dat
    if(length(df$DateTime) > 0){
      # calculate averages
      currentTime <- max(df$DateTime)

      # filter by hours
      hr12 <- df %>%
        filter(DateTime >= currentTime - hours(12))
      hr24 <- df %>%
        filter(DateTime >= currentTime - hours(24))
      hr48 <- df %>%
        filter(DateTime >= currentTime - hours(48))
      hr72 <- df %>%
        filter(DateTime >= currentTime - hours(72))

      # construct table with 12, 24, 48, 72 hr summaries
      precipSummary <- data.frame(
        c("12", "24", "48", "72"),
        c(sum(hr12$PP_Tipper), sum(hr24$PP_Tipper), sum(hr48$PP_Tipper), sum(hr72$PP_Tipper))
      )

      colnames(precipSummary) <- c("Period (hours)", "Rainfall (mm)")
    } else{
      precipSummary <- data.frame(
        c("12", "24", "48", "72"),
        c(NA,NA,NA,NA)
      )
      colnames(precipSummary) <- c("Period (hours)", "Rainfall (mm)")

    }

    return(precipSummary)
  }
}, width = '100%')

# plot temperature and rh for weekly summary page

output$plot_T_RH <- renderPlotly({
  req(preset_data_query())
  req(input$preset_site)
  
  weatherdash::graph_two(
    data = preset_data_query(),
    x = 'DateTime',
    y1 = 'Air_Temp',
    y2 = 'RH',
    y1_name = "Air Temperature (&deg;C)",
    y2_name = "Relative Humidity (%)",
    margin = marg
)
  
})

# plot snow and tipping bucket precip for weekly summary page

output$plot_Snow <- renderPlotly({
  req(preset_data_query())
  req(input$preset_site)
  
  if(input$preset_site == 'mountarrowsmith'){
    df <- preset_data_query() %>%
      select(DateTime, snow = Snow_Depth, precip = PC_Raw_Pipe) %>%
      mutate(precip = ifelse(precip < 0, 0, precip))

    precipName <- "Total Precip (mm)"
    snowName <- "Snow Depth (cm)"

  }
  # specific case for tetrahedron while snow depth is out
  
  else if(input$preset_site == 'tetrahedron'){
    df <- preset_data_query() %>%
      select(DateTime, snow = SWE, precip = PP_Tipper) %>%
      mutate(precip = ifelse(precip < 0, 0, precip))
    
    precipName <- "Rain (mm)"
    snowName <- "Snow Water Equivalent (mm)"
    
  }
  else {
    df <- preset_data_query() %>%
      select(DateTime, snow = Snow_Depth, precip = PP_Tipper)
    precipName <- "Rain (mm)"
    snowName <- "Snow Depth (cm)"
  }

  # clean snow depth data
if(input$cleanSnow == "yes"){
  
  df_cln <- spike_clean(data = df, 'DateTime', 'snow', spike_th = 10, roc_hi_th = 40, roc_low_th = 75)
  weatherdash::graph_two(
    data = as.data.frame(df_cln),
    x = 'DateTime',
    y1 = 'snow_clean',
    y2 = 'precip',
    y1_name = snowName,
    y2_name = precipName,
    margin = marg
  )
  
} else{
  weatherdash::graph_two(
    data = df,
    x = 'DateTime',
    y1 = 'snow',
    y2 = 'precip',
    y1_name = snowName,
    y2_name = precipName,
    margin = marg
  )
  
  }
})

output$windplots <- renderUI({
  if(!input$preset_site == 'lowercain'){
  wellPanel(
    fluidRow(
      column(6,
             h4("Wind Speed Frequency and Direction (24 hr)", align = "center"),
             plotlyOutput("plot_wind_24hr")
      ),
      column(6,
             h4("Wind Speed Frequency and Direction (7 Days)", align = "center"),
             plotlyOutput("plot_wind_wk")
      )
    )
  )
  }
  else{
    wellPanel(
      HTML(paste0("<h5>Please check out <a href='/?_inputs_&preset_site=%22cainridgerun%22&sidebarCollapsed=false&custom_site=%22apelake%22&smenu=%22wkly_graph%22&custom_year=%222020%22'>Cain Ridge Run</a> for closest available wind speed.</h5"))
     )
  }
})

output$plot_wind_24hr <- renderPlotly({
  req(preset_data_query())
  req(input$preset_site)
  wind_24hr <- preset_data_query() |> 
    filter(DateTime > max(DateTime) - 60*60*24)
    
  weatherdash::wind_rose(wind_24hr, 'DateTime', 'Wind_Speed', 'Wind_Dir', spd_unit = 'km/h')
})

output$plot_wind_wk <- renderPlotly({
  req(preset_data_query())
  req(input$preset_site)
  wind_1wk <- preset_data_query() |> 
    filter(DateTime > max(DateTime) - 60*60*24*7)
  
  weatherdash::wind_rose(wind_1wk, 'DateTime', 'Wind_Speed', 'Wind_Dir', spd_unit = 'km/h')
})

#### render partner logo ui ####
output$partnerLogoUI <- renderUI({
  req(input$preset_site)
  cur_stn <- input$preset_site
  station_meta[[cur_stn]]['logos']
})

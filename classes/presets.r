#### preset graphs ####

# Header
output$header <- renderUI({
  req(input$preset_site)
  str1 <- paste0("<h2>", station_meta[[input$preset_site]][1], " (", station_meta[[input$preset_site]][2], " m)", "</h2>")
  if(input$preset_site %in% list_stn_tipping_bucket_errs){
    HTML(paste(str1, p(tetrahedronDisclaimer, style = "color:red")))
  } else {
    HTML(paste(str1))
  }
})

# Reactive: pull last 7 days of data for selected station
preset_data_query <- reactive({
  req(input$preset_site)
  timeStart <- (Sys.time() - hours(8)) - days(7)  # adjust for UTC storage
  
  conn <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn))
  
  # Get SQL column mapping for this station
  sql_cols <- get_db_vars(input$preset_site, preset_vars)
  
  # Build query dynamically
  query_cols <- c("DateTime", unname(sql_cols))  # DateTime + mapped columns
  
  table_name <- if (input$preset_site == "uppercruickshank") {
    paste0("qaqc_", input$preset_site)
  } else {
    paste0("clean_", input$preset_site)
  }
  
  query <- paste0(
    "SELECT ", paste(query_cols, collapse = ", "), 
    " FROM ", table_name,
    " WHERE DateTime >= '", timeStart, "'"
  )
  
  dbGetQuery(conn, query)
})

# Warning modal for delayed transmission or offline stations
observe({
  req(preset_data_query())
  req(input$preset_site)
  if(input$smenu == "wkly_graph"){
    dt <- preset_data_query()$DateTime
    if(length(dt) == 0){
      showModal(modalDialog(
        title = "Warning:",
        "The last transmission was more than 7 days ago. Please see the custom graphs page for latest available data.",
        easyClose = TRUE
      ))
    } else if(as.integer(difftime(Sys.time() - hours(8), max(dt), units = "hours")) > 1){
      showModal(modalDialog(
        title = "Warning:",
        paste("The last transmission was", 
              as.integer(difftime(Sys.time() - hours(8), max(dt), units = "hours")), 
              "hours ago."),
        easyClose = TRUE
      ))
    } else if(input$preset_site %in% down_stations){
      showModal(modalDialog(
        title = "Warning:",
        "This station is currently offline.",
        easyClose = TRUE
      ))
    }
  }
})

# Precipitation summary table
output$precipTable <- renderTable({
  req(preset_data_query())
  req(input$preset_site)
  
  if(input$preset_site == "mountarrowsmith"){
    return(NULL)  # no tipper rain at Mount Arrowsmith
  }
  
  sql_cols <- get_db_vars(input$preset_site, preset_vars)
  tipper_col <- sql_cols[["Tipping Bucket Increment (mm)"]]
  
  df <- preset_data_query() %>%
    select(DateTime, all_of(tipper_col)) %>%
    rename(Tipper = all_of(tipper_col)) %>%
    mutate(Tipper = ifelse(Tipper < 0 | Tipper > 30, 0, Tipper))  # clean tipper data
  
  if(length(df$DateTime) == 0){
    precipSummary <- data.frame(
      "Period (hours)" = c("12", "24", "48", "72"),
      "Rainfall (mm)" = NA
    )
  } else {
    currentTime <- max(df$DateTime)
    hr12 <- df %>% filter(DateTime >= currentTime - hours(12))
    hr24 <- df %>% filter(DateTime >= currentTime - hours(24))
    hr48 <- df %>% filter(DateTime >= currentTime - hours(48))
    hr72 <- df %>% filter(DateTime >= currentTime - hours(72))
    
    precipSummary <- data.frame(
      "Period (hours)" = c("12", "24", "48", "72"),
      "Rainfall (mm)" = c(sum(hr12$Tipper), sum(hr24$Tipper), sum(hr48$Tipper), sum(hr72$Tipper))
    )
  }
  
  precipSummary
}, width = "100%")

# Temperature and RH plot
output$plot_T_RH <- renderPlotly({
  req(preset_data_query())
  req(input$preset_site)
  
  sql_cols <- get_db_vars(input$preset_site, preset_vars)
  y1_col <- sql_cols[["Air Temperature (°C)"]]
  y2_col <- sql_cols[["Relative Humidity (%)"]]
  
  weatherdash::graph_two(
    data = preset_data_query(),
    x = "DateTime",
    y1 = y1_col,
    y2 = y2_col,
    y1_name = "Air Temperature (&deg;C)",
    y2_name = "Relative Humidity (%)",
    margin = marg
  )
})

# Snow + precip plot
output$plot_Snow <- renderPlotly({
  req(preset_data_query())
  req(input$preset_site)
  
  sql_cols <- get_db_vars(input$preset_site, preset_vars)
  snow_col <- sql_cols[["Snow Depth (cm)"]]
  
  if(input$preset_site == "mountarrowsmith"){
    precip_col <- sql_cols[["Stand Pipe Raw (mm)"]]
    precip_name <- "Total Precip (mm)"
  } else {
    precip_col <- sql_cols[["Tipping Bucket Increment (mm)"]]
    precip_name <- "Rain (mm)"
  }
  
  df <- preset_data_query() %>%
    select(DateTime, snow = all_of(snow_col), precip = all_of(precip_col)) %>%
    mutate(precip = ifelse(precip < 0, 0, precip))
  
  snow_name <- "Snow Depth (cm)"
  
  if(input$cleanSnow == "yes"){
    df_cln <- spike_clean(df, "DateTime", "snow", spike_th = 10, roc_hi_th = 40, roc_low_th = 75)
    weatherdash::graph_two(
      data = as.data.frame(df_cln),
      x = "DateTime",
      y1 = "snow_clean",
      y2 = "precip",
      y1_name = snow_name,
      y2_name = precip_name,
      margin = marg
    )
  } else {
    weatherdash::graph_two(
      data = df,
      x = "DateTime",
      y1 = "snow",
      y2 = "precip",
      y1_name = snow_name,
      y2_name = precip_name,
      margin = marg
    )
  }
})

# Wind plots UI
output$windplots <- renderUI({
  if(input$preset_site != "lowercain"){
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
  } else {
    wellPanel(
      HTML("<h5>Please check out <a href='/?_inputs_&preset_site=%22cainridgerun%22&sidebarCollapsed=false&custom_site=%22apelake%22&smenu=%22wkly_graph%22&custom_year=%222020%22'>Cain Ridge Run</a> for closest available wind speed.</h5>")
    )
  }
})

# Wind 24 hr
output$plot_wind_24hr <- renderPlotly({
  req(preset_data_query())
  req(input$preset_site)
  
  sql_cols <- get_db_vars(input$preset_site, preset_vars)
  ws_col <- sql_cols[["Wind Speed (km/h)"]]
  wd_col <- sql_cols[["Wind Direction (deg)"]]
  
  wind_24hr <- preset_data_query() %>% filter(DateTime > max(DateTime) - 60*60*24)
  weatherdash::wind_rose(wind_24hr, "DateTime", ws_col, wd_col, spd_unit = "km/h")
})

# Wind 7 day
output$plot_wind_wk <- renderPlotly({
  req(preset_data_query())
  req(input$preset_site)
  
  sql_cols <- get_db_vars(input$preset_site, preset_vars)
  ws_col <- sql_cols[["Wind Speed (km/h)"]]
  wd_col <- sql_cols[["Wind Direction (deg)"]]
  
  wind_1wk <- preset_data_query() %>% filter(DateTime > max(DateTime) - 60*60*24*7)
  weatherdash::wind_rose(wind_1wk, "DateTime", ws_col, wd_col, spd_unit = "km/h")
})

# Partner logo UI
output$partnerLogoUI <- renderUI({
  req(input$preset_site)
  cur_stn <- input$preset_site
  station_meta[[cur_stn]]['logos']
})
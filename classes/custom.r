#### custom graphs ####

# ---------- Header ----------
output$header2 <- renderUI({
  req(input$custom_site)
  
  str1 <- paste0(
    "<h2>",
    station_meta[[input$custom_site]][1],
    " (",
    station_meta[[input$custom_site]][2],
    " m)</h2>"
  )
  
  if (input$custom_site %in% list_stn_tipping_bucket_errs) {
    HTML(paste(
      str1,
      p(
        "The tipping bucket is currently malfunctioning at this station; please refer to total precipitation (stand pipe) instead.",
        style = "color:red"
      )
    ))
  } else {
    HTML(str1)
  }
})

# ---------- Year Selector ----------
observe({
  req(input$custom_site)
  
  start_years <- station_meta[[input$custom_site]][3]
  min_year <- unname(unlist(lapply(start_years, max)))
  max_year <- weatherdash::wtr_yr(Sys.Date(), 10)
  
  updateSelectInput(
    session,
    "custom_year",
    "Select Water Year:",
    seq.int(min_year, max_year),
    selected = max_year
  )
})

# ---------- Variable Selection ----------
output$varSelection <- renderUI({
  req(input$custom_site)
  
  stnVars <- unname(unlist(station_meta[[input$custom_site]][6]))
  
  var_subset <- Filter(
    function(x) any(stnVars %in% x),
    varsDict
  )
  
  checkboxGroupInput(
    inputId = "custom_var",
    label = "Select Variables:",
    choices = names(var_subset),
    inline = FALSE,
    selected = intersect(
      c("Air Temperature (°C)"),
      names(var_subset)
    )
  )
})

# ---------- Snow Depth Cleaning Button ----------
output$cleanSnowButton <- renderUI({
  req(input$custom_var)
  
  if ("Snow Depth (cm)" %in% input$custom_var) {
    radioButtons(
      "cleanSnowCstm",
      "Perform automated spike correction on Snow Depth?",
      inline = TRUE,
      choices = c("Yes" = "yes", "No" = "no"),
      selected = "no"
    )
  }
})

# ---------- Slider ----------
output$slider <- renderUI({
  req(input$custom_site, input$custom_year)
  
  conn <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn))
  
  # CRUICK HOTFIX
  table_name <- if (
    input$custom_site == "uppercruickshank" &&
    !is.null(input$custom_var) &&
    "Snow Water Equivalent (mm)" %in% input$custom_var
  ) {
    paste0("qaqc_", input$custom_site)
  } else {
    paste0("clean_", input$custom_site)
  }
  
  query <- paste0(
    "SELECT DateTime FROM ",
    table_name,
    " WHERE WatYr = ",
    input$custom_year,
    ";"
  )
  
  df <- dbGetQuery(conn, query)
  validate(need(nrow(df) > 0, "No data available for this year."))
  
  sliderInput(
    inputId = "sliderTimeRange",
    label = "",
    min = min(df$DateTime),
    max = max(df$DateTime),
    value = c(min(df$DateTime), max(df$DateTime)),
    step = 3600,
    width = "85%"
  )
})

# ---------- Filtered Clean Data ----------
customDataFilter <- reactive({
  req(input$custom_site, input$custom_year, input$sliderTimeRange)
  
  conn <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn))
  
  # CRUICK HOTFIX
  table_name <- if (
    input$custom_site == "uppercruickshank" &&
    !is.null(input$custom_var) &&
    "Snow Water Equivalent (mm)" %in% input$custom_var
  ) {
    paste0("qaqc_", input$custom_site)
  } else {
    paste0("clean_", input$custom_site)
  }
  
  query <- paste0(
    "SELECT * FROM ",
    table_name,
    " WHERE WatYr = ",
    input$custom_year,
    ";"
  )
  
  df <- dbGetQuery(conn, query)
  
  df %>%
    dplyr::filter(
      DateTime >= input$sliderTimeRange[1],
      DateTime <= input$sliderTimeRange[2]
    )
})

# ---------- Final Data (Dictionary Overrides Applied Here) ----------
final_custom_data <- reactive({
  req(customDataFilter(), input$custom_var)
  
  df <- customDataFilter()
  
  # resolve SQL column names using dictionaries.R
  sql_cols <- unlist(get_db_vars(input$custom_site, input$custom_var))
  
  df <- df %>%
    dplyr::select(DateTime, dplyr::any_of(sql_cols))
  
  # optional snow depth spike cleaning
  if (
    "Snow Depth (cm)" %in% input$custom_var &&
    !is.null(input$cleanSnowCstm) &&
    input$cleanSnowCstm == "yes"
  ) {
    snow_col <- sql_cols[input$custom_var == "Snow Depth (cm)"]
    
    if (length(snow_col) == 1 && snow_col %in% names(df)) {
      df <- spike_clean(
        data = df,
        x = "DateTime",
        y = snow_col,
        spike_th = 10,
        roc_hi_th = 40,
        roc_low_th = 75
      )
    }
  }
  
  df
})

# ---------- Plot ----------
output$plot1 <- renderPlotly({
  req(final_custom_data(), input$custom_var)
  
  df <- final_custom_data()
  cols_to_plot <- setdiff(names(df), "DateTime")
  validate(need(length(cols_to_plot) > 0, "No variables selected."))
  
  # Map SQL cols -> display names
  var_map <- varsDict
  if(input$custom_site %in% c("plummerhut", "placeglacier")){
    var_map[["Air Temperature (\u00b0C)"]] <- "Air_Temp_2"
    var_map[["Air Temperature Alt (\u00b0C)"]] <- "Air_Temp"
    var_map[["Snow Depth (cm)"]] <-  "Snow_Depth_2"
    var_map[["Snow Depth Alt (cm)"]] <- "Snow_Depth"
  }
  label_map <- setNames(names(var_map), unlist(var_map)) # SQL col -> label
  
  # Build one Plotly trace per variable
  plots <- lapply(cols_to_plot, function(col){
    plot_ly(df, x = ~DateTime, y = as.formula(paste0("~`", col, "`")),
            type = 'scatter', mode = 'lines', name = label_map[col])
  })
  
  # Combine vertically with shared X-axis
  subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE) %>%
    layout(
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "#f5f5f5",
      margin = list(t = 100, b = 80, l = 80, r = 50)
    )
})

# ---------- Plot UI ----------
output$plot1_ui <- renderUI({
  req(input$custom_var)
  per_var_height <- 300  # pixels per subplot row
  total_height <- length(input$custom_var) * per_var_height
  
  plotlyOutput(
    "plot1",
    height = total_height,
    width = "100%"
  )
})


# ---------- Partner Logo ----------
output$partnerLogoUI_custom <- renderUI({
  req(input$custom_site)
  station_meta[[input$custom_site]][["logos"]]
})

# ---------- Down Station Warning ----------
observe({
  req(input$custom_site, preset_data_query())
  
  if (
    input$smenu == "cstm_graph" &&
    input$custom_site %in% down_stations
  ) {
    showModal(modalDialog(
      title = "Warning:",
      "This station is currently offline.",
      easyClose = TRUE
    ))
  }
})

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

output$plot1_ui <- renderUI({
  req(input$custom_var)
  
  plot_ids <- paste0("custom_plot_", seq_along(input$custom_var))
  
  tagList(
    lapply(plot_ids, function(id) {
      plotlyOutput(id, height = "300px")
    })
  )
})


observe({
  req(input$custom_var)
  req(final_custom_data())
  
  custom_data <- final_custom_data()
  
  # Get the database column names corresponding to the selected
  # display names.
  sql_cols <- unlist(
    get_db_vars(input$custom_site, input$custom_var)
  )
  
  # Create one plot for each selected variable.
  for (i in seq_along(input$custom_var)) {
    
    local({
      plot_id <- paste0("custom_plot_", i)
      display_name <- input$custom_var[i]
      sql_col <- sql_cols[i]
      
      output[[plot_id]] <- renderPlotly({
        
        req(
          !is.null(custom_data),
          nrow(custom_data) > 0,
          sql_col %in% names(custom_data)
        )
        
        plot_data <- custom_data %>%
          dplyr::select(
            DateTime,
            dplyr::all_of(sql_col)
          )
        
        req(nrow(plot_data) > 0)
        
        names(plot_data)[2] <- "value"
        
        p <- ggplot(
          plot_data,
          aes(x = DateTime, y = value)
        ) +
          geom_line() +
          labs(
            x = NULL,
            y = display_name
          ) +
          theme_bw()
        
        plotly::ggplotly(p) |>
          layout(
            plot_bgcolor = "#f5f5f5",
            paper_bgcolor = "#f5f5f5",
            margin = list(
              b = 40,
              r = 50,
              l = 70,
              t = 10
            )
          )
      })
    })
  }
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

#### Station Comparisons ####

#
# output$header_compare_stn <- renderUI({
#   req(input$station_site)
#   str1 <- paste0("<h2>", station_meta[[input$station_site]][1], " (", station_meta[[input$station_site]][2], " m)", "</h2>")
#   HTML(paste(str1))
# })

# # reactive element to create year list based on available years for chosen station
observe({
  req(input$station_site)
  # need to find the year range of selected sites. finds the max of the two start years as the min.
  yr_list <- lapply(input$station_site, function(x) station_meta[[x]][[3]])
  min_year <- max(unlist(yr_list))
  max_year <- weatherdash::wtr_yr(Sys.Date(), 10)
  year_range <- seq.int(min_year, max_year, by = 1)
  updateSelectInput(session, "station_year", "Select Water Year to Compare: ", year_range, selected = max_year)
})

# get intersection of variables for selected stations
output$varSelection_stn <- renderUI({
  # get colnames from reactive dataset
  allStnVars <- sapply(input$station_site, function(x) station_meta[[x]][6])
  intersect_vars <- Reduce(intersect, allStnVars)
  var_subset <- Filter(function(x) any(intersect_vars %in% x), varsDict)
  radioButtons(inputId = "compare_var", label = "Select one variable: ", choices = var_subset, inline = FALSE, selected = "Air_Temp")
})

stn_compare_data_query <- reactive({
  req(input$station_site)
  req(input$compare_var)
  req(input$station_year)
  
  withProgress(message = 'Requesting Data... ', value = 1, {
    conn <- do.call(DBI::dbConnect, args)
    on.exit(DBI::dbDisconnect(conn))
    
    query <- lapply(input$station_site, function(x) paste0("SELECT DateTime, WatYr, ", input$compare_var, ", '", x,"' as station FROM clean_", x," where WatYr = ", input$station_year)) %>%
      paste(collapse = " UNION ")
    
    data <- dbGetQuery(conn, query)
  })
  
})



# plot all years
output$plot_compare_stn <- renderPlotly({
  req(stn_compare_data_query())
  req(input$station_site)
  req(input$compare_var)
  req(input$compare_year)
  
  
  df <- stn_compare_data_query()
  
  varNames <- names(Filter(function(x) unlist(x) %in% input$compare_var, varsDict))
  
  plot_ly(df,
          x = ~DateTime, # plot without year
          y = ~df[,3],
          text = ~DateTime, # bring in datetime with year for hover text
          color = as.factor(df$station),
          colors = cbs_pal,
          type = "scatter",
          mode = "lines",
          hovertemplate = paste('<b>%{text}</b><br>%{yaxis.title.text}: %{y}<extra></extra>')
          
  ) %>%
    
    layout(
      xaxis = c(generalAxLayout),
      yaxis = c(generalAxLayout, list(title=paste0("<b>",varNames[1],"</b>"))),
      margin = list(l = 50),
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "#f5f5f5",
      hovermode = 'x'
    )
  
  
})


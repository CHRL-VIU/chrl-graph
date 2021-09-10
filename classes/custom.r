#### custom graphs ####

output$header2 <- renderUI({
  req(input$custom_site)
  str1 <- paste0("<h2>", stationMeta[[input$custom_site]][1], " (", stationMeta[[input$custom_site]][2], " m)", "</h2>")
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

# # create warning for delayed transmission
# observe({
#   req(input$custom_site)
#   req(custom_data_query())
#   if(input$smenu == "cstm_graph"){
#     sysTimePST <- Sys.time() - hours(8)
#     latestTrans <- max(custom_data_query()$DateTime)
#     timeDiff <- as.integer(difftime(sysTimePST, latestTrans, unit = "hours"))
#     if(timeDiff > 1){
#       showModal(modalDialog(
#         title = "Warning:",
#         paste("The last transmission was ", timeDiff, "hours ago."),
#         easyClose = T
#       ))
#     }
#   }
# })

# reactive element to create year list based on available years for chosen station
observe({
  # need to find the year range of selected sites. finds the max of the two start years as the min.
  start_years <- stationMeta[[input$custom_site]][3]
  min_year <- unname(unlist(lapply(start_years, max)))
  max_year <- wtr_yr(Sys.Date(), 10) # return current water year.
  year_range <- seq.int(min_year, max_year, by = 1)
  updateSelectInput(session, "custom_year", "Select Water Year:", year_range, selected = max_year)
})

# get available variables for selected station
output$varSelection <- renderUI({
  # get colnames from reactive dataset
  stnVars <- unname(unlist(stationMeta[[input$custom_site]][6]))

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
      df <- cleanSnowData(data = df, spike_th = 10, roc_hi_th = 40, roc_low_th = 75)
    }
    else{return(df)}
  }
  return(df)
})


# plot two variables on one chart
output$plot1 <- renderPlotly({
  req(input$custom_site)
  req(input$custom_year)
  req(input$custom_var)
  req(finalData())

  df <- finalData() %>%
    select(DateTime, input$custom_var)

  varNames <- names(Filter(function(x) unlist(x) %in% input$custom_var, varsDict))

  if(length(input$custom_var) ==  2){

    plot_ly(data = df,
            type = "scatter",
            mode ="lines",
            #hovertemplate = paste('%{x}<br>%{yaxis.title.text}: %{y}<extra></extra>')
            hoverinfo = 'text',
            text = ~paste(DateTime, '</br></br><b>',varNames[1],': ', round(get(names(df)[2]), 2), '</br><b>', varNames[2], ': ', round(get(names(df)[3]), 2))
    ) %>%
      add_trace(x = ~DateTime,
                y = ~(df)[[2]],
                name = varNames[1],
                line = list(color = lineGraphColour$colOne, width = 1)) %>%
      add_trace(x = ~DateTime,
                y = ~(df)[[3]],
                name = varNames[2],
                yaxis = "y2",
                line = list(color = lineGraphColour$colTwo, width = 1)) %>%

      layout(
        xaxis = c(generalAxLayout, list(title = "")),
        yaxis = c(generalAxLayout, list(title=paste0("<b>",varNames[1],"</b>"),titlefont = list(color = lineGraphColour$colOne))),
        yaxis2 = c(generalAxLayout, list(titlefont = list(color = lineGraphColour$colTwo), overlaying = "y", side = "right", title = paste0("<b>",varNames[2],"</b>"))),
        margin = marg,
        showlegend = F,
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "#f5f5f5"
      )
  } else {
    plot_ly(df) %>%
      add_lines(x = ~DateTime,
                y = ~(df)[[2]],
                name = varNames[1],
                line = list(color = lineGraphColour$colOne, width = 1),
                #hovertemplate = paste('%{x}<br>%{yaxis.title.text}: %{y:.2f}<extra></extra>')
                hoverinfo = 'text',
                text = ~paste(DateTime, '</br></br><b>',varNames[1],': ', round(get(names(df)[2]), 2))
      ) %>%
      layout(
        xaxis = c(generalAxLayout, list(title = "")),
        yaxis = c(generalAxLayout, list(title=paste0("<b>",varNames[1],"</b>"),titlefont = list(color = lineGraphColour$colOne))),
        margin = marg,
        showlegend = F,
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "#f5f5f5"
      )

  }

})

#### render partner logo ui ####
output$partnerLogoUI_custom <- renderUI({
  req(input$custom_site)
  cur_stn <- input$custom_site
  stationMeta[[cur_stn]]['logos']
})



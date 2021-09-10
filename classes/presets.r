#### preset graphs ####

output$header <- renderUI({
  req(input$preset_site)
  str1 <- paste0("<h2>", stationMeta[[input$preset_site]][1], " (", stationMeta[[input$preset_site]][2], " m)", "</h2>")
  if(input$preset_site == 'tetrahedron'){
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
  if(input$preset_site == 'tetrahedron'){
    query <- paste0("SELECT DateTime, Air_Temp, RH, Snow_Depth, PC_Raw_Pipe, Wind_Speed, Wind_Dir FROM clean_",input$preset_site," WHERE DateTime >= '",timeStart, "'")
    data <- dbGetQuery(conn, query)
  }
  else{
    query <- paste0("SELECT DateTime, Air_Temp, RH, Snow_Depth, PP_Tipper, Wind_Speed, Wind_Dir FROM clean_",input$preset_site," WHERE DateTime >= '",timeStart, "'")
    data <- dbGetQuery(conn, query)
  }
})

# create warning for delayed transmission
observe({
  req(preset_data_query())
  req(input$preset_site)
  if(input$smenu == "wkly_graph"){
    if(length(preset_data_query()$DateTime) == 0){
      showModal(modalDialog(
        title = "Warning:",
        paste("The last transmission was more than 7 days ago. Please checkout the custom graphs page for latest available data."),
        easyClose = T
      ))
    } else if (as.integer(difftime(Sys.time() - hours(8), max(preset_data_query()$DateTime), unit = "hours")) > 1){
      showModal(modalDialog(
        title = "Warning:",
        paste("The last transmission was ", as.integer(difftime(Sys.time() - hours(8), max(preset_data_query()$DateTime), unit = "hours")), "hours ago."),
        easyClose = T
      ))
    }
  }
})



# create data summary table
output$precipTable <- renderTable({
  req(preset_data_query())
  req(input$preset_site)
  if(input$preset_site == 'tetrahedron'){
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

# plot two variables on one chart
output$plot_T_RH <- renderPlotly({
  req(preset_data_query())
  req(input$preset_site)

  df <- preset_data_query() %>%
    select(DateTime, Air_Temp, RH)

  plot_ly(data = df,
          mode ="lines",
          type = "scatter",
          #hovertemplate = paste('%{x}<br>%{yaxis.title.text}: %{y}<extra></extra>') # old way with one param per box
          hoverinfo = 'text',
          text = ~paste(DateTime, '</br></br>', '<b>Air Temperature (&deg;C): </b>', round(Air_Temp, 2), '</br>', '<b>Relative Humidity (%): </b>',round(RH, 2))) %>%
    add_trace(x = ~DateTime,
              y = ~Air_Temp,
              name = "Air Temperature (&deg;C)",
              line = list(color = lineGraphColour$colOne, width = 1)) %>%
    add_trace(x = ~DateTime,
              y = ~RH,
              name = "Relative Humidity (%)",
              yaxis = "y2",
              line = list(color = lineGraphColour$colTwo, width = 1)) %>%
    layout(
      xaxis = c(generalAxLayout, list(title = "")),
      yaxis = c(generalAxLayout, list(title="<b>Air Temperature (&deg;C)</b>",titlefont = list(color = lineGraphColour$colOne))),
      yaxis2 = c(generalAxLayout, list(titlefont = list(color = lineGraphColour$colTwo),overlaying = "y",side = "right",title = "<b>Relative Humidity (%)</b>")),
      margin = marg,
      showlegend = F,
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "#f5f5f5"
    )

})


output$plot_Snow <- renderPlotly({
  req(preset_data_query())
  req(input$preset_site)

  if(input$preset_site == 'tetrahedron'){
    df <- preset_data_query() %>%
      select(DateTime, Snow_Depth, precip = PC_Raw_Pipe) %>%
      mutate(precip = ifelse(precip < 0, 0, precip))

    precipName <- "Total Precip (mm)"

  }
  else {
    df <- preset_data_query() %>%
      select(DateTime, Snow_Depth, precip = PP_Tipper)
    precipName <- "Rain (mm)"
  }

  # clean snow depth data
if(input$cleanSnow == "yes"){
  df <- cleanSnowData(data = df, spike_th = 10, roc_hi_th = 40, roc_low_th = 75)
}


  plot_ly(data = df,
          type = "scatter",
          mode ="lines",
          hoverinfo = 'text',
          text = ~paste(DateTime, '</br></br>', '<b>Snow Depth (cm): </b>', round(Snow_Depth, 2), '</br>', '<b>', precipName, '</b>',round(precip, 2))
          #hovertemplate = paste('%{x}<br>%{yaxis.title.text}: %{y}<extra></extra>')
  ) %>%
    add_trace(x = ~DateTime,
              y = ~Snow_Depth,
              name = "Snow Depth (cm)",
              line = list(color = lineGraphColour$colOne, width = 1)) %>%
    add_trace(x = ~DateTime,
              y = ~precip,
              name = precipName,
              yaxis = "y2",
              line = list(color = lineGraphColour$colTwo, width = 1)) %>%
    layout(
      xaxis = c(generalAxLayout, list(title = "")),
      yaxis = c(generalAxLayout, list(title="<b>Snow Depth (cm)</b>",titlefont = list(color = lineGraphColour$colOne))),
      yaxis2 = c(generalAxLayout, list(titlefont = list(color = lineGraphColour$colTwo),overlaying = "y",side = "right",title = paste0('<b>', precipName, '</b>'))),
      margin = marg,
      showlegend = F,
      plot_bgcolor = "#f5f5f5",
      paper_bgcolor = "#f5f5f5"
    )
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
  df <- preset_data_query()
  plotWindRose(df, hrs = 24, plotTitle = "")
})

output$plot_wind_wk <- renderPlotly({
  req(preset_data_query())
  req(input$preset_site)
  df <- preset_data_query()
  plotWindRose(df, hrs = 168)
})

#### render partner logo ui ####
output$partnerLogoUI <- renderUI({
  req(input$preset_site)
  cur_stn <- input$preset_site
  stationMeta[[cur_stn]]['logos']
})

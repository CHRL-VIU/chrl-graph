#### Station Map Tab ####

#render leaflet
output$map <- renderLeaflet({
  leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles("CartoDB.Positron") %>%
    # addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    # addLayersControl(baseGroups = c("Default", "Satellite"), options = layersControlOptions(collapsed = FALSE), position = ("topleft")) %>%
    setView(lng = -129.5, lat = 53.5, zoom = 6) %>%
    addMarkers(data = stnCoords, layerId = ~Name, ~Longitude, ~Latitude, label = ~htmltools::htmlEscape(stnCoords$Label[stnCoords$Name == Name]), labelOptions = labelOptions(textsize = "12px"))
})

# get last hour from user selected station
map_data_query <- reactive({
  conn <- do.call(DBI::dbConnect, args)
  on.exit(DBI::dbDisconnect(conn))
  req(input$map_marker_click)

  # get last hour
  query <- paste0("SELECT DateTime, Air_Temp, RH, Snow_Depth, PP_Tipper, Wind_Speed, Wind_Dir FROM clean_",input$map_marker_click$id," WHERE DateTime=(select max(DateTime) from clean_",input$map_marker_click$id,");")
  data <- dbGetQuery(conn, query)

  return(data)
})

# Show a popup at the given location
showPopup <- function(stn_click, lat, lng, zoom) {

  station_name <- stnCoords$Label[stnCoords$Name == stn_click$id]
  wx_last_hr <- tags$h5("Coordinates:", round(lat, 3), "\u00B0 N, ", round(lng, 3), "\u00B0 W", tags$br(),
                        "Elevation:", stationMeta[[stn_click$id]][2], "m", tags$br(),
                        "Last Transmission:", map_data_query()$DateTime, tags$br(), tags$br(),
                        "Temperature:", round(map_data_query()$Air_Temp, 2), "\u00B0C", tags$br(),
                        "Wind Speed (km/h): ", round(map_data_query()$Wind_Speed, 2), tags$br(),
                        "Wind Direction (deg): ", round(map_data_query()$Wind_Dir, 2),tags$br(),
                        "Snow Depth (cm): ", round(map_data_query()$Snow_Depth, 2))
  graph_icon <- tags$a(href= paste0("?_inputs_&preset_site=%22",stn_click$id,"%22&custom_site=%22apelake%22&smenu=%22wkly_graph%22"), target="_blank", icon("fas fa-chart-line", "fa-2x"))
  pic_icon <- tags$a(href= paste0("http://viu-hydromet-wx.ca/wx_station_images_reduced/", stn_click$id,".png"), target="_blank", icon("fas fa-camera", "fa-2x"), style="float: right;")

    if(length(data) == 0){
      content <- as.character(tagList(
        tags$h3(station_name),
        tags$h5("Station is offline.", style = "color:red"),
        graph_icon,
        pic_icon))
    }
    if(difftime(Sys.time(), max(map_data_query()$DateTime)) > hours(48)){
      content <- as.character(tagList(
        tags$h3(station_name),
        tags$h5("WARNING: This transmission is more than 48 hours old.", style = "color:red"),
        wx_last_hr,
        graph_icon,
        pic_icon))

    }
    else{
      content <- as.character(tagList(
        tags$h3(station_name),
        wx_last_hr,
        graph_icon,
        pic_icon))
    }

  leafletProxy("map") %>%
    setView(lng, lat, zoom = zoom) %>%
    addPopups(lng, lat, content, layerId = stn_click$id)
}

# # When map is clicked, show a popup with city info
observe({
  leafletProxy("map") %>% clearPopups()
  event <- input$map_marker_click
  zoom <- isolate(input$map_zoom)

  if (is.null(event))
    return()

  isolate({
    showPopup(event, event$lat, event$lng, zoom)
  })
})


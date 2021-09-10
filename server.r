#### server ####
server <- function(input, output, session) {
##### create disclaimer dialog box and declare it open on start up #####

  # initial state for modal so its only shown once
  modalShown <- FALSE

  observe({
    if(input$smenu == "map"){
      if(!modalShown){
        showModal(disclaimer_EN)
        modalShown <<- TRUE # need to use super asigner to affect global var
      }
    }
  })

  observeEvent(input$ack, {
    removeModal()
  })

  observeEvent(input$FN, {
    removeModal()
    showModal(disclaimer_FR)
  })

  observeEvent(input$EN, {
    showModal(disclaimer_EN)
  })


#### source rest of app ####
  source("classes/map.r", local = TRUE)
  source("classes/modal.r", local = TRUE)
  source("classes/presets.r", local = TRUE)
  source("classes/custom.r", local = TRUE)
  source("classes/annual.r", local = TRUE)
  source("classes/station_compare.r", local = TRUE)


# enable bookmarking on URL
observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

onBookmarked(function(url) {
  updateQueryString(url)
})

enableBookmarking("url")

# inputs to exclude from URL
setBookmarkExclude(c("map_click", "cleanSnow", "cleanSnowCstm","plotly_hover-A", "annual_site", "sidebarItemExpanded", "plotly_hover", "map_bounds", "map_center", "map_zoom", "map_groups",".clientValue-default-plotlyCrosstalkOpts", "plotly_afterplot-A", "sliderTimeRange", "map_marker_mouseout", "map_marker_mouseover","map_marker_click", "compare_year", "compare_site","compare_var", "plotly_relayout-A", "FN", "EN", "ack", "sidebarCollapsed", "station_site", "station_year", "custom_year","custom_var"))

}

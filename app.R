library(dplyr)
library(DBI)
library(plotly)
library(lubridate)
library(leaflet)
library(tsibble)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)
library(weatherdash)

Sys.setenv(TZ = 'UTC')

# set colorblind safe color pallet 
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette

cbs_pal <-
  c(
    "#000000",
    "#E69F00",
    "#009E73",
    "#F0E442",
    "#CC79A7",
    "#56B4E9"
  )

# grab login creds
source("config.r")

# set list of stns with tipping bucket problems 
list_stn_tipping_bucket_errs <- 'mountarrowsmith'

# set the down stations to show a popup model for

down_stations <- NA

# Station Coords
stnCoords <- read.csv("stnloc.csv")

# set initial staiton
cur_stn = "apelake"

# add message to display at top of graph pages
siteNoticeMsg <- "Note: To automatically load your favourite station when you visit our site, select a station from the 'Choose a Weather Station' dropdown and then bookmark the link in the address bar. <br/>"
tetrahedronDisclaimer <- "The tipping bucket is currently malfunctioning at this station and total precipitation (stand pipe) is shown instead."

# load graphing presets
source('R/graph-presets.R')

# load logo paths, parameter dictionary, stn name dictionary
source('R/dictionaries.R')

# Station Meta List - this is a nested list 
source('R/station-meta-list.R')

#### user interface ####
ui <- function(request) {
  dashboardPage(title = "CHRL Real-time Weather",
                dashboardHeader(title = tags$a(href='http://www.viu-hydromet-wx.ca/',
                                               tags$img(src="logo_update.png",
                                                        style="padding-right:10px;padding-bottom:10px",
                                                        height = 60), target="_blank")
                ),
                dashboardSidebar(
                  collapsed = F,
                  uiOutput("sidebarControls"),
                  sidebarMenu(id = "smenu",
                              menuItem("Current Conditions", tabName = "map", icon = icon("fas fa-map")),
                              menuItem("Weekly Summary", tabName = "wkly_graph", icon = icon("fas fa-chart-line")),
                              menuItem("Custom Graphs", tabName = "cstm_graph", icon = icon("fas fa-chart-line")),
                              menuItem("Annual Comparisons", tabName = "ann_compare", icon = icon("fas fa-chart-line")),
                              menuItem("Station Comparisons", tabName = "stn_compare", icon = icon("fas fa-chart-line")),
                              menuItem("Webcams", icon = icon("fas fa-camera"), href = "http://viu-hydromet-wx.ca/webcam-viewer/")
                  )
                ),
                
                dashboardBody(
                  # add dashboard styling
                  customTheme,
                  # add styling for graph formating
                  tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "viu.css")
                  ),
                  tabItems(
                    tabItem("map",
                            div(class="outer",
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("www/mapstyles.css")
                                ),
                                
                                leafletOutput("map", width="100%", height="100%")
                                
                            )
                            
                    ),
                    tabItem("wkly_graph",
                            fluidRow(
                              column(12,
                                     h1("Weekly Summary", align = "center")
                              )
                            ),
                            fluidRow(
                              
                              
                              column(width = 3,
                                     # tags$head(
                                     #   tags$style(type="text/css", ".well { min-width: 320px; }")
                                     # ),
                                     selectInput("preset_site",
                                                 label = "Choose a Weather Station:",
                                                 choices = stnNameDict,
                                                 selected = cur_stn,
                                                 selectize = F),
                                     tableOutput("precipTable"),
                                     hr(style = "border-top: 1px solid #bfbfbf;"),
                                     htmlOutput('partnerLogoUI'),
                                     h5(".", align = "center", style = "color:whitesmoke") # blank row so images dont span below sidebar panel.
                                     
                                     
                              ),
                              
                              
                              column(width = 9,
                                     htmlOutput('header'),
                                     wellPanel(class ="line_graph_container",
                                               plotlyOutput("plot_T_RH", height = "40vh")),
                                     wellPanel(class ="line_graph_container",
                                               plotlyOutput("plot_Snow", height = "40vh"),
                                               radioButtons("cleanSnow", "Preform automated spike correction on Snow Depth?:", inline = T,
                                                            c("Yes" = "yes",
                                                              "No" = "no"))),
                                     
                                     uiOutput("windplots")
                              )
                            )
                    ),
                    tabItem("cstm_graph",
                            fluidRow(
                              column(12,
                                     h1("Custom Graphs", align = "center")
                              )
                            ),
                            fluidRow(
                              column(2,
                                     selectInput("custom_site",
                                                 label = "Choose a Weather Station:",
                                                 choices = stnNameDict,
                                                 selected = cur_stn,
                                                 selectize = F
                                                 
                                     ),
                                     selectInput("custom_year", "Select Water Year", "",  selectize = F),
                                     uiOutput("varSelection"),
                                     uiOutput("cleanSnowButton")
                              ),
                              column(10,
                                     htmlOutput('header2'),
                                     wellPanel(
                                       plotlyOutput("plot1", height = "40vh"),
                                       chooseSliderSkin('Flat',color = "#99ccff"),
                                       div(style = "margin-top:-3.5em; margin-bottom: -2em",
                                           fluidRow(uiOutput("slider"), align = 'center'))
                                     ),
                                     htmlOutput('partnerLogoUI_custom')
                              )
                              
                            )
                    ),
                    tabItem("ann_compare",
                            fluidRow(
                              column(12,
                                     h1("Annual Comparison", align = "center")
                              )
                            ),
                            fluidRow(
                              column(2,
                                     selectInput("annual_site",
                                                 label = "Choose a Weather Station:",
                                                 choices = stnNameDict,
                                                 selected = cur_stn,
                                                 multiple = F,
                                                 selectize = F
                                     ),
                                     uiOutput("varSelection_ann"),
                                     selectInput("compare_year", "Select Years to Compare: ", "", multiple = T)
                              ),
                              column(10,
                                     htmlOutput('header3'),
                                     wellPanel(
                                       plotlyOutput("plot2", height = "40vh"),
                                     ),
                                     htmlOutput('partnerLogoUI_annCompare')
                              )
                            )
                    ),
                    tabItem("stn_compare",
                            fluidRow(
                              column(12,
                                     h1("Station Comparison", align = "center")
                              )
                            ),
                            fluidRow(
                              column(2,
                                     selectInput("station_site",
                                                 label = "Choose a Weather Station:",
                                                 choices = stnNameDict,
                                                 selected = c('homathko', 'klinaklini'),
                                                 multiple = T,
                                                 selectize = T
                                     ),
                                     uiOutput("varSelection_stn"),
                                     selectInput("station_year", "Select Year to Compare: ", "")
                              ),
                              column(10,
                                     htmlOutput('header_compare_stn'),
                                     wellPanel(
                                       plotlyOutput("plot_compare_stn", height = "40vh"),
                                     )
                              )
                            )
                    )
                  )
                  
                )
  )
}

#### server ####
server <- function(input, output, session) {
  ##### create disclaimer dialog box and declare it open on start up #####
  
  # initial state for modal so its only shown once
  modalShown <- FALSE
  
  observe({
    if(input$smenu == "map" & modalShown == FALSE){
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
  
  
  # inputs to exclude from URL
  setBookmarkExclude(c("map_click", "cleanSnow", "cleanSnowCstm","plotly_hover-A", "annual_site", "sidebarItemExpanded", "plotly_hover", "map_bounds", "map_center", "map_zoom", "map_groups",".clientValue-default-plotlyCrosstalkOpts", "plotly_afterplot-A", "sliderTimeRange", "map_marker_mouseout", "map_marker_mouseover","map_marker_click", "compare_year", "compare_site","compare_var", "plotly_relayout-A", "FN", "EN", "ack", "sidebarCollapsed", "station_site", "station_year", "custom_year","custom_var"))
  
  
}

enableBookmarking("url")
shinyApp(ui, server)



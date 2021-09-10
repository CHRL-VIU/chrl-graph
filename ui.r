
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



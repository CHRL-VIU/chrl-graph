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

# grab login creds
source("config.r")

# Station Coords
stnCoords <- read.csv("stnloc.csv")

# set initial staiton
cur_stn = "apelake"


# add message to display at top of graph pages
siteNoticeMsg <- "Note: To automatically load your favourite station when you visit our site, select a station from the 'Choose a Weather Station' dropdown and then bookmark the link in the address bar. <br/>"
tetrahedronDisclaimer <- "The tipping bucket is currently offline at this station and total precipitation (stand pipe) is shown instead."

# load graphing presets
source('R/graph-presets.R')

# load logo paths, parameter dictionary, stn name dictionary
source('R/dictionaries.R')

# Station Meta List - this is a nested list 
source('R/station-meta-list.R')




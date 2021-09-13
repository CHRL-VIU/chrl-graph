library(dplyr)
library(DBI)
library(plotly)
library(lubridate)
library(leaflet)
library(tsibble)
library(shinyWidgets)
library(shinydashboard)
library(dashboardthemes)

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

##### graph presets ####
lineGraphColour <- list(
  colOne = "rgb(0,119,187)",
  colTwo = "rgb(204,51,17)"
)

# plotly layout lists
generalAxLayout <- list(
  zeroline = FALSE,
  showline = TRUE
)

# and set standard margins for plotly
marg <- list(b = 0, r = 50, l = 50, t = 10)

# Set Vars for Date range
wk_min_dt <- paste0(now()-604800)



# grab logo paths which should be saved under /www
logoPics <- list(
  bcgov_logo = tags$a(href='https://www2.gov.bc.ca/gov/content/governments/organizational-structure/ministries-organizations/ministries/forests-lands-natural-resource-operations-and-rural-development', tags$img(src="bcgov.png", height = "90", width = "250", hspace = "10", vspace = "10")),
  bcenv_logo = tags$a(href='https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/water-science-data/water-data-tools/snow-survey-data/automated-snow-weather-stations', tags$img(src="bcenv.jpg", height = "130", width = "140", hspace = "10", vspace = "10")),
  viu_logo = tags$a(href='https://www.viu.ca/', tags$img(src="viu.png", height = "80", width = "100", hspace = "10", vspace = "10")),
  canada_logo = tags$a(href='https://www.canada.ca/en/environment-climate-change.html', tags$img(src="envcan.jpg", height = "65", width = "225", hspace = "10", vspace = "10")),
  hakai_logo = tags$a(href='https://www.hakai.org/', tags$img(src="hakai.png", height = "55", width = "160", hspace = "10", vspace = "10")),
  mec_logo = tags$a(href='https://www.mec.ca/en/', tags$img(src="mec.png", height = "75", width = "75", hspace = "10", vspace = "10")),
  geosci_logo = tags$a(href='http://www.geoscientific.com/', tags$img(src="geosci.gif", height = "50", width = "200", hspace = "10", vspace = "10")),
  unlimited_logo = tags$a(href='http://www.unlimitedfab.ca/', tags$img(src="unlimitedfab.png", height = "75", width = "153", hspace = "10", vspace = "10")),
  westcoast_logo = tags$a(href='http://westcoasthelicopters.com/', tags$img(src="wch.png", height = "75", width = "153", hspace = "10", vspace = "10")),
  bchs_logo = tags$a(href='https://www.bellacoolaheliskiing.com/', tags$img(src="bchs.jpeg", height = "75", width = "153", hspace = "10", vspace = "10")),
  mtcain_logo = tags$a(href='http://www.mountcain.com/', tags$img(src="cain.png", height = "100", width = "140", hspace = "10", vspace = "10")),
  viac_logo = tags$a(href='https://www.islandavalanchebulletin.com/', tags$img(src="viac.png", height = "100", width = "120", hspace = "10", vspace = "10")),
  taan_logo = tags$a(href='https://www.taanforest.com/', tags$img(src="taan.png", height = "60", width = "150", hspace = "10", vspace = "10")),
  unbc_logo = tags$a(href='https://www.unbc.ca/', tags$img(src="unbc.png", height = "60", width = "200", hspace = "10", vspace = "10")),
  hes_logo = tags$a(href='https://hakaienergysolutions.com/', tags$img(src="hes.png", height = "70", width = "200", hspace = "10", vspace = "10")),
  bcwildfire_logo = tags$a(href='https://www2.gov.bc.ca/gov/content/safety/wildfire-status', tags$img(src="bcwildfire.png", height = "100", width = "100", hspace = "10", vspace = "10")),
  rdn_logo = tags$a(href='https://www.rdn.bc.ca/', tags$img(src="rdn.png", height = "60", width = "160", hspace = "10", vspace = "10")),
  mabrri_logo = tags$a(href='https://mabrri.viu.ca/', tags$img(src="mabrri.png", height = "47", width = "200", hspace = "10", vspace = "10")),
  scrd_logo = tags$a(href='https://www.scrd.ca/', tags$img(src="scrd.jpg", height = "120", width = "120", hspace = "10", vspace = "10")),
  hancock_logo = tags$a(href='https://htrg.com/', tags$img(src="hancock.jpeg", height = "75", width = "150", hspace = "10", vspace = "10")),
  comox_logo = tags$a(href='https://www.comoxvalleyrd.ca/', tags$img(src="comox.png", height = "50", width = "200", hspace = "10", vspace = "10")),
  mosaic_logo = tags$a(href='https://www.mosaicforests.com/', tags$img(src="mosaic.jpg", height = "60", width = "190", hspace = "10", vspace = "10")),
  bcparks_logo = tags$a(href='https://www.comoxvalleyrd.ca/', tags$img(src="bcparks.png", height = "100", width = "100", hspace = "10", vspace = "10")),
  bcts_logo = tags$a(href='https://www.comoxvalleyrd.ca/', tags$img(src="bcts.png", height = "100", width = "200", hspace = "10", vspace = "10")),
  gwa_logo = tags$a(href='http://gitksanwatershed.com/', tags$img(src="gwa.png", height = "82", width = "120", hspace = "10", vspace = "10"))

  )

#### Station Meta List - this is a nested list.  #####

sec_gen_FTS_params = c("Air_Temp", "RH", "BP", "Wind_Speed", "Wind_Dir", "Pk_Wind_Speed", "Pk_Wind_Dir", "PC_Tipper", "PP_Tipper", "PC_Raw_Pipe", "Snow_Depth", "SWU","SWL","LWU","LWL","SWE", "Soil_Moisture", "Soil_Temperature", "Batt")

stationMeta = list(
                    apelake = list(name = "Ape Lake",
                                   elevation = 1423,
                                   startYear = 2017,
                                   lat = 52.134569,
                                   lon = -126.262681,
                                   params = c("Air_Temp","RH","Wind_Speed","Wind_Dir","Pk_Wind_Speed","Pk_Wind_Dir", "PC_Tipper", "PP_Tipper","PC_Raw_Pipe","PP_Pipe","Snow_Depth","SWE","Solar_Rad","Batt"),
                                   logos = column(12, align = "center", style='padding:0px;',
                                                    logoPics$bcgov_logo,
                                                    logoPics$hakai_logo,
                                                    logoPics$bchs_logo,
                                                    logoPics$bcenv_logo,
                                                    logoPics$viu_logo,
                                                    logoPics$canada_logo)),

                    claytonfalls = list(name = "Clayton Falls",
                                        elevation = 1260,
                                        startYear = 2015,
                                        lat = 52.280133,
                                        lon =-126.891222,
                                        params = c("Air_Temp", "RH", "BP", "Wind_Speed", "Wind_Dir", "Pk_Wind_Speed", "Pk_Wind_Dir", "PC_Tipper", "PP_Tipper", "PC_Raw_Pipe", "PP_Pipe", "Snow_Depth", "SWE", "Solar_Rad", "Batt"),
                                        logos = column(12, align = "center", style='padding:0px;',

                                                           logoPics$bcgov_logo,
                                                           logoPics$hakai_logo,
                                                           logoPics$bchs_logo,
                                                           logoPics$bcenv_logo,
                                                           logoPics$viu_logo,
                                                           logoPics$canada_logo)),

                    datlamen = list(name = "Datlamen Pass",
                                    elevation = 287,
                                    startYear = 2014,
                                    lat = 52.280133,
                                    lon =-132.584272,
                                    params = c("Air_Temp", "RH", "Wind_Speed", "Wind_Dir", "Pk_Wind_Speed", "PP_Tipper", "Solar_Rad"),
                                    logos = column(12, align = "center", style='padding:0px;',

                                                     logoPics$bcgov_logo,
                                                     logoPics$taan_logo,
                                                     logoPics$viu_logo,
                                                     logoPics$canada_logo)),

                    eastbuxton = list(name = "Buxton East Ridge",
                                      elevation = 738,
                                      startYear = 2017,
                                      lat = 51.59,
                                      lon = -127.957,
                                      params = c("Air_Temp", "RH", "BP", "Wind_Speed", "Wind_Dir", "PP_Tipper", "PC_Raw_Pipe", "PP_Pipe", "Snow_Depth", "Solar_Rad", "Batt"),
                                      logos = column(12, align = "center", style='padding:0px;',

                                                       logoPics$bcgov_logo,
                                                       logoPics$hakai_logo,
                                                       logoPics$hes_logo,
                                                       logoPics$viu_logo,
                                                       logoPics$canada_logo,
                                                       logoPics$bcparks_logo)),

                    homathko = list(name = "Homathko",
                                    elevation = 1481,
                                    startYear = 2018,
                                    lat = 51.10187,
                                    lon = -124.9502,
                                    params = sec_gen_FTS_params,
                                    logos = column(12, align = "center", style='padding:0px;',

                                                     logoPics$hakai_logo,
                                                     logoPics$unbc_logo,
                                                     logoPics$bcgov_logo,
                                                     logoPics$bcenv_logo,
                                                     logoPics$viu_logo,
                                                     logoPics$canada_logo)),

                    klinaklini = list(name = "Klinaklini",
                                      elevation = 1532,
                                      startYear = 2018,
                                      lat = 51.38002,
                                      lon = -125.7695,
                                      params = sec_gen_FTS_params,
                                      logos = column(12, align = "center", style='padding:0px;',

                                                       logoPics$hakai_logo,
                                                       logoPics$unbc_logo,
                                                       logoPics$bcgov_logo,
                                                       logoPics$bcenv_logo,
                                                       logoPics$viu_logo,
                                                       logoPics$canada_logo)),

                    lowercain = list(name = "Cain Lower",
                                     elevation = 1260,
                                     startYear = 2019,
                                     lat = 50.22562,
                                     lon = -126.3567,
                                     params = c("Air_Temp", "RH", "PC_Tipper", "PP_Tipper", "PC_Raw_Pipe", "PP_Pipe", "Snow_Depth", "SWE", "Solar_Rad", "Batt"),
                                     logos = column(12, align = "center", style='padding:0px;',

                                                      logoPics$bcgov_logo,
                                                      logoPics$mec_logo,
                                                      logoPics$geosci_logo,
                                                      logoPics$unlimited_logo,
                                                      logoPics$westcoast_logo,
                                                      logoPics$mtcain_logo,
                                                      logoPics$viac_logo,
                                                      logoPics$hes_logo,
                                                      logoPics$bcenv_logo,
                                                      logoPics$viu_logo,
                                                      logoPics$canada_logo)),
                    machmell = list(name = "Machmell",
                                    elevation = 332,
                                    startYear = 2017,
                                    lat = 51.5931,
                                    lon = -126.451,
                                    params = c("Air_Temp", "RH", "Wind_Speed", "Wind_Dir", "Pk_Wind_Speed", "PP_Tipper", "Snow_Depth"),
                                    logos = column(12, align = "center", style='padding:0px;',

                                                     logoPics$bcgov_logo,
                                                     logoPics$bcwildfire_logo,
                                                     logoPics$viu_logo,
                                                     logoPics$canada_logo)),

                    machmellkliniklini = list(name = "Machmell Kliniklini",
                                              elevation = 1601,
                                              startYear = 2017,
                                              lat = 51.655581,
                                              lon = -126.140744,
                                              params = c("Air_Temp", "RH", "Wind_Speed", "Wind_Dir", "Pk_Wind_Speed", "Pk_Wind_Dir", "PC_Tipper", "PP_Tipper", "PC_Raw_Pipe", "PP_Pipe", "Snow_Depth", "SWE", "Solar_Rad", "Batt"),
                                              logos = column(12, align = "center", style='padding:0px;',

                                                                 logoPics$bcgov_logo,
                                                                 logoPics$hakai_logo,
                                                                 logoPics$bcenv_logo,
                                                                 logoPics$viu_logo,
                                                                 logoPics$canada_logo)),

                    mountarrowsmith = list(name = "Mount Arrowsmith",
                                           elevation = 1464,
                                           startYear = 2016,
                                           lat = 49.207203,
                                           lon = -124.567056,
                                           params = sec_gen_FTS_params,
                                           logos = column(12, align = "center", style='padding:0px;',

                                                            logoPics$rdn_logo,
                                                            logoPics$mabrri_logo,
                                                            logoPics$bcgov_logo,
                                                            logoPics$bcenv_logo,
                                                            logoPics$viu_logo,
                                                            logoPics$canada_logo,
                                                            logoPics$mosaic_logo)),

                    mountcayley = list(name = "Mount Cayley",
                                       elevation = 1588,
                                       startYear = 2015,
                                       lat = 50.070919,
                                       lon = -123.277469,
                                       params = c("Air_Temp", "RH", "BP", "Wind_Speed", "Wind_Dir", "Pk_Wind_Speed", "Pk_Wind_Dir", "PC_Tipper", "PP_Tipper", "Snow_Depth", "Solar_Rad", "Soil_Moisture", "Batt"),
                                       logos = column(12, align = "center", style='padding:0px;',

                                                logoPics$bcgov_logo,
                                                logoPics$bcenv_logo,
                                                logoPics$viu_logo,
                                                logoPics$canada_logo)),

                    mountmaya = list(name = "Mount Maya",
                                     elevation = 1681,
                                     startYear = 2014,
                                     lat = 52.287,
                                     lon = -126.07,
                                     params = c("Air_Temp", "RH", "BP", "Wind_Speed", "Wind_Dir", "Pk_Wind_Speed", "PP_Tipper", "PC_Raw_Pipe", "PP_Pipe", "Snow_Depth", "Solar_Rad", "Batt"),
                                     logos = column(12, align = "center", style='padding:0px;',

                                              logoPics$bcgov_logo,
                                              logoPics$hakai_logo,
                                              logoPics$hes_logo,
                                              logoPics$bchs_logo,
                                              logoPics$bcenv_logo,
                                              logoPics$viu_logo,
                                              logoPics$canada_logo)),

                    perseverance = list(name = "Perseverance",
                                        elevation = 970,
                                        startYear = 2019,
                                        lat = 49.59356,
                                        lon = -125.1313,
                                        params = sec_gen_FTS_params,
                                        logos = column(12, align = "center", style='padding:0px;',

                                                           logoPics$comox_logo,
                                                           logoPics$hancock_logo,
                                                           logoPics$bcgov_logo,
                                                           logoPics$viu_logo,
                                                           logoPics$canada_logo)),

                    rennellpass = list(name = "Rennell Pass",
                                       elevation = 315,
                                       startYear = 2011,
                                       lat = 53.3434,
                                       lon = -132.365,
                                       params = c("Air_Temp", "RH", "Wind_Speed", "Wind_Dir", "Pk_Wind_Speed", "PP_Tipper", "Snow_Depth", "Solar_Rad"),
                                       logos = column(12, align = "center", style='padding:0px;',

                                                        logoPics$bcgov_logo,
                                                        logoPics$viu_logo,
                                                        logoPics$canada_logo)),

                    steph3 = list(name = "Stephanie 3",
                                  elevation = 1020,
                                  startYear = 2015,
                                  lat = 50.3181,
                                  lon = -126.363,
                                  params = c("Air_Temp", "RH", "Wind_Speed", "Wind_Dir", "Pk_Wind_Speed", "PP_Tipper", "Snow_Depth", "Solar_Rad", "Lysimeter"),
                                  logos = column(12, align = "center", style='padding:0px;',

                                           logoPics$bcgov_logo,
                                           logoPics$viu_logo,
                                           logoPics$canada_logo,
                                           logoPics$bcts_logo)),

                    tetrahedron = list(name = "Tetrahedron",
                                       elevation = 1423,
                                       startYear = 2018,
                                       lat = 49.599565,
                                       lon = -123.606128,
                                       params = c("Air_Temp", "RH", "BP", "Wind_Speed", "Wind_Dir", "Pk_Wind_Speed", "Pk_Wind_Dir", "PC_Tipper", "PP_Tipper", "PC_Raw_Pipe", "PP_Pipe", "Snow_Depth", "SWE", "Solar_Rad", "Soil_Moisture", "Soil_Temperature", "Batt"),
                                       logos = column(12, align = "center", style='padding:0px;',

                                                        logoPics$scrd_logo,
                                                        logoPics$bcgov_logo,
                                                        logoPics$bcenv_logo,
                                                        logoPics$viu_logo,
                                                        logoPics$canada_logo)),

                    plummerhut = list(name = "Plummer Hut",
                                      elevation = 2680,
                                      startYear = 2020,
                                      lat = 51.37333,
                                      lon = -125.16500,
                                      params = c("Air_Temp", "RH", "BP", "Wind_Speed", "Wind_Dir", "Pk_Wind_Speed", "Pk_Wind_Dir", "PC_Tipper", "PP_Tipper", "Snow_Depth", "SWU","SWL","LWU","LWL","Batt"),
                                      logos = column(12, align = "center", style='padding:0px;',

                                                         logoPics$hakai_logo,
                                                         logoPics$unbc_logo,
                                                         logoPics$bchs_logo,
                                                         logoPics$bcgov_logo,
                                                         logoPics$bcenv_logo,
                                                         logoPics$viu_logo,
                                                         logoPics$canada_logo)),

                    # this station is imported through the old database the raw col defs are : "logstamp","air_temp","rh","wind_spd","wind_spd_max","wind_dir","snow","swe","rain","precip","lys","bp","solar","solar_sw_upper","solar_sw_lower","solar_lw_upper","solar_lw_lower","battery","total_rain","total_precip","rain_12","rain_24","rain_48","rain_72","precip_12","precip_24","precip_48","precip_72"
                    cainridgerun = list(name = "Cain Ridge Run",
                                        elevation = 1330,
                                        startYear = 2019, # is actually 2011 need to append data, but this is diff station...
                                        lat = 50.226500,
                                        lon = -126.351500,
                                        params = c("Air_Temp", "Wind_Speed", "Pk_Wind_Speed", "Wind_Dir", "Pk_Wind_Dir", "Snow_Depth", "RH", "PP_Tipper", "PC_Tipper", "Solar_Rad", "Batt"),
                                        logos = column(12, align = "center", style='padding:0px;',

                                                         logoPics$mec_logo,
                                                         logoPics$geosci_logo,
                                                         logoPics$unlimited_logo,
                                                         logoPics$westcoast_logo,
                                                         logoPics$mtcain_logo,
                                                         logoPics$viac_logo,
                                                         logoPics$hes_logo,
                                                         logoPics$bcgov_logo,
                                                         logoPics$bcenv_logo,
                                                         logoPics$viu_logo,
                                                         logoPics$canada_logo)),
                    uppercruickshank = list(name = "Upper Cruickshank",
                                        elevation = 1348,
                                        startYear = 2021,
                                        lat = 49.668914,
                                        lon = -125.360990,
                                        params = sec_gen_FTS_params,
                                        logos = column(12, align = "center", style='padding:0px;',

                                                       logoPics$comox_logo,
                                                       logoPics$bcgov_logo,
                                                       logoPics$bcenv_logo,
                                                       logoPics$mosaic_logo,
                                                       logoPics$viu_logo,
                                                       logoPics$canada_logo)),
                    upperskeena = list(name = "Upper Skeena",
                                        elevation = 1546,
                                        startYear = 2021,
                                        lat = 56.53839,
                                        lon = -127.65730,
                                        params = sec_gen_FTS_params,
                                        logos = column(12, align = "center", style='padding:0px;',

                                                       logoPics$gwa_logo,
                                                       logoPics$bcgov_logo,
                                                       logoPics$bcenv_logo,
                                                       logoPics$viu_logo,
                                                       logoPics$canada_logo))                           
)

stnNameDict <- list(
                    "Datlamen Pass" = "datlamen",
                    "Rennell Pass" = "rennellpass",
                    "Mount Maya" = "mountmaya",
                    "Clayton Falls" = "claytonfalls",
                    "Ape Lake" = "apelake",
                    "Machmell Kliniklini" = "machmellkliniklini",
                    "Machmell" = "machmell",
                    "Buxton East Ridge" = "eastbuxton",
                    "Klinaklini" = "klinaklini",
                    "Plummer Hut" = "plummerhut",
                    "Homathko" = "homathko",
                    "Cain Ridge Run" = "cainridgerun",
                    "Cain Lower" = "lowercain",
                    "Stephanie 3" = "steph3",
                    "Upper Cruickshank" = "uppercruickshank",
                    "Perseverance" = "perseverance",
                    "Mount Arrowsmith" = "mountarrowsmith",
                    "Mount Cayley" = "mountcayley",
                    "Tetrahedron" = "tetrahedron",
                    "Upper Skeena" = "upperskeena"
)

varsDict <- list("DateTime" = "DateTime",
                 "Air Temperature (\u00b0C)" = "Air_Temp",
                 "Relative Humidity (%)" = "RH",
                 "Air Pressure (kPa)" = "BP",
                 "Wind Speed (km/h)" = "Wind_Speed",
                 "Wind Direction (deg)" = "Wind_Dir",
                 "Peak Wind Speed (km/h)" = "Pk_Wind_Speed",
                 "Peak Wind Speed Direction (deg)" = "Pk_Wind_Dir",
                 "Tipping Bucket Increment (mm)" = "PP_Tipper",
                 "Tipping Bucket Cumulative (mm)" = "PC_Tipper",
                 "Precip Pipe Raw (mm)" = "PC_Raw_Pipe",
                 "Precip Pipe Increment (mm)" = "PP_Pipe",
                 "Snow Depth (cm)" =  "Snow_Depth",
                 "Snow Water Equivilent (mm)" = "SWE",
                 "Solar Radiation (W/m2)" = "Solar_Rad",
                 "Short Wave Radiation Upper (W/m2)" = "SWU",
                 "Short Wave Radiation Lower (W/m2)" = "SWL",
                 "Long Wave Radiation Upper (W/m2)" = "LWU",
                 "Long Wave Radiation Lower (W/m2)" = "LWL",
                 "Lysimeter (mm)" = "Lysimeter",
                 "Soil Moisture (%)" = "Soil_Moisture",
                 "Battery (V)" = "Batt"
)

#### functions #####

# set year of posix datetime using lubridate for if else statement
setYr <- function(DATE_TIME, YEAR) {
  year(DATE_TIME) <- YEAR

  DATE_TIME
}

# calculate water year by date
wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

# cleans snow depth data
cleanSnowData <- function(data, spike_th, roc_hi_th, roc_low_th) {
  data <- as_tsibble(data, index = DateTime) %>%
    fill_gaps() %>%
    mutate(
      Snow_Depth =
        case_when(
          (Snow_Depth - lag(Snow_Depth)) < -spike_th & (lead(Snow_Depth) - lag(Snow_Depth)) < -spike_th & (lead(Snow_Depth, n = 2) - Snow_Depth) > spike_th ~ lag(Snow_Depth), #  low spike
          (Snow_Depth - lag(Snow_Depth)) > spike_th & (lead(Snow_Depth) - Snow_Depth) < -spike_th ~ lag(Snow_Depth), #  hi spike
          (Snow_Depth - lag(Snow_Depth)) < -spike_th & (lead(Snow_Depth) - Snow_Depth) > spike_th ~ lag(Snow_Depth), #  low spike
          TRUE ~ Snow_Depth # else set to raw
        )
    )

  # while the dataset does not have a rate of change higher than the threshold
  while(TRUE %in% (data$Snow_Depth - lag(data$Snow_Depth) > roc_hi_th)){
    data <- data %>%
      mutate(
        Snow_Depth =
          case_when(

            (Snow_Depth - lag(Snow_Depth)) > roc_hi_th ~ lag(Snow_Depth), # rate of change
            (Snow_Depth - lead(Snow_Depth)) > roc_low_th ~ lead(Snow_Depth), # rate of change
            TRUE ~ Snow_Depth # else set to raw
          )
      )
  }
  return(data)
}


# creates a plotly wind rose using data from viu cleaned df tables time is number of hours
plotWindRose <- function(data, hrs = 168, plotTitle = "", dirres = 15){

      if(all(data$Wind_Speed == 0) | length(data$Wind_Speed) < (hrs / 2)){ # account for stations that dont have ws sensors and if there have been gaps for over half the data period
        plot <- plotly_empty(type = "barpolar") %>%
          layout(
            margin = list(t = 60, l = 20, r = 20),
            annotations = list(text = plotTitle, xanchor = "centre",
                               yref="paper",y=1,yshift=50,showarrow=FALSE,
                               font=list(size=18,color='rgb(0,0,0)')),
            xaxis = list(
              title = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
            ),
            legend = list(orientation = "h"),
            polar = list(
              radialaxis = list(
                nticks = 7,
                angle = 45,
                tickangle = 45,
                ticksuffix = " %"
              ),
              angularaxis = list(
                tickmode = "array",
                tickvals = c(0 , 45, 90, 135, 180, 225, 270, 315),
                ticktext = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
                direction = "clockwise"
              )
            )
          )
      } else {
      dirres <- dirres
      dir.breaks <- c(-dirres/2,
                      seq(dirres/2, 360-dirres/2, by = dirres),
                      360+dirres/2)
      dir.labels <- c(seq(0, 360, by = dirres))

      startTime <- (Sys.time() - hours(8))-hours(hrs)

      wind <- data %>%
        filter(DateTime >= startTime) %>%
        select(DateTime, Wind_Speed, Wind_Dir) %>%
        mutate(ws_bin = cut(Wind_Speed, breaks=c(-Inf, 5, 10, 20, 30, 40, 50, Inf), labels = c("< 5 km/h", "5-10 km/h", "10-20 km/h", "20-30 hm/h", "30-40 km/h", "40-50 km/h",">50 km/h"))) %>%
        mutate(wd_bin = cut(Wind_Dir, breaks = dir.breaks, labels = dir.labels)) %>%
        group_by(wd_bin, ws_bin) %>%
        summarise(Freq=(n()/hrs)*100) %>%
        arrange(ws_bin)

      plot <- plot_ly(wind, type = "barpolar", hovertemplate = paste('Freq (%): %{r:.2f}<br>Dir: %{theta}\u00b0;'), colors = c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", "#FC8D59", "#D73027")) %>%
        add_trace(r = ~Freq, theta = ~wd_bin, color = ~ws_bin) %>%
      layout(
        plot_bgcolor = "#f5f5f5",
        paper_bgcolor = "#f5f5f5",
        autosize = TRUE,
        margin = list(l = 10, r = 10),
        annotations = list(text = plotTitle, xanchor = "centre",
                           yref="paper",y=1,yshift=50,showarrow=FALSE,
                           font=list(size=18,color='rgb(0,0,0)')),
        xaxis = list(
          title = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
        ),
        legend = list(orientation = "h"),
        polar = list(
          radialaxis = list(
            nticks = 7,
            angle = 45,
            tickangle = 45,
            ticksuffix = " %"
          ),
          angularaxis = list(
            tickmode = "array",
            tickvals = c(0 , 45, 90, 135, 180, 225, 270, 315),
            ticktext = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"),
            direction = "clockwise"
          )
        )
      )
      }
  plot
}

#### dashboard theme use https://nik01010.shinyapps.io/dashboardThemeDesigner/ to create ####
customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "#2D2D2D"
  ,primaryFontColor = "#0F0F0F"
  ,infoFontColor = "#0F0F0F"
  ,successFontColor = "#0F0F0F"
  ,warningFontColor = "#0F0F0F"
  ,dangerFontColor = "#0F0F0F"
  ,bodyBackColor = "#FFFFFF"

  ### header
  ,logoBackColor = "#F8F8F8"

  ,headerButtonBackColor = "#F8F8F8"
  ,headerButtonIconColor = "#808080"
  ,headerButtonBackColorHover = "#E7E7E7"
  ,headerButtonIconColorHover = "#9E9E9E"

  ,headerBackColor = "#F8F8F8"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"

  ### sidebar
  ,sidebarBackColor = "#7C7F80"
  ,sidebarPadding = "0"

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"

  ,sidebarUserTextColor = "#737373"

  ,sidebarSearchBackColor = "#F0F0F0"
  ,sidebarSearchIconColor = "#646464"
  ,sidebarSearchBorderColor = "#DCDCDC"

  ,sidebarTabTextColor = "#E6E6E6"
  ,sidebarTabTextSize = "14"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "0"

  ,sidebarTabBackColorSelected = "#E6E6E6"
  ,sidebarTabTextColorSelected = "#000000"
  ,sidebarTabRadiusSelected = "0px"

  ,sidebarTabBackColorHover = "#F5F5F5"
  ,sidebarTabTextColorHover = "#000000"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#C8C8C8"
  ,sidebarTabBorderWidthHover = "4"
  ,sidebarTabRadiusHover = "0px"

  ### boxes
  ,boxBackColor = "#FFFFFF"
  ,boxBorderRadius = "5"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#E1E1E1"
  ,boxPrimaryColor = "#5F9BD5"
  ,boxInfoColor = "#B4B4B4"
  ,boxSuccessColor = "#70AD47"
  ,boxWarningColor = "#ED7D31"
  ,boxDangerColor = "#E84C22"

  ,tabBoxTabColor = "#F8F8F8"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#646464"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#F8F8F8"
  ,tabBoxHighlightColor = "#C8C8C8"
  ,tabBoxBorderRadius = "5"

  ### inputs
  ,buttonBackColor = "#D7D7D7"
  ,buttonTextColor = "#2D2D2D"
  ,buttonBorderColor = "#969696"
  ,buttonBorderRadius = "5"

  ,buttonBackColorHover = "#BEBEBE"
  ,buttonTextColorHover = "#000000"
  ,buttonBorderColorHover = "#969696"

  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#767676"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#F5F5F5"
  ,textboxBorderColorSelect = "#6C6C6C"

  ### tables
  ,tableBackColor = "#FFFFFF"
  ,tableBorderColor = "#CCC6C6"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)



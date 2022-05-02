## this is the code for graphing related presets

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
wk_min_dt <- paste0(Sys.time()-604800)

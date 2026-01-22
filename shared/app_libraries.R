# Shared library loading for all mmcd_metrics apps
# This reduces code duplication across all treatment tracking apps

suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(tidyr)      # For pivot operations
  library(ggplot2)
  library(lubridate)
  library(rlang)      # For dynamic programming
  library(purrr)      # For map functions
  library(tibble)     # For data frame operations
  library(scales)     # For formatting
  library(DT)         # For data tables
  library(plotly)     # For interactive plots
  library(leaflet)    # For maps (used by some apps)
  library(sf)         # For spatial data (used by some apps)
  library(RColorBrewer) # For color palettes
})

cat(" Common app libraries loaded successfully\n")
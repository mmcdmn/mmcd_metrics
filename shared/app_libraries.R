# Shared library loading for all mmcd_metrics apps
# This reduces code duplication across all treatment tracking apps
# Optional packages (RPostgres, DT, plotly, leaflet, sf) are loaded
# conditionally so the file can be sourced in test environments that
# may not have all visualization/database packages installed.

suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  # RPostgres may not be installed in test environments
  if (requireNamespace("RPostgres", quietly = TRUE)) {
    library(RPostgres)
  }
  library(dplyr)
  library(tidyr)      # For pivot operations
  library(ggplot2)
  library(lubridate)
  library(rlang)      # For dynamic programming
  library(purrr)      # For map functions
  library(tibble)     # For data frame operations
  library(scales)     # For formatting
  # Optional visualization/data packages - load if available
  for (.pkg in c("DT", "plotly", "leaflet", "sf")) {
    if (requireNamespace(.pkg, quietly = TRUE)) library(.pkg, character.only = TRUE)
  }
  library(RColorBrewer) # For color palettes
})

cat(" Common app libraries loaded successfully\n")
# Shared server utilities for mmcd_metrics apps
# Common reactive patterns used across all treatment tracking apps

#' Parse zone filter input into standardized format
#' @param zone_input The zone input value from the UI
#' @return List with parsed_zones and combine_zones flag
parse_zone_filter <- function(zone_input) {
  if (zone_input == "combined") {
    list(parsed_zones = c("1", "2"), combine_zones = TRUE)
  } else if (zone_input == "1,2") {
    list(parsed_zones = c("1", "2"), combine_zones = FALSE)
  } else {
    list(parsed_zones = zone_input, combine_zones = FALSE)
  }
}

#' Parse facility filter with "all" handling
#' @param facility_input The facility input value from the UI
#' @return Cleaned facility filter value
parse_facility_filter <- function(facility_input) {
  if (is.null(facility_input) || facility_input == "all") {
    return("all")
  }
  return(facility_input)
}

#' Parse foreman filter with "all" handling
#' @param foreman_input The foreman input value from the UI
#' @return Cleaned foreman filter value
parse_foreman_filter <- function(foreman_input) {
  if (is.null(foreman_input) || length(foreman_input) == 0 || "all" %in% foreman_input) {
    return("all")
  }
  return(foreman_input)
}

#' Create common theme reactive
#' @param input Shiny input object
#' @return Reactive for current theme
create_theme_reactive <- function(input) {
  reactive({
    input$color_theme
  })
}

#' Update theme option when changed
#' @param input Shiny input object
#' @return Observer for theme changes
observe_theme_changes <- function(input) {
  observeEvent(input$color_theme, {
    options(mmcd.color.theme = input$color_theme)
  })
}

cat(" Common server utilities loaded successfully\n")
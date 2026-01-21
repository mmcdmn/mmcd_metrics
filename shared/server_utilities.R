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

#' Map facility display names to theme colors
#' @param display_names Vector of facility full names (e.g., "Brooklyn Center", "East")
#' @param theme Color theme to use
#' @return Named vector with display names as keys and theme colors as values
map_facility_display_names_to_colors <- function(display_names, theme = "MMCD") {
  if (length(display_names) == 0) return(character(0))
  
  facility_colors <- get_facility_base_colors(theme = theme)
  facility_lookup <- get_facility_lookup()
  color_mapping <- character(0)
  
  for (display_name in display_names) {
    # Remove zone suffix if present (e.g., "East (P1)" -> "East")
    base_name <- gsub(" \\(P[12]\\)$", "", display_name)
    
    # Find matching facility by full name
    matching_facility <- facility_lookup[facility_lookup$full_name == base_name, ]
    if (nrow(matching_facility) > 0) {
      short_name <- matching_facility$short_name[1]
      if (short_name %in% names(facility_colors)) {
        color_mapping[display_name] <- facility_colors[short_name]
      } else {
        color_mapping[display_name] <- get_status_colors(theme = theme)["unknown"]
      }
    } else {
      color_mapping[display_name] <- get_status_colors(theme = theme)["unknown"]
    }
  }
  
  return(color_mapping)
}

#' Map foreman display names to theme colors
#' @param display_names Vector of foreman names (e.g., "Smith J", "Smith J P1")
#' @param theme Color theme to use
#' @return Named vector with display names as keys and theme colors as values
map_foreman_display_names_to_colors <- function(display_names, theme = "MMCD") {
  if (length(display_names) == 0) return(character(0))
  
  foreman_colors <- get_themed_foreman_colors(theme = theme)
  foremen_lookup <- get_foremen_lookup()
  color_mapping <- character(0)
  
  for (display_name in display_names) {
    # Remove zone suffix if present (e.g., "Smith J P1" -> "Smith J")
    base_name <- gsub(" P[12]$", "", display_name)
    
    # Find matching foreman by name patterns (flexible matching)
    # Try exact match first, then partial match for lastname
    matching_foreman <- foremen_lookup[
      foremen_lookup$shortname == base_name |
      grepl(paste0("^", gsub("\\s+.*", "", base_name)), foremen_lookup$shortname), 
    ]
    
    if (nrow(matching_foreman) > 0) {
      shortname <- matching_foreman$shortname[1]
      if (shortname %in% names(foreman_colors)) {
        color_mapping[display_name] <- foreman_colors[shortname]
      } else {
        color_mapping[display_name] <- get_status_colors(theme = theme)["completed"]
      }
    } else {
      color_mapping[display_name] <- get_status_colors(theme = theme)["completed"]
    }
  }
  
  return(color_mapping)
}

cat(" Common server utilities loaded successfully\n")
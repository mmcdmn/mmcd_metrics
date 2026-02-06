# URL Router for Overview Dashboard
# =============================================================================
# Handles URL parameter parsing and encoding for drill-down navigation.
# Provides a unified way to navigate within the same app using URL parameters.
#
# URL Scheme:
#   /overview/?view=<view>&metric=<metrics>&zone=<zone>&facility=<facility>&date=<date>
#
# Examples:
#   ?view=district                        - District overview with all metrics
#   ?view=facility&zone=1                 - Facility view for P1
#   ?view=metric_detail&metric=drone      - Drill into drone metric only
#   ?view=facility&metric=drone,ground_prehatch&zone=1&facility=SLP
# =============================================================================

# =============================================================================
# URL PARAMETER DEFINITIONS
# =============================================================================

#' Get default URL parameters
#' @return List of default parameter values
#' @export
get_default_url_params <- function() {
  list(
    view = "district",           # View level: district, facility, metric_detail
    metric = "all",              # Metrics to show: "all" or comma-separated IDs
    zone = "1,2",                # Zone filter: "1", "2", "1,2", "separate"
    facility = "all",            # Facility filter: "all" or facility code
    fos = "all",                 # FOS filter: "all" or FOS name
    site_id = NULL,              # Site filter: NULL or specific site_id
    date = as.character(Sys.Date()),
    expiring = 7,
    theme = "MMCD",
    # Historical options
    show_historical = TRUE,
    # Drill-down breadcrumb tracking
    from_view = NULL,            # Where we came from (for back navigation)
    from_metric = NULL           # Which metric was clicked
  )
}

#' Get valid values for URL parameters
#' Used for validation
#' @return List of valid values for each parameter
#' @export
get_valid_param_values <- function() {
  list(
    view = c("district", "facility", "fos", "site_detail", "metric_detail"),
    zone = c("1", "2", "1,2", "separate"),
    theme = c("MMCD", "IBM", "Wong", "Tol", "Viridis")
  )
}

# =============================================================================
# URL PARSING
# =============================================================================

#' Parse URL query string into structured parameters
#' @param query_string The query string from session$clientData$url_search
#' @return List of parsed parameters with defaults applied
#' @export
parse_url_params <- function(query_string) {
  defaults <- get_default_url_params()
  valid_values <- get_valid_param_values()
  registry <- get_metric_registry()
  valid_metrics <- names(registry)
  
  # Parse the query string
  query <- parseQueryString(query_string)
  
  # Initialize result with defaults
  params <- defaults
  
  # Parse view
  if (!is.null(query$view) && query$view %in% valid_values$view) {
    params$view <- query$view
  }
  
  # Parse metrics - validate against registry
  if (!is.null(query$metric)) {
    if (query$metric == "all") {
      params$metric <- "all"
    } else {
      # Split and validate each metric
      requested_metrics <- strsplit(query$metric, ",")[[1]]
      valid_requested <- requested_metrics[requested_metrics %in% valid_metrics]
      if (length(valid_requested) > 0) {
        params$metric <- valid_requested
      } else {
        params$metric <- "all"  # Fallback to all if none valid
      }
    }
  }
  
  # Parse zone
  if (!is.null(query$zone)) {
    zone_val <- query$zone
    # Handle comma-separated zones
    if (grepl(",", zone_val)) {
      params$zone <- zone_val  # Keep as-is for "1,2"
    } else if (zone_val %in% c("1", "2", "separate")) {
      params$zone <- zone_val
    }
  }
  
  # Parse facility
  if (!is.null(query$facility)) {
    params$facility <- query$facility
  }
  
  # Parse fos (Field Operations Supervisor)
  if (!is.null(query$fos)) {
    params$fos <- query$fos
  }
  
  # Parse site_id
  if (!is.null(query$site_id)) {
    params$site_id <- query$site_id
  }
  
  # Parse date
  if (!is.null(query$date)) {
    tryCatch({
      params$date <- as.character(as.Date(query$date))
    }, error = function(e) {
      # Keep default
    })
  }
  
  # Parse expiring days
  if (!is.null(query$expiring)) {
    tryCatch({
      exp_val <- as.numeric(query$expiring)
      if (!is.na(exp_val) && exp_val >= 1 && exp_val <= 30) {
        params$expiring <- exp_val
      }
    }, error = function(e) {
      # Keep default
    })
  }
  
  # Parse theme
  if (!is.null(query$theme) && query$theme %in% valid_values$theme) {
    params$theme <- query$theme
  }
  
  # Parse show_historical
  if (!is.null(query$show_historical)) {
    params$show_historical <- query$show_historical %in% c("true", "1", "TRUE", "yes")
  }
  
  # Parse breadcrumb tracking
  if (!is.null(query$from_view)) {
    params$from_view <- query$from_view
  }
  if (!is.null(query$from_metric)) {
    params$from_metric <- query$from_metric
  }
  
  # Log for debugging
  cat("[URL Router] Parsed params:\n")
  cat("  view:", params$view, "\n")
  cat("  metric:", if (is.character(params$metric) && length(params$metric) == 1) params$metric else paste(params$metric, collapse = ", "), "\n")
  cat("  zone:", params$zone, "\n")
  cat("  facility:", params$facility, "\n")
  cat("  date:", params$date, "\n")
  
  params
}

#' Get the list of metrics to display based on URL params
#' @param params Parsed URL parameters
#' @return Character vector of metric IDs
#' @export
get_display_metrics <- function(params) {
  if (is.character(params$metric) && length(params$metric) == 1 && params$metric == "all") {
    return(get_active_metrics())
  }
  
  # Return the specified metrics (already validated)
  params$metric
}

# =============================================================================
# URL BUILDING
# =============================================================================

#' Build a URL with parameters for drill-down navigation
#' @param view The view to navigate to
#' @param metric Metric ID(s) - single or vector
#' @param zone Zone filter
#' @param facility Facility filter
#' @param fos FOS (Field Operations Supervisor) filter
#' @param site_id Site ID filter
#' @param date Analysis date
#' @param expiring Expiring days
#' @param theme Color theme
#' @param show_historical Whether to show historical charts
#' @param from_view Current view (for back navigation)
#' @param from_metric Current metric (for back navigation)
#' @param base_path Base path for the URL (default: current app)
#' @return URL string
#' @export
build_drill_down_url <- function(
  view = "district",
  metric = "all",
  zone = "1,2",
  facility = "all",
  fos = "all",
  site_id = NULL,
  date = Sys.Date(),
  expiring = 7,
  theme = "MMCD",
  show_historical = TRUE,
  from_view = NULL,
  from_metric = NULL,
  base_path = NULL
) {
  # Build metric string
  metric_str <- if (length(metric) == 1) {
    metric
  } else {
    paste(metric, collapse = ",")
  }
  
  # Build parameter list
  params <- list(
    view = view,
    metric = metric_str,
    zone = zone,
    facility = facility,
    date = as.character(date),
    expiring = expiring,
    theme = theme,
    show_historical = if (show_historical) "true" else "false"
  )
  
  # Add optional parameters
  if (fos != "all") {
    params$fos <- fos
  }
  if (!is.null(site_id)) {
    params$site_id <- site_id
  }
  
  # Add breadcrumb tracking if provided
  if (!is.null(from_view)) {
    params$from_view <- from_view
  }
  if (!is.null(from_metric)) {
    params$from_metric <- from_metric
  }
  
  # Build query string
  query_string <- paste(
    sapply(names(params), function(name) {
      paste0(name, "=", URLencode(as.character(params[[name]]), reserved = TRUE))
    }),
    collapse = "&"
  )
  
  # Build full URL
  if (!is.null(base_path)) {
    paste0(base_path, "?", query_string)
  } else {
    paste0("?", query_string)
  }
}

#' Build a URL for drilling down from a bar click
#' @param session Shiny session
#' @param click_data The plotly click event data
#' @param current_params Current URL parameters
#' @param metric_id The metric that was clicked
#' @param overview_config The current overview configuration
#' @return URL string for drill-down
#' @export
build_click_drill_down_url <- function(session, click_data, current_params, metric_id, overview_config) {
  # Determine what was clicked
  clicked_value <- click_data$y  # The bar label (e.g., "P1", "SLP", etc.)
  
  cat("[URL Router] Bar clicked:", clicked_value, "in metric:", metric_id, "\n")
  
  # Determine next view based on current view
  current_view <- current_params$view
  next_view <- switch(current_view,
    "district" = "facility",
    "facility" = "fos",              # Drill from facility to FOS
    "fos" = "metric_detail",         # Drill from FOS to metric detail
    "metric_detail" = "site_detail", # Drill from metric to individual site
    "site_detail" = "site_detail",   # Can't drill further
    "facility"  # Default
  )
  
  # Determine zone from click
  zone_clicked <- current_params$zone
  if (!is.null(clicked_value)) {
    clicked_str <- as.character(clicked_value)
    if (grepl("^P[12]", clicked_str)) {
      zone_clicked <- gsub("P", "", substring(clicked_str, 1, 2))
    }
  }
  
  # Determine facility from click (if in facility view)
  facility_clicked <- current_params$facility
  if (current_view == "facility" && !is.null(clicked_value)) {
    facility_clicked <- as.character(clicked_value)
  }
  
  # Determine FOS from click (if in FOS view)
  fos_clicked <- current_params$fos
  if (current_view == "fos" && !is.null(clicked_value)) {
    fos_clicked <- as.character(clicked_value)
  }
  
  # Determine site from click (if in metric_detail view)
  site_clicked <- current_params$site_id
  if (current_view == "metric_detail" && !is.null(clicked_value)) {
    site_clicked <- as.character(clicked_value)
  }
  
  # Build the URL
  build_drill_down_url(
    view = next_view,
    metric = metric_id,  # Focus on the clicked metric
    zone = zone_clicked,
    facility = facility_clicked,
    fos = fos_clicked,
    site_id = site_clicked,
    date = current_params$date,
    expiring = current_params$expiring,
    theme = current_params$theme,
    show_historical = current_params$show_historical,
    from_view = current_view,
    from_metric = if (current_params$metric == "all") "all" else metric_id
  )
}

# =============================================================================
# NAVIGATION HELPERS
# =============================================================================

#' Navigate to a new URL within the same app
#' @param session Shiny session
#' @param url The URL to navigate to (can be relative with query params)
#' @export
navigate_to_url <- function(session, url) {
  cat("[URL Router] Navigating to:", url, "\n")
  
  # Use JavaScript to update URL and reload
  session$sendCustomMessage("navigateWithParams", list(url = url))
}

#' Build a "back" URL from breadcrumb
#' @param current_params Current URL parameters
#' @return URL string for going back
#' @export
build_back_url <- function(current_params) {
  if (is.null(current_params$from_view)) {
    # No breadcrumb - go to district overview
    return(build_drill_down_url(
      view = "district",
      metric = "all",
      zone = current_params$zone,
      date = current_params$date,
      expiring = current_params$expiring,
      theme = current_params$theme
    ))
  }
  
  # Build URL to go back to previous view
  build_drill_down_url(
    view = current_params$from_view,
    metric = if (!is.null(current_params$from_metric)) current_params$from_metric else "all",
    zone = current_params$zone,
    facility = if (current_params$from_view == "district") "all" else current_params$facility,
    date = current_params$date,
    expiring = current_params$expiring,
    theme = current_params$theme
  )
}

# =============================================================================
# TITLE AND BREADCRUMB HELPERS
# =============================================================================

#' Get the title for the current view
#' @param params Parsed URL parameters
#' @return String title
#' @export
get_view_title <- function(params) {
  registry <- get_metric_registry()
  
  # Base title
  base_title <- switch(params$view,
    "district" = "District Overview",
    "facility" = "Facility Overview", 
    "fos" = "FOS Overview",
    "metric_detail" = "Metric Detail",
    "site_detail" = "Site Detail",
    "Overview"
  )
  
  # Add metric name if single metric
  metric_display <- params$metric
  if (!identical(metric_display, "all") && length(metric_display) == 1) {
    config <- registry[[metric_display]]
    if (!is.null(config)) {
      base_title <- paste(config$display_name, "-", base_title)
    }
  }
  
  # Add zone if filtered
  if (params$zone %in% c("1", "2")) {
    zone_name <- paste0("P", params$zone)
    base_title <- paste(base_title, "-", zone_name)
  }
  
  # Add facility if filtered
  if (params$facility != "all" && params$view %in% c("facility", "fos", "metric_detail", "site_detail")) {
    base_title <- paste(base_title, "-", params$facility)
  }
  
  # Add FOS if filtered
  if (!is.null(params$fos) && params$fos != "all" && params$view %in% c("fos", "metric_detail", "site_detail")) {
    base_title <- paste(base_title, "-", params$fos)
  }
  
  # Add site if filtered 
  if (!is.null(params$site_id) && params$view == "site_detail") {
    base_title <- paste(base_title, "-", params$site_id)
  }
  
  base_title
}

#' Get subtitle for the current view
#' @param params Parsed URL parameters
#' @return String subtitle
#' @export
get_view_subtitle <- function(params) {
  switch(params$view,
    "district" = "Treatment progress aggregated by zone - Click a bar to drill down",
    "facility" = "Treatment progress by facility - Click a bar to drill into FOS",
    "fos" = "Treatment progress by Field Operations Supervisor - Click a bar for metric detail",
    "metric_detail" = "Detailed metric view with historical comparison - Click a bar for site detail",
    "site_detail" = "Individual site detail with treatment history",
    "Click charts to explore"
  )
}

#' Generate breadcrumb UI
#' @param params Parsed URL parameters
#' @param session Shiny session
#' @return UI element
#' @export
generate_breadcrumb_ui <- function(params, session) {
  crumbs <- list()
  
  # Always have district as root
  crumbs[[1]] <- if (params$view == "district") {
    tags$span(class = "breadcrumb-current", "District")
  } else {
    tags$a(
      href = "#",
      class = "breadcrumb-link",
      onclick = sprintf(
        "Shiny.setInputValue('nav_to', '%s', {priority: 'event'});",
        build_drill_down_url(view = "district", metric = "all", zone = params$zone,
                             date = params$date, expiring = params$expiring, theme = params$theme)
      ),
      "District"
    )
  }
  
  # Add facility level if applicable
  if (params$view %in% c("facility", "fos", "metric_detail", "site_detail")) {
    zone_label <- if (params$zone %in% c("1", "2")) paste0(" (P", params$zone, ")") else ""
    
    crumbs[[2]] <- tags$span(" > ")
    crumbs[[3]] <- if (params$view == "facility") {
      tags$span(class = "breadcrumb-current", paste0("Facilities", zone_label))
    } else {
      tags$a(
        href = "#",
        class = "breadcrumb-link",
        onclick = sprintf(
          "Shiny.setInputValue('nav_to', '%s', {priority: 'event'});",
          build_drill_down_url(view = "facility", metric = "all", zone = params$zone,
                               date = params$date, expiring = params$expiring, theme = params$theme)
        ),
        paste0("Facilities", zone_label)
      )
    }
  }
  
  # Add FOS level if applicable
  if (params$view %in% c("fos", "metric_detail", "site_detail")) {
    fos_label <- if (!is.null(params$fos) && params$fos != "all") paste0(" (", params$fos, ")") else ""
    
    crumbs[[4]] <- tags$span(" > ")
    crumbs[[5]] <- if (params$view == "fos") {
      tags$span(class = "breadcrumb-current", paste0("FOS", fos_label))
    } else {
      tags$a(
        href = "#",
        class = "breadcrumb-link", 
        onclick = sprintf(
          "Shiny.setInputValue('nav_to', '%s', {priority: 'event'});",
          build_drill_down_url(view = "fos", metric = "all", zone = params$zone, facility = params$facility,
                               date = params$date, expiring = params$expiring, theme = params$theme)
        ),
        paste0("FOS", fos_label)
      )
    }
  }

  # Add metric detail level if applicable
  if (params$view %in% c("metric_detail", "site_detail")) {
    registry <- get_metric_registry()
    metric_name <- if (!identical(params$metric, "all") && length(params$metric) == 1) {
      config <- registry[[params$metric]]
      if (!is.null(config)) config$display_name else params$metric
    } else {
      "Detail"
    }
    
    crumbs[[6]] <- tags$span(" > ")
    crumbs[[7]] <- if (params$view == "metric_detail") {
      tags$span(class = "breadcrumb-current", metric_name)
    } else {
      tags$a(
        href = "#",
        class = "breadcrumb-link",
        onclick = sprintf(
          "Shiny.setInputValue('nav_to', '%s', {priority: 'event'});",
          build_drill_down_url(view = "metric_detail", metric = params$metric, zone = params$zone,
                               facility = params$facility, fos = params$fos,
                               date = params$date, expiring = params$expiring, theme = params$theme)
        ),
        metric_name
      )
    }
  }
  
  # Add site detail level if applicable
  if (params$view == "site_detail") {
    site_label <- if (!is.null(params$site_id)) params$site_id else "Site"
    
    crumbs[[8]] <- tags$span(" > ")
    crumbs[[9]] <- tags$span(class = "breadcrumb-current", site_label)
  }
  
  div(
    class = "breadcrumb-container",
    style = "padding: 10px 15px; background: #f8f9fa; border-radius: 4px; margin-bottom: 15px;",
    do.call(tagList, crumbs)
  )
}

# =============================================================================
# JAVASCRIPT FOR URL HANDLING
# =============================================================================

#' Get JavaScript for URL navigation
#' @return tags$script element
#' @export
get_url_navigation_js <- function() {
  tags$script(HTML("
    // Custom message handler for navigation with URL params
    Shiny.addCustomMessageHandler('navigateWithParams', function(message) {
      console.log('Navigating to:', message.url);
      
      // Update URL without full reload - push to history
      if (history.pushState) {
        history.pushState(null, null, message.url);
        
        // Trigger Shiny to re-read URL params
        Shiny.setInputValue('url_changed', {
          url: message.url,
          timestamp: Date.now()
        }, {priority: 'event'});
      } else {
        // Fallback: full page reload
        window.location.href = message.url;
      }
    });
    
    // Custom message handler to trigger refresh button click
    Shiny.addCustomMessageHandler('triggerRefresh', function(message) {
      console.log('Triggering refresh');
      $('#refresh').click();
    });
    
    // Handle browser back/forward buttons
    window.addEventListener('popstate', function(event) {
      console.log('Browser navigation detected');
      Shiny.setInputValue('url_changed', {
        url: window.location.search,
        timestamp: Date.now()
      }, {priority: 'event'});
    });
    
    // Parse URL params on page load and send to Shiny
    $(document).ready(function() {
      var initialParams = window.location.search;
      if (initialParams) {
        console.log('Initial URL params:', initialParams);
        setTimeout(function() {
          Shiny.setInputValue('initial_url_params', initialParams, {priority: 'event'});
        }, 100);
      }
    });
  "))
}

#' Get CSS for breadcrumbs
#' @return tags$style element
#' @export
get_breadcrumb_css <- function() {
  tags$style(HTML("
    .breadcrumb-container {
      font-size: 14px;
    }
    .breadcrumb-link {
      color: #007bff;
      text-decoration: none;
      cursor: pointer;
    }
    .breadcrumb-link:hover {
      text-decoration: underline;
    }
    .breadcrumb-current {
      color: #333;
      font-weight: 600;
    }
  "))
}

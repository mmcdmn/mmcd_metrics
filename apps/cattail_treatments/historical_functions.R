# Historical functions for Cattail Treatments app
# All functions needed for historical analysis

library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(purrr)
library(shiny)

source("data_functions.R")

# Function to get historical cattail inspection data
get_historical_cattail_data <- function(time_period = "monthly", display_metric = "inspections", 
                                      start_date = NULL, end_date = NULL) {
  
  # Set default date range if not provided
  if (is.null(start_date)) start_date <- as.Date("2022-01-01")
  if (is.null(end_date)) end_date <- Sys.Date()
  
  # Determine year range from dates
  start_year <- year(start_date)
  end_year <- year(end_date)
  
  tryCatch({
    # Get database connection
    con <- get_db_connection()
    if (is.null(con)) {
      warning("Could not connect to database")
      return(data.frame())
    }
    
    # Historical cattail inspections query
    historical_query <- "
      SELECT 
        i.sitecode,
        i.inspdate,
        i.action,
        i.numdip,
        i.acres_plan,
        EXTRACT(year FROM i.inspdate) as year,
        EXTRACT(month FROM i.inspdate) as month,
        EXTRACT(week FROM i.inspdate) as week,
        sc.facility,
        sc.zone,
        sc.fosarea,
        b.acres
      FROM public.dblarv_insptrt_current i
      LEFT JOIN public.loc_breeding_sites b ON i.sitecode = b.sitecode
      LEFT JOIN public.gis_sectcode sc ON LEFT(i.sitecode,7) = sc.sectcode
      WHERE i.inspdate BETWEEN $1 AND $2
        AND i.action = '9'  -- Action 9 = inspected
        AND (b.enddate IS NULL OR b.enddate > $2)
      
      UNION ALL
      
      SELECT 
        a.sitecode,
        a.inspdate,
        a.action,
        a.numdip,
        a.acres_plan,
        EXTRACT(year FROM a.inspdate) as year,
        EXTRACT(month FROM a.inspdate) as month,
        EXTRACT(week FROM a.inspdate) as week,
        sc.facility,
        sc.zone,
        sc.fosarea,
        b.acres
      FROM public.dblarv_insptrt_archive a
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      LEFT JOIN public.gis_sectcode sc ON LEFT(a.sitecode,7) = sc.sectcode
      WHERE a.inspdate BETWEEN $1 AND $2
        AND a.action = '9'  -- Action 9 = inspected
        AND (b.enddate IS NULL OR b.enddate > $2)
      
      ORDER BY inspdate
    "
    
    # Execute query
    historical_data <- dbGetQuery(con, historical_query, list(start_date, end_date))
    dbDisconnect(con)
    
    if (nrow(historical_data) == 0) {
      return(data.frame())
    }
    
    # Add facility mapping
    facility_lookup <- get_facility_lookup()
    if (nrow(facility_lookup) > 0) {
      facility_map <- setNames(facility_lookup$full_name, facility_lookup$short_name)
      matched_facilities <- facility_map[historical_data$facility]
      historical_data$facility <- ifelse(
        !is.na(matched_facilities),
        matched_facilities,
        historical_data$facility
      )
    }
    
    # Add treatment determination
    historical_data <- historical_data %>%
      mutate(
        state = case_when(
          numdip > 0 ~ "need_treatment",
          numdip == 0 ~ "under_threshold",
          TRUE ~ "under_threshold"
        ),
        inspdate = as.Date(inspdate)
      )
    
    return(historical_data)
    
  }, error = function(e) {
    warning(paste("Error loading historical cattail data:", e$message))
    if (exists("con") && !is.null(con)) dbDisconnect(con)
    return(data.frame())
  })
}

# Function to create historical analysis chart
create_historical_analysis_chart <- function(raw_data, group_by = "facility", 
                                           time_period = "monthly", chart_type = "line",
                                           display_metric = "inspections", 
                                           start_date = NULL, end_date = NULL,
                                           combine_zones = FALSE) {
  
  # Get historical data
  historical_data <- get_historical_cattail_data(
    time_period = time_period,
    display_metric = display_metric,
    start_date = start_date,
    end_date = end_date
  )
  
  if (nrow(historical_data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No historical data available"), size = 6) +
           theme_void())
  }
  
  # Create time grouping
  historical_data <- historical_data %>%
    mutate(
      time_group = case_when(
        time_period == "weekly" ~ floor_date(inspdate, "week"),
        time_period == "monthly" ~ floor_date(inspdate, "month"),
        time_period == "yearly" ~ floor_date(inspdate, "year"),
        TRUE ~ floor_date(inspdate, "month")
      ),
      group_label = case_when(
        group_by == "facility" ~ facility,
        group_by == "foreman" ~ paste("FOS", fosarea),
        group_by == "zone" ~ paste("Zone", zone),
        TRUE ~ "All"
      )
    )
  
  # Aggregate data based on display metric
  if (display_metric == "sites") {
    # Count distinct sites inspected
    plot_data <- historical_data %>%
      group_by(time_group, group_label) %>%
      summarise(
        value = n_distinct(sitecode),
        .groups = "drop"
      )
    y_label <- "Sites Inspected"
    
  } else if (display_metric == "need_treatment") {
    # Count sites needing treatment
    plot_data <- historical_data %>%
      filter(state == "need_treatment") %>%
      group_by(time_group, group_label) %>%
      summarise(
        value = n_distinct(sitecode),
        .groups = "drop"
      )
    y_label <- "Sites Need Treatment"
    
  } else {
    # Default: count inspections
    plot_data <- historical_data %>%
      group_by(time_group, group_label) %>%
      summarise(
        value = n(),
        .groups = "drop"
      )
    y_label <- "Number of Inspections"
  }
  
  # Create base plot
  if (chart_type == "bar") {
    p <- ggplot(plot_data, aes(x = time_group, y = value, fill = group_label)) +
      geom_col(position = "dodge", alpha = 0.8)
  } else {
    # Default to line chart
    p <- ggplot(plot_data, aes(x = time_group, y = value, color = group_label)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2)
  }
  
  # Format time axis based on period
  if (time_period == "yearly") {
    p <- p + scale_x_date(date_labels = "%Y", date_breaks = "1 year")
  } else if (time_period == "monthly") {
    p <- p + scale_x_date(date_labels = "%b %Y", date_breaks = "2 months")
  } else {
    p <- p + scale_x_date(date_labels = "%m/%d", date_breaks = "2 weeks")
  }
  
  # Add labels and theme
  legend_title <- stringr::str_to_title(group_by)
  
  if (chart_type == "bar") {
    p <- p +
      labs(
        title = paste("Historical Cattail", stringr::str_to_title(display_metric), "by", stringr::str_to_title(group_by)),
        x = "Date",
        y = y_label,
        fill = legend_title
      )
  } else {
    p <- p +
      labs(
        title = paste("Historical Cattail", stringr::str_to_title(display_metric), "by", stringr::str_to_title(group_by)),
        x = "Date",
        y = y_label,
        color = legend_title
      )
  }
  
  p <- p +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  return(ggplotly(p, tooltip = c("x", "y", "color")))
}

# UI Helper functions for historical tab
create_time_period_selector <- function() {
  selectInput("time_period", "Time Period:",
             choices = list(
               "Weekly" = "weekly",
               "Monthly" = "monthly", 
               "Yearly" = "yearly"
             ),
             selected = "monthly")
}

create_chart_type_selector <- function() {
  selectInput("chart_type", "Chart Type:",
             choices = list(
               "Line Chart" = "line",
               "Bar Chart" = "bar"
             ),
             selected = "line")
}

create_year_range_selector <- function() {
  current_year <- year(Sys.Date())
  years <- 2022:current_year
  
  list(
    selectInput("start_year", "Start Year:",
               choices = setNames(years, years),
               selected = 2022),
    selectInput("end_year", "End Year:",
               choices = setNames(years, years), 
               selected = current_year)
  )
}
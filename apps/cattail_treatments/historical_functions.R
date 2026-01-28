# Historical functions for Cattail Treatments app
# All functions needed for historical analysis

library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(purrr)
library(shiny)

source("data_functions.R")
source("../../shared/server_utilities.R")

# Function to get historical cattail inspection and treatment data
get_historical_cattail_data <- function(time_period = "monthly", display_metric = "need_treatment", 
                                      start_date = NULL, end_date = NULL) {
  
  # Set default date range if not provided
  if (is.null(start_date)) {
    start_date <- as.Date(paste0(year(Sys.Date()) - 4, "-01-01"))  # Default to 4 years ago
  }
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
    # Uses DOY (day-of-year) to define inspection seasons:
    # - Fall/Winter: DOY 244-365 (Sept 1 - Dec 31) - belongs to that calendar year
    # - Spring/Summer: DOY 135-213 (May 15 - Aug 1) - belongs to PREVIOUS calendar year
    inspection_query <- "
      SELECT 
        i.sitecode,
        i.inspdate,
        i.action,
        i.numdip,
        i.acres_plan,
        EXTRACT(year FROM i.inspdate) as year,
        EXTRACT(month FROM i.inspdate) as month,
        EXTRACT(week FROM i.inspdate) as week,
        EXTRACT(DOY FROM i.inspdate) as doy,
        sc.facility,
        sc.zone,
        sc.fosarea,
        b.acres,
        -- Calculate inspection_year based on season
        CASE 
          WHEN EXTRACT(DOY FROM i.inspdate) BETWEEN 244 AND 365 THEN EXTRACT(year FROM i.inspdate)
          WHEN EXTRACT(DOY FROM i.inspdate) BETWEEN 135 AND 213 THEN EXTRACT(year FROM i.inspdate) - 1
          ELSE EXTRACT(year FROM i.inspdate)
        END as inspection_year
      FROM public.dblarv_insptrt_current i
      LEFT JOIN public.loc_breeding_sites b ON i.sitecode = b.sitecode
      LEFT JOIN public.gis_sectcode sc ON LEFT(i.sitecode,7) = sc.sectcode
      WHERE i.inspdate BETWEEN $1 AND $2
        AND i.action = '9'  -- Action 9 = inspected
        AND (b.enddate IS NULL OR b.enddate > $2)
        AND (
          EXTRACT(DOY FROM i.inspdate) BETWEEN 244 AND 365  -- Fall/Winter
          OR EXTRACT(DOY FROM i.inspdate) BETWEEN 135 AND 213  -- Spring/Summer
        )
      
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
        EXTRACT(DOY FROM a.inspdate) as doy,
        sc.facility,
        sc.zone,
        sc.fosarea,
        b.acres,
        -- Calculate inspection_year based on season
        CASE 
          WHEN EXTRACT(DOY FROM a.inspdate) BETWEEN 244 AND 365 THEN EXTRACT(year FROM a.inspdate)
          WHEN EXTRACT(DOY FROM a.inspdate) BETWEEN 135 AND 213 THEN EXTRACT(year FROM a.inspdate) - 1
          ELSE EXTRACT(year FROM a.inspdate)
        END as inspection_year
      FROM public.dblarv_insptrt_archive a
      LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
      LEFT JOIN public.gis_sectcode sc ON LEFT(a.sitecode,7) = sc.sectcode
      WHERE a.inspdate BETWEEN $1 AND $2
        AND a.action = '9'  -- Action 9 = inspected
        AND (b.enddate IS NULL OR b.enddate > $2)
        AND (
          EXTRACT(DOY FROM a.inspdate) BETWEEN 244 AND 365  -- Fall/Winter
          OR EXTRACT(DOY FROM a.inspdate) BETWEEN 135 AND 213  -- Spring/Summer
        )
      
      ORDER BY inspdate
    "
    
    # Historical cattail treatments query
    # Uses DOY filtering and calculates inspection_year based on treatment season
    treatment_query <- "
      SELECT 
        t.sitecode,
        t.inspdate as trtdate,
        t.action,
        t.matcode,
        t.acres_plan as treated_acres,
        EXTRACT(year FROM t.inspdate) as year,
        EXTRACT(month FROM t.inspdate) as month,
        EXTRACT(week FROM t.inspdate) as week,
        EXTRACT(DOY FROM t.inspdate) as doy,
        sc.facility,
        sc.zone,
        sc.fosarea,
        -- Calculate inspection_year based on season
        CASE 
          WHEN EXTRACT(DOY FROM t.inspdate) BETWEEN 244 AND 365 THEN EXTRACT(year FROM t.inspdate)
          WHEN EXTRACT(DOY FROM t.inspdate) BETWEEN 135 AND 213 THEN EXTRACT(year FROM t.inspdate) - 1
          ELSE EXTRACT(year FROM t.inspdate)
        END as inspection_year
      FROM public.dblarv_insptrt_current t
      LEFT JOIN public.gis_sectcode sc ON LEFT(t.sitecode,7) = sc.sectcode
      WHERE t.inspdate BETWEEN $1 AND $2
        AND t.action IN ('3', 'A', 'D')  -- Cattail treatments
        AND t.matcode IN (
          SELECT matcode 
          FROM public.mattype_list_targetdose
          WHERE prgassign_default = 'Cat' OR prg_alt1 = 'Cat'
        )
        AND (
          EXTRACT(DOY FROM t.inspdate) BETWEEN 244 AND 365  -- Fall/Winter
          OR EXTRACT(DOY FROM t.inspdate) BETWEEN 135 AND 213  -- Spring/Summer
        )
      
      UNION ALL
      
      SELECT 
        t.sitecode,
        t.inspdate as trtdate,
        t.action,
        t.matcode,
        t.acres_plan as treated_acres,
        EXTRACT(year FROM t.inspdate) as year,
        EXTRACT(month FROM t.inspdate) as month,
        EXTRACT(week FROM t.inspdate) as week,
        EXTRACT(DOY FROM t.inspdate) as doy,
        sc.facility,
        sc.zone,
        sc.fosarea,
        -- Calculate inspection_year based on season
        CASE 
          WHEN EXTRACT(DOY FROM t.inspdate) BETWEEN 244 AND 365 THEN EXTRACT(year FROM t.inspdate)
          WHEN EXTRACT(DOY FROM t.inspdate) BETWEEN 135 AND 213 THEN EXTRACT(year FROM t.inspdate) - 1
          ELSE EXTRACT(year FROM t.inspdate)
        END as inspection_year
      FROM public.dblarv_insptrt_archive t
      LEFT JOIN public.gis_sectcode sc ON LEFT(t.sitecode,7) = sc.sectcode
      WHERE t.inspdate BETWEEN $1 AND $2
        AND t.action IN ('3', 'A', 'D')  -- Cattail treatments
        AND t.matcode IN (
          SELECT matcode 
          FROM public.mattype_list_targetdose
          WHERE prgassign_default = 'Cat' OR prg_alt1 = 'Cat'
        )
        AND (
          EXTRACT(DOY FROM t.inspdate) BETWEEN 244 AND 365  -- Fall/Winter
          OR EXTRACT(DOY FROM t.inspdate) BETWEEN 135 AND 213  -- Spring/Summer
        )
      
      ORDER BY trtdate
    "
    
    # Execute queries
    inspection_data <- dbGetQuery(con, inspection_query, list(start_date, end_date))
    treatment_data <- dbGetQuery(con, treatment_query, list(start_date, end_date))
    safe_disconnect(con)
    
    # Add facility mapping to both datasets
    facility_lookup <- get_facility_lookup()
    if (nrow(facility_lookup) > 0) {
      facility_map <- setNames(facility_lookup$full_name, facility_lookup$short_name)
      
      if (nrow(inspection_data) > 0) {
        matched_facilities <- facility_map[inspection_data$facility]
        inspection_data$facility <- ifelse(
          !is.na(matched_facilities),
          matched_facilities,
          inspection_data$facility
        )
      }
      
      if (nrow(treatment_data) > 0) {
        matched_facilities <- facility_map[treatment_data$facility]
        treatment_data$facility <- ifelse(
          !is.na(matched_facilities),
          matched_facilities,
          treatment_data$facility
        )
      }
    }
    
    # Process inspection data
    if (nrow(inspection_data) > 0) {
      inspection_data <- inspection_data %>%
        mutate(
          need_treatment = numdip > 0,
          inspdate = as.Date(inspdate),
          inspection_season = case_when(
            doy >= 244 & doy <= 365 ~ "Fall/Winter",
            doy >= 135 & doy <= 213 ~ "Spring/Summer",
            TRUE ~ "Other"
          )
        )
    }
    
    # Process treatment data
    if (nrow(treatment_data) > 0) {
      treatment_data <- treatment_data %>%
        mutate(
          trtdate = as.Date(trtdate),
          treatment_season = case_when(
            doy >= 244 & doy <= 365 ~ "Fall/Winter",
            doy >= 135 & doy <= 213 ~ "Spring/Summer",
            TRUE ~ "Other"
          )
        )
    }
    
    return(list(
      inspections = inspection_data,
      treatments = treatment_data
    ))
    
  }, error = function(e) {
    warning(paste("Error loading historical cattail data:", e$message))
    if (exists("con") && !is.null(con)) safe_disconnect(con)
    return(list(inspections = data.frame(), treatments = data.frame()))
  })
}

# Function to create historical analysis chart
create_historical_analysis_chart <- function(raw_data, group_by = "facility", 
                                           time_period = "monthly", chart_type = "line",
                                           display_metric = "need_treatment", 
                                           start_date = NULL, end_date = NULL,
                                           combine_zones = FALSE, metric_type = "sites", theme = "MMCD",
                                           facility_filter = "all", foreman_filter = "all") {
  
  # Get facility colors for chart
  facility_colors <- get_facility_base_colors(theme = theme)
  status_colors <- get_status_colors(theme = theme)
  
  # Get theme-aware foreman colors (based on facility colors with theme)
  foreman_colors <- get_themed_foreman_colors(theme = theme)
  
  # Get facility lookup to map full names to short codes
  facility_lookup <- get_facility_lookup()
  
  # Get foremen lookup to map fosarea (emp_num) to foreman names
  foremen_lookup <- get_foremen_lookup()
  
  # Get historical data
  hist_data <- get_historical_cattail_data(
    time_period = time_period,
    display_metric = display_metric,
    start_date = start_date,
    end_date = end_date
  )
  
  inspection_data <- hist_data$inspections
  treatment_data <- hist_data$treatments
  
  if (nrow(inspection_data) == 0 && nrow(treatment_data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No historical data available"), size = 6) +
           theme_void())
  }
  
  # Create group labels for inspections and treatments BEFORE filtering
  if (nrow(inspection_data) > 0) {
    # Map full facility names to short codes and fosarea to foreman names
    inspection_data <- inspection_data %>%
      left_join(facility_lookup %>% select(facility_full = full_name, facility_short = short_name), by = c("facility" = "facility_full")) %>%
      left_join(foremen_lookup %>% select(fosarea_num = emp_num, foreman_name = shortname), by = c("fosarea" = "fosarea_num")) %>%
      mutate(
        facility_short = if_else(is.na(facility_short), facility, facility_short),  # Fallback to full name if no match
        foreman_name = if_else(is.na(foreman_name), paste("FOS", fosarea), foreman_name),  # Fallback if no match
        group_label = case_when(
          group_by == "facility" & !combine_zones ~ paste(facility_short, "- Zone", zone),
          group_by == "facility" & combine_zones ~ facility_short,
          group_by == "foreman" & !combine_zones ~ paste(foreman_name, "- Zone", zone),
          group_by == "foreman" & combine_zones ~ foreman_name,
          group_by == "zone" ~ paste("Zone", zone),
          TRUE ~ "All"
        )
      )
  }
  
  # Create group labels for treatments
  if (nrow(treatment_data) > 0) {
    # Map full facility names to short codes and fosarea to foreman names
    treatment_data <- treatment_data %>%
      left_join(facility_lookup %>% select(facility_full = full_name, facility_short = short_name), by = c("facility" = "facility_full")) %>%
      left_join(foremen_lookup %>% select(fosarea_num = emp_num, foreman_name = shortname), by = c("fosarea" = "fosarea_num")) %>%
      mutate(
        facility_short = if_else(is.na(facility_short), facility, facility_short),  # Fallback to full name if no match
        foreman_name = if_else(is.na(foreman_name), paste("FOS", fosarea), foreman_name),  # Fallback if no match
        group_label = case_when(
          group_by == "facility" & !combine_zones ~ paste(facility_short, "- Zone", zone),
          group_by == "facility" & combine_zones ~ facility_short,
          group_by == "foreman" & !combine_zones ~ paste(foreman_name, "- Zone", zone),
          group_by == "foreman" & combine_zones ~ foreman_name,
          group_by == "zone" ~ paste("Zone", zone),
          TRUE ~ "All"
        )
      )
  }
  
  # Apply facility filter AFTER group_label is created using shared helper
  if (is_valid_filter(facility_filter)) {
    if (nrow(inspection_data) > 0) {
      inspection_data <- inspection_data %>% filter(facility %in% facility_filter)
    }
    if (nrow(treatment_data) > 0) {
      treatment_data <- treatment_data %>% filter(facility %in% facility_filter)
    }
  }
  
  # Apply foreman filter AFTER group_label is created using shared helper
  if (is_valid_filter(foreman_filter)) {
    if (nrow(inspection_data) > 0) {
      inspection_data <- inspection_data %>% filter(foreman_name %in% foreman_filter)
    }
    if (nrow(treatment_data) > 0) {
      treatment_data <- treatment_data %>% filter(foreman_name %in% foreman_filter)
    }
  }
  
  # Aggregate data based on display metric
  if (display_metric == "need_treatment") {
    # Count sites or acres needing treatment at end of inspections for each inspection year
    # Get the most recent inspection per site per inspection_year
    if (metric_type == "acres") {
      plot_data <- inspection_data %>%
        filter(need_treatment == TRUE) %>%
        group_by(sitecode, inspection_year, group_label) %>%
        arrange(desc(inspdate)) %>%
        slice(1) %>%
        ungroup() %>%
        group_by(inspection_year, group_label) %>%
        summarise(
          value = sum(acres, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          # Create time_group for plotting (use middle of inspection year)
          time_group = as.Date(paste0(inspection_year, "-06-01"))
        )
      y_label <- "Acres Need Treatment"
    } else {
      plot_data <- inspection_data %>%
        filter(need_treatment == TRUE) %>%
        group_by(sitecode, inspection_year, group_label) %>%
        arrange(desc(inspdate)) %>%
        slice(1) %>%
        ungroup() %>%
        group_by(inspection_year, group_label) %>%
        summarise(
          value = n_distinct(sitecode),
          .groups = "drop"
        ) %>%
        mutate(
          # Create time_group for plotting (use middle of inspection year)
          time_group = as.Date(paste0(inspection_year, "-06-01"))
        )
      y_label <- "Sites Need Treatment"
    }
    
  } else if (display_metric == "treated") {
    # Count sites or acres treated per inspection year
    if (metric_type == "acres") {
      plot_data <- treatment_data %>%
        group_by(sitecode, inspection_year, group_label) %>%
        arrange(desc(trtdate)) %>%
        slice(1) %>%
        ungroup() %>%
        group_by(inspection_year, group_label) %>%
        summarise(
          value = sum(coalesce(treated_acres, 0), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          # Create time_group for plotting (use middle of inspection year)
          time_group = as.Date(paste0(inspection_year, "-06-01"))
        )
      y_label <- "Acres Treated"
    } else {
      plot_data <- treatment_data %>%
        group_by(inspection_year, group_label) %>%
        summarise(
          value = n_distinct(sitecode),
          .groups = "drop"
        ) %>%
        mutate(
          # Create time_group for plotting (use middle of inspection year)
          time_group = as.Date(paste0(inspection_year, "-06-01"))
        )
      y_label <- "Sites Treated"
    }
    
  } else if (display_metric == "pct_treated") {
    # Calculate % of sites needing treatment that were treated
    # For each inspection year, count sites needing treatment and sites treated
    need_treatment_counts <- inspection_data %>%
      filter(need_treatment == TRUE) %>%
      group_by(sitecode, inspection_year, group_label) %>%
      arrange(desc(inspdate)) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(inspection_year, group_label) %>%
      summarise(
        sites_need = n_distinct(sitecode),
        .groups = "drop"
      )
    
    treated_counts <- treatment_data %>%
      group_by(inspection_year, group_label) %>%
      summarise(
        sites_treated = n_distinct(sitecode),
        .groups = "drop"
      )
    
    plot_data <- need_treatment_counts %>%
      left_join(treated_counts, by = c("inspection_year", "group_label")) %>%
      mutate(
        sites_treated = ifelse(is.na(sites_treated), 0, sites_treated),
        value = ifelse(sites_need > 0, (sites_treated / sites_need) * 100, 0),
        # Create time_group for plotting (use middle of inspection year)
        time_group = as.Date(paste0(inspection_year, "-06-01"))
      )
    
    y_label <- "% Sites Treated (of Need Treatment)"
    
  } else {
    # Default: sites needing treatment
    plot_data <- inspection_data %>%
      filter(need_treatment == TRUE) %>%
      group_by(sitecode, inspection_year, group_label) %>%
      arrange(desc(inspdate)) %>%
      slice(1) %>%
      ungroup() %>%
      group_by(inspection_year, group_label) %>%
      summarise(
        value = n_distinct(sitecode),
        .groups = "drop"
      ) %>%
      mutate(
        # Create time_group for plotting (use middle of inspection year)
        time_group = as.Date(paste0(inspection_year, "-06-01"))
      )
    y_label <- "Sites Need Treatment"
  }
  
  if (nrow(plot_data) == 0) {
    return(ggplot() + 
           geom_text(aes(x = 1, y = 1, label = "No data for selected metric"), size = 6) +
           theme_void())
  }
  
  # Create appropriate color mapping based on group_by BEFORE changing labels
  if (group_by == "facility") {
    # For facility grouping, ensure we have the right color keys
    if (combine_zones) {
      # When zones are combined, group_label is just the facility code (e.g., "N", "E")
      color_mapping <- facility_colors
    } else {
      # When zones are NOT combined, group_label is "N - Zone 1", etc.
      # We need to extract the facility code and create new color mapping
      unique_labels <- unique(plot_data$group_label)
      color_mapping <- setNames(
        sapply(unique_labels, function(label) {
          # Extract facility code (first part before " - Zone")
          facility_code <- sub(" - Zone.*", "", label)
          facility_colors[facility_code]
        }),
        unique_labels
      )
    }
  } else if (group_by == "foreman") {
    # For foreman grouping, use foreman_colors which already has foreman shortnames as keys
    # If we have more unique foremen than colors, extend the palette
    unique_labels <- unique(plot_data$group_label)
    
    if (length(unique_labels) <= length(foreman_colors)) {
      # Use foreman_colors directly
      color_mapping <- foreman_colors[unique_labels]
    } else {
      # Need to extend the palette - combine foreman, facility, and status colors
      extended_palette <- c(
        foreman_colors,
        unname(status_colors),
        unname(facility_colors)
      )
      color_mapping <- setNames(
        extended_palette[seq_along(unique_labels)],
        unique_labels
      )
    }
  } else if (group_by == "zone") {
    # For zones, create simple colors
    color_mapping <- c(
      "Zone 1" = unname(status_colors["active"]),
      "Zone 2" = unname(status_colors["planned"])
    )
  } else {
    # For "all" or other groupings
    color_mapping <- c("All" = unname(status_colors["active"]))
  }
  
  # NOW replace short codes with full names in group_label for DISPLAY
  # AND update color_mapping keys to match
  if (group_by == "facility") {
    facility_lookup <- get_facility_lookup()
    if (nrow(facility_lookup) > 0) {
      fac_map <- setNames(facility_lookup$full_name, facility_lookup$short_name)
      
      # Create new color mapping with full names as keys
      new_color_mapping <- setNames(
        sapply(names(color_mapping), function(old_key) {
          color_mapping[old_key]
        }),
        sapply(names(color_mapping), function(old_key) {
          # Extract facility short code
          facility_code <- sub(" - Zone.*", "", old_key)
          if (facility_code %in% names(fac_map)) {
            full_name <- fac_map[facility_code]
            # If key has " - Zone X", append it
            if (grepl(" - Zone", old_key)) {
              zone_part <- sub(".*( - Zone \\d+).*", "\\1", old_key)
              paste0(full_name, zone_part)
            } else {
              full_name
            }
          } else {
            old_key
          }
        })
      )
      color_mapping <- new_color_mapping
      
      # Update group_label in data
      plot_data <- plot_data %>%
        mutate(
          group_label = sapply(group_label, function(label) {
            # Extract facility short code
            facility_code <- sub(" - Zone.*", "", label)
            if (facility_code %in% names(fac_map)) {
              full_name <- fac_map[facility_code]
              # If label has " - Zone X", append it
              if (grepl(" - Zone", label)) {
                zone_part <- sub(".*( - Zone \\d+).*", "\\1", label)
                paste0(full_name, zone_part)
              } else {
                full_name
              }
            } else {
              label
            }
          })
        )
    }
  }
  
  # Prepare data for shared chart function - needs time_period column
  if (chart_type %in% c("bar", "stacked")) {
    plot_data$time_period <- format(plot_data$time_group, "%Y")
  } else {
    plot_data$time_period <- as.character(year(plot_data$time_group))
  }
  
  # Build chart title
  metric_title <- case_when(
    display_metric == "need_treatment" ~ "Sites Need Treatment",
    display_metric == "treated" ~ "Sites Treated (as of Aug 1)",
    display_metric == "pct_treated" ~ "% Sites Treated (as of Aug 1)",
    TRUE ~ "Historical Analysis"
  )
  chart_title <- paste("Historical Cattail:", metric_title, "by", stringr::str_to_title(group_by))
  
  # Map chart types to shared function format
  shared_chart_type <- switch(chart_type,
    "bar" = "grouped_bar",
    "stacked" = "stacked_bar",
    "line"  # default
  )
  
  # Use shared chart function
  create_trend_chart(
    data = plot_data,
    chart_type = shared_chart_type,
    title = chart_title,
    x_label = "Inspection Year (Fall-Summer Season)",
    y_label = y_label,
    colors = color_mapping
  )
}

# UI Helper functions for historical tab
create_chart_type_selector <- function() {
  selectInput("chart_type", "Chart Type:",
             choices = list(
               "Line Chart" = "line",
               "Grouped Bar Chart" = "bar",
               "Stacked Bar Chart" = "stacked"
             ),
             selected = "line")
}

create_year_range_selector <- function() {
  current_year <- year(Sys.Date())
  min_year <- 2000  # Allow historical data back to 2000
  default_start <- max(min_year, current_year - 4)
  
  sliderInput("year_range", "Inspection Year Range:",
             min = min_year,
             max = current_year,
             value = c(default_start, current_year),
             step = 1,
             sep = "")
}

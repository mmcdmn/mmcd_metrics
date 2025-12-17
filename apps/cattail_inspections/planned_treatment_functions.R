# Planned Treatment Functions for Cattail Inspection Progress App
# Functions to handle the planned treatment tab functionality

# Function to get treatment plan data
get_treatment_plan_data <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  # Query to fetch data with acres_plan
  query <- "
SELECT a.facility, a.airgrnd_plan, SUM(a.acres_plan) as total_acres
FROM public.dblarv_insptrt_current a
LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
WHERE a.action = '9'
AND a.acres_plan IS NOT NULL
AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
GROUP BY a.airgrnd_plan, a.facility
ORDER BY a.airgrnd_plan, a.facility
"
  
  result <- dbGetQuery(con, query)
  safe_disconnect(con)
  
  # Ensure we have all treatment plan types represented with clear names
  result$plan_type <- factor(
    result$airgrnd_plan,
    levels = c("A", "D", "G", "N", "U"),
    labels = c("Air", "Drone", "Ground", "None", "Unknown")
  )
  
  # Round the acres to full numbers
  result$total_acres <- round(result$total_acres)
  
  return(result)
}

# Function to get site details for treatment planning
get_site_details_data <- function(facility_filter, plan_types_filter) {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  # Build facility filter clause
  facility_clause <- if (facility_filter != "all") {
    sprintf("AND a.facility = '%s'", facility_filter)
  } else {
    ""
  }
  
  # Build plan types filter clause
  plan_types_quoted <- paste0("'", plan_types_filter, "'", collapse = ", ")
  plan_types_clause <- sprintf("AND a.airgrnd_plan IN (%s)", plan_types_quoted)
  
  # Query to fetch site details - using only real columns
  query <- sprintf("
SELECT 
  a.sitecode,
  a.facility,
  a.airgrnd_plan,
  a.inspdate,
  a.wet,
  a.numdip,
  a.acres,
  a.acres_plan
FROM public.dblarv_insptrt_current a
LEFT JOIN public.loc_breeding_sites b ON a.sitecode = b.sitecode
WHERE a.action = '9'
  AND a.acres_plan IS NOT NULL
  AND (b.enddate IS NULL OR b.enddate > CURRENT_DATE)
  %s
  %s
ORDER BY a.facility, a.airgrnd_plan, a.sitecode
", facility_clause, plan_types_clause)
  
  result <- dbGetQuery(con, query)
  safe_disconnect(con)
  
  # Add plan type names
  result$plan_name <- factor(
    result$airgrnd_plan,
    levels = c("A", "D", "G", "N", "U"),
    labels = c("Air", "Drone", "Ground", "None", "Unknown")
  )
  
  # Round acres
  result$acres_plan <- round(result$acres_plan, 1)
  
  # Format inspection date
  result$inspdate <- as.Date(result$inspdate)
  
  return(result)
}

# Function to create treatment plan plot with pre-fetched data
create_treatment_plan_plot_with_data <- function(data, facility_filter, plan_types_filter, view_type = "acres", theme = "MMCD") {
  # Filter based on selected facility
  if (facility_filter != "all") {
    data <- data %>% filter(facility == facility_filter)
  }
  
  # Filter based on selected treatment plan types
  data <- data %>% filter(airgrnd_plan %in% plan_types_filter)
  
  # If no data is available after filtering
  if (nrow(data) == 0) {
    return(
      ggplot() +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = "No data available for the selected filters.",
          size = 6
        ) +
        theme_void()
    )
  }
  
  # Determine what to display based on view_type
  y_column <- if (view_type == "sites") "site_count" else "total_acres"
  y_label <- if (view_type == "sites") "Number of Sites" else "Total Acres"
  title_suffix <- if (view_type == "sites") "(Number of Sites)" else "(Acres)"
  
  # Calculate summary stats if "all" is selected
  if (facility_filter == "all") {
    summary_data <- data %>%
      group_by(plan_type, airgrnd_plan) %>%
      summarize(
        total_acres = sum(total_acres, na.rm = TRUE),
        site_count = n(),
        .groups = "drop"
      )
    
    # Plot summary data for all facilities
    # Get centralized colors for treatment plan types
    plan_colors <- get_treatment_plan_colors(use_names = TRUE, theme = theme)
    
    p <- ggplot(summary_data,
                aes(x = plan_type, y = .data[[y_column]], fill = plan_type)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = plan_colors) +
      labs(
        title = paste("Treatment Plan", title_suffix, "by Type (All Facilities)"),
        x = "Treatment Plan Type",
        y = y_label,
        fill = "Plan Type"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 16),
        axis.text.y = element_text(face = "bold", size = 16),
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16),
        legend.text = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold", size = 14)
      )
    
    # Add values on top of bars
    p + geom_text(
      aes(label = scales::comma(.data[[y_column]])),
      vjust = -0.5,
      color = "black",
      size = 5,
      fontface = "bold"
    )
    
  } else {
    # Plot data for a specific facility
    # Get centralized colors for treatment plan types
    plan_colors <- get_treatment_plan_colors(use_names = TRUE, theme = theme)
    
    # Get full facility name for display
    facility_lookup <- get_facility_lookup()
    facility_map <- setNames(facility_lookup$full_name, facility_lookup$short_name)
    facility_display <- ifelse(facility_filter %in% names(facility_map), 
                               facility_map[facility_filter], 
                               facility_filter)
    
    # Calculate site count for specific facility
    facility_data <- data %>%
      group_by(plan_type, airgrnd_plan) %>%
      summarize(
        total_acres = sum(total_acres, na.rm = TRUE),
        site_count = n(),
        .groups = "drop"
      )
    
    p <- ggplot(facility_data, aes(x = plan_type, y = .data[[y_column]], fill = plan_type)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = plan_colors) +
      labs(
        title = paste(
          "Treatment Plan", title_suffix, "by Type -",
          facility_display,
          "Facility"
        ),
        x = "Treatment Plan Type",
        y = y_label,
        fill = "Plan Type"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 16),
        axis.text.y = element_text(face = "bold", size = 16),
        axis.title.x = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16),
        legend.text = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold", size = 14)
      )
    
    # Add values on top of bars
    p + geom_text(
      aes(label = scales::comma(.data[[y_column]])),
      vjust = -0.5,
      color = "black",
      size = 5,
      fontface = "bold"
    )
  }
}

# Function to create treatment plan plot (legacy wrapper)
create_treatment_plan_plot <- function(facility_filter, plan_types_filter, view_type = "acres", theme = "MMCD") {
  data <- get_treatment_plan_data()
  create_treatment_plan_plot_with_data(data, facility_filter, plan_types_filter, view_type, theme)
}

# Planned Treatment Functions for Cattail Inspection Progress App
# Functions to handle the planned treatment tab functionality

# Function to get treatment plan data
get_treatment_plan_data <- function() {
  con <- get_db_connection()
  if (is.null(con)) return(data.frame())
  
  # Query to fetch data with acres_plan
  query <- "
SELECT facility, airgrnd_plan, SUM(acres_plan) as total_acres
FROM public.dblarv_insptrt_current
WHERE action = '9'
AND acres_plan IS NOT NULL
GROUP BY airgrnd_plan, facility
ORDER BY airgrnd_plan, facility
"
  
  result <- dbGetQuery(con, query)
  dbDisconnect(con)
  
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

# Function to create treatment plan plot
create_treatment_plan_plot <- function(facility_filter, plan_types_filter) {
  # Get data
  data <- get_treatment_plan_data()
  
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
  
  # Calculate summary stats if "all" is selected
  if (facility_filter == "all") {
    summary_data <- data %>%
      group_by(plan_type, airgrnd_plan) %>%
      summarize(total_acres = sum(total_acres, na.rm = TRUE),
                .groups = "drop")
    
    # Plot summary data for all facilities
    # Get centralized colors for treatment plan types
    plan_colors <- get_treatment_plan_colors(use_names = TRUE)
    
    p <- ggplot(summary_data,
                aes(x = plan_type, y = total_acres, fill = plan_type)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = plan_colors) +
      labs(
        title = "Treatment Plan Acres by Type (All Facilities)",
        x = "Treatment Plan Type",
        y = "Total Acres",
        fill = "Plan Type"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Add acre values on top of bars
    p + geom_text(
      aes(label = scales::comma(total_acres)),
      vjust = -0.5,
      color = "black",
      size = 4
    )
    
  } else {
    # Plot data for a specific facility
    # Get centralized colors for treatment plan types
    plan_colors <- get_treatment_plan_colors(use_names = TRUE)
    
    # Get full facility name for display
    facility_lookup <- get_facility_lookup()
    facility_map <- setNames(facility_lookup$full_name, facility_lookup$short_name)
    facility_display <- ifelse(facility_filter %in% names(facility_map), 
                               facility_map[facility_filter], 
                               facility_filter)
    
    p <- ggplot(data, aes(x = plan_type, y = total_acres, fill = plan_type)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = plan_colors) +
      labs(
        title = paste(
          "Treatment Plan Acres by Type -",
          facility_display,
          "Facility"
        ),
        x = "Treatment Plan Type",
        y = "Total Acres",
        fill = "Plan Type"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Add acre values on top of bars
    p + geom_text(
      aes(label = scales::comma(total_acres)),
      vjust = -0.5,
      color = "black",
      size = 4
    )
  }
}
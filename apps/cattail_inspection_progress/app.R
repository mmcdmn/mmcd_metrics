# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
})

ui <- fluidPage(
  titlePanel("Cattail Inspection Progress by Facility"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "goal_year",
        "Year:",
        choices = 2010:2025,
        selected = as.numeric(format(Sys.Date(), "%Y"))
      ),
      selectInput(
        "goal_column",
        "Goal Type:",
        choices = c(
          "Primary Goal (p1_totsitecount)" = "p1_totsitecount",
          "Secondary Goal (p2_totsitecount)" = "p2_totsitecount"
        ),
        selected = "p1_totsitecount"
      )
    ),
    mainPanel(
      plotOutput("progressPlot", height = "600px")
    )
  )
)

server <- function(input, output) {
  inspection_data <- reactive({
    con <- dbConnect(
      RPostgres::Postgres(),
      dbname = "mmcd_data",
      host = "rds-readonly.mmcd.org",
      port = 5432,
      user = "mmcd_read",
      password = "mmcd2012"
    )
    # Get actual inspections from archive (filter by year)
    query_archive <- sprintf(
      "SELECT facility, COUNT(*) AS inspections FROM public.dblarv_insptrt_archive WHERE action = '9' AND EXTRACT(YEAR FROM inspdate) = %d GROUP BY facility",
      as.numeric(input$goal_year)
    )
    archive <- dbGetQuery(con, query_archive)
    # Get actual inspections from current (filter by year)
    query_current <- sprintf(
      "SELECT facility, COUNT(*) AS inspections FROM public.dblarv_insptrt_current WHERE action = '9' AND EXTRACT(YEAR FROM inspdate) = %d GROUP BY facility",
      as.numeric(input$goal_year)
    )
    current <- dbGetQuery(con, query_current)
    # Combine actuals
    actuals <- bind_rows(archive, current) %>%
      mutate(facility = toupper(trimws(facility))) %>%
      group_by(facility) %>%
      summarize(inspections = sum(inspections, na.rm = TRUE), .groups = "drop")
    print("ACTUALS:"); print(actuals)
    # Get goals (no year column)
    goals <- dbGetQuery(con, "SELECT facility, p1_totsitecount, p2_totsitecount FROM public.cattail_pctcomplete_base") %>%
      mutate(facility = toupper(trimws(facility))) %>%
      select(-any_of("inspections"))
    print("GOALS:"); print(goals)
    dbDisconnect(con)
    # Merge actuals and goals
    merged <- goals %>%
      left_join(actuals, by = "facility") %>%
      mutate(inspections = ifelse(is.na(inspections), 0, inspections))
    print("MERGED:"); print(merged)
    return(merged)
  })

  output$progressPlot <- renderPlot({
    data <- inspection_data()
    print("DATA FOR PLOTTING:"); print(data)
    goal_col <- input$goal_column
    # Prepare data for plotting
    plot_data <- data %>%
      select(facility, inspections, goal = all_of(goal_col)) %>%
      tidyr::pivot_longer(
        cols = c("inspections", "goal"),
        names_to = "type",
        values_to = "count"
      ) %>%
      mutate(type = recode(type, inspections = "Actual Inspections", goal = "Goal"))
    print("PLOT DATA:"); print(plot_data)
    # Plot
    ggplot(plot_data, aes(x = facility, y = count, fill = type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(
        values = c("Actual Inspections" = "#2c5aa0", "Goal" = "#e67e22"),
        labels = c("Actual Inspections", "Goal")
      ) +
      labs(
        title = "Cattail Inspections vs. Goal by Facility",
        x = "Facility",
        y = "Count",
        fill = "Legend"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)

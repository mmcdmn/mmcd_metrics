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
    print("ACTUALS facility unique:"); print(unique(actuals$facility)); print(str(actuals$facility))
    # Get goals (no year column)
    goals <- dbGetQuery(con, "SELECT facility, p1_totsitecount, p2_totsitecount FROM public.cattail_pctcomplete_base") %>%
      mutate(facility = toupper(trimws(facility))) %>%
      select(-any_of("inspections"))
    print("GOALS: "); print(goals)
    print("GOALS facility unique:"); print(unique(goals$facility)); print(str(goals$facility))
    dbDisconnect(con)
    # Create a complete facility list
    all_facilities <- union(actuals$facility, goals$facility)
    print("ALL FACILITIES:"); print(all_facilities)
    # Build actuals and goals for all facilities
    actuals_long <- data.frame(
      facility = all_facilities,
      goal = sapply(all_facilities, function(f) {
        val <- actuals$inspections[actuals$facility == f]
        if (length(val) == 0) 0 else val
      }),
      source = "Actual Inspections"
    )
    goals_long <- data.frame(
      facility = all_facilities,
      goal = sapply(all_facilities, function(f) {
        val <- goals[[input$goal_column]][goals$facility == f]
        if (length(val) == 0) 0 else val
      }),
      source = "Goal"
    )
    plot_data <- bind_rows(actuals_long, goals_long)
    print("PLOT DATA (complete):"); print(plot_data)
    return(plot_data)
  })

  output$progressPlot <- renderPlot({
    plot_data <- inspection_data()
    ggplot(plot_data, aes(x = facility, y = goal, fill = source)) +
      geom_bar(stat = "identity", position = "dodge")
  })
}

shinyApp(ui = ui, server = server)

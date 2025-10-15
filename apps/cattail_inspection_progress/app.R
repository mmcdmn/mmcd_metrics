# Load required libraries
suppressPackageStartupMessages({
  library(shiny)
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(tibble)
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
      ),
      dateInput(
        "custom_today",
        "Pretend Today is:",
        value = Sys.Date(),
        format = "yyyy-mm-dd"
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
    # Get actual inspections from archive (filter by year, date, reinspect, and join for zone)
    query_archive <- sprintf(
      paste(
        "SELECT a.facility, g.zone, COUNT(*) AS inspections",
        "FROM public.dblarv_insptrt_archive a",
        "LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, POSITION('-' IN a.sitecode)-1) = LEFT(g.sectcode, LENGTH(g.sectcode)-1)",
        "WHERE a.action = '9'",
        "  AND EXTRACT(YEAR FROM a.inspdate) = %d",
        "  AND a.inspdate <= '%s'",
        "  AND (a.reinspect IS NULL OR a.reinspect = 'f')",
        "GROUP BY a.facility, g.zone",
        sep = "\n"
      ),
      as.numeric(input$goal_year),
      as.character(input$custom_today)
    )
    archive <- dbGetQuery(con, query_archive)
    # Get actual inspections from current (filter by year, date, reinspect, and join for zone)
    query_current <- sprintf(
      paste(
        "SELECT a.facility, g.zone, COUNT(*) AS inspections",
        "FROM public.dblarv_insptrt_current a",
        "LEFT JOIN public.gis_sectcode g ON LEFT(a.sitecode, POSITION('-' IN a.sitecode)-1) = LEFT(g.sectcode, LENGTH(g.sectcode)-1)",
        "WHERE a.action = '9'",
        "  AND EXTRACT(YEAR FROM a.inspdate) = %d",
        "  AND a.inspdate <= '%s'",
        "  AND (a.reinspect IS NULL OR a.reinspect = 'f')",
        "GROUP BY a.facility, g.zone",
        sep = "\n"
      ),
      as.numeric(input$goal_year),
      as.character(input$custom_today)
    )
    current <- dbGetQuery(con, query_current)
    # Combine actuals
    actuals <- bind_rows(archive, current) %>%
      mutate(facility = toupper(trimws(facility)), zone = as.character(zone)) %>%
      group_by(facility, zone) %>%
      summarize(inspections = sum(as.numeric(inspections), na.rm = TRUE), .groups = "drop")
    actuals$inspections <- as.numeric(actuals$inspections)
    # Get goals
    goals <- dbGetQuery(con, "SELECT facility, p1_totsitecount, p2_totsitecount FROM public.cattail_pctcomplete_base") %>%
      mutate(facility = toupper(trimws(facility)))
    dbDisconnect(con)
    # Determine which zone to use based on goal_column
    selected_zone <- ifelse(input$goal_column == "p1_totsitecount", "1", "2")
    # Filter actuals for the selected zone
    actuals_zone <- actuals %>% filter(zone == selected_zone)
    # Build a complete facility list for this zone
    all_facilities <- union(actuals_zone$facility, goals$facility)
    # Build actuals and goals for all facilities in this zone
    # Use tibble and purrr for robust construction
    actuals_long <- tibble::tibble(
      facility = all_facilities,
      count = purrr::map_dbl(all_facilities, function(f) {
        val <- actuals_zone$inspections[actuals_zone$facility == f]
        if (length(val) == 0) 0 else val
      }),
      type = "Actual Inspections"
    )
    goals_long <- tibble::tibble(
      facility = all_facilities,
      count = purrr::map_dbl(all_facilities, function(f) {
        val <- goals[[input$goal_column]][goals$facility == f]
        if (length(val) == 0) 0 else val
      }),
      type = "Goal"
    )
    plot_data <- dplyr::bind_rows(actuals_long, goals_long)
    return(plot_data)
  })

  output$progressPlot <- renderPlot({
    plot_data <- inspection_data()
    ggplot(plot_data, aes(x = facility, y = count, fill = type)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.7) +
      scale_fill_manual(values = c("Actual Inspections" = "#2c5aa0", "Goal" = "#e67e22")) +
      labs(
        title = "Cattail Inspections vs. Goal by Facility",
        x = "Facility",
        y = "Number of Sites",
        fill = "Legend"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)

library(shiny)
library(leaflet)
library(DBI)
library(RPostgres)

# Source shared database helpers
source("../../../shared/db_helpers.R", local = TRUE)

ui <- fluidPage(
  titlePanel("QGIS Server Integration Test"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Test Configuration"),
      
      # QGIS Server URL input
      textInput("qgis_url", 
                "QGIS Server URL:", 
                value = "http://localhost/qgis/"),
      
      # Test buttons
      actionButton("test_qgis", "Test QGIS Server", class = "btn-primary"),
      actionButton("test_db", "Test Database", class = "btn-info"),
      actionButton("load_wms", "Load WMS Layer", class = "btn-success"),
      
      hr(),
      
      h4("Test Results"),
      verbatimTextOutput("test_results"),
      
      hr(),
      
      h4("Layer Configuration"),
      textInput("layer_name", "Layer Name:", value = "trap_sites"),
      textInput("map_file", "QGIS Project:", value = "/qgis/projects/trap_surveillance.qgs")
    ),
    
    mainPanel(
      h3("Interactive Map with QGIS WMS"),
      leafletOutput("map", height = "600px"),
      
      hr(),
      
      h4("Database Query Results"),
      tableOutput("db_results")
    )
  )
)

server <- function(input, output, session) {
  
  # Test results reactive value
  test_output <- reactiveVal("")
  
  # Test QGIS Server connection
  observeEvent(input$test_qgis, {
    tryCatch({
      # Construct GetCapabilities URL
      capabilities_url <- paste0(
        input$qgis_url,
        "?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetCapabilities"
      )
      
      # Try to fetch capabilities
      result <- readLines(capabilities_url, warn = FALSE)
      
      if (length(result) > 0 && any(grepl("WMS_Capabilities", result))) {
        test_output(paste0(
          "✓ QGIS Server is running!\n",
          "✓ Capabilities URL: ", capabilities_url, "\n",
          "✓ Response received: ", nchar(paste(result, collapse = "")), " characters"
        ))
      } else {
        test_output("✗ QGIS Server responded but no valid WMS capabilities found")
      }
    }, error = function(e) {
      test_output(paste0(
        "✗ QGIS Server connection failed\n",
        "Error: ", e$message, "\n",
        "URL: ", input$qgis_url
      ))
    })
  })
  
  # Test database connection
  observeEvent(input$test_db, {
    tryCatch({
      conn <- get_db_connection()
      
      # Test query
      query <- "SELECT COUNT(*) as count FROM traps LIMIT 1"
      result <- dbGetQuery(conn, query)
      
      dbDisconnect(conn)
      
      test_output(paste0(
        "✓ Database connection successful!\n",
        "✓ Traps table accessible\n",
        "✓ Connection type: PostgreSQL"
      ))
    }, error = function(e) {
      test_output(paste0(
        "✗ Database connection failed\n",
        "Error: ", e$message
      ))
    })
  })
  
  # Load WMS layer on map
  observeEvent(input$load_wms, {
    tryCatch({
      # Construct WMS URL
      wms_url <- paste0(
        input$qgis_url,
        "?map=", input$map_file
      )
      
      # Update map with WMS layer
      leafletProxy("map") %>%
        clearTiles() %>%
        clearGroup("wms_layer") %>%
        addTiles() %>%
        addWMSTiles(
          baseUrl = wms_url,
          layers = input$layer_name,
          options = WMSTileOptions(
            format = "image/png",
            transparent = TRUE,
            version = "1.3.0"
          ),
          group = "wms_layer"
        )
      
      test_output(paste0(
        "✓ WMS layer loaded on map\n",
        "Layer: ", input$layer_name, "\n",
        "URL: ", wms_url
      ))
    }, error = function(e) {
      test_output(paste0(
        "✗ Failed to load WMS layer\n",
        "Error: ", e$message
      ))
    })
  })
  
  # Display test results
  output$test_results <- renderText({
    test_output()
  })
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -93.2650, lat = 44.9778, zoom = 10)  # Minneapolis area
  })
  
  # Query database for trap data using real query from trap_surveillance_test
  output$db_results <- renderTable({
    tryCatch({
      conn <- get_db_connection()
      
      # Real query from trap_surveillance_test - get recent trap inspections with species counts
      query <- "
        WITH ranked_traps AS (
          SELECT t.ainspecnum, t.facility, t.x, t.y, t.survtype, t.inspdate,
                 ROW_NUMBER() OVER (PARTITION BY t.x, t.y, t.facility ORDER BY t.inspdate DESC) as rn
          FROM public.dbadult_insp_current t
          WHERE t.inspdate::date >= (CURRENT_DATE - INTERVAL '30 days')
            AND t.x IS NOT NULL AND t.y IS NOT NULL
            AND t.survtype IN ('4', '5', '6')
        ),
        latest_traps AS (
          SELECT ainspecnum, facility, x, y, survtype, inspdate
          FROM ranked_traps
          WHERE rn = 1
          LIMIT 10
        )
        SELECT 
          lt.ainspecnum as inspection_id,
          lt.facility,
          lt.x as longitude,
          lt.y as latitude,
          lt.survtype as trap_type,
          lt.inspdate::date as inspection_date,
          COALESCE(SUM(s.cnt), 0) as total_count
        FROM latest_traps lt
        LEFT JOIN public.dbadult_species_current s ON lt.ainspecnum = s.ainspecnum
        GROUP BY lt.ainspecnum, lt.facility, lt.x, lt.y, lt.survtype, lt.inspdate
        ORDER BY lt.inspdate DESC
      "
      
      result <- dbGetQuery(conn, query)
      dbDisconnect(conn)
      
      # Format trap type for display
      result$trap_type <- sapply(result$trap_type, function(x) {
        switch(as.character(x),
               "4" = "Elevated CO2",
               "5" = "Gravid Trap",
               "6" = "CO2 Overnight",
               x)
      })
      
      result
    }, error = function(e) {
      data.frame(Error = paste("Database query failed:", e$message))
    })
  })
}

shinyApp(ui = ui, server = server)
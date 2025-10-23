library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Source shared helper functions for testing
source("../../shared/db_helpers.R")

# Load environment variables for database testing
load_env_variables()

# Test application with shared functions
ui <- fluidPage(
  titlePanel("MMCD Test Application - Database Functions"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Test Application"),
      p("This application tests the shared database helper functions."),
      
      hr(),
      h4("Built-in R Data Tests"),
      selectInput("dataset", "Choose a dataset:",
                  choices = c("mtcars", "iris", "faithful")),
      
      numericInput("obs", "Number of observations:", 
                   value = 10, min = 1, max = 100),
      
      hr(),
      h4("Database Function Tests"),
      actionButton("test_db", "Test Database Connection", class = "btn-info"),
      br(), br(),
      actionButton("test_facilities", "Test Facility Lookup", class = "btn-warning"),
      br(), br(),
      actionButton("test_materials", "Test Material Lookup", class = "btn-success"),
      br(), br(),
      actionButton("test_colors", "Test Color Mappings", class = "btn-secondary"),
      br(), br(),
      actionButton("refresh", "Refresh", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 plotOutput("plot")),
        tabPanel("Summary",
                 verbatimTextOutput("summary")),
        tabPanel("Data", 
                 tableOutput("table")),
        tabPanel("DB Connection Test",
                 h4("Database Connection Status"),
                 verbatimTextOutput("db_status")),
        tabPanel("Facility Test",
                 h4("Facility Lookup Results"),
                 verbatimTextOutput("facility_lookup"),
                 h4("Facility Choices"),
                 verbatimTextOutput("facility_choices"),
                 h4("Facility Mapping Test"),
                 DT::dataTableOutput("facility_mapping_test")),
        tabPanel("Material Test",
                 h4("Material Types"),
                 DT::dataTableOutput("material_types"),
                 h4("Material Choices"),
                 verbatimTextOutput("material_choices")),
        tabPanel("Color Test",
                 h4("Comprehensive Status Colors"),
                 plotOutput("all_colors_plot"),
                 verbatimTextOutput("color_list"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Get selected dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "mtcars" = mtcars,
           "iris" = iris,
           "faithful" = faithful)
  })
  
  # Create plot
  output$plot <- renderPlot({
    dataset <- datasetInput()
    
    if (input$dataset == "mtcars") {
      ggplot(dataset, aes(x = wt, y = mpg)) +
        geom_point() +
        geom_smooth(method = "lm") +
        theme_minimal() +
        labs(title = "Car Weight vs MPG", x = "Weight", y = "Miles per Gallon")
    } else if (input$dataset == "iris") {
      ggplot(dataset, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point() +
        theme_minimal() +
        labs(title = "Iris Sepal Dimensions", x = "Sepal Length", y = "Sepal Width")
    } else {
      ggplot(dataset, aes(x = waiting, y = eruptions)) +
        geom_point() +
        theme_minimal() +
        labs(title = "Old Faithful Geyser", x = "Waiting Time", y = "Eruption Duration")
    }
  })
  
  # Show summary
  output$summary <- renderPrint({
    summary(datasetInput())
  })
  
  # Show data table
  output$table <- renderTable({
    head(datasetInput(), input$obs)
  })
  
  # Database connection test
  observeEvent(input$test_db, {
    output$db_status <- renderText({
      tryCatch({
        con <- get_db_connection()
        if (is.null(con)) {
          "❌ Database connection failed - get_db_connection() returned NULL"
        } else {
          # Test a simple query
          result <- dbGetQuery(con, "SELECT 1 as test")
          dbDisconnect(con)
          if (nrow(result) == 1 && result$test == 1) {
            "✅ Database connection successful! Simple query worked."
          } else {
            "⚠️ Database connected but query returned unexpected result"
          }
        }
      }, error = function(e) {
        paste("❌ Database connection error:", e$message)
      })
    })
  })
  
  # Facility lookup test
  observeEvent(input$test_facilities, {
    output$facility_lookup <- renderText({
      tryCatch({
        facilities <- get_facility_lookup()
        if (nrow(facilities) == 0) {
          "❌ No facilities found - check database connection and table structure"
        } else {
          paste(
            "✅ Found", nrow(facilities), "facilities:",
            paste(capture.output(print(facilities)), collapse = "\n"),
            sep = "\n"
          )
        }
      }, error = function(e) {
        paste("❌ Facility lookup error:", e$message)
      })
    })
    
    output$facility_choices <- renderText({
      tryCatch({
        choices <- get_facility_choices()
        paste(
          "✅ Facility choices for dropdowns:",
          paste(names(choices), "=", choices, collapse = "\n"),
          sep = "\n"
        )
      }, error = function(e) {
        paste("❌ Facility choices error:", e$message)
      })
    })
    
    output$facility_mapping_test <- DT::renderDataTable({
      tryCatch({
        # Create test data with facility codes
        test_data <- data.frame(
          facility = c("N", "E", "UNKNOWN", "S"),
          value = c(100, 200, 50, 150),
          stringsAsFactors = FALSE
        )
        
        # Test the mapping function
        mapped_data <- map_facility_names(test_data, "facility")
        mapped_data
      }, error = function(e) {
        data.frame(Error = paste("Facility mapping error:", e$message))
      })
    }, options = list(pageLength = 10))
  })
  
  # Material lookup test
  observeEvent(input$test_materials, {
    output$material_types <- DT::renderDataTable({
      tryCatch({
        get_material_types()
      }, error = function(e) {
        data.frame(Error = paste("Material types error:", e$message))
      })
    }, options = list(pageLength = 10))
    
    output$material_choices <- renderText({
      tryCatch({
        choices <- get_material_choices()
        if (length(choices) <= 20) {
          paste(
            "✅ Material choices for dropdowns:",
            paste(names(choices), "=", choices, collapse = "\n"),
            sep = "\n"
          )
        } else {
          paste(
            "✅ Material choices for dropdowns (showing first 20):",
            paste(names(choices)[1:20], "=", choices[1:20], collapse = "\n"),
            paste("... and", length(choices) - 20, "more"),
            sep = "\n"
          )
        }
      }, error = function(e) {
        paste("❌ Material choices error:", e$message)
      })
    })
  })
  
  # Color mapping tests
  observeEvent(input$test_colors, {
    # Comprehensive color test
    output$all_colors_plot <- renderPlot({
      tryCatch({
        colors <- get_status_colors()
        descriptions <- get_status_descriptions()
        
        color_data <- data.frame(
          status = names(colors),
          color = colors,
          description = descriptions[names(colors)],
          y_pos = length(colors):1,  # Reverse order for better display
          stringsAsFactors = FALSE
        )
        
        # Group colors by type for better organization
        color_data$category <- case_when(
          color_data$status %in% c("total", "active", "expiring") ~ "Treatment Progress",
          color_data$status %in% c("RED", "YELLOW", "GREEN", "HIGH", "MEDIUM", "LOW") ~ "Priority",
          TRUE ~ "General Status"
        )
        
        ggplot(color_data, aes(x = 1, y = y_pos, fill = status)) +
          geom_col(width = 0.6) +
          scale_fill_manual(values = setNames(color_data$color, color_data$status)) +
          geom_text(aes(label = paste(status, "-", description, "[", category, "]")), 
                    x = 1.7, hjust = 0, size = 3) +
          labs(title = "All Status Colors (from get_status_colors())",
               subtitle = "Comprehensive color mapping used across all applications") +
          theme_void() +
          theme(legend.position = "none",
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 10),
                axis.text = element_blank()) +
          xlim(0.5, 6)
      }, error = function(e) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Error loading colors:", e$message)) +
          theme_void()
      })
    })
    
    # Color list output
    output$color_list <- renderText({
      tryCatch({
        colors <- get_status_colors()
        descriptions <- get_status_descriptions()
        
        color_info <- paste(
          "✅ Loaded", length(colors), "color mappings:",
          "",
          "Treatment Progress Colors:",
          paste("  • total =", colors["total"], "(", descriptions["total"], ")"),
          "",
          "General Status Colors:",
          paste("  • Active Treatment =", colors["Active Treatment"], "(", descriptions["Active Treatment"], ")"),
          paste("  • Needs Treatment =", colors["Needs Treatment"], "(", descriptions["Needs Treatment"], ")"),
          paste("  • Completed =", colors["Completed"], "(", descriptions["Completed"], ")"),
          paste("  • Pending =", colors["Pending"], "(", descriptions["Pending"], ")"),
          "",
          "Priority Colors:",
          paste("  • HIGH/RED =", colors["HIGH"], "(", descriptions["HIGH"], ")"),
          paste("  • MEDIUM/YELLOW =", colors["MEDIUM"], "(", descriptions["MEDIUM"], ")"),
          paste("  • LOW/BLUE =", colors["LOW"], "(", descriptions["LOW"], ")"),
          "",
          "Special Status Colors:",
          paste("  • PREHATCH =", colors["PREHATCH"], "(", descriptions["PREHATCH"], ")"),
          "",
          sep = "\n"
        )
        
        return(color_info)
      }, error = function(e) {
        return(paste("❌ Error loading colors:", e$message))
      })
    })
  })
}

shinyApp(ui = ui, server = server)

library(shiny)
library(ggplot2)
library(dplyr)

# Simple test application without database dependency
ui <- fluidPage(
  titlePanel("MMCD Test Application"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Test Application"),
      p("This is a simple test application to verify that Shiny Server is working properly."),
      
      selectInput("dataset", "Choose a dataset:",
                  choices = c("mtcars", "iris", "faithful")),
      
      numericInput("obs", "Number of observations:", 
                   value = 10, min = 1, max = 100),
      
      actionButton("refresh", "Refresh", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 plotOutput("plot")),
        tabPanel("Summary",
                 verbatimTextOutput("summary")),
        tabPanel("Data", 
                 tableOutput("table"))
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
}

shinyApp(ui = ui, server = server)

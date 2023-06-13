#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(leaflet)
library(dplyr)

# Read the CSV data
data <- read.csv("https://raw.githubusercontent.com/emneli/emneli/main/listingsSummaryMadrid.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("neighborhood", "Select Neighborhood:",
                  choices = unique(data$neighbourhood_group)),
      selectInput("room_type", "Select Room Type:",
                  choices = unique(data$room_type))
    ),
    mainPanel(
      leafletOutput("map"),
      plotlyOutput("scatterplot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Filter the data based on user inputs
  filteredData <- reactive({
    data %>%
      filter(neighbourhood_group == input$neighborhood,
             room_type == input$room_type)
  })
  
  # Render the Leaflet map
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~name
      )
  })
  
  # Render the scatter plot
  output$scatterplot <- renderPlotly({
    filteredData() %>%
      plot_ly(x = ~price, y = ~reviews_per_month, type = "scatter", mode = "markers")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
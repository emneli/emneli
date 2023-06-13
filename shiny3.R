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
library(shiny)
library(leaflet)
library(geojsonio)
library(dplyr)

library(leaflet)
library(geojson)
library(sp)

# Read the CSV data
data <- read.csv("https://raw.githubusercontent.com/emneli/emneli/main/listingsSummaryMadrid.csv")

# Read the GeoJSON file
data_geojson <- geojson_read("https://raw.githubusercontent.com/emneli/emneli/main/neighbourhoodsGeojon.geojson", what = "sp")

# Extract the neighborhood and price columns
neighborhood <- data$neighbourhood
price <- data$price

# Calculate the average price per neighborhood
average_price <- tapply(price, neighborhood, mean)

# Create a new data frame with neighborhood and average price
neighborhood_prices <- data.frame(neighborhood = names(average_price), average_price)

# Merge neighborhood_prices with GeoJSON data
merged_data <- merge(data_geojson, neighborhood_prices, by.x = "neighbourhood", by.y = "neighborhood", all.x = TRUE)



# Objetos necesarios para la obtencion del choropleth
bins <- c(30, 50, 75, 100, 150, 200, 300, 400, 500)
pal <- colorBin("YlOrRd", domain = merged_data$average_price, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g euros",
  merged_data$neighbourhood, merged_data$average_price
) %>% lapply(htmltools::HTML)




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
      leafletOutput("choropleth"),
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
  
  # Render the Leaflet Choropleth
  output$choropleth <- renderLeaflet({
    leaflet(merged_data) %>%
              setView(lng = -3.70379, lat = 40.41678, zoom = 9.5) %>%
              addProviderTiles("MapBox", options = providerTileOptions(
                id = "mapbox.light",
                accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
              addPolygons(
                fillColor = ~pal(average_price),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlightOptions = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto")) %>%
              addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                        position = "bottomright")
  })
  
  
  # Render the scatter plot
  output$scatterplot <- renderPlotly({
    filteredData() %>%
      plot_ly(x = ~price, y = ~reviews_per_month, type = "scatter", mode = "markers")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
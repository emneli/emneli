
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




bins <- c(30, 50, 75, 100, 150, 200, 300, 400, 500)
pal <- colorBin("YlOrRd", domain = merged_data$average_price, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g euros",
  merged_data$neighbourhood, merged_data$average_price
) %>% lapply(htmltools::HTML)

leaflet(merged_data) %>%
  setView(lng = -3.70379, lat = 40.41678, zoom = 13) %>%
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

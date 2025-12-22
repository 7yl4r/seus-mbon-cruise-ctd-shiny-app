# SE-US MBON Cruise CTD Data Shiny App
# This app displays an interactive map on the left and plots on the right

library(shiny)
library(leaflet)
library(plotly)
library(dplyr)

# Sample CTD data for demonstration
# In a real application, this would be loaded from a file
sample_ctd_data <- data.frame(
  station_id = c("ST001", "ST002", "ST003", "ST004", "ST005"),
  latitude = c(32.0, 31.5, 31.0, 30.5, 30.0),
  longitude = c(-79.0, -79.5, -80.0, -80.5, -81.0),
  depth = c(50, 75, 100, 125, 150),
  temperature = c(22.5, 21.8, 20.2, 19.5, 18.9),
  salinity = c(35.2, 35.5, 35.8, 36.0, 36.2),
  oxygen = c(6.5, 6.2, 5.8, 5.5, 5.2)
)

# Helper function to create CTD profile plots
create_ctd_plot <- function(data, x_var, y_var, x_label, color, hover_format) {
  plot_ly(data, 
          x = as.formula(paste0("~", x_var)), 
          y = as.formula(paste0("~-", y_var)),
          type = 'scatter',
          mode = 'markers+lines',
          text = ~station_id,
          hovertemplate = hover_format,
          customdata = as.formula(paste0("~", y_var)),
          marker = list(size = 10, color = color)) %>%
    layout(
      xaxis = list(title = x_label),
      yaxis = list(title = "Depth (m)"),
      margin = list(l = 50, r = 20, t = 40, b = 40)
    )
}

# Define UI
ui <- fluidPage(
  titlePanel("SE-US MBON Cruise CTD Data Explorer"),
  
  fluidRow(
    # Left column: Interactive Map
    column(width = 6,
           wellPanel(
             h4("Station Locations"),
             leafletOutput("map", height = "600px")
           )
    ),
    
    # Right column: Plots
    column(width = 6,
           wellPanel(
             h4("CTD Data Visualizations"),
             tabsetPanel(
               tabPanel("Temperature vs Depth",
                        plotlyOutput("temp_plot", height = "250px")
               ),
               tabPanel("Salinity vs Depth",
                        plotlyOutput("salinity_plot", height = "250px")
               ),
               tabPanel("Oxygen vs Depth",
                        plotlyOutput("oxygen_plot", height = "250px")
               )
             )
           ),
           wellPanel(
             h4("Station Information"),
             verbatimTextOutput("station_info")
           )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store selected station
  selected_station <- reactiveVal(NULL)
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet(sample_ctd_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        color = "#0066CC",
        fillColor = "#0066CC",
        fillOpacity = 0.7,
        popup = ~paste0("<b>Station: ", station_id, "</b><br>",
                       "Lat: ", latitude, "<br>",
                       "Lon: ", longitude, "<br>",
                       "Depth: ", depth, " m<br>",
                       "Temp: ", temperature, " °C<br>",
                       "Salinity: ", salinity, " PSU"),
        layerId = ~station_id
      ) %>%
      fitBounds(
        lng1 = min(sample_ctd_data$longitude) - 0.5,
        lat1 = min(sample_ctd_data$latitude) - 0.5,
        lng2 = max(sample_ctd_data$longitude) + 0.5,
        lat2 = max(sample_ctd_data$latitude) + 0.5
      )
  })
  
  # Handle map marker clicks
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    selected_station(click$id)
  })
  
  # Temperature vs Depth plot
  output$temp_plot <- renderPlotly({
    create_ctd_plot(
      sample_ctd_data,
      "temperature",
      "depth",
      "Temperature (°C)",
      "#FF6B6B",
      paste('<b>%{text}</b><br>',
            'Temperature: %{x:.1f} °C<br>',
            'Depth: %{customdata} m<br>',
            '<extra></extra>')
    )
  })
  
  # Salinity vs Depth plot
  output$salinity_plot <- renderPlotly({
    create_ctd_plot(
      sample_ctd_data,
      "salinity",
      "depth",
      "Salinity (PSU)",
      "#4ECDC4",
      paste('<b>%{text}</b><br>',
            'Salinity: %{x:.1f} PSU<br>',
            'Depth: %{customdata} m<br>',
            '<extra></extra>')
    )
  })
  
  # Oxygen vs Depth plot
  output$oxygen_plot <- renderPlotly({
    create_ctd_plot(
      sample_ctd_data,
      "oxygen",
      "depth",
      "Dissolved Oxygen (mg/L)",
      "#95E1D3",
      paste('<b>%{text}</b><br>',
            'Oxygen: %{x:.1f} mg/L<br>',
            'Depth: %{customdata} m<br>',
            '<extra></extra>')
    )
  })
  
  # Station information display
  output$station_info <- renderText({
    if (is.null(selected_station())) {
      "Click on a station marker on the map to see details."
    } else {
      station_data <- sample_ctd_data %>% 
        filter(station_id == selected_station())
      
      if (nrow(station_data) > 0) {
        # Format longitude with proper hemisphere indicator
        lon_hemisphere <- if (station_data$longitude < 0) "W" else "E"
        lon_value <- abs(station_data$longitude)
        
        # Format latitude with proper hemisphere indicator
        lat_hemisphere <- if (station_data$latitude < 0) "S" else "N"
        lat_value <- abs(station_data$latitude)
        
        paste0(
          "Station ID: ", station_data$station_id, "\n",
          "Position: ", lat_value, "°", lat_hemisphere, ", ", 
                       lon_value, "°", lon_hemisphere, "\n",
          "Depth: ", station_data$depth, " m\n",
          "Temperature: ", station_data$temperature, " °C\n",
          "Salinity: ", station_data$salinity, " PSU\n",
          "Dissolved Oxygen: ", station_data$oxygen, " mg/L"
        )
      } else {
        "Station data not found."
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

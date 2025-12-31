# SE-US MBON Cruise CTD Data Shiny App
# This app displays an interactive map on the left and plots on the right

library(shiny)
library(leaflet)
library(plotly)
library(dplyr)

# Sample CTD data for demonstration
# In a real application, this would be loaded from CSV files
# Data structure matches the real CSV format: scan, salinity, temperature, pressure, time, station, cruise_id
ctd_profile_data <- data.frame(
  scan = c(
    # Station WS data
    1355.23, 888.65, 650.42, 450.33, 320.15, 220.87, 150.42, 100.25, 75.18, 50.12,
    # Station ST001 data
    1400.5, 950.2, 680.1, 480.5, 340.2, 240.8, 160.3, 110.4, 80.2, 55.1,
    # Station ST002 data
    1380.3, 920.4, 660.8, 470.2, 330.5, 230.6, 155.7, 105.8, 78.3, 52.4,
    # Station ST003 data
    1420.1, 970.3, 700.5, 500.1, 360.4, 250.2, 170.6, 120.3, 85.7, 60.2,
    # Station ST004 data
    1390.8, 940.6, 675.3, 485.7, 345.8, 235.4, 158.9, 108.5, 79.6, 53.8
  ),
  salinity = c(
    # Station WS data
    36.36, 36.48, 36.52, 36.55, 36.58, 36.60, 36.62, 36.64, 36.65, 36.66,
    # Station ST001 data
    35.20, 35.45, 35.60, 35.75, 35.85, 35.92, 35.98, 36.02, 36.05, 36.08,
    # Station ST002 data
    35.50, 35.72, 35.85, 35.95, 36.02, 36.08, 36.12, 36.15, 36.18, 36.20,
    # Station ST003 data
    35.80, 35.98, 36.08, 36.15, 36.20, 36.24, 36.27, 36.30, 36.32, 36.34,
    # Station ST004 data
    36.00, 36.15, 36.22, 36.28, 36.32, 36.35, 36.38, 36.40, 36.42, 36.44
  ),
  temperature = c(
    # Station WS data
    24.80, 24.83, 24.65, 24.20, 23.50, 22.80, 21.90, 20.80, 19.50, 18.20,
    # Station ST001 data
    22.50, 22.30, 22.00, 21.50, 21.00, 20.40, 19.70, 18.90, 18.00, 17.20,
    # Station ST002 data
    21.80, 21.60, 21.30, 20.90, 20.40, 19.80, 19.10, 18.30, 17.50, 16.80,
    # Station ST003 data
    20.20, 20.00, 19.70, 19.30, 18.80, 18.20, 17.50, 16.80, 16.10, 15.50,
    # Station ST004 data
    19.50, 19.30, 19.00, 18.60, 18.10, 17.50, 16.90, 16.20, 15.60, 15.00
  ),
  pressure = c(
    # Station WS data (approximately depth in decibars)
    2.5, 3.0, 5.0, 10.0, 15.0, 20.0, 30.0, 40.0, 50.0, 60.0,
    # Station ST001 data
    2.5, 3.5, 6.0, 12.0, 18.0, 25.0, 35.0, 45.0, 55.0, 65.0,
    # Station ST002 data
    3.0, 4.0, 7.0, 13.0, 20.0, 28.0, 38.0, 48.0, 58.0, 70.0,
    # Station ST003 data
    3.5, 5.0, 8.0, 15.0, 23.0, 32.0, 42.0, 52.0, 62.0, 75.0,
    # Station ST004 data
    4.0, 6.0, 10.0, 18.0, 27.0, 37.0, 47.0, 57.0, 67.0, 80.0
  ),
  time = NA,  # Time data not provided in sample
  station = c(
    rep("WS", 10),
    rep("ST001", 10),
    rep("ST002", 10),
    rep("ST003", 10),
    rep("ST004", 10)
  ),
  cruise_id = "WS21093"
)

# Station location metadata (for the map)
station_locations <- data.frame(
  station = c("WS", "ST001", "ST002", "ST003", "ST004"),
  latitude = c(32.0, 31.5, 31.0, 30.5, 30.0),
  longitude = c(-79.0, -79.5, -80.0, -80.5, -81.0)
)

# Helper function to create CTD profile plots for a specific station
create_ctd_plot <- function(data, x_var, y_var, x_label, color, hover_format) {
  plot_ly(data, 
          x = as.formula(paste0("~", x_var)), 
          y = as.formula(paste0("~-", y_var)),
          type = 'scatter',
          mode = 'markers+lines',
          text = ~station,
          hovertemplate = hover_format,
          customdata = as.formula(paste0("~", y_var)),
          marker = list(size = 8, color = color),
          line = list(color = color, width = 2)) %>%
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
    leaflet(station_locations) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 8,
        color = "#0066CC",
        fillColor = "#0066CC",
        fillOpacity = 0.7,
        popup = ~paste0("<b>Station: ", station, "</b><br>",
                       "Lat: ", latitude, "<br>",
                       "Lon: ", longitude),
        layerId = ~station
      ) %>%
      fitBounds(
        lng1 = min(station_locations$longitude) - 0.5,
        lat1 = min(station_locations$latitude) - 0.5,
        lng2 = max(station_locations$longitude) + 0.5,
        lat2 = max(station_locations$latitude) + 0.5
      )
  })
  
  # Handle map marker clicks
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    selected_station(click$id)
  })
  
  # Temperature vs Depth plot
  output$temp_plot <- renderPlotly({
    # Filter data for selected station, or show all if none selected
    plot_data <- if (!is.null(selected_station())) {
      ctd_profile_data %>% filter(station == selected_station())
    } else {
      ctd_profile_data
    }
    
    create_ctd_plot(
      plot_data,
      "temperature",
      "pressure",
      "Temperature (°C)",
      "#FF6B6B",
      paste('<b>Station: %{text}</b><br>',
            'Temperature: %{x:.2f} °C<br>',
            'Depth: %{customdata:.1f} m<br>',
            '<extra></extra>')
    )
  })
  
  # Salinity vs Depth plot
  output$salinity_plot <- renderPlotly({
    # Filter data for selected station, or show all if none selected
    plot_data <- if (!is.null(selected_station())) {
      ctd_profile_data %>% filter(station == selected_station())
    } else {
      ctd_profile_data
    }
    
    create_ctd_plot(
      plot_data,
      "salinity",
      "pressure",
      "Salinity (PSU)",
      "#4ECDC4",
      paste('<b>Station: %{text}</b><br>',
            'Salinity: %{x:.2f} PSU<br>',
            'Depth: %{customdata:.1f} m<br>',
            '<extra></extra>')
    )
  })
  
  # Oxygen vs Depth plot - Note: oxygen data not in CSV, showing message
  output$oxygen_plot <- renderPlotly({
    # Create empty plot with message
    plot_ly() %>%
      layout(
        xaxis = list(title = "Dissolved Oxygen (mg/L)"),
        yaxis = list(title = "Depth (m)"),
        annotations = list(
          text = "Oxygen data not available in CTD profiles",
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 14, color = "#999")
        ),
        margin = list(l = 50, r = 20, t = 40, b = 40)
      )
  })
  
  # Station information display
  output$station_info <- renderText({
    if (is.null(selected_station())) {
      "Click on a station marker on the map to see details."
    } else {
      station_loc <- station_locations %>% 
        filter(station == selected_station())
      
      station_profile <- ctd_profile_data %>%
        filter(station == selected_station())
      
      if (nrow(station_loc) > 0 && nrow(station_profile) > 0) {
        # Format longitude with proper hemisphere indicator
        lon_hemisphere <- if (station_loc$longitude < 0) "W" else "E"
        lon_value <- abs(station_loc$longitude)
        
        # Format latitude with proper hemisphere indicator
        lat_hemisphere <- if (station_loc$latitude < 0) "S" else "N"
        lat_value <- abs(station_loc$latitude)
        
        # Calculate summary statistics
        depth_range <- range(station_profile$pressure, na.rm = TRUE)
        temp_range <- range(station_profile$temperature, na.rm = TRUE)
        sal_range <- range(station_profile$salinity, na.rm = TRUE)
        
        paste0(
          "Station: ", selected_station(), "\n",
          "Cruise ID: ", unique(station_profile$cruise_id), "\n",
          "Position: ", lat_value, "°", lat_hemisphere, ", ", 
                       lon_value, "°", lon_hemisphere, "\n\n",
          "Profile Summary:\n",
          "  Depth range: ", sprintf("%.1f - %.1f m", depth_range[1], depth_range[2]), "\n",
          "  Temperature: ", sprintf("%.2f - %.2f °C", temp_range[1], temp_range[2]), "\n",
          "  Salinity: ", sprintf("%.2f - %.2f PSU", sal_range[1], sal_range[2]), "\n",
          "  Data points: ", nrow(station_profile)
        )
      } else {
        "Station data not found."
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

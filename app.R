# SE-US MBON Cruise CTD Data Shiny App
# This app displays an interactive map on the left and plots on the right

library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(readr)

# Load CTD profile data from CSV files
load_ctd_data <- function() {
  message("Loading CTD profile data from CSV files...")
  
  # Get list of all CSV files in data/02_clean directory
  csv_dir <- "data/02_clean"
  csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
  
  message(paste("Found", length(csv_files), "CSV files"))
  
  # Read and combine all CSV files
  ctd_data_list <- lapply(csv_files, function(file) {
    tryCatch({
      # Read CSV file with proper column types
      data <- read_csv(file, 
                      col_types = cols(
                        scan = col_double(),
                        salinity = col_double(),
                        temperature = col_double(),
                        pressure = col_double(),
                        time = col_character(),  # time may be NA or character
                        station = col_character(),
                        cruise_id = col_character()
                      ),
                      show_col_types = FALSE)
      
      # Remove the row index column if it exists (unnamed first column)
      if (names(data)[1] %in% c("", "...1")) {
        data <- data[, -1]
      }
      
      return(data)
    }, error = function(e) {
      warning(paste("Error reading file", file, ":", e$message))
      return(NULL)
    })
  })
  
  # Remove NULL entries (failed reads)
  ctd_data_list <- ctd_data_list[!sapply(ctd_data_list, is.null)]
  
  # Combine all data frames
  if (length(ctd_data_list) > 0) {
    ctd_data <- bind_rows(ctd_data_list)
    message(paste("Loaded", nrow(ctd_data), "CTD profile records from", length(ctd_data_list), "files"))
    return(ctd_data)
  } else {
    warning("No CTD data could be loaded")
    return(data.frame())
  }
}

# Load the CTD profile data
ctd_profile_data <- load_ctd_data()

# Load station location metadata from CSV
load_station_locations <- function() {
  message("Loading station location data...")
  
  tryCatch({
    station_locs <- read_csv("data/Station_Mean_Coords.csv",
                            col_types = cols(
                              station = col_character(),
                              lat_mean = col_double(),
                              lon_mean = col_double()
                            ),
                            show_col_types = FALSE)
    
    # Rename columns to match expected names
    station_locs <- station_locs %>%
      rename(latitude = lat_mean, longitude = lon_mean)
    
    message(paste("Loaded", nrow(station_locs), "station locations"))
    return(station_locs)
  }, error = function(e) {
    warning(paste("Error loading station locations:", e$message))
    # Return empty dataframe with correct structure
    return(data.frame(station = character(), latitude = numeric(), longitude = numeric()))
  })
}

# Station location metadata (for the map)
station_locations <- load_station_locations()

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
  
  # Reactive expression to get filtered data for selected station
  filtered_ctd_data <- reactive({
    if (!is.null(selected_station())) {
      ctd_profile_data %>% filter(station == selected_station())
    } else {
      ctd_profile_data
    }
  })
  
  # Temperature vs Depth plot
  output$temp_plot <- renderPlotly({
    create_ctd_plot(
      filtered_ctd_data(),
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
    create_ctd_plot(
      filtered_ctd_data(),
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
  
  # Oxygen vs Depth plot - Note: oxygen data not in CSV format
  output$oxygen_plot <- renderPlotly({
    # Create empty plot with context-aware message
    message_text <- if (!is.null(selected_station())) {
      paste0("Oxygen data not available for station ", selected_station())
    } else {
      "Oxygen data not available in CTD profiles"
    }
    
    plot_ly() %>%
      layout(
        xaxis = list(title = "Dissolved Oxygen (mg/L)"),
        yaxis = list(title = "Depth (m)"),
        annotations = list(
          text = message_text,
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

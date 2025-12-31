# seus-mbon-cruise-ctd-shiny-app
Shiny App for exploring SE-US MBON Research Cruise CTD data

## Description

This interactive Shiny application visualizes CTD (Conductivity, Temperature, Depth) data collected during SE-US Marine Biodiversity Observation Network (MBON) research cruises. The application features:

- **Interactive Map (Left Panel)**: Displays sampling station locations with clickable markers
- **Data Visualizations (Right Panel)**: Shows CTD measurements including:
  - Temperature vs Depth profiles
  - Salinity vs Depth profiles
  - Dissolved Oxygen vs Depth profiles
- **Station Information**: Displays detailed information when a station is selected on the map

## Installation

### Required R Packages

```r
install.packages(c("shiny", "leaflet", "plotly", "dplyr"))
```

## Usage

To run the application locally:

```r
# Option 1: Using shiny::runApp()
shiny::runApp()

# Option 2: Direct execution
R -e "shiny::runApp()"
```

The application will open in your default web browser.

## Application Layout

- **Left Column**: Interactive map showing station locations
- **Right Column**: 
  - Tabbed plots for different CTD measurements
  - Station information panel

## Data

### Setup
Data from seus-mbon-cruise-ctd-processing/data/02_clean should be copied into data/02_clean.

# Application Layout

## Overview
The Shiny application uses a two-column layout with the following structure:

```
┌─────────────────────────────────────────────────────────────────┐
│              SE-US MBON Cruise CTD Data Explorer                │
├─────────────────────────────────┬───────────────────────────────┤
│                                 │                               │
│    Interactive Map              │   CTD Data Visualizations     │
│    (Left - 50% width)           │   (Right - 50% width)         │
│                                 │                               │
│  ┌───────────────────────────┐  │  ┌─────────────────────────┐ │
│  │                           │  │  │ ┌──┬──┬──┐               │ │
│  │   Leaflet Map             │  │  │ │T │S │O │ (Tabs)        │ │
│  │                           │  │  │ └──┴──┴──┘               │ │
│  │   • Shows station         │  │  │                          │ │
│  │     locations             │  │  │  Temperature vs Depth    │ │
│  │   • Clickable markers     │  │  │  Plot                    │ │
│  │   • Auto-fitted bounds    │  │  │                          │ │
│  │                           │  │  │  (or Salinity/Oxygen)    │ │
│  │   [Blue circles with      │  │  │                          │ │
│  │    station info popups]   │  │  │                          │ │
│  │                           │  │  └─────────────────────────┘ │
│  │                           │  │                               │
│  │                           │  │  ┌─────────────────────────┐ │
│  │                           │  │  │ Station Information     │ │
│  │                           │  │  │                         │ │
│  │                           │  │  │ Station ID: ST001       │ │
│  └───────────────────────────┘  │  │ Position: 32.0°N, 79°W  │ │
│                                 │  │ Depth: 50 m             │ │
│                                 │  │ Temperature: 22.5 °C    │ │
│                                 │  │ Salinity: 35.2 PSU      │ │
│                                 │  │ Oxygen: 6.5 mg/L        │ │
│                                 │  └─────────────────────────┘ │
│                                 │                               │
└─────────────────────────────────┴───────────────────────────────┘
```

## Components

### Left Column (6/12 width)
- **Interactive Leaflet Map**
  - Displays station locations along SE-US coast
  - Blue circle markers for each station
  - Popup on hover with station details
  - Click to select station (updates right panel)
  - Map tiles from OpenStreetMap

### Right Column (6/12 width)
- **Upper Panel: Tabbed Plots**
  - Tab 1: Temperature vs Depth profile
  - Tab 2: Salinity vs Depth profile
  - Tab 3: Dissolved Oxygen vs Depth profile
  - All plots use Plotly for interactivity
  - Hover tooltips show exact values
  
- **Lower Panel: Station Information**
  - Displays selected station details
  - Updates when clicking map markers
  - Shows: ID, coordinates, depth, measurements

## Data Flow
1. User clicks a station marker on the map
2. Map click event triggers `selected_station` reactive value
3. Station info panel updates with selected station data
4. Plots show all stations but could be filtered in future

## Sample Data Structure
```r
station_id | latitude | longitude | depth | temperature | salinity | oxygen
-----------|----------|-----------|-------|-------------|----------|-------
ST001      | 32.0     | -79.0     | 50    | 22.5        | 35.2     | 6.5
ST002      | 31.5     | -79.5     | 75    | 21.8        | 35.5     | 6.2
...
```

## Technologies Used
- **Shiny**: Web application framework
- **Leaflet**: Interactive maps
- **Plotly**: Interactive plots
- **dplyr**: Data manipulation

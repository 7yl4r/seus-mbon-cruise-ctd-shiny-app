# Function to get all available CSV files and their station mappings 
# using from a directory of csv files named with the 
# pattern "*_<station_name>.csv"

get_station_file_mapping <- function(csv_dir="data/02_clean") {
  csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = FALSE)

  # Extract station name from filename 
  # (last part after final underscore, before .csv)
  station_names <- sub(".*_([^_]+)\\.csv$", "\\1", csv_files)

  # Create a data frame mapping station names to file paths
  mapping <- data.frame(
      station = station_names,
      filename = csv_files,
      filepath = file.path(csv_dir, csv_files),
      stringsAsFactors = FALSE
  )

  return(mapping)
}
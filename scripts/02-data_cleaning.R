#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers
# Author: Justin [...UPDATE THIS...]
# Date: 21 September 2024
# Contact: justin@example.com [...UPDATE THIS...]
# License: MIT
# Pre-requisites: Dataset with geometry field containing valid JSON-like data



#### Workspace setup ####
library(readr)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(tidyverse)



#### Load data ####
data <- read_csv("data/raw_data/raw_data.csv")



#### Clean data ####

# Function to extract and clean coordinates from geometry
extract_coordinates <- function(geo) {
  if (!is.na(geo)) {
    # Clean and prepare the JSON-like structure
    geo_clean <- gsub("'", "\"", geo)
    
    # Convert to JSON and extract coordinates
    json_data <- tryCatch({
      fromJSON(geo_clean)
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(json_data) && !is.null(json_data$coordinates)) {
      # Flatten coordinates (assuming it's a nested list)
      coords <- unlist(json_data$coordinates, recursive = TRUE)
      
      # Separate longitude (starting with -79) and latitude (starting with 43)
      longitude <- coords[grepl("^-79", coords)]
      latitude <- coords[grepl("^43", coords)]
      
      # Ensure we have pairs
      if (length(longitude) == length(latitude)) {
        coords_matrix <- cbind(longitude, latitude)
        return(as.data.frame(coords_matrix))
      }
    }
  }
  
  return(NULL)  # Return NULL for invalid or empty data
}

# Apply the function to extract all coordinates from the 'geometry' column
all_coordinates <- lapply(data$geometry, extract_coordinates)

# Filter out any NULL values from the list of extracted coordinates
valid_coordinates <- all_coordinates[!sapply(all_coordinates, is.null)]

# Create a unique ID for each geometry feature (so we know which points belong together)
data$id <- seq_len(nrow(data))

# Combine all valid coordinates into a single dataframe with IDs
coordinates_data <- do.call(rbind, Map(function(coords, id) {
  coords$id <- id  # Add ID to each set of coordinates
  return(coords)
}, valid_coordinates, data$id[!sapply(all_coordinates, is.null)]))

# Set column names for the longitude and latitude
colnames(coordinates_data) <- c("longitude", "latitude", "id")

# Ensure no NA values exist in longitude and latitude
coordinates_data <- coordinates_data %>%
  filter(!is.na(longitude), !is.na(latitude))



#### Save data ####
write_csv(
  x = coordinates_data,
  file = "data/analysis_data/coordinates_data.csv"
)



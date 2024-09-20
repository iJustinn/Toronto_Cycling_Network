#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]



#### Workspace setup ####
library(readr)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(tidyverse)



#### Load data ####
data <- read_csv("data/raw_data/raw_data.csv")



#### Clean data ####

# Map data
# Function to replace single quotes with double quotes and parse JSON
extract_coordinates <- function(geo) {
  if (!is.na(geo)) {
    # Replace single quotes with double quotes to create valid JSON
    geo_clean <- gsub("'", "\"", geo)
    
    # Convert the geometry field from JSON-like to a proper R list and extract the 'coordinates'
    json_data <- fromJSON(geo_clean)
    
    # Extract and flatten coordinates (longitude and latitude pairs)
    coords <- unlist(json_data$coordinates, recursive = TRUE)
    
    # Group every two values as a pair (longitude, latitude)
    coords_matrix <- matrix(coords, ncol = 2, byrow = TRUE)
    
    # Return the coordinates matrix as a dataframe
    return(as.data.frame(coords_matrix))
  } else {
    return(NA)
  }
}

# Apply the function to extract all coordinates from the 'geometry' column
all_coordinates <- lapply(data$geometry, extract_coordinates)

# Create a unique ID for each geometry feature (so we know which points belong together)
data$id <- seq_len(nrow(data))

# Combine all coordinates into a single dataframe, including their corresponding IDs
coordinates_data <- do.call(rbind, Map(function(coords, id) {
  coords$id <- id  # Add ID to each set of coordinates
  return(coords)
}, all_coordinates, data$id))

# Set column names for the longitude and latitude
colnames(coordinates_data) <- c("longitude", "latitude", "id")

# Remove rows with NA values
coordinates_data <- coordinates_data %>% filter(!is.na(longitude), !is.na(latitude))






#### Save data ####
write_csv(
  x = coordinates_data,
  file = "data/analysis_data/coordinates_data.csv"
)



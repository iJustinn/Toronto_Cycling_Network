#### Preamble ####
# Purpose: Cleans and processes cycling network data for analysis, including extracting coordinates, summarizing upgrades and installations, and classifying lane types by comfort level.
# Author: Ziheng Zhong
# Date: 23 September 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: NA
# Any other information needed: NA



#### Workspace setup ####
library(readr)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(tidyverse)



#### Clean data 1 ####
# Map data
# Load the dataset
data <- read_csv("data/raw_data/raw_data.csv")

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

# Save data
write_csv(
  x = coordinates_data,
  file = "data/analysis_data/coordinates_data.csv"
)



#### Clean data 2 ####
# Upgrade data
# Load the dataset
data <- read_csv("data/raw_data/raw_data.csv")

# Select and clean the 'UPGRADED' column, then summarize by year
upgraded_data <- data %>%
  select(UPGRADED) %>%                     # Keep only the 'UPGRADED' column
  filter(!is.na(UPGRADED) & UPGRADED > 1800) %>%  # Remove NA and invalid years
  mutate(UPGRADED = as.numeric(UPGRADED)) %>% # Convert 'UPGRADED' to numeric
  group_by(UPGRADED) %>%                    # Group by the upgrade year
  summarise(num_lanes = n())                # Count the number of upgrades per year

# Save the summarized data
write_csv(
  x = upgraded_data,
  file = "data/analysis_data/upgraded_data.csv"
)



#### Clean data 3 ####
# Installed data
# Load the dataset
data <- read_csv("data/raw_data/raw_data.csv")

# Select and clean the 'INSTALLED' column, then summarize by year
installed_data <- data %>%
  select(INSTALLED) %>%                        # Keep only the 'INSTALLED' column
  filter(!is.na(INSTALLED) & INSTALLED > 1800) %>%  # Remove NA and invalid years
  mutate(INSTALLED = as.numeric(INSTALLED)) %>%     # Convert 'INSTALLED' to numeric
  group_by(INSTALLED) %>%                          # Group by the installation year
  summarise(num_bikeways = n())                    # Count the number of bikeways per year

# Save the summarized data
write_csv(
  x = installed_data,
  file = "data/analysis_data/installed_data.csv"
)



#### Clean data 4 ####
# Lane Type data
# Load the dataset
data <- read_csv("data/raw_data/raw_data.csv")

# Clean and classify the 'INFRA-HIGHORDER' and 'INFRA-LOWORDER' columns, and retain year data
lane_type_data <- data %>%
  select(INFRA_HIGHORDER, INFRA_LOWORDER, INSTALLED, UPGRADED) %>%   # Select relevant columns including year data
  filter(!is.na(INFRA_HIGHORDER) & !is.na(INFRA_LOWORDER)) %>%       # Remove rows with NA values
  filter((INSTALLED >= 1800 | is.na(INSTALLED)) & (UPGRADED >= 1800 | is.na(UPGRADED))) %>%  # Remove rows where INSTALLED or UPGRADED equals 1
  mutate(
    Comfort_Level = case_when(                                       # Combine both columns into one classification
      str_detect(INFRA_HIGHORDER, "Protected|Multi-Use") | str_detect(INFRA_LOWORDER, "Protected|Multi-Use") ~ "High Comfort",
      str_detect(INFRA_HIGHORDER, "Bike Lane") | str_detect(INFRA_LOWORDER, "Bike Lane") ~ "Moderate Comfort",
      TRUE ~ "Low Comfort"
    )
  ) %>%
  select(INSTALLED, UPGRADED, Comfort_Level)                         # Keep year data and the final classification column

# Save the cleaned and classified data
write_csv(
  x = lane_type_data,
  file = "data/analysis_data/lane_type_data.csv"
)



#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(sf)
library(readr)
library(ggplot2)
library(osmdata)



#### Chart 1 ####
# Map
# Load the data
coordinates_data <- read_csv("data/analysis_data/coordinates_data.csv")

# Get bounding box for Toronto (approximate limits for Toronto)
toronto_bbox <- c(-79.6303856025883, 43.5822069220627, -79.1180336515019, 43.8554571861712)

# Fetch map data from OpenStreetMap (OSM)
toronto_map <- opq(bbox = toronto_bbox) %>%
  add_osm_feature(key = 'highway', value = 'primary') %>%
  osmdata_sf()

# Convert map data to Simple Features (sf) objects
streets <- toronto_map$osm_lines

# Plot the map with bike lanes data
ggplot() +
  geom_sf(data = streets, color = "gray", size = 0.3, alpha = 0.6) +  # OSM street map layer
  geom_point(data = coordinates_data, aes(x = longitude, y = latitude), 
             size = 0.1, color = "black") +  # Bike lanes points
  labs(title = "Map of Bike Lanes Locations in Toronto", 
       x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-79.6303856025883, -79.1180336515019), 
           ylim = c(43.5822069220627, 43.8554571861712), expand = FALSE) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),  # Remove panel background
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save Chart
ggsave("other/charts/map.jpg")



#### Chart 2 ####
# Upgraded
# Load the data
upgraded_data <- read_csv("data/analysis_data/upgraded_data.csv")

# Generate the bar chart for "Number of Bikeways Upgraded by Year"
ggplot(upgraded_data, aes(x = UPGRADED, y = num_lanes)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Number of Bikeways Upgraded by Year",
       x = "Year of Upgrade",
       y = "Number of Bikeways") +
  scale_x_continuous(breaks = seq(min(upgraded_data$UPGRADED), max(upgraded_data$UPGRADED), by = 1)) +  # Display all years
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotates x-axis labels for readability

# Save Chart
ggsave("other/charts/upgrade.jpg")



#### Chart 3 ####
# Installed
# Load the data
installed_data <- read_csv("data/analysis_data/installed_data.csv")

# Generate the bar chart for "Number of Bikeways Installed by Year"
ggplot(installed_data, aes(x = INSTALLED, y = num_bikeways)) +
  geom_bar(stat = "identity", fill = "grey") +
  labs(title = "Number of Bikeways Installed by Year",
       x = "Year of Installation",
       y = "Number of Bikeways") +
  scale_x_continuous(breaks = seq(min(installed_data$INSTALLED), max(installed_data$INSTALLED), by = 1)) +  # Display all years
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotates x-axis labels for readability

# Save Chart
ggsave("other/charts/installed.jpg")






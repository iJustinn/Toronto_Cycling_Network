#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(readr)
library(ggplot2)



#### Load data ####
coordinates_data <- read_csv("data/analysis_data/coordinates_data.csv")



#### Chart 1 ####
# Map
ggplot(coordinates_data, aes(x = longitude, y = latitude)) +
  geom_point(size = 0.1, color = "black") +  # Plot points instead of paths
  labs(title = "Map of Bike Lanes Locations in Toronto", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  # Set appropriate limits for Toronto area coordinates
  coord_cartesian(xlim = c(-79.63, -79.15), ylim = c(43.58, 43.87)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#### Save Charts ####




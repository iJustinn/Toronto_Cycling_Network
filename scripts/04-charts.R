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



#### Chart 4 ####
# Lane Type
# Load the data
lane_type_data <- read_csv("data/analysis_data/lane_type_data.csv")

# Combine the 'INSTALLED' and 'UPGRADED' years into one column for better visualization
lane_type_years <- lane_type_data %>%
  pivot_longer(cols = c(INSTALLED, UPGRADED), names_to = "Year_Type", values_to = "Year") %>%
  filter(!is.na(Year)) %>%  # Remove NA values in the 'Year' column
  group_by(Comfort_Level) %>%
  summarise(count = n())  # Count the occurrences of each Comfort Level

# Calculate percentages
lane_type_years <- lane_type_years %>%
  mutate(percentage = round((count / sum(count)) * 100, 2))  # Add percentage column

# Define a more contrasting Morandi-inspired color scheme
contrasting_morandi_colors <- c(
  "High Comfort" = "#B39B8E",   # Muted brownish-pink
  "Moderate Comfort" = "#8A7967", # Stronger grey-brown
  "Low Comfort" = "#6E7062"     # Dark muted gray-green
)

# Generate the pie chart with border lines and larger text/legend
ggplot(lane_type_years, aes(x = "", y = percentage, fill = Comfort_Level)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Add black border lines around pie sections
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 6,   # Increase text size for percentage labels
            fontface = "bold") +  # Make percentage text bold
  scale_fill_manual(values = contrasting_morandi_colors) +  # Apply the more contrasting Morandi colors
  labs(title = "Bikeway Comfort Level Distribution", fill = "Comfort Level") +
  theme_void() +  # Remove axis, grid lines, and background
  theme(legend.position = "right", 
        legend.title = element_text(size = 16),  # Increase legend title size
        legend.text = element_text(size = 14))  # Increase legend text size

# Save the chart
ggsave("other/charts/lane_type.jpg")



#### Chart 5 ####
# Upgraded of Low Comfort
# Load the data
lane_type_data <- read_csv("data/analysis_data/lane_type_data.csv")

# Filter data for 'Low Comfort' lanes and categorize as 'Upgraded' or 'Not Upgraded'
low_comfort_data <- lane_type_data %>%
  filter(Comfort_Level == "Low Comfort") %>%
  mutate(
    Upgrade_Status = ifelse(is.na(UPGRADED), "Not Upgraded", "Upgraded")
  ) %>%
  group_by(Upgrade_Status) %>%
  summarise(count = n())  # Count the occurrences of each category

# Calculate percentages
low_comfort_data <- low_comfort_data %>%
  mutate(percentage = round((count / sum(count)) * 100, 2))  # Add percentage column

# Define colors for 'Upgraded' and 'Not Upgraded' categories
upgrade_colors <- c(
  "Upgraded" = "#8A7967",  # Gray-brown for upgraded
  "Not Upgraded" = "#B39B8E"  # Muted pinkish tone for not upgraded
)

# Generate the pie chart with borders and larger text/legend for Low Comfort lanes
ggplot(low_comfort_data, aes(x = "", y = percentage, fill = Upgrade_Status)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Add black border lines around pie sections
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 6,   # Increase text size for percentage labels
            fontface = "bold") +  # Make percentage text bold
  scale_fill_manual(values = upgrade_colors) +  # Apply colors for Upgraded vs Not Upgraded
  labs(title = "Distribution of Low Comfort Lanes: Upgraded vs Not Upgraded", fill = "Upgrade Status") +
  theme_void() +  # Remove axis, grid lines, and background
  theme(legend.position = "right", 
        legend.title = element_text(size = 16),  # Increase legend title size
        legend.text = element_text(size = 14))  # Increase legend text size

# Save the chart
ggsave("other/charts/low_comfort_upgrade_distribution.jpg")



#### Chart 6 ####
# Upgraded of Moderate Comfort
# Load the data
lane_type_data <- read_csv("data/analysis_data/lane_type_data.csv")

# Filter data for 'Moderate Comfort' lanes and categorize as 'Upgraded' or 'Not Upgraded'
moderate_comfort_data <- lane_type_data %>%
  filter(Comfort_Level == "Moderate Comfort") %>%
  mutate(
    Upgrade_Status = ifelse(is.na(UPGRADED), "Not Upgraded", "Upgraded")
  ) %>%
  group_by(Upgrade_Status) %>%
  summarise(count = n())  # Count the occurrences of each category

# Calculate percentages
moderate_comfort_data <- moderate_comfort_data %>%
  mutate(percentage = round((count / sum(count)) * 100, 2))  # Add percentage column

# Define colors for 'Upgraded' and 'Not Upgraded' categories
upgrade_colors_moderate <- c(
  "Upgraded" = "#8A7967",  # Gray-brown for upgraded
  "Not Upgraded" = "#B39B8E"  # Muted brownish-pink for not upgraded
)

# Generate the pie chart for Moderate Comfort lanes
ggplot(moderate_comfort_data, aes(x = "", y = percentage, fill = Upgrade_Status)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Add black border lines around pie sections
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 6,   # Increase text size for percentage labels
            fontface = "bold") +  # Make percentage text bold
  scale_fill_manual(values = upgrade_colors_moderate) +  # Apply colors for Upgraded vs Not Upgraded
  labs(title = "Distribution of Moderate Comfort Lanes: Upgraded vs Not Upgraded", fill = "Upgrade Status") +
  theme_void() +  # Remove axis, grid lines, and background
  theme(legend.position = "right", 
        legend.title = element_text(size = 16),  # Increase legend title size
        legend.text = element_text(size = 14))  # Increase legend text size

# Save the chart
ggsave("other/charts/moderate_comfort_upgrade_distribution.jpg")



#### Chart 7 ####
# Upgraded of High Comfort
# Load the data
lane_type_data <- read_csv("data/analysis_data/lane_type_data.csv")
# Load the cleaned lane type data
lane_type_data <- read_csv("data/analysis_data/lane_type_data.csv")

# Filter data for 'High Comfort' lanes and categorize as 'Upgraded' or 'Not Upgraded'
high_comfort_data <- lane_type_data %>%
  filter(Comfort_Level == "High Comfort") %>%
  mutate(
    Upgrade_Status = ifelse(is.na(UPGRADED), "Not Upgraded", "Upgraded")
  ) %>%
  group_by(Upgrade_Status) %>%
  summarise(count = n())  # Count the occurrences of each category

# Calculate percentages
high_comfort_data <- high_comfort_data %>%
  mutate(percentage = round((count / sum(count)) * 100, 2))  # Add percentage column

# Define colors for 'Upgraded' and 'Not Upgraded' categories
upgrade_colors_high <- c(
  "Upgraded" = "#6E7062",  # Dark muted gray-green for upgraded
  "Not Upgraded" = "#B39B8E"  # Muted brownish-pink for not upgraded
)

# Generate the pie chart for High Comfort lanes
ggplot(high_comfort_data, aes(x = "", y = percentage, fill = Upgrade_Status)) +
  geom_bar(stat = "identity", width = 1, color = "black") +  # Add black border lines around pie sections
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 6,   # Increase text size for percentage labels
            fontface = "bold") +  # Make percentage text bold
  scale_fill_manual(values = upgrade_colors_high) +  # Apply colors for Upgraded vs Not Upgraded
  labs(title = "Distribution of High Comfort Lanes: Upgraded vs Not Upgraded", fill = "Upgrade Status") +
  theme_void() +  # Remove axis, grid lines, and background
  theme(legend.position = "right", 
        legend.title = element_text(size = 16),  # Increase legend title size
        legend.text = element_text(size = 14))  # Increase legend text size

# Save the chart
ggsave("other/charts/high_comfort_upgrade_distribution.jpg")



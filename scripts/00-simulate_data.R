#### Preamble ####
# Purpose: Simulates a dataset representing Toronto's cycling network with route attributes such as length, surface type, and average daily riders.
# Author: Ziheng Zhong
# Date: 23 September 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT



#### Workspace setup ####
library(tidyverse)
library(dplyr)



#### Simulate data ####
# Set seed for reproducibility
set.seed(304)

# Create a simulated dataset for Toronto Cycling Network
toronto_cycling_network <- data.frame(
  route_id = 1:100, # Unique identifier for each cycling route
  route_name = paste("Route", 1:100), # Route name
  length_km = round(rgamma(100, shape = 2, scale = 2), 2), # Gamma distribution for route length (right-skewed)
  surface_type = sample(c("Asphalt", "Concrete", "Gravel"), 100, replace = TRUE, prob = c(0.6, 0.3, 0.1)), # Asphalt is more common
  ward = sample(1:25, 100, replace = TRUE, prob = dpois(1:25, lambda = 12) + 0.01), # Poisson distribution for ward, centered around 12
  year_installed = sample(1990:2023, 100, replace = TRUE, prob = dnorm(1990:2023, mean = 2010, sd = 8)), # More routes in recent years
  maintenance_status = sample(c("Good", "Fair", "Poor"), 100, replace = TRUE, prob = c(0.5, 0.3, 0.2)), # 50% chance of "Good" maintenance
  avg_daily_riders = round(rlnorm(100, meanlog = log(300), sdlog = 0.4)) # Log-normal distribution for daily riders
)

# Check the dataset
head(toronto_cycling_network)

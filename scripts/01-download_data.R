#### Preamble ####
# Purpose: Downloads and saves the data from [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]



#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(dplyr)



#### Download raw data ####
# get package
package <- show_package("cycling-network")

# get all resources for this package
resources <- list_package_resources("cycling-network")

# choose .csv to download
datastore_resources <- filter(resources, tolower(format) %in% c('csv'))
raw_data <- filter(datastore_resources, row_number()==1) %>% get_resource()



#### save data ####
write_csv(
  x = raw_data,
  file = "data/raw_data/raw_data.csv"
)



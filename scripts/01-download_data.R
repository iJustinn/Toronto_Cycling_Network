#### Preamble ####
# Purpose: Downloads and saves the data from the Toronto Open Data cycling network package.
# Author: Ziheng Zhong
# Date: 23 September 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT



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
datastore_resources <- filter(resources, tolower(format) %in% c("csv"))
raw_data <- filter(datastore_resources, row_number() == 1) %>% get_resource()



#### save data ####
write_csv(
  x = raw_data,
  file = "data/raw_data/raw_data.csv"
)

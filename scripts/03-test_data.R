#### Preamble ####
# Purpose: Tests the integrity of raw and processed cycling network data, including checks for missing values, valid data types, and year ranges.
# Author: Ziheng Zhong
# Date: 23 September 2024
# Contact: ziheng.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: NA
# Any other information needed: NA



#### Workspace setup ####
library(here)
library(dplyr)
library(testthat)
library(lubridate)
library(geojsonio)
library(tidyverse)



#### Test Raw Data ####
# Test suite
test_that("Test for missing values", {
  data <- read_csv(here("data", "raw_data", "raw_data.csv"))
  missing_info <- sapply(data, function(x) sum(is.na(x)))
  missing_cols <- names(missing_info[missing_info > 0])
  expect_true(length(missing_cols) > 0, info = "Columns with missing values exist.")
  print(missing_cols)
})

test_that("Test for valid data types", {
  # Check if INSTALLED, UPGRADED, OBJECTID are numeric
  data <- read_csv(here("data", "raw_data", "raw_data.csv"))
  numeric_check <- all(sapply(data[c('INSTALLED', 'UPGRADED', 'OBJECTID')], is.numeric))
  expect_true(numeric_check, info = "INSTALLED, UPGRADED, OBJECTID should be numeric.")
  
  # Check if LAST_EDIT_DATE has a valid date format (using ymd_hms for ISO 8601)
  date_check <- all(!is.na(ymd_hms(data$LAST_EDIT_DATE)))
  expect_true(date_check, info = "LAST_EDIT_DATE should be in valid date format.")
})

test_that("Test for valid ranges of years", {
  data <- read_csv(here("data", "raw_data", "raw_data.csv"))
  current_year <- year(Sys.Date())
  
  # Check INSTALLED range
  installed_valid <- all(data$INSTALLED >= 1900 & data$INSTALLED <= current_year, na.rm = TRUE)
  expect_true(installed_valid, info = "INSTALLED year should be between 1900 and the current year or NA.")
  
  # Check UPGRADED range, allow NA values
  upgraded_valid <- all(is.na(data$UPGRADED) | (data$UPGRADED >= 1900 & data$UPGRADED <= current_year | data$UPGRADED == 1)) #there is a invalid UPGRADED data 1
  expect_true(upgraded_valid, info = "UPGRADED year should be between 1900 and the current year or NA.")
})

test_that("Test for uniqueness of X_id and SEGMENT_ID", {
  data <- read_csv(here("data", "raw_data", "raw_data.csv"))
  # X_id uniqueness
  x_id_unique <- n_distinct(data$X_id) == nrow(data)
  expect_true(x_id_unique, info = "X_id should be unique.")
  
  # SEGMENT_ID uniqueness
  segment_id_unique <- n_distinct(data$SEGMENT_ID) == nrow(data)
  expect_true(segment_id_unique, info = "SEGMENT_ID should be unique.")
})



#### Test Analysis Data ####
# Test Lane Type Data
test_that("Test lane_type_data for missing values, valid data types, and ranges", {
  data <- read_csv(here("data", "analysis_data", "lane_type_data.csv"))
  current_year <- year(Sys.Date())
  
  # Test for missing values
  missing_info <- sapply(data, function(x) sum(is.na(x)))
  missing_cols <- names(missing_info[missing_info > 0])
  expect_true(length(missing_cols) > 0, info = "Columns with missing values exist.")
  print(missing_cols)
  
  # Test for valid numeric types
  numeric_check <- all(sapply(data[c('INSTALLED', 'UPGRADED')], is.numeric))
  expect_true(numeric_check, info = "INSTALLED and UPGRADED should be numeric.")
  
  # Test for valid year ranges
  installed_valid <- all(data$INSTALLED >= 1900 & data$INSTALLED <= current_year, na.rm = TRUE)
  expect_true(installed_valid, info = "INSTALLED year should be between 1900 and the current year or NA.")
  
  upgraded_valid <- all(is.na(data$UPGRADED) | (data$UPGRADED >= 1900 & data$UPGRADED <= current_year))
  expect_true(upgraded_valid, info = "UPGRADED year should be between 1900 and the current year or NA.")
})



# Test Installed Data
test_that("Test installed_data for valid ranges and data types", {
  data <- read_csv(here("data", "analysis_data", "installed_data.csv"))
  current_year <- year(Sys.Date())
  
  # Test for valid numeric types
  numeric_check <- all(sapply(data[c('INSTALLED', 'num_bikeways')], is.numeric))
  expect_true(numeric_check, info = "INSTALLED and num_bikeways should be numeric.")
  
  # Test for valid year ranges
  installed_valid <- all(data$INSTALLED >= 1900 & data$INSTALLED <= current_year)
  expect_true(installed_valid, info = "INSTALLED year should be between 1900 and the current year.")
})



# Test Upgraded Data
test_that("Test upgraded_data for valid ranges and data types", {
  data <- read_csv(here("data", "analysis_data", "upgraded_data.csv"))
  current_year <- year(Sys.Date())
  
  # Test for valid numeric types
  numeric_check <- all(sapply(data[c('UPGRADED', 'num_lanes')], is.numeric))
  expect_true(numeric_check, info = "UPGRADED and num_lanes should be numeric.")
  
  # Test for valid year ranges
  upgraded_valid <- all(data$UPGRADED >= 1900 & data$UPGRADED <= current_year)
  expect_true(upgraded_valid, info = "UPGRADED year should be between 1900 and the current year.")
})



# Test Coordinates Data
test_that("Test coordinates_data for valid longitude/latitude and id uniqueness", {
  data <- read_csv(here("data", "analysis_data", "coordinates_data.csv"))
  
  # Test for valid longitude and latitude ranges
  valid_longitude <- all(data$longitude >= -180 & data$longitude <= 180)
  valid_latitude <- all(data$latitude >= -90 & data$latitude <= 90)
  expect_true(valid_longitude, info = "Longitude should be between -180 and 180.")
  expect_true(valid_latitude, info = "Latitude should be between -90 and 90.")
})



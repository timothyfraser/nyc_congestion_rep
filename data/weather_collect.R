#' @name weather_collect.R
#' @title Collect weather data for all sites!
#' @author Tim Fraser

# View weather.gov API FAQ here:
# https://weather-gov.github.io/api/general-faqs
# https://weather-gov.github.io/api/gridpoints
# View OpenAPI v3 format for API at: 
# https://api.weather.gov/openapi.json

# Set working directory to test_aqi
# Try to get project directory from RStudio, fallback to current directory if not in RStudio
project_dir <- tryCatch({
  rstudioapi::getActiveProject()
}, error = function(e) {
  # If rstudioapi fails, use current working directory
  getwd()
})

# Only change directory if we got a valid path
if (!is.null(project_dir) && project_dir != "" && dir.exists(project_dir)) {
  setwd(project_dir)
}


# Load packages
library(dplyr) # for data wrangling
library(readr) # for reading data
library(sf) # for spatial data wrangling
library(lubridate) # for date management
library(httr2) # for API queries
library(tidyr) # for expand_grid()
library(purrr) # for possibly()

readRenviron("secret/.env")
source("data/functions.R")

# datetimes = read_csv("data/datetimes.csv") %>%
#   group_by(current) %>%
#   summarize(start = min(date, na.rm = TRUE),
#             end = max(date, na.rm = TRUE)) %>%
#   group_by(current) %>%
#   reframe(date = seq_date(start = start, end = end, ndays = 365)) %>%
#   group_by(current) %>%
#   mutate(series = 1:n(), start = date, end = lead(date-1, n = 1)) %>%
#   mutate(end = if_else(series == n() - 1, true = end + 1, false = end)) %>%
#   filter(!is.na(end)) %>%
#   ungroup()

# Return sites and coordinates, rounded to 4 decimal points - max detail 
# grid = read_rds("data/sites.rds") %>%
#   { data = .; tibble(aqs_id_full = data$aqs_id_full, data %>% st_coordinates() %>% round(4) %>% as_tibble() %>% select(lat = Y, lon = X) ) } %>%
#   expand_grid(
#     datetimes
#   ) %>%
#   arrange(desc(current), aqs_id_full) %>%
#   # filter(aqs_id_full != 840421010055) %>%
#   # filter(!id %in% c(1:6))
#   # Which sites do we already have data for?
#   anti_join(
#     by = "aqs_id_full",
#     y = read_csv("data/weather.csv", show_col_types = FALSE) %>%
#       select(aqs_id_full) %>%
#       distinct()
#   )

# read_csv("../data/weather.csv", show_col_types = FALSE) %>%
#   filter(aqs_id_full == 840999999991, stringr::str_detect(datetime, "2024-01-01")) %>%
#   View()

# Find missing sensor-time frames
# Make the full grid and join the weather observations, then filter to missing
missing = read_csv("data/datetimes.csv", show_col_types = FALSE) %>%
  filter(current == TRUE) %>%
  select(date, current) %>%
  distinct() %>%
  expand_grid(
    aqs_id_full = read_rds("data/sites.rds")$aqs_id_full
  )  %>%
  # Join in the daily mean weather observations so far...
  left_join(
    by = c("aqs_id_full", "date"),
    y = read_csv("data/weather.csv", show_col_types = FALSE) %>%
      mutate(date = lubridate::date(datetime)) %>%
      group_by(aqs_id_full, date) %>%
      summarize(temp = mean(temp, na.rm = TRUE), .groups = "drop")
  ) %>%
  # Filter to just MISSING observations
  filter(is.na(temp)) %>%
  # Get distinct sensor-dateranges
  select(aqs_id_full, date) %>%
  distinct() %>%
  # Get distinct sensor-dateranges
  arrange(aqs_id_full, date) %>%
  group_by(aqs_id_full) %>%
  mutate(
    date_diff = as.integer(date - lag(date, default = first(date) - 1)),
    group = cumsum(date_diff != 1)
  ) %>%
  group_by(aqs_id_full, group) %>%
  summarise(
    start = min(date),
    end = max(date),
    .groups = "drop"
  ) %>%
  # Now split up those dateranges every N days
  group_by(aqs_id_full, group) %>%
  reframe(
    date = seq_date(start = start, end = end, ndays = 100)
  ) %>%
  group_by(aqs_id_full, group) %>%
  mutate(series = 1:n(), start = date, end = lead(date-1, n = 1)) %>%
  mutate(end = if_else(series == n() - 1, true = end + 1, false = end)) %>%
  filter(!is.na(end)) %>%
  ungroup() %>%
  mutate(current = TRUE)

# Get the grid of sites and dates to collect weather for
grid = read_rds("data/sites.rds") %>%
  {data = .; tibble(aqs_id_full = data$aqs_id_full, data %>% st_coordinates() %>% round(4) %>% as_tibble() %>% select(lat = Y, lon = X)) } %>%
  right_join(by = "aqs_id_full", y = missing) %>%
  mutate(id = 1:n())

#   bind_rows(missing) %>%
#   mutate(id = 1:n())
    



# read_csv("data/weather.csv", show_col_types = FALSE) %>%
#   tail()
# A bunch of places are missing weather data.


# status check:
# read_csv("data/weather_log.csv", show_col_types = FALSE)
# read_csv("data/weather.csv", show_col_types = FALSE) %>% tail()
# grid %>% filter(aqs_id_full == 840421010055)

# Bind in other previously completed runs...
# prior = bind_rows(
#   tibble(aqs_id_full = 840421010055, lat = 39.9225, lon = -75.1868, start = ymd("2025-05-28"), end = ymd("2025-05-28"), id = 0),
#   tibble(aqs_id_full = 840421010055, lat = 39.9225, lon = -75.1868, start = ymd("2025-05-26"), end = ymd("2025-05-27"), id = 0),
#   tibble(aqs_id_full = 840421010055, lat = 39.9225, lon = -75.1868, start = ymd("2025-05-01"), end = ymd("2025-05-25"), id = 0),
#   tibble(aqs_id_full = 840421010055, lat = 39.9225, lon = -75.1868, start = ymd("2025-01-01"), end = ymd("2025-04-30"), id = 0),
#   tibble(aqs_id_full = 840421010055, lat = 39.9225, lon = -75.1868, start = ymd("2024-01-01"), end = ymd("2024-12-31"), id = 0)
# )
# ymd("2025-01-01")- 1
# minigrid = 

grid %>%
  get_weather_many(grid = ., path = "data/weather.csv", overwrite = FALSE, maxdist = 32188)

# Diagnostic check!
# read_csv("data/weather.csv", show_col_types = FALSE) %>%
#   filter(aqs_id_full %in% minigrid$aqs_id_full) %>%
#   filter(datetime >= minigrid$start & datetime < minigrid$end + 1)

# Remove any completely null rows from weather.csv


# Count a row as missing if it is missing these 5 key variables
read_csv("data/weather.csv", show_col_types = FALSE) %>%
  # cut these invalid rows
  filter( ! (is.na(temp) & is.na(dew) & is.na(humidity) & is.na(precip) & is.na(cloudcover)) ) %>%
  # Save to file
  write_csv("data/weather.csv")

rm(list = ls())


# read_csv("data/weather.csv", show_col_types = FALSE) %>% head()

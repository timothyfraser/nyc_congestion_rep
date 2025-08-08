# weather_collect_anywhere.R

# A script to collect weather data from Visual Crossing API for multiple cities

# Set working directory
library(here, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(readr, quietly = TRUE, warn.conflicts = FALSE)
library(purrr, quietly = TRUE, warn.conflicts = FALSE)
library(httr2, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)
library(sf, quietly = TRUE, warn.conflicts = FALSE)
library(tigris, quietly = TRUE, warn.conflicts = FALSE)
library(lubridate, quietly = TRUE, warn.conflicts = FALSE)
library(stringr, quietly = TRUE, warn.conflicts = FALSE)

# Set working directory
setwd(here())

# Load environmental variables
readRenviron("secret/.env")
# Test read environmental variables
# Sys.getenv("VISUALCROSSINGKEY")

# Enable data caching!
options(tigris_use_cache = TRUE)
source("data/functions.R")

# Define cities to collect weather data for
cities = tribble(
  ~city, ~abbrev,
  "San Francisco", "sf",
  "Boston", "boston", 
  "Hartford", "hartford",
  "Philadelphia", "philadelphia",
  "Baltimore", "baltimore",
  "Washington D.C.", "dc") %>%
  mutate(path = paste0("data/weather_", abbrev, ".csv"))

# Date range for weather collection

# Loop through each city
for(i in 1:nrow(cities)){
    ndays = 60

 start = "2024-01-01"; end = "2025-06-01"

  #  i = 1
  cat("\n---city: ", cities$city[i], " starting...\n")
  
  if(!file.exists(paste0("data/aqi_", cities$abbrev[i], ".csv"))){
    cat("\n---city: ", cities$city[i], " path does not exist! Skipping...\n")
    next
  }

  # Get geographic boundaries for the city
  #shapes = get_bbox(city = cities$city[i], year = 2022) 
  
  # Get area of shape in km^2
#   size = shapes %>% st_area() %>% {as.numeric(.) / 1e6}
#   if(size > 20000){
#     cat("\n---city: ", cities$city[i], " is too large! Skipping...\n")
#     next
#   }
  
  # Get bounding box coordinates
  # bbox = shapes %>% st_bbox()
  
  # Get path to save data
  path = cities$path[i]
  
  # If the file exists, get the last date and set the start date to the next day
  if(file.exists(path)){
    last_date = read_csv(path, show_col_types = FALSE) %>% 
      select(datetime) %>% 
      distinct() %>% 
      with(datetime) %>% 
      max() %>% 
      lubridate::floor_date(unit = "day") 
    start = last_date + days(1)
    start = stringr::str_sub(start, 1, 10)

      if(start > end){
    cat("\n---city: ", cities$city[i], " no data to collect!\n")
    next
    }
  }
  


  # Create a grid of points within the bounding box for weather collection
  # Generate a regular grid of points within the bounding box
  # No, I want to source the grid points per aqs_id_full from the corresponding aqi_[city].csv file.
  grid_points = read_csv(paste0("data/aqi_", cities$abbrev[i], ".csv")) %>% select(aqs_id_full, lat, lon) %>% distinct() %>%
    # Create date ranges for each point
    expand_grid(
      date = seq_date(start = start, end = end, ndays = ndays)
    ) %>%
    group_by(aqs_id_full) %>%
    mutate(
      series = 1:n(),
      start = date,
      end = lead(date - 1, n = 1)
    ) %>%
    mutate(end = if_else(series == n() - 1, true = end + 1, false = end)) %>%
    filter(!is.na(end)) %>%
    ungroup() %>%
    mutate(id = 1:n())

  # Get weather data for the city
  get_weather_many(
    grid = grid_points,
    path = path,  
    overwrite = FALSE, 
    maxdist = 32188  # 20 miles in meters
  )

  # Upon completion, remove duplicates and clean data
  if(file.exists(path)){
    read_csv(path, show_col_types = FALSE) %>%
      # Remove completely null rows
      filter(!(is.na(temp) & is.na(dew) & is.na(humidity) & is.na(precip) & is.na(cloudcover))) %>%
      # Remove duplicates
      distinct() %>%
      write_csv(path)
  }

  cat("\n---city: ", cities$city[i], " finished!\n")
}

cat("\n--- All cities weather collection complete!\n") 
# aqi_collect.R

# A script to collect AQI and PM2.5 data from AIRNOW API

# set working directory
library(here, quietly = TRUE, warn.conflicts = FALSE  )
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
# Sys.getenv("AIRNOWKEY")

# Enable data caching!
options(tigris_use_cache = TRUE)
source("data/functions.R")


cities =  tribble(
  ~city, ~abbrev,
  "San Francisco", "sf",
  "Boston", "boston",
  "Hartford", "hartford",
  "Philadelphia", "philadelphia",
  "Baltimore", "baltimore",
  "Washington D.C.", "dc") %>%
  mutate(path = paste0("data/aqi_", abbrev, ".csv"))


# Run this!
# get_aqdata(
#   bdate = c("2025-05-06-T00") %>% as_datetime_normal(),
#   edate = c("2025-06-05T00") %>% as_datetime_normal(),
#   bbox = bbox, parameters = "PM25", datatype = "B", verbose = 1, path = path)


for(i in 1:nrow(cities)){
  ndays = 20
  start = "2024-01-01"; end = "2025-06-01"
  #  i = 1  
  cat("\n---city: ", cities$city[i], " starting...\n")
  shapes = get_bbox(city = cities$city[i], year = 2022) 
  # Get area of shape in km^2
  size = shapes %>% st_area() %>% {as.numeric(.) / 1e6}
  if(size > 20000){
    cat("\n---city: ", cities$city[i], " is too large! Skipping...\n")
    next
  }
  # Get bounding box
  bbox = shapes %>% st_bbox()
  # Get path to save data
  path = cities$path[i]
  # If the file exists, get the last date and set the start date to the next day
  if(file.exists(path)){
    last_date = read_csv(path) %>% select(datetime) %>% distinct() %>% with(datetime) %>% max() %>% lubridate::floor_date(unit = "day") 
    start = last_date + days(1)
    start = stringr::str_sub(start, 1,10)
  }
  if(start > end){
    cat("\n---city: ", cities$city[i], " no data to collect!\n")
    next
  }

# Example for testing
#   get_aqdata(
#     bdate = "2024-01-01-T00" %>% as_datetime_normal(),
#     edate = "2024-01-02-T00" %>% as_datetime_normal(),
#     bbox = bbox,
#     parameters = "PM25", 
#     datatype = "C", verbose = 1, path = path)
# read_csv(path)
# unlink(path)
  

  # Get AQI data for city X. Get raw concentrations
  get_aqdata_many(
    start = start, end = end, ndays = ndays, 
    parameters = "PM25", 
    path = path, bbox = bbox, verbose = 1, 
    overwrite = FALSE,
    datatype = "C")

  # Upon completion, remove duplicates.
  read_csv(path, show_col_types = FALSE) %>%
    distinct() %>%
    write_csv(path) 

  cat("\n---city: ", cities$city[i], " finished!\n")
}

# read_csv("data/aqi.csv") %>%
#   distinct() %>%
#   write_csv("data/aqi.csv")




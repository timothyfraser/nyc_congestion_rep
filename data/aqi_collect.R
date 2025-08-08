# aqi_collect.R

# A script to collect AQI and PM2.5 data from AIRNOW API

# set working directory
setwd(rstudioapi::getActiveProject())

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(readr, quietly = TRUE, warn.conflicts = FALSE)
library(purrr, quietly = TRUE, warn.conflicts = FALSE)
library(httr2, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)

# Load environmental variables
readRenviron("secret/.env")
# Test read environmental variables
# Sys.getenv("AIRNOWKEY")

source("data/functions.R")

metro = read_rds("data/metro.rds")

path = "data/aqi.csv"

bbox = read_rds("data/bbox_nums.rds")

# Run this!
# get_aqdata(
#   bdate = c("2025-05-06-T00") %>% as_datetime_normal(),
#   edate = c("2025-06-05T00") %>% as_datetime_normal(),
#   bbox = bbox, parameters = "PM25", datatype = "B", verbose = 1, path = path)

get_aqdata_many(
  start = "2018-01-01", end = "2019-06-01", ndays = 20, 
  parameters = "PM25", 
  path = path, bbox = bbox, verbose = 1,
  overwrite = FALSE)

# read_csv("data/aqi.csv") %>%
#   distinct() %>%
#   write_csv("data/aqi.csv")




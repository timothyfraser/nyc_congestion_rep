# census_collect.R

# Script to collect census data for sites surrounding air quality sensors.

setwd(paste0(rstudioapi::getActiveProject()))

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



datetimes = read_csv("data/datetimes.csv", show_col_types = FALSE) %>%
  group_by(current) %>%
  reframe(date = c(min(date, na.rm = TRUE), max(date, na.rm = TRUE))) %>%
  mutate(year = lubridate::year(date)) %>%
  left_join(
    by = "year", 
    y = tibble(
      year = c(2009:2025),
      year_census = if_else(year > 2023, true = 2023, false = year)
    )
  )


# Return sites and coordinates, rounded to 4 decimal points - max detail 
grid = read_rds("data/sites.rds") %>% select(aqs_id_full, geometry) %>%
  st_join(read_rds("data/counties.rds") %>% select(geoid, geometry), left = FALSE) %>%
  as_tibble() %>%
  select(geoid) %>%
  # Bind in the metro areas just to be sure.
  bind_rows(read_rds("data/metro.rds") %>%
              select(geoid)) %>%
  distinct() %>%
  # iterate by year!
  expand_grid(
    year = datetimes$year_census %>% unique()
  ) %>% 
  arrange(desc(year)) %>%
  mutate(id = 1:n()) 


# data = retrieve_census(geoid = "36109", year = 2023, region = "block group:*", key = Sys.getenv("CENSUS_API_KEY"))
data = get_census_many(grid = grid, path = "data/census.csv", overwrite = FALSE)

rm(list = ls())

# read_csv("data/census.csv", show_col_types = FALSE) %>% tail()
# data = read_rds("data/NYC_Hourly_Monitoring_2019_2025.rds")
# read_csv("data/NYC_PM25_Monitor_Location.csv")


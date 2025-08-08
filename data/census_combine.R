# census_combine.R

# Get the average traits of census variables within a 1 km buffer of AQI sensor.

library(dplyr)
library(readr)
library(sf)

setwd(paste0(rstudioapi::getActiveProject()))

# st_crs(sites)
# st_crs(bg)
sites = read_rds("data/sites.rds")
bg = read_sf("data/bg.geojson")  %>% st_transform(crs = 4326) %>%
  mutate(area_land = area_land / (1e3*1e3) ) 
counties = read_rds("data/counties.rds")



# 5 km radius
km = 5

sites %>%
  # Make 5 km buffers around each sensor
  mutate(geometry = st_buffer(geometry, dist = 1e3*km )) %>%
  # Grab the block group ids of any polygon that overlaps with the buffers
  st_join(y = bg %>% select(geoid, area_land, geometry)) %>%
  as_tibble() %>%
  select(aqs_id_full, geoid, area_land) %>%
  distinct() %>%
  write_csv("data/sites_bg_crosswalk_5km.csv")

km = 3
sites %>%
  # Make 5 km buffers around each sensor
  mutate(geometry = st_buffer(geometry, dist = 1e3*km )) %>%
  # Grab the block group ids of any polygon that overlaps with the buffers
  st_join(y = bg %>% select(geoid, area_land, geometry)) %>%
  as_tibble() %>%
  select(aqs_id_full, geoid, area_land) %>%
  distinct() %>%
  write_csv("data/sites_bg_crosswalk_3km.csv")

km = 1
sites %>%
  # Make 1 km buffers around each sensor
  mutate(geometry = st_buffer(geometry, dist = 1e3*km )) %>%
  # Grab the block group ids of any polygon that overlaps with the buffers
  st_join(y = bg %>% select(geoid, area_land, geometry)) %>%
  as_tibble() %>%
  select(aqs_id_full, geoid, area_land) %>%
  distinct() %>%
  write_csv("data/sites_bg_crosswalk_1km.csv")

# cleanup!
rm(list = ls())

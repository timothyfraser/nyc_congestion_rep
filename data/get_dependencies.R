# get_dependencies.R

# A script to download necessary dependencies...

# packages ############################
library(dplyr)
library(readr)
library(purrr)

setwd(paste0(rstudioapi::getActiveProject()))
source("data/functions.R")

# metro geoids #########################


read_csv("data/metro.csv") %>%
  mutate(geoid = stringr::str_pad(geoid, width = 5, side = "left", pad = "0")) %>%
  saveRDS("data/metro.rds")



# county polygons #########################
bind_rows(
  tigris::counties(state = "NY", cb = TRUE, year = 2023)  %>%
    select(state = STUSPS, geoid = GEOID, name = NAME, area_land = ALAND, geometry) %>%
    st_as_sf() %>%
    st_transform(crs = 4326),
  tigris::counties(state = "NJ", cb = TRUE, year = 2023)  %>%
    select(state = STUSPS, geoid = GEOID, name = NAME, area_land = ALAND, geometry) %>%
    st_as_sf() %>%
    st_transform(crs = 4326)
) %>%
  filter(geoid %in% read_rds("data/metro.rds")$geoid) %>%
  saveRDS("data/counties.rds")

# block group polygons ########################
# unlink("data/bg.geojson", force = TRUE)
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(sf)
# Download
read_rds("data/metro.rds") %>%
  split(.$geoid) %>%
  purrr::walk(~tigris::block_groups(
    state = .x$state, county = stringr::str_sub(.x$geoid, 3,5), year = 2022, cb = TRUE 
  ) %>%
    st_as_sf(crs = 4326) %>%
    mutate(county = stringr::str_sub(GEOID, 1,5)) %>%
    select(county, geoid = GEOID, area_land = ALAND, geometry) %>%
    write_sf("data/bg.geojson", delete_layer = FALSE, append = TRUE)
  )

               

# bounding box #########################
# Get county bounding box coordinates
read_rds("data/counties.rds") %>% st_bbox() %>% saveRDS("data/bbox_nums.rds")
# Make a literal box polygon out of it and save it
read_rds("data/bbox_nums.rds") %>% st_as_sfc() %>% tibble(geometry = .) %>% st_as_sf(crs = 4326) %>% saveRDS("data/bbox.rds")


# zone ################################
# https://data.ny.gov/Transportation/MTA-Central-Business-District-Geofence-Beginning-J/srxy-5nxn/about_data
data = httr::GET("https://data.ny.gov/resource/srxy-5nxn.geojson")
rawToChar(data$content) %>% write_lines("data/zone.geojson")
# Turn the many polygons into one big multi-polygon
read_sf("data/zone.geojson") %>%
  summarize(geometry = st_union(geometry)) %>%
  saveRDS("data/zone.rds")

# sites #################################
zone = read_rds("data/zone.rds")

sites = bind_rows(
  read_csv("data/sites_crosswalk.csv", show_col_types = FALSE) %>%
    select(aqs_id, aqs_id_full, site_name, lat, lon),
  read_csv("data/aqi.csv", show_col_types = FALSE) %>%
    select(lat, lon, aqs_id, aqs_id_full) %>%
    mutate(aqs_id = as.numeric(aqs_id)) %>%
    distinct()
) %>%
  st_as_sf(crs = 4326, coords = c("lon", "lat"))

# Calculate distance to nearest point of congestion relief zone.
mydist = sites %>% 
  mutate(
    geometry = sf::st_nearest_points(
      x = geometry, 
      y = zone
    )
  ) %>%
  mutate(dist = as.numeric(st_length(geometry) / 1000)) %>%
  as_tibble() %>%
  select(aqs_id_full, dist)

sites = sites %>%
  # Join in the idstances
  left_join(by = "aqs_id_full", y = mydist) %>% 
  # Join in the unique NYC Id for the local sample sensors
  left_join(
    by = "aqs_id_full", 
    y = read_csv("data/sites_crosswalk.csv", show_col_types = FALSE) %>%
              select(aqs_id_full, nyc_id))

# Add in counties and zone
sites = sites  %>%
  st_join(
    #left = FALSE, 
    y = read_rds("data/counties.rds") %>%
      # filter(name %in% c("Bronx", "Queens", "Kings", "New York", "Richmond")) %>%
      select(name)
  ) %>%
  mutate(name = if_else(is.na(name), true = "Beyond", false = name)) 
  
# within zone or not?
# sites = sites %>%
#   st_join(
#     y = read_rds("data/zone.rds") %>% select(geometry) %>% mutate(within = 1), 
#     join = st_intersects, 
#   ) %>%
# mutate(within = if_else(is.na(within), true = 0, false = within))

# these sites should be considered within the zone
sites = sites %>%
  mutate(within = if_else(
    condition = aqs_id_full %in% c(840999999991 ,840999999995, 840999999996, 840999999913, 840999999914, 840999999916),
    true = 1, false = 0))

sites %>% saveRDS("data/sites.rds")


rm(list = ls())


# highways #########################################

r1 = tigris::primary_secondary_roads(state = "NY", year = 2022)
r2 = tigris::primary_secondary_roads(state = "NJ", year = 2022)
bind_rows(r1, r2) %>%
  st_transform(crs = 4326) %>% 
  setNames(nm = tolower(names(.))) %>%
  st_intersection(
    y = read_rds("data/bbox.rds")
  ) %>%
  saveRDS("data/highways.rds")
remove(r1,r2)

read_rds("data/highways.rds")

# roads ############################################
metro = read_rds("data/metro.rds")
n = nrow(metro)

for(i in 1:n){
  tigris::roads(state = metro$state[i], county = stringr::str_sub(metro$geoid[i], 3,5),  year = 2022) %>%
    st_transform(crs = 4326) %>%
    mutate(geoid = metro$geoid[i]) %>%
    setNames(nm = tolower(names(.))) %>%
    write_sf("data/roads.geojson", delete_layer = FALSE, append = TRUE)
  cat("\n", i, " / ", n, "\n")
}

# Resave it as a compressed RDS file for common use
read_sf("data/roads.geojson") %>%
  saveRDS("data/roads.rds")

# Now zip it
zip(zipfile = "data/roads.zip", files = "data/roads.geojson")

# Remove the geojson file, which is too big for common storage on github.
unlink("data/roads.geojson")


# vehicles #####################################

# This script aims to grab from data.gov
# the number of vehicle entries into the congestion relief zone
# every 10 minutes.
# https://catalog.data.gov/dataset/mta-congestion-relief-zone-vehicle-entries-beginning-2025
# https://data.ny.gov/Transportation/MTA-Congestion-Relief-Zone-Vehicle-Entries-Beginni/t6yz-b64h/explore/query/SELECT%0A%20%20%60toll_date%60%2C%0A%20%20%60toll_hour%60%2C%0A%20%20%60toll_10_minute_block%60%2C%0A%20%20%60minute_of_hour%60%2C%0A%20%20%60hour_of_day%60%2C%0A%20%20%60day_of_week_int%60%2C%0A%20%20%60day_of_week%60%2C%0A%20%20%60toll_week%60%2C%0A%20%20%60time_period%60%2C%0A%20%20%60vehicle_class%60%2C%0A%20%20%60detection_group%60%2C%0A%20%20%60detection_region%60%2C%0A%20%20%60crz_entries%60%2C%0A%20%20%60excluded_roadway_entries%60%0AORDER%20BY%20%60toll_10_minute_block%60%20DESC%20NULL%20FIRST/page/filter

# Downloaded 6/4/2025
read_csv("data/zone_vehicle_entries.csv", show_col_types = FALSE) %>%
  setNames(nm = c("toll_date", "toll_hour", "toll_10min_block", "minute", "hour", "day_int", "day", "toll_week", "time_period", "vehicle_class", "detection_group", "detection_region", "crz_entries", "excluded_roadway_entries")) %>%
  mutate(toll_date = lubridate::mdy(toll_date),
         toll_hour = lubridate::mdy_hms(toll_hour),
         toll_10min_block = lubridate::mdy_hms(toll_10min_block),
         toll_week = lubridate::mdy(toll_week)) %>%
  saveRDS("data/zone_vehicle_entries.rds")


zip(zipfile =  "data/zone_vehicle_entries.zip",
    files = "data/zone_vehicle_entries.csv")

unlink("data/zone_vehicle_entries.csv")


# datetimes #########################################

# construct a dataset of all date-times in our study period.
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)

# read_csv("aqi.csv") %>% select(datetime) %>% 
#   filter(datetime <= "2020-01-01") 
  
tibble(
  date = c(
    seq(from = ymd("2018-01-01"), to = ymd("2019-06-01"), by = "1 day"),
    seq(from = ymd("2024-01-01"), to = ymd("2025-06-01"), by = "1 day")
  )
) %>%
  expand_grid(
    hours = 0:23
  ) %>%
  mutate(datetime = lubridate::make_datetime(
    year = year(date),
    month = month(date),
    day = day(date),
    hour = hours,
    tz = "UTC")
  ) %>%
  select(date, datetime) %>%
  mutate(
    # Create a binary current era / pre-pandemic era indicator
    current = case_when(
    date < "2020-01-01" ~ FALSE,
    date >= "2020-01-01" ~ TRUE
  ),
  # Create a binary treated indicator
    treated = case_when(
      # Congestion pricing started Sunday morning (midnight) on 2025-01-05 00:00:00 EDT,
      # which in UTC, is 05:00:00
      datetime >= "2025-01-05 05:00:00" ~ TRUE,
      # If we're looking at the pre-pandemic period, repeat the same idea, but clip it at 2020.
      datetime >= "2019-01-05 05:00:00" & date < "2020-01-01" ~ TRUE,
      # Otherwise, make it false
      TRUE ~ FALSE
    )
  ) %>%
  write_csv("data/datetimes.csv")

read_csv("data/datetimes.csv", show_col_types = FALSE) %>% glimpse()

# water #######################

# Gather water area data if it doesn't already exist
if(!file.exists("data/water.geojson")){
  m = read_csv("data/metro.csv", show_col_types = FALSE)
  path = "data/water.geojson"
  n = nrow(m)
  for(i in 19:n){
    #i = 1
    state = m$state[i]
    county = stringr::str_sub(m$geoid[i], 3,5)
    
    shape = tigris::area_water(state = state, county = county, year = 2020, keep_zipped_shapefile = TRUE, refresh = FALSE)
    
    shape %>% setNames(nm = names(.) %>% tolower()) %>%
      select(hydroid, fullname, mtfcc, awater) %>%
      st_as_sf() %>%
      st_transform(crs = 4326) %>%
      write_sf(path, append = TRUE)
    cat("\n", i, "/", n,"\n")
  }  
}

# superw = w %>% st_collection_extract("POLYGON") %>% summarize(geometry = st_union(geometry))
# superw %>% saveRDS("../data/water_cbsa.rds")


# States ####################
tigris::states(cb = TRUE, year = 2022) %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  saveRDS("../data/states.rds")

rm(list = ls())


#?tigris::area_water

# cleanup ######################

# Cleanup
rm(list = ls())

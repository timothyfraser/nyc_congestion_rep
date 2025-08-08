# 01_dataset.R

# Script to create panel dataset from raw data components.

# SETUP ##########################################

library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(lubridate)

setwd(paste0(rstudioapi::getActiveProject(), "/descriptives"))
source("00_functions.R")

# CREATE DATASET ##########################################

## SITES #####################################
# Where are our sensors?
# Let's classify these...
# sites = read_rds("../data/sites.rds")  %>%
#   st_join(
#     #left = FALSE, 
#     y = read_rds("../data/counties.rds") %>%
#       # filter(name %in% c("Bronx", "Queens", "Kings", "New York", "Richmond")) %>%
#       select(name)
#   ) %>%
#   mutate(name = if_else(is.na(name), true = "Beyond", false = name)) %>%
#   # within zone or not?
#   st_join(
#     y = read_rds("../data/zone.rds") %>% select(geometry) %>% mutate(within = 1)
#   ) %>%
#   mutate(within = if_else(is.na(within), true = 0, false = within))
sites = read_rds("../data/sites.rds") 

# zone = read_rds("../data/zone.rds")
# b = st_bbox(zone)
# ggplot() +
#   geom_sf(data = zone) +
#   geom_sf_label(data = sites, mapping = aes(label = id)) +
#   coord_sf(xlim = b[c("xmin", "xmax")], ylim = b[c("ymin","ymax")])
# 
# sites %>%
#   filter(id %in% c(1,14,5,16,6,13))


# We could take the AVERAGE from a few sites outside of highways...
# Eg. Passaic, Morris, Hunterdon, Middlesex

## DISTANCES ######################################
if(!file.exists("distances.csv")){
  #sites = read_rds("../data/sites.rds") %>% st_transform(crs = 4326)
  highways = read_rds("../data/highways.rds") %>% st_transform(crs = 4326) %>%
    select(linearid, geometry) %>%
    # Constrain to highways that overall 10 km buffer around sites
    st_join(
      left = FALSE, 
      y = sites %>% mutate(geometry = st_buffer(geometry, dist = 1000*5)) %>% 
        select(aqs_id_full, geometry) ) %>%
    select(-aqs_id_full) %>%
    distinct()
  
  # highways$geometry %>% plot()
  library(sf)
  
  
  sites %>%
    split(.$aqs_id_full) %>%
    purrr::map_dfr(
      .f = ~st_nearest_points(x = .x$geometry, highways) %>%
        st_as_sf(crs = 4326) %>% rename(geometry = x) %>%
        mutate(dist = st_length(geometry) %>% as.numeric()) %>%
        as_tibble() %>%
        summarize(
          distmin = min(dist, na.rm = TRUE),
          distmed = median(dist, na.rm = TRUE)
        ), .id = "aqs_id_full"
    ) %>% 
    write_csv("distances.csv")
}

## PANEL #####################################
# ggplot() +
#   geom_sf(data = read_rds("../data/counties.rds"), fill = "white", color = "#373737") +
#   geom_sf(data = read_rds("../data/highways.rds"), color = "red") +
#   geom_sf(data = read_rds("../data/sites.rds")) +
#   geom_sf_label(data = sites, mapping = aes(label = name), alpha = 0.5)

# Passaic

# sites$within
# bbox = read_rds("../data/zone.rds") %>% st_bbox()
# ggplot() +
#   geom_sf(data = read_rds("../data/zone.rds")) +
#   geom_sf(data = read_rds("../data/sites.rds"), alpha = 0.1) +
#   coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
#            ylim = c(bbox["ymin"], bbox["ymax"]))

# Create the grid of sensor-date times 
read_csv("../data/datetimes.csv") %>%
  filter(current == TRUE) %>%
  select(date, treated) %>%
  distinct() %>%
  # Make a grid using all the sites located within the New York City core-based statistical area
  tidyr::expand_grid(
    aqs_id_full = sites %>% filter(!name %in% "Beyond") %>% with(aqs_id_full)
  ) %>%
  write_csv("grid.csv")


# Get max daily AQI
aqi = read_csv("../data/air_quality.csv", show_col_types = FALSE) %>%
  filter(value >= 0) %>%
  filter(pollutant %in% c("PM2.5", "PM25"), unit == "UG/M3") %>%
  # Filter to study time period of interest 
  filter(datetime >= "2024-01-01") %>%
  # Classify as treated (UTC time)
  mutate(treated = datetime >= "2025-01-05 10:00:00") %>%
  mutate(date = lubridate::date(datetime)) %>%
  # center in on the 99% most common values
  filter(value < quantile(value, prob = 0.995)) %>%
  filter(value > quantile(value, prob = 0.005)) %>%
  group_by(aqs_id_full, treated, date) %>%
  summarize(value = max(value, na.rm = TRUE), .groups = "drop")

data = read_csv("grid.csv") %>%
  left_join(by = c("date", "treated", "aqs_id_full"), y = aqi) %>%
  # Join in county identifier
  left_join(by = c("aqs_id_full"), y = sites %>% as_tibble() %>% select(aqs_id_full, name, within)) %>%
  # Filter out any sensors located beyond the New York metro area
  filter(!name %in% "Beyond")   %>%
  # Join in block group identifier
  left_join(
    by = "aqs_id_full", 
    y = read_csv("../data/sites_bg_crosswalk_1km.csv", show_col_types = FALSE) %>%
      left_join(
        by = "geoid", 
        y = read_csv("../data/census.csv", show_col_types = FALSE) %>%
          filter(year == max(year)) %>%
          select(
            "geoid",
            "total_population",
            "edu_some_college", 
            "edu_postgrad", 
            "age_under_18",
            "age_65_and_over", 
            "median_income", 
            "population_by_race_White",
            "population_by_race_Black_or_African_American", 
            "population_by_race_Asian", 
            "Hispanic_or_Latino_Population"
          ) %>%
          mutate(some_college = (edu_postgrad + edu_some_college) / total_population,
                 age_elders = age_65_and_over / total_population,
                 age_kids = age_under_18 / total_population,
                 white = population_by_race_White / total_population,
                 black = population_by_race_Black_or_African_American / total_population,
                 asian = population_by_race_Asian / total_population,
                 hisplat = Hispanic_or_Latino_Population / total_population
          ) %>%
          select(geoid, pop = total_population,
                 some_college, age_elders, age_kids, white, black, asian, hisplat, median_income)
      ) %>%
      # calculate population per square kilometer
      mutate(pop_density = pop / area_land) %>%
      mutate(nonwhite = 1 - white) %>%
      # order columns
      select(aqs_id_full, geoid, pop, area_land, pop_density, 
             some_college, age_elders, age_kids, white, nonwhite, black, asian, hisplat, median_income) %>%
      group_by(aqs_id_full) %>% 
      summarize(
        # Get the total people and area of block groups in the buffer zone
        across(.cols = pop:area_land, .fns = ~sum(.x, na.rm = TRUE)),
        # Get the average rates and percentages for block group traits in the buffer zone
        across(.cols = pop_density:median_income, .fns = ~mean(.x, na.rm = TRUE))
      )
  ) %>%
  # Join in weather data
  left_join(
    by = c("aqs_id_full", "date"),
    y = read_csv("../data/weather.csv", show_col_types = FALSE) %>%
      mutate(date = lubridate::date(datetime)) %>%
      group_by(aqs_id_full, date) %>%
      summarize(across(.cols = temp:uvindex, .fns = ~mean(.x, na.rm = TRUE) ))
  ) %>%
  # Add other conceptualizations of time
  mutate(month = lubridate::month(date)) %>%
  mutate(week = lubridate::week(date)) %>%
  mutate(day = lubridate::wday(date, week_start = 1, locale = "EDT")) %>%
  mutate(weekend = if_else(day %in% c(6,7), true = TRUE, false = FALSE)) %>%
  mutate(weekyear = paste0(lubridate::week(date), "-", lubridate::year(date))) %>%
  # Create a daysafter counter, starting at zero
  mutate(daysafter = as.numeric(date - lubridate::date("2025-01-05")),
         daysafter = if_else(daysafter > 0, true = daysafter, false = 0) ) %>%
  # Classify points by area
  mutate(area = case_when(
    within == 1 ~ "crz",
    within == 0 & name %in% c("Richmond", "New York", "Bronx", "Queens", "Kings") ~ "nyc",
    within == 0 & !name %in% c("Richmond", "New York", "Bronx", "Queens", "Kings") ~ "cbsa"
  )) %>%
  # Join distance from nearest road to each sensor value
  left_join(by = c("aqs_id_full"), y = read_csv("distances.csv", show_col_types = FALSE))


# Join in background concentration levels
data = data %>% 
  left_join(
    by = c("date"), 
    y = data %>%
      filter(name %in% c('Hunterdon', "Middlesex", "Morris", "Passaic")) %>%
      select(name, date, value) %>%
      group_by(date) %>%
      summarize(bgmean = mean(value, na.rm = TRUE),
                bgsd = sd(value, na.rm = TRUE), .groups = "drop"))

# Join in 
data = data %>% 
  left_join(by = "aqs_id_full",
            y = sites %>% 
              as_tibble() %>% 
              select(aqs_id_full, distcrz = dist))

data %>%
  # Save to file
  saveRDS("panel_daily_nyc.rds")



## BACKGROUND LEVELS #########################################

# Select a few sites in the CSBA but not in the deeply concentrated road network space

# subset = data %>% 
#   filter(name %in% c('Hunterdon', "Middlesex", "Morris", "Passaic")) %>%
#   select(name, date, value)
# 
# ggplot() +
#   geom_line(
#     data = subset,
#     mapping = aes(x = date, y = value, group = name, color = name)
#   ) +
#   facet_wrap(~name)


rm(list = ls())

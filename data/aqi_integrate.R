# aqi_integrate.R

# Integrate two samples of AQI data.

library(dplyr)
library(readr)
library(stringr)
library(sf)
library(ggplot2)

setwd(paste0(rstudioapi::getActiveProject()))

# Create a crosswalk for the sites...
# read_csv("data/NYC_PM25_Monitor_Location.csv") %>%
#   mutate(lon = Longitude,
#          lat = Latitude,
#          site_agency = "NYC",
#          site_name = SiteName,
#          nyc_id = as.character(SiteID),
#          aqs_id = stringr::str_pad(string = 1:n(), width = 9, side = "left", pad = "9"),
#          aqs_id_full = paste0(840, aqs_id)) %>%
#   write_csv("data/sites_crosswalk.csv")

# visualize
# sites = read_rds("data/sites.rds") %>%
#   mutate(type = !is.na(nyc_id))
# counties = read_rds("data/counties.rds")
# boroughs = counties %>% filter(geoid %in% c("36005", "36047", "36061", "36081", "36085"))
# bbox2 = st_bbox(boroughs)
# 
# gg = ggplot() +
#   geom_sf(data = counties, fill = NA, color = "#373737") +
#   geom_sf(data = sites, mapping = aes(color = type, label = id), size = 3, alpha = 0.5) +
#   coord_sf(xlim = c(bbox2["xmin"], bbox2["xmax"]), ylim = c(bbox2["ymin"], bbox2["ymax"]) )
# 
# # plotly::ggplotly(gg, tooltip = c("label"))
# # read_csv("data/aqi.csv", show_col_types = FALSE, n_max = 5)
# 

# Pool both into a simpler format, showing...
# - aqs_id_full
# - datetime
# - pollutant
# - value
# - unit


# unlink("data/air_quality.csv")

# If the file already exists, do not proceed.
if(file.exists("data/air_quality.csv")){ 
  stop("STOP! Proceding will overwrite sensitive data!")}else{
  # If it does not exist, initialize a new file.

    # Make template
    tibble(
      aqs_id_full = NA_character_,
      datetime = NA_POSIXct_,
      pollutant = NA_character_,
      value = NA_real_,
      unit = NA_character_
    ) %>%
      slice(0) %>%
      write_csv("data/air_quality.csv", append = FALSE)

}

# add in pm2.5 concentratsion from AIRNOW
read_csv("data/aqi.csv", show_col_types = FALSE) %>%
  select(aqs_id_full, datetime, pollutant = param, value = concentration, unit = unit) %>%
  filter(!is.na(value)) %>%
  write_csv(file = "data/air_quality.csv", append = TRUE)

# Add in aqis from AIRNOW
read_csv("data/aqi.csv", show_col_types = FALSE) %>%
  select(aqs_id_full, datetime, pollutant = param, value = aqi) %>% 
  mutate(unit = "AQI") %>%
  filter(!is.na(value)) %>%
  write_csv(file = "data/air_quality.csv", append = TRUE)

# Add in pm2.5 from nyc
read_csv("data/NYC_Hourly_Monitoring_2019_2025.csv", show_col_types = FALSE) %>%
  select(nyc_id = SiteID, datetime = ObservationTimeUTC, value = Value) %>%
  mutate(pollutant = "PM25", unit = "UG/M3") %>%
  left_join(by = "nyc_id", y = read_csv("data/sites_crosswalk.csv", show_col_types = FALSE) %>%
              select(nyc_id, aqs_id_full)) %>%
  filter(lubridate::date(datetime) >= "2024-01-01" | lubridate::date(datetime) < "2020-01-01") %>%
  select(aqs_id_full, datetime, pollutant, value, unit) %>%
  filter(!is.na(value)) %>%
  write_csv(file = "data/air_quality.csv", append = TRUE)

rm(list = ls())



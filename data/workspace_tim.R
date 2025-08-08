# workspace.R

# set working directory
setwd(rstudioapi::getActiveProject())

library(dplyr)
library(readr)
library(httr2)
library(purrr)

# Load environmental variables
readRenviron("secret/.env")
# Test read environmental variables
# Sys.getenv("AQIKEY"); Sys.getenv("AQIEMAIL")

source("data/functions.R")



# https://aqs.epa.gov/data/api/dailyData/bySite?email=test@aqs.api&key=test&param=44201&bdate=20230601&edate=20230601&state=37&county=183&site=0014
# https://aqs.epa.gov/aqsweb/documents/data_api.html#variables

read_csv("data/sites.csv") %>%
  left_join(by = c("code" = "site_number"), y = read_csv("data/active_monitors.csv") ) 

# county of interest
geoid = 36061

.state = stringr::str_sub(geoid, 1,2)
.county = stringr::str_sub(geoid, 3,5)

metro = c("Bronx" = "36005",
          "Brooklyn" = "36047",
          "Manhattan" = "36061",
          "Queens" = "36081",
          "Staten Island" = "36085",
          "Nassau" = "36059",
          "Suffolk" = "36103",
          "Westchester" = "36119",
          "Rockland" = "36083",
          "Orange" = "36029")

# The Bronx is Bronx County (ANSI / FIPS 36005)
# Brooklyn is Kings County (ANSI / FIPS 36047)
# Manhattan is New York County (ANSI / FIPS 36061)
# Queens is Queens County (ANSI / FIPS 36081)
# Staten Island is Richmond County (ANSI / FIPS 36085)

# Nassau County: 36059
# Suffolk County: 36103
# Westchester County: 36119
# Rockland County: 36083
# Orange County: 36029 



# get active monitors ###############################

.param = read_csv("data/pollutants.csv") %>%
  filter(value_represented == "PM2.5 - Local Conditions") %>%
  with(code)


# time frame
bdate = "20250101"
edate = "20250530"





# data %>% slice(0) %>% write_csv("active_monitors.csv", overwrite = TRUE)


read_csv("active_monitors.csv") %>% View()
.param = read_csv("data/pollutants.csv") %>%
  filter(value_represented == "PM2.5 - Local Conditions") %>%
  with(code)







data = request("https://aqs.epa.gov") %>%
  req_method("GET") %>%
  req_url_path("data/api/dailyData/byCounty")  %>%
  req_headers("Accept" = "application/json") %>%
  req_url_query(email = Sys.getenv("AQIEMAIL"), key = Sys.getenv("AQIKEY")) %>%
  req_url_query(state = .state, county = .county) %>%
  req_url_query(param = .param) %>%
  req_url_query(bdate = bdate, edate = edate) 
data
req_perform()

data %>% 
  resp_body_json(simplifyVector = TRUE) 


sites = read_csv("data/active_monitors.csv") %>%
  filter(parameter_code == 88101,
         county_code == "005") %>%
  with(site_number)

read_csv("data/active_monitors.csv") %>%
  filter(parameter_code == 88101) %>%
  mutate(geoid = paste0(stringr::str_pad(state_code, side = "left", pad = "0", width = 2), county_code)) %>%
  split(.$geoid) %>%
  purrr::walk(.f = ~get_dailydata_by_site(geoids = .$geoid[1], param = .$parameter_code[1], site = .$site_number, path = "data/dailydata_by_site.csv"))

read_csv("data/dailydata_by_site.csv")


test = read_rds("data/counties.rds") %>%
  filter(geoid == "36061") %>%
  st_bbox() %>%
  get_aqdata(
    bdate = NULL,
    edate = NULL,
    edate = "2025-06-02 12:53:32 EDT",
    bbox = .,
    parameters = c("PM25"), datatype = "B"
  )

# Get AQI for a 2 week period

start = "2025-05-06"
end = "2025-06-01"
library(lubridate)
ndays = 26
as.numeric(ymd(end) - ymd(start))
seq_datetime(start = "2024-01-01", end = "2025-06-01", ndays = 17)


# as.numeric(ymd("2025-06-01") - ymd("2025-05-06"))

get_aqdata(
  bdate = c("2025-05-06-T00") %>% as_datetime_normal(),
  edate = c("2025-06-01T00") %>% as_datetime_normal(),
  bbox = read_rds("data/bbox_nums.rds"),
  parameters = c("PM25"), datatype = "B",
  path = "data/aqi.csv"
) 

read_rds("data/metro.rds")
# 500 queries per hour limit
library(dplyr)
library(sf)
library(ggplot2)
data = read_csv("data/aqi.csv") %>%
  filter(concentration > 0)

# data$datetime %>% unique()
ggplot() + 
  geom_point(data = data, mapping = aes(x = datetime, y = concentration))


ggplot() +
  geom_point(data = data, mapping = aes(x = lon, y = lat, color = concentration))

# level of spatial aggregation
# unit of analysis
# 1. sensor
# 2. grid cells --- LET'S SKIP THIS!
# 3. block groups 
# 4. road links

# level of temporal aggregation
# humidity, wind speed, and direction

# DOE
# Pick sensors that are close to congestion zone
# Pick sensors that are far from congestion zone

# Pattern across time, across space

ggplot() +
  geom_sf(data = read_rds("data/bbox.rds") %>% st_as_sfc() %>% st_as_sf(crs = 4326),
          fill = "pink", alpha = 0.5) +
  geom_sf(data = read_rds("data/counties.rds"), fill = NA, color = "#373737")
  


# Comparison
# CBD below 16th

# ----------------------------
# Regression Discontinuity Design / DiD
# --- describing change in New York City and throughout NYC

# ----------------------------
# Synthetic Control Experiment
# - Take a bunch of East Coast / Mid-Atlantic / Northeast cities
# - How much better did NYC do than:
#      Boston, Hartford, Providence, 
#      DC, Baltimore, Philadelphia, Newark
#      options: Non-Attainment Area cities




data = read_csv("data/aqi.csv") %>%
  filter(concentration >= 0) %>%
  filter(datetime > "2020-01-01") %>%
  mutate(date = lubridate::date(datetime)) %>%
  group_by(lat, lon, date, aqs_id) %>%
  summarize(concentration = median(concentration, na.rm = TRUE),
            aqi = median(aqi, na.rm = TRUE), .groups = "drop") %>%
  mutate(tr = if_else(date >= "2025-01-01", 1, 0))

data %>%
  lm(formula = concentration ~ tr + factor(date)) %>%
  summary()

data %>%
  group_by(tr) %>%
  summarize(concentration = mean(concentration, na.rm = TRUE))

ggplot() +
  geom_line(data = data, mapping = aes(x = date, y = concentration, group = aqs_id), alpha = 0.5, color = "lightblue")



library(dplyr)
library(readr)
library(sf)

readr::read_csv("data/aqi.csv", n_max = 5, show_col_types = FALSE) %>% 
  glimpse()

readr::read_rds("data/sites.rds") %>% glimpse()


readr::read_rds("data/counties.rds")  %>% glimpse()

readr::read_rds("data/bbox.rds")  %>% glimpse()

readr::read_rds("data/zone.rds")  %>% glimpse()
readr::read_rds("data/roads.rds")  %>% glimpse()
readr::read_rds("data/highways.rds")  %>% glimpse()
readr::read_rds("data/zone_vehicle_entries.rds")  %>% glimpse()

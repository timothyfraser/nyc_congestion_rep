# descriptives/08_media.R

# Script for media coverage inquiries of the tolling program.

# Get Started #########################################################
library(readr)
library(dplyr)
library(sf)
library(stringr)
library(ggplot2)

setwd("C:/Users/tmf77/nyc_congestion_pricing")

# Bronx Sites #########################################################


library(dplyr)
library(readr)
library(sf)
library(stringr)
library(gtools)

setwd("C:/Users/tmf77/nyc_congestion_pricing")


read_rds("data/sites.rds") %>%
   filter(str_detect(site_name, "Mott Haven"))
# 840999999997

read_rds("data/sites.rds") %>%
   filter(str_detect(site_name, "Hunts Point"))
# 840999999992

read_rds("data/sites.rds")  %>%
   as_tibble() %>%
   select(name, site_name, aqs_id_full, nyc_id, within) %>%
   View()


read_rds("data/sites.rds")  %>%
   as_tibble() %>%
   select(name, site_name, aqs_id_full, nyc_id, within) %>%
   filter(!is.na(site_name)) %>%
   with(site_name)

# Air quality change near Specific Sites

## Mott Haven and Hunts Point #########################################################
mysites = bind_rows(
   read_rds("data/sites.rds") %>%
   filter(str_detect(site_name, "Mott Haven")),
read_rds("data/sites.rds") %>%
   filter(str_detect(site_name, "Hunts Point"))
)
# 840999999997
# 840999999992

read_csv("descriptives/effects.csv") %>% 
   filter(type == "per_sensor", model == "nyc3") %>%
   filter(aqs_id_full %in% mysites$aqs_id_full) %>%
   left_join(by = "aqs_id_full", y = mysites %>% select(aqs_id_full, site_name)) %>%
   select(aqs_id_full, site_name, att, se_att) %>%
   mutate(lower = att - se_att * qnorm(0.975),
         upper = att + se_att * qnorm(0.975))

## Bronx Borough #########################################################
stat = read_csv("descriptives/effects.csv") %>% 
   filter(type == "per_sensor", model == "nyc3") %>%
   left_join(by = "aqs_id_full", y = read_rds("data/sites.rds")  %>% 
   as_tibble() %>% select(aqs_id_full, site_name, name)) %>%
   select(aqs_id_full, site_name, name, att, se_att) 


stat %>%
   filter(name %in% c("Bronx")) %>%
   nrow()
   summarize(att = range(att, na.rm = TRUE))


stat %>%
   filter(name %in% c("Bronx", "Queens", "Kings", "New York", "Richmond")) %>%
   summarize(att = range(att, na.rm = TRUE))


# In the process of calculating the average policy effects for the NYC 5 boroughs, 
# we did calculate effects for each sensor throughout NYC, although they were not the focus of the study.
# Mott Haven saw a 1.03 μg/m3 reduction in PM2.5 (SE = 0.03, p < 0.001), while Hunts Point saw a 1.02 μg/m3 reduction (SE = 0.03, p < 0.001).

# Long Island Sites #########################################################

read_csv("descriptives/effects.csv") %>% 
   filter(type == "per_sensor", model == "nyc3") %>%
   left_join(
      by = "aqs_id_full", 
      y = read_rds("data/sites.rds")  %>% 
   as_tibble() %>% select(aqs_id_full, site_name, name)) %>%
   select(aqs_id_full, site_name, name, att, se_att) 

read_csv("descriptives/att.csv")

read_rds("data/counties.rds")

# Get sites in counties that overlap with Long Island
mysites = read_rds("data/sites.rds") %>%
  st_join(
    y = read_rds("data/counties.rds") %>% 
    filter(name %in% c("Kings", "Queens", "Nassau", "Suffolk")) %>%
    select(geometry), left = FALSE) %>%
  select(aqs_id_full, name, geometry)

# How many sites are in Long Island?
mysites %>%
   as_tibble() %>%
   group_by(name) %>%
   count()
# Kings (4), Queens (6), Suffolk (1), Nassau (0)

# What is the average air quality change near Long Island?
# myeffects = read_csv("descriptives/effects.csv") %>%
#    filter(type == "per_sensor", model == "nyc3") %>%
#    inner_join(
#       by = c("aqs_id_full"), 
#       y = mysites %>% as_tibble() %>%select(aqs_id_full))

read_csv("descriptives/att.csv")  %>%
   filter(area == "NYC")

read_csv("descriptives/att.csv") %>% 
   filter(area %in% c(
     "Long Island", "Long Island (Nassau & Suffolk)", 
     "Kings", "Queens", "Nassau", "Suffolk"))

# Kings = -1.09 (se = 0.0151)
# Queens = -1.05 (se = 0.0122)
# Nassau = N/A
# Suffolk = -0.668 (se = 0.0216)
# Long Island = -1.03 (se = 0.00884)
# Long Island (Nassau & Suffolk) = -0.668 (se = 0.0216)

# We checked the data for Long Island,
# looking at the 4 counties that overlap with Long Island

# For Long Island, that's a 9.91% reduction in PM2.5 concentration.
 









# George Washington Bridge Sites #########################################################
# GW is 40.852° N, 73.952° W)

sites = read_rds("data/sites.rds")  %>%
   select(name, site_name, aqs_id_full, nyc_id, within, geometry) 

gw = tibble(name = "GW", site_name = "GW", aqs_id_full = "XXXXXXXXXXX", nyc_id = "XXXXXXXXXXX", within = 0, 
geometry = st_point(c(-73.952, 40.852)) %>% st_sfc(crs = 4326)) %>%
    st_as_sf(crs = 4326)

counties = read_rds("data/counties.rds")
ggplot() + 
   geom_sf(data = counties)  +
   geom_sf(data = gw, color = "red", size = 5, alpha = 0.5) +
   geom_sf(data = sites, color = "blue", size = 3)

nearby = sites %>% mutate(distgw = sf::st_nearest_points(gw, sites) %>% st_length() %>% as.numeric() %>% {./1000}) %>%
   arrange(distgw) 

nearby = sites %>% mutate(distgw = sf::st_nearest_points(gw, sites) %>% st_length() %>% as.numeric() %>% {./1000}) %>%
   arrange(distgw) %>%
   filter(distgw < 2) %>%
   with(aqs_id_full)

# Our study included 3 sites within 2 km of George Washington Bridge, including 

read_rds("descriptives/panel_daily_nyc.rds") %>%
   filter(aqs_id_full %in% nearby) %>% 
   group_by(aqs_id_full) %>%
   summarize(missing = sum(is.na(value)),
             all = n(),
             .groups = "drop") %>%
   mutate(percent = missing / all)

# Our study included 3 sites within 2 km of George Washington Bridge, plus 3 more sites within 5 km.
read_rds("descriptives/panel_daily_nyc.rds") %>%
   filter(aqs_id_full %in% nearby) %>% 
   group_by(aqs_id_full) %>%
   summarize(missing = sum(!is.na(value)),
             all = n(),
             .groups = "drop") %>%
   mutate(percent = missing / all)


# Air quality change per sensor near GWB
nearby = sites %>% mutate(distgw = sf::st_nearest_points(gw, sites) %>% st_length() %>% as.numeric() %>% {./1000} %>% {.*0.621371}) %>%
   arrange(distgw) %>%
   filter(distgw < 3) 

nearby_sites = nearby %>%
   with(aqs_id_full)

read_csv("descriptives/effects.csv") %>% 
   filter(type == "per_sensor", model == "cbsa3") %>%
   filter(aqs_id_full %in% nearby_sites) %>%
   summarize(
      att = mean(att, na.rm = TRUE)
   )



#read_rds("descriptives/sims.rds") %>% head()

# NJ Counties #########################################################
# read_rds("data/counties.rds")
# Which NJ counties are included in the study?
read_rds("data/metro.rds") %>%
filter(state == "NJ") %>%
with(paste0(str_remove(county, " County"), collapse = ", "))

# N sensors by County #############################################

read_rds("descriptives/panel_daily_nyc.rds") %>%
   group_by(name) %>%
   summarize(n = n()) %>%
   arrange(desc(n)) %>%
   left_join(by = "name", y = read_rds("data/metro.rds") %>% 
   select(name = county, state) %>% 
   mutate(name = str_remove(name, " County")))


# N sensors in NYC 5 boroughs ########################################
read_rds("descriptives/panel_daily_nyc.rds") %>% 
   filter(name %in% c("Bronx", "Queens", "Kings", "New York", "Richmond"))  %>%
   select(aqs_id_full) %>% distinct() %>% count()

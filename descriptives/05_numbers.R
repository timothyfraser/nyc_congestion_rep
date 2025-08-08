# 05_numbers.R

# Script for generating key numbers for paper text.

# SETUP ##########################################

library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(lubridate)

setwd(paste0(rstudioapi::getActiveProject(), "/descriptives"))
source("00_functions.R")



# N OBS #####################################

# N dates
read_csv("grid.csv", show_col_types = FALSE) %>%
  select(date) %>% distinct() %>% count()
# N sensors
read_csv("grid.csv", show_col_types = FALSE) %>%
  select(aqs_id_full) %>% distinct() %>% count()
# N dates for treatment vs control
read_csv("grid.csv", show_col_types = FALSE) %>%
  group_by(treated) %>%
  select(date) %>% distinct() %>% count()
# Total hours
read_csv("../data/datetimes.csv", show_col_types = FALSE) %>%
  filter(current == TRUE) %>%
  count()

# Total sensors that are from AIRNOW vs. Danni's NYC sample
read_rds("../data/sites.rds") %>%
  # filter to just core-based statistical area
  filter(!name %in% "Beyond") %>%
  group_by(airnow = !stringr::str_detect(aqs_id_full, "9999999") )  %>%
  count()

# Size of unbalanced panel
read_rds("panel_daily_nyc.rds") %>%
  summarize(
    valid = sum(!is.na(value)),
    total = n(),
    percent = valid / total
  )

# True range of data before 99% outlier thresholding
read_csv("../data/air_quality.csv", show_col_types = FALSE) %>%
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
  summarize(lower = min(value, na.rm = TRUE),
            upper = max(value, na.rm = TRUE))

# Missing data by treatment vs control period
read_rds("../descriptives/panel_daily_nyc.rds") %>%
  group_by(treated) %>%
  summarize(missing = sum(is.na(value)), 
            all = n(),
            .groups = "drop") %>%
  mutate(percent = missing / sum(all))

read_rds("../descriptives/panel_daily_nyc.rds") %>%
  group_by(area) %>%
  summarize(count = n(),
            sensors = length(unique(aqs_id_full)))

# Missing data by sensor type 
read_rds("../descriptives/panel_daily_nyc.rds") %>%
  # classify each sensor as an airnow sensor or not
  mutate(airnow = !stringr::str_detect(aqs_id_full, "9999999")) %>%
  # measure 
  group_by(airnow, treated) %>%
  summarize(missing = sum(is.na(value)), 
            all = n(),
            .groups = "drop") %>%
  mutate(percent = missing / sum(all))


# view missing data grid
ggplot() +
  geom_tile(
    data = read_rds("panel_daily_nyc.rds"),
    mapping = aes(
      x = reorder(factor(aqs_id_full), aqs_id_full),
      y = date,
      fill = is.na(value)
    )
  ) +
  # geom_tile(
  #   data = read_rds("panel_daily_nyc.rds") %>%
  #     filter(within == 1),
  #   mapping = aes(
  #     x = reorder(factor(aqs_id_full), aqs_id_full),
  #     y = date
  #   ),
  #   color = "black",
  #   #linewidth = 0.01,
  #   alpha = 0.1
  # ) +
  coord_flip()


rm(list = ls())

# ABSTRACT ###########################################
library(dplyr)
library(readr)
library(sf)
library(stringr)

# Area of CRZ in square kilometers
read_rds("../data/zone.rds") %>%
  summarize(area = st_area(geometry) %>% {./(1e3^2) } %>% as.numeric()) %>%
  with(area)

# Size of unbalanced panel
read_rds("panel_daily_nyc.rds") %>%
  summarize(total = n())

# N dates
read_rds("panel_daily_nyc.rds") %>%
  select(date) %>% distinct() %>% count()

# N sensors
read_rds("panel_daily_nyc.rds") %>%
  select(aqs_id_full) %>% distinct() %>% count()

# Effects
read_csv("effects.csv") %>%
  filter(type == "overall") %>%
  filter(spec == 3) %>%
  mutate(lower = att - se_att * qnorm(0.975),
         upper = att + se_att * qnorm(0.975)) %>%
  mutate(
    across(
      .cols = c("att", "lower", "upper"),
      .fns = ~scales::number(.x, accuracy = 0.01)
      ),
    se_att = scales::number(se_att, accuracy = 0.001)
  ) %>%
  select(model, att, se_att, lower, upper, stars)


# Air Pollution within Each Zone
read_rds("panel_daily_nyc.rds") %>%
  filter(date == "2024-06-01" | date == "2025-06-01") %>%
  group_by(area) %>%
  summarize(
    min = min(value, na.rm = TRUE),
    mu = mean(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  )
  
read_rds("panel_daily_nyc.rds") %>%
  filter((date >= "2024-01-05" & date <= "2024-06-01") | 
           (date >= "2025-01-05" & date <= "2025-06-01") ) %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(area, year) %>%
  summarize(
    min = min(value, na.rm = TRUE),
    mu = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    upper = quantile(value, na.rm = TRUE, probs = 0.90)
  ) %>%
  ungroup() %>%
  mutate(aqi = pm25_aqi(max))

# Calculate projected reduction
stat = read_rds("models.rds")[c(9)] %>%
  purrr::map_dfr(~get_qis(m = .x, areas = c("crz")), .id = "model") %>%
  filter(type == "overall") %>%
  mutate(percent = 1 - yhat1 / yhat0) %>%
  select(model, yhat1, yhat0, att, percent)







# VIF #############################################

# First, let's get the max VIF

data = read_rds("../descriptives/panel_daily_nyc.rds")

# data %>%
#   select(treated, date, month) %>%
#   mutate(across(everything(), .fns = ~as.numeric(.x))) %>%
#   cor(use = "pairwise.complete.obs")

get_vif = function(m){
  myvif = car::vif(m)
  if(is.matrix(myvif)){
    tibble(term = rownames(myvif), vif = myvif[,3]^2)
  }else{ tibble(term = names(myvif), vif = myvif) }
  
}  

formula1 = sqrt(value) ~ 
  # Treatment variable
  treated +
  # Interaction
  # I(treated * daysafter ) +
  # Temporal control
  factor(week) +
  # day-by-day variation
  factor(day) +
  # background pollution levels     
  sqrt(bgmean) +
  # site-by-site variation
  # factor(aqs_id_full) +
  # distance from Manhattan
  #log(distcrz + 1) +
  # distance from roads
  sqrt(distmin + 1) +
  # factor(area)
  log(temp) + humidity + windspeed + precip + cloudcover +
  log(pop_density + 1 ) + 
  # log(pop + 1) + log(area_land + 1) +
  #log(pop_density + 1) +
  log(median_income + 1) +
  nonwhite + hisplat

formula3 = sqrt(value) ~ 
  # Treatment variable
  treated +
  # Interaction
  # I(treated * daysafter ) +
  # Temporal control
  factor(week) +
  # day-by-day variation
  factor(day) +
  # background pollution levels     
  sqrt(bgmean) +
  # site-by-site variation
  # factor(aqs_id_full) +
  # distance from Manhattan
  # log(distcrz + 1) +
  # distance from roads
  sqrt(distmin + 1) +
  # factor(area)
  log(temp) + humidity + windspeed + precip + cloudcover +
  log(pop_density + 1) +
  log(median_income + 1)


# Calculate the VIF per term for each of our 3 models.
bind_rows(
  data %>% 
    filter(area %in% c("cbsa", "nyc", "crz")) %>%
    lm(formula = formula1) %>%
    get_vif() %>%
    mutate(model = 1),
  data %>% 
    filter(area %in% c("nyc", "crz")) %>%
    lm(formula = formula1) %>%
    get_vif() %>%
    mutate(model = 2),
  data %>% 
    filter(area %in% c("crz")) %>%
    lm(formula = formula3) %>%
    get_vif() %>%
    mutate(model = 3)
) %>%
  mutate(vif = round(vif, 2)) %>%
  write_csv("vif.csv")

# Get the max VIF
read_csv("vif.csv") %>%
  group_by(model) %>%
  summarize(max = max(vif),
            term = term[vif == max(vif)])


# Correlations between moderate-to-high VIF elements
vars = read_csv("vif.csv") %>%
  filter(vif > 3) %>%
  select(term) %>%
  distinct() %>%
  mutate(term = if_else(
    condition = stringr::str_detect(term, "^log\\("),
    true = stringr::str_extract(term, "(?<=log\\()[^\\s\\+\\)]+"),
    false = term
  ))

read_rds("panel_daily_nyc.rds") %>%
  select(any_of(vars$term))  %>%
  {
    data = .
    v = names(data)
    if("distcrz" %in% v){ data$distcrz = log(data$distcrz + 1) }
    if("temp" %in% v){ data$temp = log(data$temp) }
    if("pop_density" %in% v){ data$pop_density = log(data$pop_density + 1) }
    if("median_income" %in% v){ data$median_income = log(data$median_income + 1) }
    if("distmin" %in% v){ data$distmin = log(data$distmin + 1) }
    data
  } %>%
  # use custom function from 00_functions.R to get a tidy upper correlation matrix
  cor_tidy() %>%
  filter(abs(cor) > 0.7)

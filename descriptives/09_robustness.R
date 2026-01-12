# 09_robustness.R


# DAILY AVERAGES VS DAILY MAX ############################
# read_rds("descriptives/panel_daily_nyc.rds")  %>% glimpse()
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(broom)
setwd("C:/Users/tmf77/nyc_congestion_rep")
data = read_rds("descriptives/panel_daily_nyc.rds")
source("descriptives/00_functions.R")

# What would happen if we used the daily average instead of the daily max?
formula3b =   sqrt(value)   ~ 
    treated + I(treated * daysafter ) + factor(week) + factor(day) +
    #bgmean +
    sqrt(bgmean) + #log(distcrz + 1) + #log(distmin + 1) +
    log(temp) + humidity + windspeed + precip + cloudcover +
    log(pop_density + 1 ) + log(median_income + 1) #+ nonwhite + hisplat
    
formula3b2 =   sqrt(value_avg)   ~ 
    treated + I(treated * daysafter ) + factor(week) + factor(day) +
    #bgmean +
    sqrt(bgmean) + #log(distcrz + 1) + #log(distmin + 1) +
    log(temp) + humidity + windspeed + precip + cloudcover +
    log(pop_density + 1 ) + log(median_income + 1) #+ nonwhite + hisplat
    

m0 = data %>% filter(within == 1) %>% lm(formula = formula3b)
m = data %>% filter(within == 1) %>% lm(formula = formula3b2)

bind_rows(
    broom::tidy(m0) %>% mutate(model = "max"),
    broom::tidy(m) %>% mutate(model = "average")
) %>% filter(term == "I(treated * daysafter)")
# Both show a statistically significant decrease...


  
# Use THIS MODEL for CRZ -----------------------
areas = "crz"
useobs = FALSE; impute = FALSE; start = "2025-01-05"; end = "2025-06-01"
path_data = "descriptives/panel_daily_nyc.rds"

# Get predictions for treatment and counterfactual
results = list(m0, m) %>%
    purrr::map_dfr(
        .f = ~get_yhat(m = .x, path_data = path_data, useobs = useobs, impute = impute) %>% 
        filter(area %in% areas) %>%
        get_simeffects(start = start, end = end) %>%
        group_by(area) %>% 
        get_att(), 
        .id = "model")

results %>%
   select(model, area, yhat1, yhat0, att, se_att, p_value, stars, percentchange)

# Results remain pretty consistent.

# A tibble: 2 × 9
#   model area  yhat1 yhat0   att se_att p_value stars percentchange
#   <chr> <chr> <dbl> <dbl> <dbl>  <dbl>   <dbl> <chr>         <dbl>
# 1 1     crz    9.31 12.2  -3.05 0.0220       0 ***           0.249
# 2 2     crz    5.85  8.09 -2.31 0.0110       0 ***           0.286
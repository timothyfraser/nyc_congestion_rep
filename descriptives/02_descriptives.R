# 02_descriptives.R

# SETUP ##########################################

library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(lubridate)

setwd("C:/Users/tmf77/nyc_congestion_pricing/descriptives")
# setwd(paste0(rstudioapi::getActiveProject(), "/descriptives"))
setwd("C:/Users/tmf77/nyc_congestion_pricing/descriptives")
# setwd(paste0(rstudioapi::getActiveProject(), "/descriptives"))
source("00_functions.R")




# VISUALIZE ##########################################


## Daily ###################################
source("00_functions.R")

data = read_rds("panel_daily_nyc.rds")
gg = data %>%
  filter(name %in% c("Bronx", "Queens", "Kings", "New York", "Richmond")) %>%
  get_lines()

ggsave(plot = gg, filename = "fig_lines.png", dpi = 300, width = 8, height = 5)
browseURL("fig_lines.png")

rm(list = ls())

## Day of Week Curves ########################

data = read_rds("../descriptives/panel_daily_nyc.rds") %>%
  mutate(month = lubridate::month(date, label = TRUE))

# What do the trends usually look like by day of week and month?
gg = ggplot() +
  geom_line(data = data,
            mapping = aes(x = factor(day), y = value, group = paste0(weekyear, "-", aqs_id_full), 
                          color = factor(treated) ), alpha = 0.2) +
  facet_wrap(~month) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(color = "Treated", y = "PM2.5 Concentration (ug/m3)",
       title = "Variation in Air Quality by Day of Week") +
  scale_x_discrete(expand = expansion(c(0.05,0.05))) +
  scale_y_continuous(expand = expansion(c(0,0))) +
  scale_color_manual(values = c("lightgrey", "#DC267F")) +
  labs(x = "Day of Week (Monday = 1)")

ggsave(plot = gg, filename = "fig_daysofweek.png", dpi = 300, width = 8, height = 8)
browseURL("fig_daysofweek.png")

rm(list = ls())


# MODELING ##########################################

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(purrr)
library(gtools)
library(broom)
library(car)

## Make Models #######################################
data = read_rds("../descriptives/panel_daily_nyc.rds")

source("00_functions.R")

models = tribble(
  ~short, ~long, 
  "cbsa1", "CBSA 1",
  "cbsa2", "CBSA 2",
  "cbsa3","CBSA 3",
  "nyc1", "NYC 1",
  "nyc2","NYC 2",
  "nyc3","NYC 3",
  "crz1", "CRZ 1",
  "crz2","CRZ 2",
  "crz3","CRZ 3"
) %>%
  mutate(id = 1:n())

# Create the models, which will be named according to models$short
m = data %>% get_many_models() 

m %>% saveRDS("models.rds")

rm(list = ls())

## Model Tables #########################################

source("00_functions.R")

read_rds("models.rds") %>% get_table() %>% 
  knitr::kable(
    format = "html", 
    align = c("l", "c","c","c",  "c","c","c",  "c","c","c"), 
    col.names = c("Predictor",
                  "CBSA<br>Base Model",
                  "CBSA<br>+Weather",
                  "CBSA<br>+Demographics",
                  "NYC<br>Base Model",
                  "NYC<br>+Weather",
                  "NYC<br>+Demographics",
                  "CRZ<br>Base Model",
                  "CRZ<br>+Weather",
                  "CRZ<br>+Demographics"
                  ),
    caption = paste0(
      "<b>Linear Models of Air Pollution in New York City Metropolitan Area</b>",
      "<br>",
      "<i>Dependent Variable: &radic;PM<sub>2.5</sub> Concentration ug/m<sup>3</sup></i>",
      "<br>",
      "Main Effects and Goodness of Fit Statistics",
      "<hr></hr>"
    ), escape = FALSE
  ) %>% 
  cat(file = "table_models.html")



read_rds("models.rds") %>% get_table(fe = TRUE) %>% 
  knitr::kable(
    format = "html", 
    align = c("l", "c","c","c",  "c","c","c",  "c","c","c"), 
    col.names = c("Predictor",
                  "CBSA<br>Base Model",
                  "CBSA<br>+Weather",
                  "CBSA<br>+Demographics",
                  "NYC<br>Base Model",
                  "NYC<br>+Weather",
                  "NYC<br>+Demographics",
                  "CRZ<br>Base Model",
                  "CRZ<br>+Weather",
                  "CRZ<br>+Demographics"
    ),
    caption = paste0(
      "<b>Linear Models of Air Pollution in New York City Metropolitan Area</b>",
      "<br>",
      "<i>Dependent Variable: &radic;PM<sub>2.5</sub> Concentration ug/m<sup>3</sup></i>",
      "<br>",
      "Fixed Effects",
      "<hr></hr>"
    ), escape = FALSE
  ) %>% 
  cat(file = "table_models_fe.html")


browseURL("table_models.html")

browseURL("table_models_fe.html")


ggplot() +
  geom_sf(data = read_rds("../data/counties.rds"))

## Latex Rows ############################

# # Convert the HTML rows to LaTeX
# library(dplyr)
# library(stringr)
# tibble(lines = read_lines("table_models.html") ) %>%
#   filter(str_detect(lines, pattern = "([<]tr|[<]td)")) %>%
#   with(lines) %>%
#   paste0(collapse = "") %>%
#   get_latex()

# QIs ############################################################

# Finally, let's generate some meaningful quantities of interest

# i = 3
# m = read_rds("models.rds")[[i]]
# start = "2025-01-05"; end = "2025-06-01"; path_data=  "../descriptives/panel_daily_nyc.rds"

# Run only if effects.csv does not exist
source("00_functions.R")
if(!file.exists("effects.csv")){
  bind_rows(
    read_rds("models.rds")[c(1:3)] %>%
      purrr::map_dfr(~get_qis(m = .x, areas = c("cbsa", "nyc", "crz")), .id = "model"),
    
    read_rds("models.rds")[c(4:6)] %>%
      purrr::map_dfr(~get_qis(m = .x, areas = c("nyc", "crz")), .id = "model"),
    
    read_rds("models.rds")[c(7:9)] %>%
      purrr::map_dfr(~get_qis(m = .x, areas = c("crz")), .id = "model")
  ) %>%
    mutate(spec = stringr::str_extract(model, "[0-9]{1}") %>% as.integer()) %>%
    write_csv("effects.csv")
  # Clear cache
  gc()
}


# BENCHMARKS #################################################
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(gtools)
setwd("C:/Users/tmf77/nyc_congestion_pricing/descriptives")
source("00_functions.R")

# First, we're going to calculate the treatment effects as done in the original study.
path_att = report_att(path_att = "../descriptives/att.csv", 
  start = "2025-01-05", end = "2025-06-01", useobs = FALSE, impute = FALSE,
  path_qi = "../descriptives/qi_by_sensordate.csv", path_data =  "../descriptives/panel_daily_nyc.rds")
read_csv(path_att, show_col_types = FALSE) %>% 
   filter(area %in% c("CRZ", "NYC", "CBSA"))

# What if we use the basic controls models?
unlink("../descriptives/qi_by_sensordate_impute.csv")
path_att_basic = report_att(path_att = "../descriptives/att_controls_basic.csv", 
  start = "2025-01-05", end = "2025-06-01", useobs = FALSE, impute = FALSE, controls = FALSE,
  path_qi = "../descriptives/qi_by_sensordate_controls_basic.csv", path_data =  "../descriptives/panel_daily_nyc.rds")
read_csv(path_att_basic, show_col_types = FALSE) %>% 
   filter(area %in% c("CRZ", "NYC", "CBSA"))

# What if we imputed missing covariates like cloudcover, based on other known weather, but didn't use observed values?
unlink("../descriptives/qi_by_sensordate_impute.csv")
path_att_impute = report_att(path_att = "../descriptives/att_impute.csv", 
  start = "2025-01-05", end = "2025-06-01", useobs = FALSE, impute = TRUE,
  path_qi = "../descriptives/qi_by_sensordate_impute.csv", path_data =  "../descriptives/panel_daily_nyc.rds")
read_csv(path_att_impute, show_col_types = FALSE) %>% glimpse()


# Next, we're going to do some robustness tests, calculating the treatment effects using observed values for the treated group.
unlink("../descriptives/qi_by_sensordate_obs.csv")
path_att_obs = report_att(path_att = "../descriptives/att_obs.csv", 
  start = "2025-01-05", end = "2025-06-01", useobs = TRUE, impute = FALSE,
  path_qi = "../descriptives/qi_by_sensordate_obs.csv", path_data =  "../descriptives/panel_daily_nyc.rds")


# Finally, calculate using observed values for the treated group, and imputed missing covariates.
# Most robust - avoids losing any 
unlink("../descriptives/qi_by_sensordate_obs_impute.csv")
path_att_obs_impute = report_att(path_att = "../descriptives/att_obs_impute.csv", 
  start = "2025-01-05", end = "2025-06-01", useobs = TRUE, impute = TRUE,
  path_qi = "../descriptives/qi_by_sensordate_obs_impute.csv", path_data =  "../descriptives/panel_daily_nyc.rds")
read_csv(path_att_obs_impute, show_col_types = FALSE) %>% glimpse()


# SENSITIVITY TEST #################################################
# Next, we're going to do a little sensitivity test,
# evaluating how the treatment effects change when we change the time period of the treatment.

path_qi = "../descriptives/qi_by_sensordate_obs_impute.csv"
result1 = get_att_by_time(path_qi = path_qi, start = rep("2025-01-05", 5), end = lubridate::make_date(year = 2025, month = 2:6, day = 1))

# result2 = get_att_by_time(path_qi = path_qi, start = c("2025-01-05", lubridate::make_date(year = 2025, month = 2:5, day = 1)) %>% as.character(), end = lubridate::make_date(year = 2025, month = 2:6, day = 1))

# How did the average treatment effects change over time?
# Cumulative effect of the treatment
result1 %>% 
   filter(area == "CRZ")
result1 %>% 
   filter(area == "NYC")
result1 %>% 
   filter(area == "CBSA")
result1 %>% 
   filter(area == "Overall")

result1 %>% 
   filter(area == "Bronx")



rm(list = ls())

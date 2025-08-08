# 02_background.R

# Comparing sites to use for background concentration levels

library(dplyr)
library(readr)
library(ggplot2)

data = read_rds("descriptives/panel_daily_nyc.rds")


# Grab the air quality monitoring stations in counties with these names:
mysample = c('Hunterdon', "Middlesex", "Morris", "Passaic")


# Get subset
subset = data %>%
  select(aqs_id_full, name, date, value) %>%
  filter(name %in% mysample) 

# Calculate mean background concentration
bg = data %>%
  select(aqs_id_full, name, date, value) %>%
  filter(name %in% mysample) %>%
  group_by(date) %>%
  summarize(bgmean = mean(value, na.rm = TRUE),
            bgsd = sd(value, na.rm = TRUE), .groups = "drop")


# View it
ggplot() +
  geom_line(
    data = data %>%
      filter(area == "crz") %>%
      select(date, value, aqs_id_full) %>%
      tidyr::expand_grid(name = mysample),
    mapping = aes(x = date, y = value, group = aqs_id_full), color = "grey", alpha = 0.25
  ) +
  geom_line(
    data = subset,
    mapping = aes(x = date, y = value, group = name, color = name)
  ) +
  facet_wrap(~name) +
  theme_bw() +
  theme(legend.position = "bottom")

# Decidedly lower in the background concentration sample towns than in the crz.
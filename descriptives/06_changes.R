# 06_changes.R

# visualize and report our key changes

library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)

# Load vehicle entries data
vehicle_weekly <- readr::read_rds("data/zone_vehicle_entries.rds") %>%
  mutate(week = floor_date(toll_10min_block, unit = "week")) %>%
  group_by(week, vehicle_class) %>%
  summarize(total_entries = sum(crz_entries, na.rm = TRUE), .groups = "drop") %>%
  mutate(rate = total_entries / (24 * 7))

# Define the weeks of interest
week_jan6 <- lubridate::floor_date(as.Date("2025-01-06"), unit = "week")
week_june1 <- lubridate::floor_date(as.Date("2025-06-01"), unit = "week")

entries_weekly = vehicle_weekly %>%
    group_by(term = vehicle_class) %>%
    summarize(start = rate[week == min(week, na.rm = TRUE)],
              end = rate[week == max(week, na.rm = TRUE)],
              percent_change = 100 * (end - start) / start
        )

# Calculate percent change in ATT (AQI) for each area from first to last week
att_weekly <- readr::read_csv("descriptives/effects.csv", show_col_types = FALSE) %>%
  filter(type == "per_week", model %in% c("crz3", "nyc3", "cbsa3")) %>%
  group_by(term = model) %>%
  summarize(
    start = att[week == min(week, na.rm = TRUE)],
    end = att[week == max(week, na.rm = TRUE)], 
    percent_change = 100 * (end - start) / abs(start)
  )


data = bind_rows(entries_weekly, att_weekly)
# percent change in effects and entries
ggplot() +
    geom_col(data = data, aes(x = term, y = percent_change, fill = term)) 


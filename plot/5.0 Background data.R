library(dplyr)
library(readr)
library(lubridate)

# Step 1: Read panel data and extract unique dates for "New York"
panel <- readRDS("panel_daily_nyc.rds")

ny_dates <- panel %>%
  filter(name == "New York") %>%
  select(date) %>%
  distinct() %>%
  mutate(date = as.Date(date))  # Ensure proper Date format

# Step 2: Read hourly monitoring data for NYC
nyc_hourly <- read_csv("NYC_Hourly_Monitoring_2019_2025.csv")

# Step 3: Clean and summarize to get daily mean background concentration
nyc_hourly_clean <- nyc_hourly %>%
  select(ObservationTimeUTC, Value) %>%
  mutate(
    ObservationTimeUTC = ymd_hms(ObservationTimeUTC, quiet = TRUE),
    date = as.Date(ObservationTimeUTC)
  ) %>%
  filter(!is.na(date), !is.na(Value)) %>%
  group_by(date) %>%
  summarise(bgmean = mean(Value, na.rm = TRUE), .groups = "drop")

# Step 4: Join with New York dates from panel to fill missing values
background <- ny_dates %>%
  left_join(nyc_hourly_clean, by = "date")

# Step 5: Save the final background data
write_csv(background, "background.csv")




library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# ==== Step 1: Read hourly PM2.5 data ====
df <- read_csv("NYC_Hourly_Monitoring_2019_2025.csv") %>%
  mutate(
    datetime = ymd_hms(ObservationTimeUTC),
    date = as.Date(datetime)
  )

# ==== Step 2: Read daily background values ====
background <- read_csv("background.csv") %>%
  mutate(date = as.Date(date))  # ensure matching format

# ==== Step 3: Join background data & calculate net PM2.5 ====
df_with_net <- df %>%
  left_join(background, by = "date") %>%
  mutate(
    net_pm25 = pmax(Value - bgmean, 0)
  ) %>%
  filter(!is.na(bgmean))  

# ==== Step 3.1: Identify top high days ====
# Step A: Compute daily average net PM2.5
target_months <- c(1, 2, 3, 4,5, 6)

daily_summary <- df_with_net %>%
  filter(year(date) == 2025, month(date) %in% target_months) %>%
  mutate(Year = year(date), Month = month(date)) %>%
  group_by(date, Year, Month) %>%
  summarise(daily_net_pm25 = mean(net_pm25, na.rm = TRUE), .groups = "drop")

# Step B: For each month, compute 80th percentile threshold
thresholds <- daily_summary %>%
  group_by(Year, Month) %>%
  summarise(threshold = quantile(daily_net_pm25, 0.85, na.rm = TRUE), .groups = "drop")

# Step C: Identify high-concentration dates for all 3 months
high_days <- daily_summary %>%
  inner_join(thresholds, by = c("Year", "Month")) %>%
  filter(daily_net_pm25 > threshold) %>%
  pull(date)

# Step D: Remove these high days from df_with_net
df_with_net <- df_with_net %>%
  filter(!(date %in% high_days))

# ==== Step 3.3: Export cleaned data ====
write_csv(df_with_net, "NYC_PM25_with_background_and_net.csv")

# ==== Step 4: Define CBD sites ====
cbd_site_ids <- c(
  "36061NY08454", "36061NY09734", "36061NY08653",
  "36061NY10130", "36061NY08552", "36061NY09929"
)

# ==== Step 5: Aggregate to daily net PM2.5 by region ====
df_daily <- df_with_net %>%
  mutate(
    month = floor_date(date, "month"),
    Region = ifelse(SiteID %in% cbd_site_ids, "CRZ", "Outside CRZ")
  ) %>%
  filter(date >= as.Date("2024-01-01")) %>%
  group_by(date, month, Region) %>%
  summarise(Net_PM25_daily = mean(net_pm25, na.rm = TRUE), .groups = "drop")

# ==== Step 5.5: Remove outliers by IQR within each region/month ====
df_daily_clean <- df_daily %>%
  group_by(Region, month) %>%
  filter(
    Net_PM25_daily >= quantile(Net_PM25_daily, 0.25, na.rm = TRUE) - 1.5 * IQR(Net_PM25_daily, na.rm = TRUE),
    Net_PM25_daily <= quantile(Net_PM25_daily, 0.75, na.rm = TRUE) + 1.5 * IQR(Net_PM25_daily, na.rm = TRUE)
  ) %>%
  ungroup()

# ==== Step 6: Visualization ====
ggplot(df_daily_clean, aes(x = month, y = Net_PM25_daily, group = month)) +
  geom_boxplot(outlier.shape = NA, fill = "steelblue", alpha = 0.6) +
  facet_wrap(~Region, ncol = 1) +
  coord_cartesian(ylim = c(0, 5)) +
  labs(
    title = "Monthly Distribution of Daily Net PM2.5 (2024–2025)",
    x     = "Month",
    y     = "Net PM2.5 (µg/m³)"
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 16)
  )

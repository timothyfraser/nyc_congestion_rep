library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# ==== read data ====
df <- read_csv("NYC_Hourly_Monitoring_2019_2025.csv")
cbd_site_ids <- c(
  "36061NY08454","36061NY09734","36061NY08653",
  "36061NY10130","36061NY08552","36061NY09929"
)

# ==== daily average ====
df_daily <- df %>%
  mutate(
    ObservationTimeUTC = ymd_hms(ObservationTimeUTC),
    date   = as.Date(ObservationTimeUTC),
    month  = floor_date(date, "month"),
    Region = ifelse(SiteID %in% cbd_site_ids, "CBD", "Outside CBD")
  ) %>%
  filter(date >= as.Date("2024-01-01")) %>%
  group_by(date, month, Region) %>%
  summarise(PM25_daily = mean(Value, na.rm = TRUE), .groups = "drop")

# ==== figure ====
ggplot(df_daily, aes(x = month, y = PM25_daily, group = month)) +
  geom_boxplot(outlier.size = 0.5, fill = "steelblue", alpha = 0.6) +
  facet_wrap(~Region, ncol = 1) +
  labs(
    title = "Monthly Distribution of Daily PM2.5 (2024–2025)",
    x     = "Month",
    y     = "PM2.5 (µg/m³)"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"  # 英文月份缩写，如 Jan, Feb
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 16)
  )




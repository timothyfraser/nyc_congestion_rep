library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# ==== read data ====
df <- read_csv("NYC_Hourly_Monitoring_2019_2025.csv")

# ==== CBD site ====
cbd_site_ids <- c("36061NY08454", "36061NY09734", "36061NY08653", 
                  "36061NY10130", "36061NY08552", "36061NY09929")

# ==== data process ====
df_daily <- df %>%
  mutate(
    ObservationTimeUTC = ymd_hms(ObservationTimeUTC),
    date = as.Date(ObservationTimeUTC),
    year = year(date),
    month_num = month(date),
    month_name = sprintf("%02d", month_num),  # 两位数月份
    Region = ifelse(SiteID %in% cbd_site_ids, "CBD", "Outside CBD")
  ) %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2025-05-31")) %>%
  group_by(Region, SiteID, date, year, month_num, month_name) %>%
  summarise(PM25_daily = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_for_x = paste0(year, "-", month_name))

# ==== set x ====
month_levels <- c(
  paste0("2024-", sprintf("%02d", 1:12)),
  paste0("2025-", sprintf("%02d", 1:5))
)
df_daily$month_for_x <- factor(df_daily$month_for_x, levels = month_levels)

# ==== figure ====
ggplot(df_daily, aes(x = month_for_x, y = PM25_daily)) +
  geom_boxplot(outlier.size = 0.5, fill = "steelblue", alpha = 0.6) +
  geom_vline(xintercept = 12.5, linetype = "dashed", color = "red") +  # 分隔 2024 和 2025
  facet_wrap(~ Region, ncol = 1) +
  scale_x_discrete(labels = function(x) substr(x, 6, 7)) +  # 横轴只显示月份
  labs(
    title = "PM2.5 Concentration (CBD vs Outside CBD)",
    x = "Month",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16)
  )


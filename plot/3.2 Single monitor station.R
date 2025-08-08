library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# ==== read data ====
df <- read_csv("NYC_Hourly_Monitoring_2019_2025.csv")
site_info <- read_csv("NYC_PM25_Monitor_Location.csv")

# ==== target ID ====
target_site <- "36061NY10130"

# target_site <- "36061NY08454"

#target_site <- "36081NY07615"


# ==== extract siteid ====
site_name <- site_info %>%
  filter(SiteID == target_site) %>%
  pull(SiteName)

if(length(site_name) == 0) site_name <- "Unknown Site"

# ==== data process ====
df_daily <- df %>%
  mutate(
    ObservationTimeUTC = ymd_hms(ObservationTimeUTC),
    date = as.Date(ObservationTimeUTC),
    hour = hour(ObservationTimeUTC),
    Year = year(date),
    MonthNum = month(date),
    Month = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  filter(
    SiteID == target_site,
    Year %in% c(2024, 2025),
    MonthNum %in% 1:6
   
  ) %>%
  group_by(date, Year, Month) %>%
  summarise(PM25_daily = mean(Value, na.rm = TRUE), .groups = "drop")

# ==== boxplot（2024 vs 2025 ） ====
p1 <- ggplot(df_daily, aes(x = Month, y = PM25_daily, fill = factor(Year))) +
  geom_boxplot(
    outlier.size = 0.4,
    alpha = 0.6,
    width = 0.6,
    position = position_dodge(width = 0.7)
  ) +
  scale_fill_manual(
    values = c(`2024` = "#1f77b4", `2025` = "#ff7f0e"),
    name = "Year"
  ) +
  labs(
    title = paste("Station", target_site, "-", site_name),
    x = "Month",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top"
  )

print(p1)

# ==== median change ====
monthly_median <- df_daily %>%
  group_by(Year, Month) %>%
  summarise(PM25_median = median(PM25_daily, na.rm = TRUE), .groups = "drop")

monthly_median_wide <- monthly_median %>%
  pivot_wider(names_from = Year, values_from = PM25_median, names_prefix = "Year_") %>%
  mutate(
    PercentChange = (Year_2025 - Year_2024) / Year_2024 * 100,
    ArrowLabel = ifelse(
      PercentChange >= 0,
      paste0("▲ ", round(PercentChange, 1), "%"),
      paste0("▼ ", round(abs(PercentChange), 1), "%")
    )
  )

# ==== 添加中位数变化百分比标注 ====
p2 <- p1 +
  geom_text(
    data = monthly_median_wide,
    aes(
      x = Month,
      y = max(df_daily$PM25_daily, na.rm = TRUE) + 2,
      label = ArrowLabel
    ),
    inherit.aes = FALSE,
    size = 4
  )

print(p2)

# ==== output ====
# ggsave("PM25_SingleStation_JanMay_Comparison.png", p2, width = 10, height = 6, dpi = 300)



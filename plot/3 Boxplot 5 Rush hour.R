library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# ========= 1) read data =========
df <- read_csv("NYC_Hourly_Monitoring_2019_2025.csv")

cbd_site_ids <- c(
  "36061NY08454", "36061NY09734", "36061NY08653",
  "36061NY10130", "36061NY08552", "36061NY09929"
)


df_base <- df %>%
  mutate(
    ObservationTimeUTC = ymd_hms(ObservationTimeUTC),
    date   = as.Date(ObservationTimeUTC, tz = "UTC"),
    hour   = hour(ObservationTimeUTC),
    Year   = year(date),
    Month  = month(date, label = TRUE, abbr = TRUE),
    Region = ifelse(SiteID %in% cbd_site_ids, "CBD", "Outside CBD")
  ) %>%
  filter(
    date >= as.Date("2024-01-01"),
    date <= as.Date("2025-06-30"),
    Month %in% month(1:6, label = TRUE, abbr = TRUE)
  )

# ========= 2) peak hours =========
# morning peak (7am-10am)
morning_peak <- df_base %>%
  filter(hour >= 7, hour <= 10) %>%
  group_by(date, Year, Month, Region) %>%
  summarise(PM25_daily = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Peak = "Morning (7am-10am)")

# evening peak (4pm-7pm)
evening_peak <- df_base %>%
  filter(hour >= 16, hour <= 19) %>%
  group_by(date, Year, Month, Region) %>%
  summarise(PM25_daily = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Peak = "Evening (4pm-7pm)")

# overall
df_peak <- bind_rows(morning_peak, evening_peak) %>%
  mutate(
    Peak = factor(Peak, levels = c("Morning (7am-10am)", "Evening (4pm-7pm)")),
    YearRegion = paste0(Year, " ", Region),
    YearRegion = factor(
      YearRegion,
      levels = c("2024 CBD", "2024 Outside CBD", "2025 CBD", "2025 Outside CBD")
    )
  )

# ========= 3) figure 1 =========
p1 <- ggplot(df_peak, aes(x = Month, y = PM25_daily, fill = YearRegion)) +
  geom_boxplot(outlier.size = 0.4, alpha = 0.9, width = 0.65,
               position = position_dodge(width = 0.75)) +
  facet_wrap(~ Peak, ncol = 1) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "2024 CBD" = "#2165AA",
      "2024 Outside CBD" = "#408FBF",
      "2025 CBD" = "#89BED9",
      "2025 Outside CBD" = "#BFDAE9"
    )
  ) +
  labs(
    title = "PM2.5 (Peak Hours 2024 vs 2025)",
    x = "Month",
    y = "Avg PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12)
  )
# ggsave("Boxplot_PM25_PeakHours_Combined.png", p1, width = 10, height = 8, dpi = 300)
print(p1)

# ========= 4) 第二张图：按区域和高峰时段分面 =========
p2 <- ggplot(df_peak, aes(x = Month, y = PM25_daily, fill = factor(Year))) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.6, width = 0.6,
               position = position_dodge(width = 0.7)) +
  facet_grid(Peak ~ Region) +
  scale_fill_manual(
    values = c("2024" = "#1f77b4", "2025" = "#ff7f0e"),
    name = "Year"
  ) +
  labs(
    title = "PM2.5 (Peak Hours 2024 vs 2025)",
    x = "Month",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 10)
  )
# ggsave("Boxplot_PM25_PeakHours_ByRegion.png", p2, width = 10, height = 8, dpi = 300)
print(p2)

# ========= 5) figure 2 =========
monthly_median <- df_peak %>%
  group_by(Year, Month, Region, Peak) %>%
  summarise(PM25_median = median(PM25_daily, na.rm = TRUE), .groups = "drop")

monthly_median_wide <- monthly_median %>%
  pivot_wider(
    names_from = Year,
    values_from = PM25_median,
    names_prefix = "Year_"
  ) %>%
  mutate(
    PercentChange = (Year_2025 - Year_2024) / Year_2024 * 100,
    Peak = factor(Peak, levels = c("Morning (7am-10am)", "Evening (4pm-7pm)"))
  )

p3 <- ggplot(monthly_median_wide, aes(x = Month, y = PercentChange, fill = Region)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = paste0(round(PercentChange, 1), "%")),
    position = position_dodge(width = 0.8),
    vjust = ifelse(monthly_median_wide$PercentChange >= 0, -0.3, 1.3),
    size = 3.5
  ) +
  facet_wrap(~ Peak, ncol = 1) +
  scale_fill_manual(values = c("CBD" = "#e5b5b5", "Outside CBD" = "#5f89b1")) +
  labs(
    title = "PM2.5 Median Percent Change (Rush Hours)",
    x = "Month",
    y = "Percent Change (%)"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12)
  )
# ggsave("PM25_Median_Percent_Change_Peak.png", p3, width = 10, height = 8, dpi = 300)
print(p3)
#=============adjust===============
p3 <- ggplot(monthly_median_wide, aes(x = Month, y = PercentChange, fill = Region)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(
    aes(label = paste0(round(PercentChange, 1), "%")),
    position = position_dodge(width = 0.8),
    vjust = ifelse(monthly_median_wide$PercentChange >= 0, -0.3, 1.3),
    size = 3.5
  ) +
  facet_wrap(~ Peak, ncol = 1) +
  scale_fill_manual(values = c("CBD" = "#e5b5b5", "Outside CBD" = "#5f89b1")) +
  labs(
    title = "PM2.5 Median Percent Change (Rush Hours)",
    x = "Month",
    y = "Percent Change (%)"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_y_continuous(limits = c(-50, NA)) +  # Extend the y-axis down to -50
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(face = "bold", size = 12),
    panel.spacing = unit(1, "lines"),  # Space between facets
    strip.background = element_blank(),  # Optional: Remove background from facet labels
    plot.margin = margin(t = 20, b = 20)  # Top and bottom margins for better spacing
  )

# ggsave("PM25_Median_Percent_Change_Peak.png", p3, width = 10, height = 8, dpi = 300)
print(p3)



# ===== 6) arrow =====
monthly_median_wide <- monthly_median_wide %>%
  mutate(
    ArrowLabel = ifelse(
      PercentChange >= 0,
      paste0("▲\n", round(PercentChange, 1), "%"),
      paste0("▼\n", round(abs(PercentChange), 1), "%")
    )
  )

  # === note ===
p2 <- ggplot(df_peak, aes(x = Month, y = PM25_daily, fill = factor(Year))) +
  geom_boxplot(
    outlier.size = 0.5,
    alpha = 0.6,
    width = 0.6,
    position = position_dodge(width = 0.7)
  ) +
  geom_text(
    data = monthly_median_wide,
    aes(
      x = Month,
      y = max(df_peak$PM25_daily, na.rm = TRUE) + 2,
      label = ArrowLabel
    ),
    inherit.aes = FALSE,
    size = 3.2,
    lineheight = 0.9
  ) +
  facet_grid(Peak ~ Region) +
  scale_fill_manual(
    values = c("2024" = "#1f77b4", "2025" = "#ff7f0e"),
    name = "Year"
  ) +
  labs(
    title = "PM2.5 (Peak Hours 2024 vs 2025)",
    x = "Month",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 16),
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 10)
  )

# ggsave("Boxplot_PM25_PeakHours_ByRegion_Arrow2Lines.png", p2, width = 12, height = 10, dpi = 300)
print(p2)


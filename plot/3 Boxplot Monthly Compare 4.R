library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# ========= 1) data =========
df <- read_csv("NYC_Hourly_Monitoring_2019_2025.csv")

cbd_site_ids <- c(
  "36061NY08454", "36061NY09734", "36061NY08653",
  "36061NY10130", "36061NY08552", "36061NY09929"
)

df_daily <- df %>%
  mutate(
    ObservationTimeUTC = ymd_hms(ObservationTimeUTC),
    date   = as.Date(ObservationTimeUTC),
    hour   = hour(ObservationTimeUTC),
    Year   = year(date),
    Month  = month(date, label = TRUE, abbr = TRUE),
    Region = ifelse(SiteID %in% cbd_site_ids, "CBD", "Outside CBD")
  ) %>%
  filter(
    date >= as.Date("2024-01-01"),
    date <= as.Date("2025-06-30"),
    Month %in% month(1:6, label = TRUE, abbr = TRUE),
  ) %>%
  group_by(date, Year, Month, Region) %>%
  summarise(PM25_daily = mean(Value, na.rm = TRUE), .groups = "drop")

# ========= 2) figure 1 =========
df_daily <- df_daily %>%
  mutate(YearRegion = paste0(Year, " ", Region)) %>%
  mutate(YearRegion = factor(
    YearRegion,
    levels = c("2024 CBD", "2024 Outside CBD", "2025 CBD", "2025 Outside CBD")
  ))

p1 <- ggplot(df_daily,
             aes(x = Month, y = PM25_daily, fill = YearRegion)) +
  geom_boxplot(
    outlier.size = 0.4,
    alpha = 0.9,
    width = 0.65,
    position = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "2024 CBD"         = "#2165AA",
      "2024 Outside CBD" = "#408FBF",
      "2025 CBD"         = "#89BED9",
      "2025 Outside CBD" = "#BFDAE9"
    )
  ) +
  labs(
    title = "PM2.5 Concentration (2024 vs 2025)",
    x = "Month",
    y = "Avg PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16)
  )
print(p1)
ggsave("Boxplot_PM25.png", p1, width = 10, height = 6, dpi = 300)

# ========= 3) figure 2=========
p2 <- ggplot(df_daily,
             aes(x = Month, y = PM25_daily, fill = factor(Year))) +
  geom_boxplot(
    outlier.size = 0.5,
    alpha = 0.6,
    width = 0.6,
    position = position_dodge(width = 0.7)
  ) +
  facet_wrap(~ Region, ncol = 1) +
  scale_fill_manual(
    values = c(`2024` = "#1f77b4", `2025` = "#ff7f0e"),
    name = "Year"
  ) +
  labs(
    title = "PM2.5 (2024 vs 2025)",
    x = "Month",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 16),
    legend.position = "top"
  )
print(p2)
# ggsave("Boxplot_PM25_2.png", p2, width = 10, height = 7, dpi = 300)

# ========= 4) calcuate=========
monthly_median <- df_daily %>%
  group_by(Year, Month, Region) %>%
  summarise(PM25_median = median(PM25_daily, na.rm = TRUE), .groups = "drop")

# 转为宽格式
monthly_median_wide <- monthly_median %>%
  pivot_wider(
    names_from = Year,
    values_from = PM25_median,
    names_prefix = "Year_"
  ) %>%
  mutate(
    PercentChange = (Year_2025 - Year_2024) / Year_2024 * 100
  )

print(monthly_median_wide)
# ========= 5) figure 3=========
p3_median <- ggplot(monthly_median_wide, aes(x = Month, y = PercentChange, fill = Region)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("CBD" = "#e5b5b5", "Outside CBD" = "#5f89b1")
  ) +
  geom_text(
    aes(label = paste0(formatC(PercentChange, format = "f", digits = 1), "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 4
  ) +
  labs(
    title = "PM2.5 Change (Median) from 2024 to 2025",
    y = "Percent Change (%)",
    x = "Month"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top"
  )
print(p3_median)


# ========= 3) figure 3 =========
p2 <- ggplot(df_daily,
             aes(x = Month, y = PM25_daily, fill = factor(Year))) +
  geom_boxplot(
    outlier.size = 0.5,
    alpha = 0.6,
    width = 0.6,
    position = position_dodge(width = 0.7)
  ) +
  # 添加中位数百分比变化标注
  geom_text(
    data = monthly_median_wide,
    aes(x = Month, y = max(df_daily$PM25_daily, na.rm = TRUE) + 2,  # 添加到图上方
        label = paste0(round(PercentChange, 1), "%")),
    inherit.aes = FALSE,
    position = position_dodge(width = 0.7),
    size = 4
  ) +
  facet_wrap(~ Region, ncol = 1) +
  scale_fill_manual(
    values = c(`2024` = "#1f77b4", `2025` = "#ff7f0e"),
    name = "Year"
  ) +
  labs(
    title = "PM2.5 Concentration (2024 vs 2025)",
    x = "Month",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 16),
    legend.position = "top"
  )
print(p2)
# ggsave("Boxplot_PM25_2_Annotated.png", p2, width = 10, height = 7, dpi = 300)
#========arrow===========================
monthly_median_wide <- monthly_median_wide %>%
  mutate(
    ArrowLabel = ifelse(
      PercentChange >= 0,
      paste0("▲ ", round(PercentChange, 1), "%"),
      paste0("▼ ", round(abs(PercentChange), 1), "%")
    )
  )
p2 <- ggplot(df_daily,
             aes(x = Month, y = PM25_daily, fill = factor(Year))) +
  geom_boxplot(
    outlier.size = 0.5,
    alpha = 0.6,
    width = 0.6,
    position = position_dodge(width = 0.7)
  ) +
  # arrow+note
  geom_text(
    data = monthly_median_wide,
    aes(
      x = Month,
      y = max(df_daily$PM25_daily, na.rm = TRUE) + 2,
      label = ArrowLabel
    ),
    inherit.aes = FALSE,
    position = position_dodge(width = 0.7),
    size = 4
  ) +
  facet_wrap(~ Region, ncol = 1) +
  scale_fill_manual(
    values = c(`2024` = "#1f77b4", `2025` = "#ff7f0e"),
    name = "Year"
  ) +
  labs(
    title = "PM2.5 Concentration (2024 vs 2025)",
    x = "Month",
    y = "PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 16),
    legend.position = "top"
  )

print(p2)
# ggsave("Boxplot_PM25_2_Annotated_With_Arrows.png", p2, width = 10, height = 7, dpi = 300)



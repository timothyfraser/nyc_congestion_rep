library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# Step 1: Define CBD sites
cbd_site_ids <- c(
  "36061NY08454", "36061NY09734", "36061NY08653",
  "36061NY10130", "36061NY08552", "36061NY09929"
)

# Step 2: Aggregate to daily net PM2.5 by Region
df_with_net <- read_csv("NYC_PM25_with_background_and_net.csv")
df_daily <- df_with_net %>%
  mutate(
    datetime = ymd_hms(ObservationTimeUTC),
    date     = as.Date(datetime),
    hour     = hour(datetime),
    Year     = year(date),
    Month    = month(date, label = TRUE, abbr = TRUE),
    Region   = ifelse(SiteID %in% cbd_site_ids, "CRZ", "Outside CRZ")
  ) %>%
  filter(
    date >= as.Date("2024-01-01"),
    date <= as.Date("2025-06-30"),
    Month %in% month(1:6, label = TRUE, abbr = TRUE),
    hour >= 5, hour <= 21  # Keep 5am–9pm only
  ) %>%
  group_by(date, Year, Month, Region) %>%
  summarise(PM25_daily = mean(net_pm25, na.rm = TRUE), .groups = "drop")

# Step 3: Remove outliers using IQR method within each Month × Year × Region
df_daily <- df_daily %>%
  group_by(Month, Year, Region) %>%
  filter(
    PM25_daily >= quantile(PM25_daily, 0.25, na.rm = TRUE) - 1.5 * IQR(PM25_daily, na.rm = TRUE),
    PM25_daily <= quantile(PM25_daily, 0.75, na.rm = TRUE) + 1.5 * IQR(PM25_daily, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 4: Plot 2 – Facet by Region, color by Year
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
    title = "Net PM2.5 (5am-9pm,2024 vs 2025)",
    x = "Month",
    y = "Net PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 16),
    legend.position = "top"
  )
print(p2)

# Step 5: Compute percent change in monthly medians (2025 vs 2024)
monthly_median <- df_daily %>%
  group_by(Year, Month, Region) %>%
  summarise(PM25_median = median(PM25_daily, na.rm = TRUE), .groups = "drop")

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

# Step 6: Plot percent change bar chart with labels
p3_median <- ggplot(monthly_median_wide, aes(x = Month, y = PercentChange, fill = Region)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("CRZ" = "#e5b5b5", "Outside CRZ" = "#5f89b1")
  ) +
  geom_text(
    aes(label = paste0(formatC(PercentChange, format = "f", digits = 1), "%")),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 4
  ) +
  labs(
    title = "Net PM2.5 (5am-9pm,Median, 2025 vs 2024)",
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

# Step 7: Add ▲▼ arrows to percent change labels on boxplot
monthly_median_wide <- monthly_median_wide %>%
  mutate(
    ArrowLabel = ifelse(
      PercentChange >= 0,
      paste0("▲ ", round(PercentChange, 1), "%"),
      paste0("▼ ", round(abs(PercentChange), 1), "%")
    )
  )

p2_arrow <- ggplot(df_daily,
                   aes(x = Month, y = PM25_daily, fill = factor(Year))) +
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
    title = "Net PM2.5 (2024 vs 2025) with Change Arrows",
    x = "Month",
    y = "Net PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 16),
    legend.position = "top"
  )

print(p2_arrow)



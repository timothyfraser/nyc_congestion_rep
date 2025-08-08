# ========== Load Required Packages ==========
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# ========== Read Input Data ==========
df <- read_csv("NYC_Hourly_Monitoring_2019_2025.csv")

# ========== Define CBD Monitoring Site IDs ==========
cbd_site_ids <- c(
  "36061NY08454", "36061NY09734", "36061NY08653",
  "36061NY10130", "36061NY08552", "36061NY09929"
)

# ========== Clean and Annotate Data ==========
df <- df %>%
  mutate(
    ObservationTimeUTC = ymd_hms(ObservationTimeUTC),
    hour = floor_date(ObservationTimeUTC, unit = "hour"),
    Region = ifelse(SiteID %in% cbd_site_ids, "CBD", "Outside CBD"),
    Policy = case_when(
      !is.na(hour) & hour < ymd("2025-01-05") ~ "Before", # policy implemention date
      !is.na(hour) & hour >= ymd("2025-01-05") ~ "After",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Policy)) %>%
  filter(Value >= 0, Value <= quantile(Value, 0.99, na.rm = TRUE))  # Remove top 1% PM2.5 outliers

# ========== Compute Hourly Averages ==========
df_hourly <- df %>%
  group_by(hour, SiteID, Region, Policy) %>%
  summarise(PM25 = mean(Value, na.rm = TRUE), .groups = "drop")

df_hourly$Policy <- factor(df_hourly$Policy, levels = c("Before", "After"))

# ========== Plot 1: All NYC (No Region Split) ==========
ggplot(df_hourly, aes(x = Policy, y = PM25)) +
  geom_boxplot(fill = "#8888cc", outlier.shape = NA, alpha = 0.7, width = 0.6) +
  labs(
    title = "Hourly PM2.5 Concentration: All NYC",
    x = "Policy Implementation Period",
    y = "Hourly Avg PM2.5 (µg/m³)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16)
  )

# ========== Plot 2: CBD vs Outside CBD Comparison ==========
ggplot(df_hourly, aes(x = Policy, y = PM25, fill = Region)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.6, position = position_dodge(0.7)) +
  labs(
    title = "Hourly PM2.5 Concentration Before and After Congestion Pricing",
    x = "Policy Implementation Period",
    y = "Hourly Avg PM2.5 (µg/m³)",
    fill = "Region"
  ) +
  scale_fill_manual(values = c("CBD" = "#1f77b4", "Outside CBD" = "#ff7f0e")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16)
  )

# ========== Plot 3: Faceted View by Region ==========
ggplot(df_hourly, aes(x = Policy, y = PM25, fill = Policy)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.6) +
  labs(
    title = "Hourly PM2.5 by Region",
    x = "Policy",
    y = "Hourly Avg PM2.5 (µg/m³)"
  ) +
  facet_wrap(~Region) +
  scale_fill_manual(values = c("Before" = "#6baed6", "After" = "#fd8d3c")) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 16)
  )

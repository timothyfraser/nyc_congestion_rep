# ==== Load required packages ====
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# ==== Load data ====
df <- read_csv("NYC_Hourly_Monitoring_2019_2025.csv")       # PM2.5 hourly data
location <- read_csv("NYC_PM25_Monitor_Location.csv")       # Station metadata

# ==== Define CBD SiteIDs ====
cbd_site_ids <- c(
  "36061NY08454", "36061NY09734", "36061NY08653",
  "36061NY10130", "36061NY08552", "36061NY09929"
)

# ==== Data preprocessing ====
df <- df %>%
  mutate(
    ObservationTimeUTC = ymd_hms(ObservationTimeUTC),
    date = as.Date(ObservationTimeUTC),
    month = floor_date(date, "month"),
    Region = ifelse(SiteID %in% cbd_site_ids, "CBD", "Outside CBD")
  ) %>%
  filter(date >= as.Date("2023-01-01"))

# ==== Monthly mean by station ====
monthly_station <- df %>%
  group_by(month, SiteID, Region) %>%
  summarise(PM25_mean = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  left_join(location %>% select(SiteID, Latitude, Longitude, SiteName), by = "SiteID")

# ==== ==== 方案 A：每站一条线，按 Region 上色 ====
plot_station_trends_two_colors <- function() {
  ggplot(monthly_station,
         aes(x = month,
             y = PM25_mean,
             group = interaction(Region, SiteName),
             color = Region)) +
    geom_line(alpha = 0.7, size = 1) +
    geom_point(alpha = 0.9, size = 2) +
    scale_color_manual(values = c("CBD" = "#D95F02", "Outside CBD" = "#1B9E77")) +
    labs(title = "Monthly PM2.5 Trends (Station-Level, Region Colored)",
         x = "Month",
         y = "PM2.5 Concentration (µg/m³)",
         color = "Region") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# ==== 保存图像（方案 A）====
ggsave("PM25_Trends_By_Station_RegionColor.png",
       plot_station_trends_two_colors(),
       width = 8, height = 6, dpi = 300)


# ==== ==== 方案 B：两区域平均，两条粗线 ====
monthly_region <- df %>%
  group_by(month, Region) %>%
  summarise(PM25_mean = mean(Value, na.rm = TRUE), .groups = "drop")

plot_region_average <- function() {
  ggplot(monthly_region,
         aes(x = month, y = PM25_mean, color = Region)) +
    geom_line(size = 1.5) +
    geom_point(size = 3) +
    scale_color_manual(values = c("CBD" = "#D95F02", "Outside CBD" = "#1B9E77")) +
    labs(title = "Monthly Mean PM2.5: CBD vs. Outside CBD",
         x = "Month",
         y = "PM2.5 Concentration (µg/m³)",
         color = "Region") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# ==== 保存图像（方案 B）====
ggsave("PM25_Trends_RegionAverage.png",
       plot_region_average(),
       width = 8, height = 6, dpi = 300)

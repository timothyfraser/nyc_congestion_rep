library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# === read data ===
df <- read_csv("NYC_Hourly_Monitoring_2019_2025.csv")
site_info <- read_csv("NYC_PM25_Monitor_Location.csv")

# === target site ===
target_sites <- c("36061NY10130", "36061NY08454", "36081NY07615")

# === set time ===
time_periods <- list(
  `5-21` = c(5,21)
)

# === repeat ===
for (site in target_sites) {
  
  site_name <- site_info %>%
    filter(SiteID == site) %>%
    pull(SiteName)
  if(length(site_name) == 0) site_name <- "Unknown Site"
  
  for (period_name in names(time_periods)) {
    hours <- time_periods[[period_name]]
    
    # 数据筛选
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
        SiteID == site,
        Year %in% c(2024, 2025),
        MonthNum %in% 1:6,
        hour >= hours[1], hour <= hours[2]
      ) %>%
      group_by(date, Year, Month) %>%
      summarise(PM25_daily = mean(Value, na.rm = TRUE), .groups = "drop")
    
    if (nrow(df_daily) == 0) {
      message(paste("No data for", site, period_name))
      next
    }
    
    # 中位数变化
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
    
    # 绘图
    p <- ggplot(df_daily, aes(x = Month, y = PM25_daily, fill = factor(Year))) +
      geom_boxplot(
        outlier.size = 0.4,
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
        size = 4
      ) +
      scale_fill_manual(
        values = c(`2024` = "#1f77b4", `2025` = "#ff7f0e"),
        name = "Year"
      ) +
      labs(
        title = paste("Station", site, "-", site_name, period_name, "(2024 vs 2025)"),
        x = "Month",
        y = "PM2.5 (µg/m³)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "top"
      )
    
    print(p)
    
    # savefig
    # filename <- paste0("PM25_", site, "_", gsub(" ", "", period_name), ".png")
    # ggsave(filename, p, width = 10, height = 6, dpi = 300)
  }
}

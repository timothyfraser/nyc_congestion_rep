library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# === Step 1: Load Data ===
df_with_net <- read_csv("NYC_PM25_with_background_and_net.csv")
site_info   <- read_csv("NYC_PM25_Monitor_Location.csv")

# === Step 2: Extract SiteIDs present in df_with_net & site_info ===
target_sites <- intersect(unique(df_with_net$SiteID), site_info$SiteID)

# === Step 3: Time period (5am to 9pm) ===
time_periods <- list(`5-21` = c(5, 21))

# === Step 4: Loop over each SiteID and period ===
for (site in target_sites) {
  
  site_name <- site_info %>%
    filter(SiteID == site) %>%
    pull(SiteName)
  if (length(site_name) == 0) site_name <- "Unknown Site"
  
  for (period_name in names(time_periods)) {
    hours <- time_periods[[period_name]]
    
    # Step 4.1: Filter and aggregate net PM2.5
    df_daily <- df_with_net %>%
      mutate(
        datetime  = ymd_hms(ObservationTimeUTC),
        date      = as.Date(datetime),
        hour      = hour(datetime),
        Year      = year(date),
        MonthNum  = month(date),
        Month     = month(date, label = TRUE, abbr = TRUE)
      ) %>%
      filter(
        SiteID == site,
        Year %in% c(2024, 2025),
        MonthNum %in% 1:6,
        hour >= hours[1], hour <= hours[2],
        !is.na(net_pm25)
      ) %>%
      group_by(date, Year, Month) %>%
      summarise(PM25_daily = mean(net_pm25, na.rm = TRUE), .groups = "drop")
    
    if (nrow(df_daily) == 0) {
      message(paste("No data for", site, period_name))
      next
    }
    
    # Step 4.2: Monthly median & % change
    monthly_median <- df_daily %>%
      group_by(Year, Month) %>%
      summarise(PM25_median = median(PM25_daily, na.rm = TRUE), .groups = "drop")
    
    monthly_median_wide <- monthly_median %>%
      pivot_wider(
        names_from = Year,
        values_from = PM25_median,
        names_prefix = "Year_"
      ) %>%
      mutate(
        PercentChange = (Year_2025 - Year_2024) / Year_2024 * 100,
        ArrowLabel = ifelse(
          PercentChange >= 0,
          paste0("▲ ", round(PercentChange, 1), "%"),
          paste0("▼ ", round(abs(PercentChange), 1), "%")
        )
      )
    
    # Step 4.3: Plot
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
        title = paste("Net PM2.5 –", site_name, "(", site, ") [", period_name, "]"),
        x = "Month",
        y = "Net PM2.5 (µg/m³)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title  = element_text(face = "bold", size = 14),
        legend.position = "top"
      )
    
    print(p)
    
    # Optional: save plot
    # ggsave(paste0("plots/NetPM25_", site, "_", gsub(" ", "", period_name), ".png"), 
    #        plot = p, width = 10, height = 6, dpi = 300)
  }
}


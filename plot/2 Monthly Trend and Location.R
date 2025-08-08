# If not installed, install required packages
# install.packages(c("readr", "dplyr", "ggplot2", "lubridate", "sf", "tigris", "ggrepel"))

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)
library(tigris)
library(ggrepel)

# Cache shapefile data locally
options(tigris_use_cache = TRUE)

# ==== Load data ====
df <- read_csv("NYC_Hourly_Monitoring_2019_2025.csv")       # Main PM2.5 hourly data
location <- read_csv("NYC_PM25_Monitor_Location.csv")          # Metadata: station name, coordinates

# ==== Define SiteIDs within CBD area ====
cbd_site_ids <- c(
  "36061NY08454", "36061NY09734", "36061NY08653",
  "36061NY10130", "36061NY08552", "36061NY09929"
)

# ==== Clean and transform main dataset ====
df <- df %>%
  mutate(
    ObservationTimeUTC = ymd_hms(ObservationTimeUTC),      # Convert timestamp to datetime
    date = as.Date(ObservationTimeUTC),                    # Extract date
    month = floor_date(date, "month"),                     # Round to first day of each month
    Region = ifelse(SiteID %in% cbd_site_ids, "CBD", "Outside CBD")  # Label as CBD or not
  ) %>%
  filter(date >= as.Date("2023-01-01"))                    # Filter to 2025 data

# ==== Calculate monthly average PM2.5 by site ====
monthly_station <- df %>%
  group_by(month, SiteID, Region) %>%
  summarise(PM25_mean = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  left_join(location %>% select(SiteID, Latitude, Longitude, SiteName), by = "SiteID")

# ==== Plot function: monthly PM2.5 trend lines ====
plot_station_trends <- function(region_filter, region_label) {
  data_region <- monthly_station %>% filter(Region %in% region_filter)
  
  ggplot(data_region, aes(x = month, y = PM25_mean, group = SiteName, color = SiteName)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste("Monthly PM2.5 Trends per Station:", region_label),
      x = "Month",
      y = "PM2.5 Concentration (µg/m³)",
      color = "Station"
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", size = 16)
    )
}

# ==== Load NYC borough shapefile (for background map) ====
nyc_boroughs <- counties(state = "NY", cb = TRUE, class = "sf") %>%
  filter(NAME %in% c("New York", "Kings", "Bronx", "Queens", "Richmond"))

# ==== Create spatial point data for station locations ====
site_points <- monthly_station %>%
  distinct(SiteID, SiteName, Latitude, Longitude, Region) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# ==== Plot function: station locations on NYC map ====
plot_station_map <- function(region_filter, region_label) {
  site_data <- site_points %>% filter(Region %in% region_filter)
  
  ggplot() +
    geom_sf(data = nyc_boroughs, fill = "gray95", color = "black") +
    geom_sf(data = site_data, aes(color = SiteName), size = 3) +
    geom_text_repel(data = site_data, aes(geometry = geometry, label = SiteName),
                    stat = "sf_coordinates", size = 3, max.overlaps = 20) +
    labs(title = paste("Monitoring Station Locations:", region_label)) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 16)
    )
}

# ==== Generate plots ====
plot_station_trends("CBD", "CBD Area")
plot_station_trends("Outside CBD", "Outside CBD Area")
plot_station_trends(c("CBD", "Outside CBD"), "All NYC")

plot_station_map("CBD", "CBD Area")
plot_station_map("Outside CBD", "Outside CBD Area")
plot_station_map(c("CBD", "Outside CBD"), "All NYC")

# ==== Save output figures ====
ggsave("PM25_Trends_CBD.png", plot_station_trends("CBD", "CBD Area"), width = 8, height = 6)
ggsave("Station_Map_NYC.png", plot_station_map(c("CBD", "Outside CBD"), "All NYC"), width = 8, height = 6)

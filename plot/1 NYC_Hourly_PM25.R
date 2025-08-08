library(readr)
library(dplyr)

# Create a grid of all year-month combinations from Jan 2019 to May 2025
years <- 2019:2025
months <- 1:12
date_grid <- expand.grid(year = years, month = months) %>%
  arrange(year, month) %>%
  filter(!(year == 2025 & month > 6))  # Only include up to May 2025

# Initialize an empty data frame to store all monthly data
all_data <- data.frame()

# Loop through each year-month combination and download the corresponding CSV
for (i in 1:nrow(date_grid)) {
  yr <- date_grid$year[i]
  mo <- date_grid$month[i]
  
  # Construct the download URL
  url <- paste0("https://azdohv2staticweb.blob.core.windows.net/$web/hist/csv/",
                yr, "/", mo, "/hourlyMonitoring.csv")
  
  message("Reading: ", url)
  
  # Attempt to read the CSV file
  tryCatch({
    temp_data <- read_csv(url, show_col_types = FALSE)
    
    # Add year and month columns for reference
    temp_data$year <- yr
    temp_data$month <- mo
    
    # Combine with previous data
    all_data <- bind_rows(all_data, temp_data)
  }, error = function(e) {
    warning("Failed to read: ", url)
  })
}

# Save the full dataset as .rds 
saveRDS(all_data, "NYC_Hourly_Monitoring_2019_2025.rds")

# Also save it as .csv for general use
write_csv(all_data, "NYC_Hourly_Monitoring_2019_2025.csv")



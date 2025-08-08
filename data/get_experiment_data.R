# get_experiment_data.R

# A giant function to acquire all the data we need for the synthetic control experiment, comparing the effects of the tolling experiment on PM2.5 (or other pollutants) in city A vs. cities B,C,D,...Z.

get_experiment_data <- function(city_name, 
                               pollutant = "PM25", 
                               overwrite = FALSE, 
                               output_file = NULL,
                               start_date = "2024-01-01",
                               end_date = "2025-06-01") {
  # ---- Setup ----
    if (!requireNamespace("dplyr", quietly = TRUE)) stop("❌ dplyr required")
    if (!requireNamespace("readr", quietly = TRUE)) stop("❌ readr required")
    if (!requireNamespace("sf", quietly = TRUE)) stop("❌ sf required")
    if (!requireNamespace("lubridate", quietly = TRUE)) stop("❌ lubridate required")
    if (!requireNamespace("stringr", quietly = TRUE)) stop("❌ stringr required")
    if (!requireNamespace("here", quietly = TRUE)) stop("❌ here required")    
  
  # Load packages
  library(dplyr)
  library(readr)
  library(sf)
  library(lubridate)
  library(stringr)
  library(here)
  
  source("data/functions.R")
  readRenviron("secret/.env")
  
  # ---- Output file logic ----
  if (is.null(output_file)) {
    safe_city <- stringr::str_replace_all(tolower(city_name), "[^a-z0-9]", "_")
    output_file <- file.path("data", paste0("panel_", safe_city, ".csv"))
  }
  cat("\n📁 Output file will be:", output_file, "\n")
  
  # ---- 1. AQI Data ----
  cat("\n🌬️  Checking AQI data for", city_name, "...\n")
  safe_city <- stringr::str_replace_all(tolower(city_name), "[^a-z0-9]", "_")
  aqi_file <- file.path("data", paste0("aqi_", safe_city, ".csv"))
  sites_file <- file.path("data", paste0("sites_", safe_city, ".rds"))

  # Always collect AQI data (since get_aqdata_many appends and new sensors might appear)
  if (overwrite || !file.exists(aqi_file)) {
    cat("🔍 Collecting AQI data for", city_name, "with pollutant:", pollutant, "...\n")
    # Get bounding box for city (assume get_bbox exists in functions.R)
    bbox <- get_bbox(city = city_name, year = 2022)
    # Use customizable date range
    start <- start_date
    end <- end_date
    # Call get_aqdata_many (assume this function exists and works as in aqi_collect_anywhere.R)
    get_aqdata_many(
      start = start, end = end, ndays = 20,
      parameters = pollutant,
      path = aqi_file, bbox = bbox, verbose = 1,
      overwrite = FALSE, datatype = "C"  # Don't overwrite, append
    )
    cat("✅ AQI data collection complete.\n")
  } else {
    cat("⏭️  AQI data file exists for", city_name, "- skipping collection.\n")
  }

  # Clean AQI file and update sites (always do this after any AQI collection)
  if (file.exists(aqi_file)) {
    aqi_data <- readr::read_csv(aqi_file, show_col_types = FALSE)
    # Try to select relevant columns, fallback if names differ
    keep_cols <- intersect(c("aqs_id_full", "datetime", "param", "concentration", "unit", "lat", "lon", "site_name", "site_agency"), names(aqi_data))
    aqi_data <- aqi_data[, keep_cols]
    # Save only the core AQI columns to CSV
    aqi_data %>% select(aqs_id_full, datetime, param, concentration, unit) %>% distinct() %>%
      readr::write_csv(aqi_file)
    cat("🧹 Cleaned AQI data written to", aqi_file, "\n")
    # Always update sites file (in case new sensors appeared)
    if (all(c("aqs_id_full", "lat", "lon") %in% names(aqi_data))) {
      sites <- aqi_data %>% select(aqs_id_full, lat, lon, site_name, site_agency) %>% distinct()
      saveRDS(sites, sites_file)
      cat("📍 Updated AQI sensor sites written to", sites_file, "\n")
    } else {
      cat("⚠️  Could not extract site info (lat/lon missing) from AQI data.\n")
    }
  } else {
    cat("❌ AQI data file not found after attempted collection!\n")
  }
  
  # ---- 2. Weather Data ----
  cat("\n🌤️  Checking weather data for", city_name, "...\n")
  # TODO: Check if weather file exists and is up-to-date. If not, call get_weather_many() for AQI sensor locations.
  # (Refer to weather_collect_anywhere.R)
  
  # ---- 3. Census Data ----
  cat("\n👥 Checking census data ...\n")
  # TODO: Check if census file exists and is up-to-date. If not, call get_census_many().
  # (Refer to census_collect.R)
  
  # ---- 4. Crosswalk (Sensor-to-Census) ----
  cat("\n🔗 Checking sensor-to-census crosswalk ...\n")
  # TODO: Check if crosswalk file exists for this city (1km buffer). If not, compute using logic from census_combine.R.
  
  # ---- 5. Data Integration ----
  cat("\n🔧 Integrating data for", city_name, "...\n")
  # TODO: Use logic from descriptives/01_dataset.R to join AQI, weather, census, and crosswalk data for this city.
  # Write the final panel dataset to output_file.
  
  cat("\n🎉 Data acquisition and integration complete for", city_name, "!\n")
  cat("📊 Output written to:", output_file, "\n")
  
  invisible(output_file)
}

# Example usage:
# get_experiment_data("San Francisco", pollutant = "PM25")



# GENERAL #########################################


# DATES ##########################################

#' @name as_datetime_utc
#' @title Convert from Date-Time to UTC Date-Time string
#' @author Tim Fraser
#' @description Converts a datetime vector from EDT timezone to UTC, and formats it as a string like "2025-06-02T16".
#' @param x [dttm]: Datetime vector of the format "2025-06-02 12:53:32 EDT"
#' @return A character vector formatted in UTC format like "2025-06-02T16"
#' @importFrom lubridate ymd_hms with_tz
#' @importFrom dplyr %>%
#' @export
as_datetime_utc = function(x){
  x %>%
    lubridate::ymd_hms(tz = "EDT") %>%
    lubridate::with_tz("UTC") %>%
    format("%Y-%m-%dT%H")
}

#' @name as_datetime_normal
#' @title Convert UTC datetime strings to fixed EDT (UTC-4) time
#' @author Tim Fraser
#' @description Takes UTC-formatted datetime strings like "2025-06-02T16" and converts them to EDT time, with fixed offset (UTC-4).
#' @param x [str]: Vector like "2025-06-02T16", representing UTC datetime hour
#' @return A character vector like "2025-06-02 12:00:00 EDT"
#' @importFrom lubridate ymd_hms force_tz
#' @importFrom stringr str_split
#' @export
as_datetime_normal = function(x) {
  values <- stringr::str_split(x, pattern = "T", simplify = TRUE)
  utc_string <- paste0(values[, 1], " ", sprintf("%02d:00:00", as.integer(values[, 2])))
  datetime_utc <- lubridate::ymd_hms(utc_string, tz = "UTC")
  datetime_edt <- lubridate::force_tz(datetime_utc, tzone = "Etc/GMT+4")
  format(datetime_edt, "%Y-%m-%d %H:%M:%S EDT")
}

#' @name get_two_weeks_prior
#' @title Get datetime 14 days prior to a UTC string
#' @description Given a UTC datetime string (e.g., "2025-06-01T00"), this returns a datetime object representing 14 days prior, in UTC.
#' @author Tim Fraser
#' @param x [str]: A UTC datetime string in the format "YYYY-MM-DDTHH"
#' @return A POSIXct datetime object 14 days before input
#' @importFrom lubridate as_datetime
#' @importFrom dplyr %>%
#' @export
get_two_weeks_prior = function(x = "2025-06-01T00"){
  x %>%
    as_datetime_normal() %>%
    lubridate::as_datetime() %>%
    {. - 24*60*60*14}
}

#' @name seq_date
#' @title Generate a sequence of dates with fixed interval, ensuring inclusion of end date
#' @description Produces a sequence of dates every `ndays` apart, ensuring the `end` date is always included, even if it doesn't fall on the interval.
#' @param start [str]: Start date in "YYYY-MM-DD" format
#' @param end [str]: End date in "YYYY-MM-DD" format
#' @param ndays [int]: Number of days between each date
#' @return A Date vector
#' @importFrom lubridate ymd
#' @export
seq_date = function(start = "2024-01-01", end = "2025-06-01", ndays = 17){
  date_seq <- seq(from = lubridate::ymd(start), to = lubridate::ymd(end), by = paste0(ndays, " days"))
  if (tail(date_seq, 1) != lubridate::ymd(end)) {
    date_seq <- c(date_seq, lubridate::ymd(end))
  }
  return(date_seq)
}

#' @name seq_datetime
#' @title Generate datetime strings from date sequence
#' @author Tim Fraser
#' @description Creates a sequence of datetime strings (hour 00) from a start and end date, spaced `ndays` apart, and ensures end date is included.
#' @param start [str]: Start date in "YYYY-MM-DD" format
#' @param end [str]: End date in "YYYY-MM-DD" format
#' @param ndays [int]: Number of days between each datetime
#' @return A character vector like "2024-01-01 00:00:00 EDT"
#' @importFrom dplyr %>%
#' @export
seq_datetime = function(start = "2024-01-01", end = "2025-06-01", ndays = 17){
  seq_date(start = start, end = end, ndays = ndays) %>%
    paste0("T00") %>%
    as_datetime_normal()
}

progress_meter <- function(current, total, width = 50, elapsed = NULL) {
  pct <- current / total
  done <- round(pct * width)
  bar <- paste0(
    "\r[", 
    paste0(rep("=", done), collapse = ""), 
    paste0(rep(" ", width - done), collapse = ""), 
    "] ", sprintf("%3.0f%%", pct * 100), " | ", current, " / ", total 
  )
  # If a valid elapsed time is shared,
  if(!is.null(elapsed)){ bar = paste0(bar, " | ", elapsed) }
  cat(bar)
  flush.console()
  if (current == total) cat("\n")  # move to next line at end
}


# AIRNOW API #########################



#' Retrieve Air Quality Data from AirNow API
#'
#' Downloads air quality data (AQI and/or concentration) from the U.S. EPA AirNow API for a specified time period and bounding box.
#'
#' Main Parameters
#' @param bdate Start date-time in character or POSIX format. If `NULL`, defaults to 24 hours prior to `edate`.
#' @param edate End date-time in character or POSIX format. Default is `"2025-06-02 12:53:32 EDT"`.
#' @param bbox A vector of bounding box coordinates in the form `c(west, south, east, north)` or a previously saved RDS object. Default reads from `"data/bbox_nums.rds"`.
#' @param parameters Pollutant(s) to retrieve. Default is `"PM25"`. Can be a single pollutant code or a comma-separated string of multiple codes.
#' @param datatype Type of data requested: `"A"` = AQI only, `"B"` = AQI + Concentrations, `"C"` = Concentrations only. Default is `"B"`.
#'
#' @return A tibble of air quality measurements if successful, or `NULL` with a warning if the API request fails.
#'
#' @details
#' Requires an API key stored in the environment variable `AIRNOWKEY`. Dates are automatically converted to ISO 8601 UTC format (`YYYY-MM-DDTHH`).
#'
#' The API request URL is patched to avoid improper URL encoding for slashes and commas, which the AirNow API does not tolerate.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(AIRNOWKEY = "your_api_key_here")
#' data <- get_aqdata()
#' }
#'
#' @importFrom dplyr `%>%` mutate select
#' @importFrom httr2 request req_method req_url_path req_headers req_url_query req_perform resp_body_string
#' @importFrom readr read_csv
#' @importFrom stringr str_replace_all str_remove
#' @importFrom lubridate as_datetime force_tz
#' @export
get_aqdata = function(bdate = NULL, edate = "2025-06-02 12:53:32 EDT", bbox = read_rds("data/bbox_nums.rds"),
                      parameters = "PM25", datatype = "B",verbose=0,
                      path = "data/aqi.csv"){
  # Testing values
  # bdate = NULL; edate = NULL; bbox = read_rds("data/bbox_nums.rds"); parameters = c("PM25", "SO2"); datatype = "B";
  # bbox = read_rds("data/counties.rds") %>% filter(geoid == "36061") %>% st_bbox()
  
  # Stop if these values are not available  
  if(nchar(Sys.getenv("AIRNOWKEY")) == 0){ stop("environmental variable AIRNOWKEY missing...")}
  
  # Stop if invalid parameters.
  stopifnot(((is.character(edate) | lubridate::is.timepoint(edate))  & length(edate) == 1) | is.null(edate))
  stopifnot(((is.character(bdate) | lubridate::is.timepoint(bdate)) & length(bdate) == 1) | is.null(bdate))
  stopifnot(is.numeric(bbox) & length(bbox) == 4)
  stopifnot(tolower(parameters) %in% c("O3", "pm25", "pm10", "co", "no2", "so2") & length(parameters) <= 6)
  stopifnot(datatype %in% c("A", "B", "C") & length(datatype) == 1)
  
  
  # If no edate is supplied, use current edate
  if(is.null(edate)){ edate = lubridate::now(tzone = "America/New_York") }
  # If no bdate supplied, grab 24 hours prior to the current period.
  if(is.null(bdate)){ bdate = lubridate::as_datetime(edate) - 60*60*24 }
  
  
  # Format dates
  endDate = as_datetime_utc(edate)
  startDate = as_datetime_utc(bdate)
  
  # Format bounding box
  BBOX = bbox %>% round(6) %>% paste0(., collapse = ",")
  
  # Static settings
  prior_file = file.exists(path)
  format="text/csv"
  monitorType = 0; raw = 0
  
  stopifnot(verbose %in% c(0, 1) & length(verbose) == 1)
  stopifnot(monitorType %in% c(0,1) & length(monitorType) == 1)
  stopifnot(raw %in% c(0,1) & length(raw) == 1)
  
  # packages
  require(dplyr, quietly = TRUE,warn.conflicts = FALSE)
  require(httr2, quietly = TRUE,warn.conflicts = FALSE)
  require(stringr, quietly = TRUE,warn.conflicts = FALSE)
  require(readr, quietly = TRUE,warn.conflicts = FALSE)
  
  
  # Example query
  # https://www.airnowapi.org/aq/data/?startDate=2025-06-02T16&endDate=2025-06-02T17&parameters=PM25&BBOX=-124.205070,28.716781,-75.337882,45.419415&dataType=A&format=text/csv&verbose=0&monitorType=0&includerawconcentrations=0&API_KEY=XXXX
  
  req = request("https://www.airnowapi.org") %>%
    req_method("GET") %>%
    req_url_path("aq/data/")  %>%
    req_headers("Accept" = "application/json") %>%
    req_url_query(format=format, verbose = verbose) %>%
    req_url_query(BBOX = BBOX) %>%
    req_url_query(startDate= startDate, endDate = endDate) %>%
    req_url_query(parameters = parameters, .multi = "comma") %>%
    req_url_query(datatype = datatype, monitorType = monitorType, includerawconcentrations=raw) |>
    req_url_query(API_KEY = Sys.getenv("AIRNOWKEY"))
  
  # patch fix the url into the form airnow will understand
  req$url = req$url %>% stringr::str_replace_all(pattern = c("%2F" = "/", "%2C" = ","))
  # Perform request
  
  # resp = req %>% req_perform()
  tryCatch({
    resp = req %>% req_perform()
  }, error = function(e){
    clean_url <- stringr::str_replace(req$url, "API_KEY=[^&]+", "API_KEY=[HIDDEN]")
    stop("Request failed: ", conditionMessage(e), "\nURL: ", clean_url)
  })
  
  # Construct the column names
  cols_core = c("lat", "lon", "datetime", "param")
  if(datatype == "A"){ cols_data = c("aqi", "category") }
  if(datatype == "B" & raw == 0){ cols_data = c("concentration", "unit", "aqi", "category")}
  if(datatype == "B" & raw == 1){ cols_data = c("concentration", "unit", "raw",  "aqi", "category")}
  if(datatype == "C" & raw == 0){ cols_data = c("concentration", "unit")}
  if(datatype == "C" & raw == 1){ cols_data = c("concentration", "unit", "raw")}
  if(verbose == 1){ cols_verbose = c("site_name",  "site_agency", "aqs_id", "aqs_id_full") }else{ cols_verbose = c()}
  cols = c(cols_core, cols_data, cols_verbose)
  
  if(prior_file == FALSE){
    tibble(
      lat = NA_real_,
      lon = NA_real_,
      datetime = NA_POSIXct_,
      param = NA_character_,
      concentration = NA_real_,
      unit = NA_character_,
      raw = NA_real_,
      aqi = NA_real_,
      category = NA_real_,
      site_name = NA_character_,
      site_agency = NA_character_,
      aqs_id = NA_character_,
      aqs_id_full = NA_character_
    ) %>% slice(0) %>%
      select(any_of(cols)) %>%
      write_csv(path, append = FALSE)
  }
  
  # If successful...
  if(resp$status_code == 200){
    # Extract the csv result
    data = resp %>% resp_body_string() %>% read_csv(show_col_types = FALSE, col_names = cols)
    
    # Write the data to file
    write_csv(x = data, file = path, append = TRUE)    
    
    # return it
    return(path)
    
  }else{
    warning(paste0("invalid query... ", req$url %>% stringr::str_remove("&API_KEY.*"), paste0("&AQI_KEY=[**HIDDEN**]")))
    return(NULL)
  }
}  


get_aqdata_many = function(start = "2024-01-01", end = "2025-06-01", ndays = 17, parameters = "PM25", datatype = "C",verbose = 1, path = "data/aqi.csv", bbox = read_rds("data/bbox.rds"), overwrite = FALSE){
  
  # Create a log path
  path_log = path %>% 
    stringr::str_remove("[.]csv") %>%
    paste0("_log.csv")  
  
  # If overwrite, remove the path and path log
  if(overwrite == TRUE){
    unlink(path)
    unlink(path_log)
  }
  
  # Make a log file if it doesn't exist.
  if(!file.exists(path_log)){
    tibble(id = NA_integer_, success = NA_real_, parameters = NA_character_, start = NA_character_, end = NA_character_) %>% 
      slice(0) %>% write_csv(path_log)   
  }
  
  # Get date ranges
  dates = seq_datetime(start = start, end = end, ndays = ndays)
  
  grid = tibble(
    start = dates,
    end = lead(dates, 1),
  ) %>%
    # Drop the last row
    slice(-n()) %>%
    tidyr::expand_grid(
      parameters = parameters
    ) %>%
    mutate(id = 1:n()) %>%
    mutate(success = NA_real_) %>%
    select(id, success, parameters, start, end)
  
  
  # Get number of API calls
  n = nrow(grid)
  
  progress_meter(current = 0, total = n, width = 50)
  
  for(i in  1:n){
    Sys.sleep(0.1)
    
    # Get AQI for a 2 week period
    result = purrr::possibly(
      .f = ~get_aqdata(
        bdate = grid$start[i],
        edate = grid$end[i],
        bbox = bbox,
        parameters = grid$parameters[i], 
        datatype = datatype, 
        verbose = verbose,
        path = path),
      otherwise = NULL)()
    
    # Log outcome
    if(!is.null(result)){
      grid[i, ] %>% mutate(success = 1) %>% write_csv(path_log, append = TRUE)
    }else{ grid[i, ] %>% mutate(success = 0) %>% write_csv(path_log, append = TRUE)}
    
    # Update progress meter
    progress_meter(current = i, total = n, width = 50)
    
  }
  
  cat("\n--- API calls complete!\n")
  return(path)
  
}


# VISUAL CROSSING API ########################################


#' @name get_weather
#' @title Download hourly weather data from Visual Crossing API
#' @author Tim Fraser
#' 
#' @description Retrieves weather data for a given site and date range from the Visual Crossing API and appends it to a local CSV file.
#' 
#' @param id [int] Unique site identifier (e.g., AQS site ID)
#' @param lat [num] Latitude of the site
#' @param lon [num] Longitude of the site
#' @param start [str] Start date in "YYYY-MM-DD" format
#' @param end [str] End date in "YYYY-MM-DD" format
#' @param path [str] File path to store weather data (default: `"data/weather.csv"`)
#' 
#' @return Logical value: `TRUE` if the data was successfully retrieved and saved, `FALSE` otherwise
#' 
#' @importFrom dplyr `%>%` mutate select any_of slice
#' @importFrom httr2 request req_headers req_perform resp_body_string
#' @importFrom readr read_csv write_csv
#' @importFrom tibble tibble
#' 
#' @export
get_weather = function(id, lat, lon, start, end, path = "data/weather.csv", maxdist = 16094){
  # testing values
  # id = points$aqs_id_full[i]
  # lat = points$lat[i]; lon = points$lon[i]
  # start = dates$start[j]; end = dates$end[j]
  # start = "2025-05-29"; end = "2025-05-29"
  
  # path = "data/weather.csv"
  if(nchar(Sys.getenv("VISUALCROSSINGKEY")) == 0){ stop("missing environmental variable VISUALCROSSINGKEY...")}
  
  cols = c("aqs_id_full", "datetime", "temp", "dew", "humidity", "precip", "windspeed", "winddir", "cloudcover", "solarradiation", "uvindex")
  
  if(!file.exists(path)){
    tibble(
      aqs_id_full = NA_integer_,
      datetime = NA_POSIXct_,
      temp = NA_real_,
      dew = NA_real_,
      humidity = NA_real_,
      precip = NA_real_,
      windspeed = NA_real_,
      winddir = NA_real_,
      cloudcover = NA_real_,
      solarradiation = NA_real_,
      uvindex = NA_real_
    ) %>%
      select(any_of(cols)) %>%
      slice(0) %>%
      write_csv(path, append = FALSE)
    
  }
  
  # Check your usage
  # https://www.visualcrossing.com/usage/
  
  url = paste0(
    # base
    "https://weather.visualcrossing.com/",
    # endpoint
    "VisualCrossingWebServices/rest/services/timeline/",
    # LOCATION
    lat, "%2C", lon, "/",
    # DATE1
    start,
    "/",
    # DATE2
    end,
    # query parameters
    "?unitGroup=us&maxDistance=", maxdist, "&elements=datetime%2Ctemp%2Cdew%2Chumidity%2Cprecip%2Cwindspeed%2Cwinddir%2Ccloudcover%2Csolarradiation%2Cuvindex&include=hours%2Cobs&key=TEWR95SSN4HVNWGJWSWHTU5AQ&options=nonulls&contentType=csv"
  )
  
  resp = request(url) |>
    req_headers("Accept" = "text/csv") |>
    req_perform()
  
  # resp = httr::GET(url)
  # resp$content %>% rawToChar()
  
  if(resp$status_code == 200){
    content = resp %>% resp_body_string() %>% read_csv(show_col_types = FALSE) %>%
      mutate(aqs_id_full = id) %>%
      select(any_of(cols))
    if(nrow(content) > 0){
      write_csv(x = content, file = path, append = TRUE)
    }
    return(TRUE)
  }else{
    return(FALSE)
  }
  
}


#' @name get_weather_many
#' @title Batch download of hourly weather data
#' @author Tim Fraser
#' 
#' @description Downloads and logs weather data for multiple locations and date ranges using the Visual Crossing API. Progress is shown during execution.
#' 
#' @param grid [data.frame] A data frame with columns: `id`, `aqs_id_full`, `lat`, `lon`, `start`, and `end`
#' @param path [str] File path to store weather data (default: `"data/weather.csv"`)
#' @param overwrite [logical] If `TRUE`, clears existing data and logs before downloading (default: `FALSE`)
#' 
#' @return The path where the weather data was saved
#' 
#' @importFrom dplyr mutate select tibble
#' @importFrom purrr possibly
#' @importFrom readr write_csv
#' @importFrom stringr str_remove
#' 
#' @export
get_weather_many = function(grid, path = "data/weather.csv", overwrite = FALSE, maxdist = 16094){
  # Testing values
  # start = "2025-05-29"; end = "2025-05-29"; path = "data/weather.csv"; overwrite = FALSE
  
  # Create a log path
  path_log = path %>% 
    stringr::str_remove("[.]csv") %>%
    paste0("_log.csv")  
  
  # If overwrite, remove the path and path log
  if(overwrite == TRUE){
    unlink(path)
    unlink(path_log)
  }
  
  # Make a log file if it doesn't exist.
  if(!file.exists(path_log)){
    tibble(id = NA_integer_, success = NA_real_, aqs_id_full = NA_real_, start = NA_character_, end = NA_character_) %>% 
      slice(0) %>% write_csv(path_log, append = FALSE)   
  }
  
  # Get total length of grid
  n = nrow(grid)
  
  if(n == 0){ stop("`grid` has 0 rows. Stopping...")}
  
  # Start progress meter
  progress_meter(current = 0, total = n, width = 50)
  
  # For each site-daterange pair...
  for(i in 1:n){
    # Testing values
    # i = 1
    # id = grid$aqs_id_full[i]; lat = grid$lat[i]; lon = grid$lon[i]; start = grid$start[i]; end = grid$end[i]
    Sys.sleep(0.01)
    
    # Collect weather data...
    valid = purrr::possibly(.f = ~get_weather(
      id = grid$aqs_id_full[i], lat = grid$lat[i], lon = grid$lon[i],
      start = grid$start[i], end = grid$end[i], path = path, maxdist = maxdist), 
      otherwise = FALSE, quiet = FALSE)()
    
    # Log outcome
    if(valid){
      grid[i, ] %>% mutate(success = 1) %>% select(id, success, aqs_id_full, start, end) %>% write_csv(path_log, append = TRUE)
    }else{ grid[i, ] %>% mutate(success = 0) %>% select(id, success, aqs_id_full, start, end) %>% write_csv(path_log, append = TRUE)}
    
    # Update progress meter
    progress_meter(current = i, total = n, width = 50)
  }
  
  cat("\n--- API calls complete!\n")
  return(path)
  
}


# CENSUS ACS5 #####################################

#' @name get_census
#' @title Retrieve Census Data
#' @description
#' Function to retrieve census data for all census block groups / geographies for one state in a given year.
#' @param geoid:str a full county FIPS code (eg. for Tompkins County, New York, `"36109"`)
#' @param year:int year of ACS-5 census data to return. For example, 2022 returns the 5-year average of 2020, 2021, 2022, 2023, and 2024.
#' @param region:str code to pass to `censusapi` to tell what type of geography to return. Eg. `"tract:*"` returns every census tract.
#' @param key:str census API key. By default, passed as environmental variable `CENSUS_API_KEY`. Can skip if already loaded as environmental variable.
#' @export
get_census = function(geoid, year, region = "block_group:*", key = Sys.getenv("CENSUS_API_KEY")){
  library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
  library(tidyr, warn.conflicts = FALSE, quietly = TRUE)
  library(readr, warn.conflicts = FALSE, quietly = TRUE)
  library(stringr, warn.conflicts = FALSE, quietly = TRUE)
  library(censusapi, warn.conflicts = FALSE, quietly = TRUE)
  
  # Extract fips codes from geoid  
  state_fips = stringr::str_sub(geoid, 1,2)
  county_fips = stringr::str_sub(geoid, 3,5)
  .geoid = geoid
  
  # testint values
  # state_fips = "36"; year = 2022; region = "tract:*"; key = Sys.getenv("CENSUS_API_KEY")
  
  # Define variables for census data retrieval
  general_vars <- c(
    "geoid" = "GEO_ID",
    
    total_population = "B01003_001",  
    
    Not_Hispanic_or_Latino_Population= "B03002_002",
    Hispanic_or_Latino_Population = "B03002_012",
    
    population_by_race_White = "B02001_002", 
    population_by_race_Black_or_African_American = "B02001_003",
    population_by_race_American_Indian_and_Alaska_Native = "B02001_004",
    population_by_race_Asian = "B02001_005",
    population_by_race_Native_Hawaiian_and_Other_Pacific_Islander = "B02001_006",
    
    median_income = "B19013_001",       # Median household income
    
    population_by_gender_male = "B01001_002", # Male population (use B01001_026 for Female)
    population_by_gender_female= "B01001_026"
  )
  
  race_vars = c(
    White_alone_Not_Hispanic_or_Latino_Population = "B03002_003",
    Black_or_African_American_Not_Hispanic_or_Latino_Population = "B03002_004",
    American_Indian_and_Alaska_Not_Native_Hispanic_or_Latino_Population = "B03002_005",
    Asian_Not_Hispanic_or_Latino_Population = "B03002_006",
    Native_Hawaiian_and_Other_Pacific_Islander_Not_Latino_Population = "B03002_007",
    
    
    Asian_Hispanic_or_Latino_Population = "B03002_016",
    White_alone_Hispanic_or_Latino_Population = "B03002_013",
    Black_or_African_American_Hispanic_or_Latino_Population = "B03002_014",
    American_Indian_and_Alaska_Native_Hispanic_or_Latino_Population = "B03002_015",
    Native_Hawaiian_and_Other_Pacific_Islander_Latino_Population = "B03002_017"
  )
  
  
  # Define ACS Income Variables (B19001 - Household Income)
  income_vars <- c(
    # $0 - $24,999 (B19001_002 to B19001_005)
    "B19001_002", "B19001_003", "B19001_004", "B19001_005",
    
    # $25,000 - $49,999 (B19001_006 to B19001_010)
    "B19001_006", "B19001_007", "B19001_008", "B19001_009", "B19001_010",
    
    # $50,000 - $74,999 (B19001_011 to B19001_012)
    "B19001_011", "B19001_012",
    
    # $75,000 - $99,999 (B19001_013)
    "B19001_013",
    
    # $100,000 - $124,999 (B19001_014)
    "B19001_014",
    
    # $125,000 - $149,999 (B19001_015)
    "B19001_015",
    
    # $150,000 - $199,999 (B19001_016)
    "B19001_016",
    
    # $200,000 or more (B19001_017)
    "B19001_017"
  )
  
  
  # Define ACS Variables for New Age Groups
  age_vars <- c(
    # Under 18 (Male: 003-006, Female: 027-030)
    "B01001_003", "B01001_004", "B01001_005", "B01001_006",
    "B01001_027", "B01001_028", "B01001_029", "B01001_030",
    
    # 18-34 (Male: 007-012, Female: 031-036)
    "B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011", "B01001_012",
    "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", "B01001_036",
    
    # 35-64 (Male: 013-019, Female: 037-043)
    "B01001_013", "B01001_014", "B01001_015", "B01001_016", "B01001_017", "B01001_018", "B01001_019",
    "B01001_037", "B01001_038", "B01001_039", "B01001_040", "B01001_041", "B01001_042", "B01001_043",
    
    # 65 and over (Male: 020-025, Female: 044-049)
    "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
    "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049"
  )
  
  edu_vars = c(
    # edu_less_than_hs 
    "B15003_002", "B15003_003", "B15003_004", "B15003_005",
    "B15003_006", "B15003_007", "B15003_008", "B15003_009",
    "B15003_010", "B15003_011", "B15003_012", "B15003_013",
    "B15003_014", "B15003_015", "B15003_016",
    
    # edu_hs_grad
    "B15003_017", "B15003_018",
    
    # edu_some_college_or_more 
    "B15003_019", "B15003_020", "B15003_021",
    
    # edu_postgrad 
    "B15003_022", "B15003_023", "B15003_024", "B15003_025"
  )
  
  # activity_vars = c(
  #   "B08301_001E",# : Total workers 16 years and over
  #   
  #   "B08301_002E", # : Car, truck, or van — drove alone
  #   
  #   "B08301_003E",#: Carpool
  #   
  #   "B08301_010E",#: Public transportation (excluding taxicab)
  #   
  #   "B08301_016E",#: Bicycle
  #   
  #   "B08301_017E",#: Walked
  #   
  #   "B08301_018E",#: Taxicab, motorcycle, or other
  #   
  #   "B08301_019E",#: Worked at home
  # )
  # 
  
  myvars = c(general_vars, race_vars, income_vars, age_vars, edu_vars)
  myvars = tibble(id = myvars, variable = names(myvars))
  myvars = myvars %>%
    mutate(idE = if_else(id == "GEO_ID", true = id, false = paste0(id, "E")))
  
  download = getCensus(name = "acs/acs5", vintage = year, vars = myvars$idE,
                       key = key,
                       region = region,
                       regionin = paste0("county: ", county_fips, " state:", state_fips))
  
  data = download %>%
    # Keep just the rows with our specified names
    select(myvars$idE) %>%
    # Rename them to simpler, more easily understandable names
    setNames(nm = c(myvars$id)) %>%
    # better format the geoid
    rename(geoid = GEO_ID) %>%
    mutate(geoid = str_remove(geoid, ".*US"))
  
  # Group and Summarize Household Income Data
  income_summary = data %>%
    select(any_of(c("geoid", income_vars))) %>%
    pivot_longer(cols = any_of(income_vars), names_to = "variable", values_to = "estimate") %>%
    group_by(geoid) %>%
    summarise(
      income_0_24999 = sum(estimate[variable %in% c("B19001_002", "B19001_003", "B19001_004", "B19001_005")]),
      income_25000_49999 = sum(estimate[variable %in% c("B19001_006", "B19001_007", "B19001_008", "B19001_009", "B19001_010")]),
      income_50000_74999 = sum(estimate[variable %in% c("B19001_011", "B19001_012")]),
      income_75000_99999 = sum(estimate[variable %in% c("B19001_013")]),
      income_100000_124999 = sum(estimate[variable %in% c("B19001_014")]),
      income_125000_149999 = sum(estimate[variable %in% c("B19001_015")]),
      income_150000_199999 = sum(estimate[variable %in% c("B19001_016")]),
      income_200000_or_more = sum(estimate[variable %in% c("B19001_017")]),
      .groups = "drop"
    )
  
  # Group and Summarize Age Data
  age_summary = data %>%
    select(any_of(c("geoid", age_vars))) %>%
    pivot_longer(cols = any_of(age_vars), names_to = "variable", values_to = "estimate") %>%
    group_by(geoid)  %>%
    summarise(
      age_under_18 = sum(estimate[variable %in% c(
        "B01001_003", "B01001_004", "B01001_005", "B01001_006",
        "B01001_027", "B01001_028", "B01001_029", "B01001_030")]),
      
      age_18_34 = sum(estimate[variable %in% c(
        "B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_011", "B01001_012",
        "B01001_031", "B01001_032", "B01001_033", "B01001_034", "B01001_035", "B01001_036")]),
      
      age_35_64 = sum(estimate[variable %in% c(
        "B01001_013", "B01001_014", "B01001_015", "B01001_016", "B01001_017", "B01001_018", "B01001_019",
        "B01001_037", "B01001_038", "B01001_039", "B01001_040", "B01001_041", "B01001_042", "B01001_043")]),
      
      age_65_and_over = sum(estimate[variable %in% c(
        "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
        "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049")])
    )
  
  edu_summary = data %>%
    select(any_of(c("geoid", edu_vars))) %>%
    pivot_longer(cols = any_of(edu_vars), names_to = "variable", values_to = "estimate") %>%
    group_by(geoid)  %>%
    summarize(
      edu_less_than_hs = sum(estimate[variable %in% c(
        "B15003_002", "B15003_003", "B15003_004", "B15003_005",
        "B15003_006", "B15003_007", "B15003_008", "B15003_009",
        "B15003_010", "B15003_011", "B15003_012", "B15003_013",
        "B15003_014", "B15003_015", "B15003_016")]),
      
      edu_hs_grad = sum(estimate[variable %in% c(
        "B15003_017", "B15003_018")]),
      
      edu_some_college = sum(estimate[variable %in% c(
        "B15003_019", "B15003_020", "B15003_021")]),
      
      edu_postgrad = sum(estimate[variable %in% c(
        "B15003_022", "B15003_023", "B15003_024", "B15003_025")])
    )
  
  
  race_summary = data %>%
    select(any_of(c("geoid", race_vars)))
  
  general_summary = data %>%
    select(any_of(c("geoid", general_vars)))
  
  output = data %>%
    select(geoid) %>%
    distinct() %>%
    left_join(by = "geoid", y = age_summary) %>%
    left_join(by = "geoid", y = income_summary) %>%
    left_join(by = "geoid", y = race_summary) %>%
    left_join(by = "geoid", y = edu_summary) %>%
    left_join(by = "geoid", y = general_summary) %>%
    # append the county and year
    mutate(county = .geoid )  %>%
    mutate(year = year)
  
  
  col_names = output %>% select(-geoid, -year, -county) %>% names()
  # Handle data errors
  output = output %>%
    mutate(across(.cols = any_of(col_names),
                  .fns = ~if_else(.x < 0, true = NA_real_, false = .x)))
  
  output = output %>%
    select(county, year, geoid, any_of(col_names))
  
  output = output %>%
    mutate(county = stringr::str_pad(county, width = 5, pad = "0", side = "left"),
           year = as.integer(year))
  return(output)
}


#' @name get_census_many
#' @title Batch download of census block group data
#' @description Downloads and logs ACS census data for multiple counties and years at the block group level.
#'
#' @param grid [data.frame] A data frame with columns: `id`, `geoid`, `year`
#' @param path [str] File path to store census data (default: "data/census.csv")
#' @param overwrite [logical] If TRUE, clears existing data and logs before downloading (default: FALSE)
#'
#' @return The path where the census data was saved
#'
#' @importFrom dplyr mutate select tibble slice bind_rows
#' @importFrom purrr possibly
#' @importFrom readr write_csv
#' @export
get_census_many <- function(grid, path = "data/census.csv", overwrite = FALSE) {
  
  # Create a log path
  path_log <- path %>%
    stringr::str_remove("[.]csv") %>%
    paste0("_log.csv")
  
  # Handle overwriting
  if (overwrite) {
    unlink(path)
    unlink(path_log)
  }
  
  # Create log file if it doesn't exist
  if(!file.exists(path_log)) {
    tibble(id = NA_integer_, success = NA_real_, geoid = NA_character_, year = NA_integer_) %>%
      slice(0) %>% write_csv(path_log, append = FALSE)
  }
  
  n <- nrow(grid)
  if (n == 0) stop("`grid` has 0 rows. Stopping...")
  
  progress_meter(current = 0, total = n, width = 50)
  
  for (i in 1:n) {
    Sys.sleep(0.01)
    
    result <- purrr::possibly(.f = ~get_census(
      geoid = grid$geoid[i],
      year = grid$year[i],
      region = "block group:*",
      key = Sys.getenv("CENSUS_API_KEY")
    ), otherwise = NULL, quiet = FALSE)()
    
    
    if (!is.null(result)) {
      readr::write_csv(x = result, file = path, append = file.exists(path))
      grid[i, ] %>%
        mutate(success = 1) %>%
        select(id, success, geoid, year) %>%
        write_csv(path_log, append = TRUE)
    } else {
      grid[i, ] %>%
        mutate(success = 0) %>%
        select(id, success, geoid, year) %>%
        write_csv(path_log, append = TRUE)
    }
    
    progress_meter(current = i, total = n, width = 50)
  }
  
  cat("\n--- Census API calls complete!\n")
  return(path)
}



# BOUNDING BOX MAKER #####################

#' @name get_geo 
#' @description
#' Get core geographic data for a city in a given year
#' 
#' @param city name of city, for searching Core Based Statistical Areas
#' @param year reference year
get_geo = function(city = "Boston", year = 2022){
  require(tigris, quietly = TRUE, warn.conflicts = FALSE)
  require(sf, quietly = TRUE, warn.conflicts = FALSE)
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(readr, quietly = TRUE, warn.conflicts = FALSE)
  require(stringr, quietly = TRUE, warn.conflicts = FALSE)
  # Testing values
  # city = "Boston"; year = 2022
  
  # Get the most recently available year for CBSA (decennial)
  year_cbsa = floor(year / 10)*10
  
  # Prepare a bounding box for a given set of county geoids
  cbsa = tigris::core_based_statistical_areas(year = year_cbsa, cb = TRUE, resolution = "20m") %>%
    st_as_sf() %>% st_transform(crs = 4326) %>%
    filter(stringr::str_detect(NAME, city))
  
  # Return all counties that lie within this CBSA
  counties = tigris::counties(cb = TRUE, year = year, resolution = "20m", filter_by = cbsa) %>%
    st_transform(crs = 4326) %>%
    select(geoid = GEOID, name = NAME, state = STUSPS, geometry)

  states = tigris::states(cb = TRUE, year = year, resolution = "20m", filter_by = cbsa) %>%
    st_transform(crs = 4326) %>%
    select(geoid = GEOID, name = NAME, state = STUSPS, geometry)
  
  # Create the bounding box
  bbox = cbsa %>% st_bbox() %>% st_as_sfc() %>% tibble(geometry = .) %>% st_as_sf(crs = 4326)
  
  output = list(
    cbsa = cbsa,
    counties = counties,
    states = states,
    bbox = bbox
  )
  
  return(output)
  
}



#' @name get_counties 
#' @description
#' Get county polygons for a city.
#' 
#' @param city name of city, for searching Core Based Statistical Areas
#' @param year reference year
get_counties = function(city = "Boston", year = 2022){
  require(tigris, quietly = TRUE, warn.conflicts = FALSE)
  require(sf, quietly = TRUE, warn.conflicts = FALSE)
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(readr, quietly = TRUE, warn.conflicts = FALSE)
  require(stringr, quietly = TRUE, warn.conflicts = FALSE)
  # Testing values
  # city = "Boston"; year = 2022
  
  # Get the most recently available year for CBSA (decennial)
  year_cbsa = floor(year / 10)*10
  
  # Prepare a bounding box for a given set of county geoids
  cbsa = tigris::core_based_statistical_areas(year = year_cbsa, cb = TRUE, resolution = "20m") %>%
    st_as_sf() %>% st_transform(crs = 4326) %>%
    filter(stringr::str_detect(NAME, city))
  
  # Return all counties that lie within this CBSA
  counties = tigris::counties(cb = TRUE, year = year, resolution = "20m", filter_by = cbsa) %>%
    st_transform(crs = 4326) %>%
    select(geoid = GEOID, name = NAME, state = STUSPS, geometry)

  return(counties)
  
}



#' @name get_bbox 
#' @description
#' Get bounding box of a city for a given year
#' 
#' @param city name of city, for searching Core Based Statistical Areas
#' @param year reference year
get_bbox = function(city = "Boston", year = 2022){
  require(tigris, quietly = TRUE, warn.conflicts = FALSE)
  require(sf, quietly = TRUE, warn.conflicts = FALSE)
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require(readr, quietly = TRUE, warn.conflicts = FALSE)
  require(stringr, quietly = TRUE, warn.conflicts = FALSE)
  # Testing values
  # city = "Boston"; year = 2022
  
  # Get the most recently available year for CBSA (decennial)
  year_cbsa = floor(year / 10)*10
  
  # Prepare a bounding box for a given set of county geoids
  cbsa = tigris::core_based_statistical_areas(year = year_cbsa, cb = TRUE, resolution = "20m") %>%
    st_as_sf() %>% st_transform(crs = 4326) %>%
    filter(stringr::str_detect(NAME, city))
  
  # Create the bounding box
  bbox = cbsa %>% st_bbox() %>% st_as_sfc() %>% tibble(geometry = .) %>% st_as_sf(crs = 4326)

  return(bbox)
  
}

#' @name get_bg
#' @param geoid:[str] vector of N 5-digit county fips codes
get_bg = function(geoid, year = 2022, path = "data/bg.geojson"){
  
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(sf)
  tibble(geoid = geoid) %>%
  # Download
    split(.$geoid) %>%
    purrr::walk(~tigris::block_groups(
      state = stringr::str_sub(.x$geoid, 1,2), 
      county = stringr::str_sub(.x$geoid, 3,5), 
      year = year, cb = TRUE 
    ) %>%
      st_as_sf(crs = 4326) %>%
      mutate(county = stringr::str_sub(GEOID, 1,5)) %>%
      select(county, geoid = GEOID, area_land = ALAND, geometry) %>%
      write_sf(path, delete_layer = FALSE, append = TRUE)
    )

}


# test = get_geo(city = "Hartford", year = 2022)
# 
# tigris::block_groups(filter_by = test$)



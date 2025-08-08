# EPA AQI API #########################################

#' @name get_pollutants
#' @title Get Pollutants
#' Get Pollutant Parameter Codes for Criteria Air Pollutants
#'
#' Queries the EPA AQS API for the list of pollutant parameter codes classified as "CRITERIA" pollutants
#' (e.g., PM2.5, ozone, NO₂). Requires the environment variables `AQIEMAIL` and `AQIKEY` to be set
#' for API authentication. Writes the results to a CSV file at the specified path.
#'
#' @param path:str Output file path for saving the list of pollutant parameters. Default is `"pollutants"`.
#' 
#' @return This function has no return value. It writes the retrieved parameter list to a CSV file.
#'
#' @details This function sends a GET request to the AQS API endpoint 
#' `https://aqs.epa.gov/data/api/list/parametersByClass` with the parameter class `pc = "CRITERIA"`. 
#' It retrieves the set of pollutant parameters that fall under the criteria pollutant classification and 
#' writes the resulting table to the specified output file.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(AQIEMAIL = "you@example.com", AQIKEY = "yourkey123")
#' get_pollutants(path = "criteria_pollutants.csv")
#' }
#'
#' @importFrom httr2 request req_method req_url_path req_headers req_url_query req_perform resp_body_json
#' @importFrom readr write_csv
#' @importFrom dplyr `%>%`
#' @export
get_pollutants = function(path = "pollutants"){
  # Get list of valid parameters
  # Return criterion pollutant ids
  request("https://aqs.epa.gov") %>%
    req_method("GET") %>%
    req_url_path("data/api/list/parametersByClass")  %>%
    req_headers("Accept" = "application/json") %>%
    req_url_query(email = Sys.getenv("AQIEMAIL"), key = Sys.getenv("AQIKEY")) %>%
    req_url_query(pc="CRITERIA") %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE) %>%
    with(Data) %>%
    write_csv(path)
}


#' @name get_active_monitors
#' @title Get Active Monitors
#' 
#' Get Active Air Quality Monitors by County and Pollutant
#'
#' Queries the EPA AQS API for all air quality monitors that were active
#' in the specified date range for a set of county FIPS codes (`geoids`) and pollutants (`param`).
#' Requires the environment variables `AQIEMAIL` and `AQIKEY` to be set for API authentication.
#' Appends the results to a file called `"active_monitors.csv"`.
#'
#' @param geoids:[str] Character vector of 5-digit county FIPS codes (GEOIDs).
#' @param bdate:str Character string in "YYYYMMDD" format. Start date for the query. Default is `"20250101"`.
#' @param edate:str Character string in "YYYYMMDD" format. End date for the query. Default is `"20250530"`.
#' @param param:int Integer or vector of integers indicating pollutant parameter codes (e.g., 88101 for PM2.5).
#' @param path:str output csv path.
#' 
#' @return This function has no return value. It writes results to a path, eg. `"active_monitors.csv"`, and prints progress.
#'
#' @details This function sends GET requests to the AQS API endpoint 
#' `https://aqs.epa.gov/data/api/monitors/byCounty`. 
#' Each combination of `geoids` and `param` is queried, and matching monitor data 
#' is extracted and appended to a local CSV file.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(AQIEMAIL = "you@example.com", AQIKEY = "yourkey123")
#' get_active_monitors(geoids = c("36005", "36047"), bdate = "20250101", edate = "20250530", param = 88101, path = "active_monitors.csv")
#' }
#'
#' @importFrom stringr str_sub
#' @importFrom httr2 request req_method req_url_path req_headers req_url_query req_perform resp_body_json
#' @importFrom readr write_csv
#' @importFrom dplyr `%>%` tibble slice
#' @export
get_active_monitors = function(geoids, 
                               bdate = "20250101",
                               edate = "20250530",
                               param = 88101,
                               path = "data/active_monitors.csv"
){
  
  # Testing values
  # geoids = c("36005", "36047"); bdate = "20250101"; edate = "20250530"; param = 88101
  
  # Stop if these values are not available  
  if(nchar(Sys.getenv("AQIEMAIL")) == 0){ stop("environmental variable AQIEMAIL missing...")}
  if(nchar(Sys.getenv("AQIKEY")) == 0){ stop("environmental variable AQIEMAIL missing...")}
  
  
  require(dplyr, quietly = TRUE,warn.conflicts = FALSE)
  require(httr2, quietly = TRUE,warn.conflicts = FALSE)
  require(stringr, quietly = TRUE,warn.conflicts = FALSE)
  require(readr, quietly = TRUE,warn.conflicts = FALSE)
  
  # If the file does not exist, write this initial file...
  if(!file.exists(path)){
    empty_monitors <- tibble(
      state_code              = as.numeric(NA),
      county_code             = as.character(NA),
      site_number             = as.character(NA),
      parameter_code          = as.numeric(NA),
      poc                     = as.numeric(NA),
      parameter_name          = as.character(NA),
      open_date               = as.Date(NA),
      close_date              = as.logical(NA),
      concurred_exclusions    = as.character(NA),
      dominant_source         = as.character(NA),
      measurement_scale       = as.character(NA),
      measurement_scale_def   = as.character(NA),
      monitoring_objective    = as.character(NA),
      last_method_code        = as.numeric(NA),
      last_method_description = as.character(NA),
      last_method_begin_date  = as.Date(NA),
      naaqs_primary_monitor   = as.character(NA),
      qa_primary_monitor      = as.character(NA),
      monitor_type            = as.character(NA),
      networks                = as.character(NA),
      monitoring_agency_code  = as.character(NA),
      monitoring_agency       = as.character(NA),
      si_id                   = as.numeric(NA),
      latitude                = as.numeric(NA),
      longitude               = as.numeric(NA),
      datum                   = as.character(NA),
      lat_lon_accuracy        = as.numeric(NA),
      elevation               = as.numeric(NA),
      probe_height            = as.numeric(NA),
      pl_probe_location       = as.character(NA),
      local_site_name         = as.character(NA),
      address                 = as.character(NA),
      state_name              = as.character(NA),
      county_name             = as.character(NA),
      city_name               = as.character(NA),
      cbsa_code               = as.numeric(NA),
      cbsa_name               = as.character(NA),
      csa_code                = as.numeric(NA),
      csa_name                = as.character(NA),
      tribal_code             = as.logical(NA),
      tribe_name              = as.logical(NA)
    ) %>% 
      slice(0) %>%
      write_csv(path)
  }
  
  # For each pollutant...
  for(p in param){
    # For each county...
    for(i in geoids){
      .state = stringr::str_sub(i, 1,2)
      .county = stringr::str_sub(i, 3,5)
      Sys.sleep(0.1)
      
      # Return all monitors by county that were operating in time frame
      data = request("https://aqs.epa.gov") %>%
        req_method("GET") %>%
        req_url_path("data/api/monitors/byCounty") %>%
        req_headers("Accept" = "application/json") %>%
        req_url_query(email = Sys.getenv("AQIEMAIL"), key = Sys.getenv("AQIKEY")) %>%
        req_url_query(state = .state, county = .county) %>%
        req_url_query(param = p) %>%
        req_url_query(bdate = bdate, edate = edate)  %>%
        req_perform() %>%
        resp_body_json(simplifyVector = TRUE) %>%
        with(Data)
      if(is.data.frame(data)){
        data %>%
          write_csv(path, append = TRUE)
      }
      cat("\n--geoid: ", i, ", --pollutant param: ", p, "\n")
    }
  }
}

# read_csv("data/active_monitors.csv") %>% 
#   slice(0) %>% glimpse()


#' @name get_sites
#' @title Get Sites
#' 
#' Get Site Metadata for Air Quality Monitoring by County
#'
#' Queries the EPA AQS API for a list of monitoring sites in each specified county FIPS code (`geoids`)
#' during a given date range. Requires the environment variables `AQIEMAIL` and `AQIKEY` to be set
#' for API authentication. Appends the results to a file at the specified `path`.
#'
#' @param geoids:[str] Character vector of 5-digit county FIPS codes (GEOIDs).
#' @param bdate:str Character string in "YYYYMMDD" format. Start date for the query. Default is `"20250101"`.
#' @param edate:str Character string in "YYYYMMDD" format. End date for the query. Default is `"20250530"`.
#' @param path:str Output CSV file path to append results to. If the file doesn't exist, it will be created.
#' 
#' @return Character string. Returns the path to the output CSV file after writing results.
#'
#' @details This function sends GET requests to the AQS API endpoint 
#' `https://aqs.epa.gov/data/api/list/sitesByCounty`. Each county geoid is split into state and county codes, 
#' and queried individually. Valid API responses are parsed and appended to the output file. 
#' A 0-row template file with matching columns is created if the output file does not exist.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(AQIEMAIL = "you@example.com", AQIKEY = "yourkey123")
#' get_sites(geoids = c("36061", "36109"), bdate = "20250101", edate = "20250530", path = "sites.csv")
#' }
#'
#' @importFrom stringr str_sub
#' @importFrom httr2 request req_method req_url_path req_headers req_url_query req_perform resp_body_json resp_status
#' @importFrom readr write_csv
#' @importFrom dplyr `%>%` tibble slice mutate
#' @export
get_sites = function(
    geoids = c("36061", "36109"),
    bdate = "20250101",
    edate = "20250530",
    path = "data/sites.csv"
){
  
  # Testing values
  # geoids = c("36005", "36047"); bdate = "20250101"; edate = "20250530"; param = 88101
  
  # Stop if these values are not available  
  if(nchar(Sys.getenv("AQIEMAIL")) == 0){ stop("environmental variable AQIEMAIL missing...")}
  if(nchar(Sys.getenv("AQIKEY")) == 0){ stop("environmental variable AQIEMAIL missing...")}
  
  # Load packages 
  require(dplyr, quietly = TRUE,warn.conflicts = FALSE)
  require(httr2, quietly = TRUE,warn.conflicts = FALSE)
  require(stringr, quietly = TRUE,warn.conflicts = FALSE)
  require(readr, quietly = TRUE,warn.conflicts = FALSE)
  
  
  if(!file.exists(path)){
    tibble(code = NA_character_, value_represented = NA_character_, geoid = NA_character_) %>%
      slice(0) %>%
      write_csv(file = path)
  }
  
  # For each county...
  for(i in geoids){
    .state = stringr::str_sub(i, 1,2)
    .county = stringr::str_sub(i, 3,5)
    Sys.sleep(0.1)
    
    # We're going to collect a list of all valid AQI stations in the NYC metro area...
    response = request("https://aqs.epa.gov") %>%
      req_method("GET") %>%
      req_url_path("data/api/list/sitesByCounty")  %>%
      req_headers("Accept" = "application/json") %>%
      req_url_query(email = Sys.getenv("AQIEMAIL"), key = Sys.getenv("AQIKEY")) %>%
      req_url_query(state = .state, county = .county) %>%
      req_perform()
    
    # If valid response, write it to file...
    if(resp_status(response)){
      response %>%
        resp_body_json(simplifyVector = TRUE) %>%
        with(Data) %>%
        mutate(geoid = geoid) %>%
        write_csv(file = path, append = TRUE)
    }
    # Message
    cat("\n---geoid: ", i, "\n")
    
  }
  
  return(path)
}

#' @name get_dailydata_by_site
#' @title Get Daily Air Quality Data by Site
#'
#' Queries the EPA AQS API for daily air quality data for specified county FIPS codes (`geoids`), 
#' pollutant parameter codes (`param`), and monitoring site numbers (`site`) within a given date range.
#' Requires the environment variables `AQIEMAIL` and `AQIKEY` to be set for API authentication.
#' Appends the results to a CSV file (default: `"dailydata_by_site.csv"`).
#'
#' @param geoids:[str] Character vector of 5-digit county FIPS codes. Only the first value is used in the current implementation.
#' @param param:[int] Integer or vector of integers representing pollutant parameter codes (e.g., 88101 for PM2.5).
#' @param site:[str] Character vector of 4-digit site numbers (e.g., `"0002"`).
#' @param bdate:str Start date in `"YYYYMMDD"` format (default `"20250101"`).
#' @param edate:str End date in `"YYYYMMDD"` format (default `"20250530"`).
#' @param path:str Path to CSV file where results will be saved (default `"dailydata_by_site.csv"`).
#'
#' @return This function has no return value. It writes results to the specified `path` and prints progress.
#'
#' @details The function sends GET requests to the AQS API endpoint 
#' `https://aqs.epa.gov/data/api/dailyData/bySite` for each combination of pollutant (`param`) and site (`site`) 
#' within the specified county. If data is returned, it is appended to the output file.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(AQIEMAIL = "you@example.com", AQIKEY = "yourkey123")
#' get_dailydata(geoids = "36061", param = 88101, site = "0002", bdate = "20250101", edate = "20250530")
#' }
#'
#' @importFrom stringr str_sub
#' @importFrom httr2 request req_method req_url_path req_headers req_url_query req_perform resp_body_json
#' @importFrom readr write_csv
#' @importFrom dplyr tibble slice `%>%`
#' @export
get_dailydata_by_site = function(geoids = "36061", param = 88101, site = "0002", bdate = "20250101", edate = "20250530", path = "dailydata_by_site.csv"){
  
  # Testing values
  # param = read_csv("data/pollutants.csv") %>%   filter(value_represented == "PM2.5 - Local Conditions") %>% with(code)
  
  # site = read_csv("data/sites.csv")$code[1]
  # geoid = "36061"; bdate = "20250101"; edate = "20250530";
  
  if(!file.exists(path)){
    
    tibble(
      state_code = NA_character_,
      county_code = NA_character_,
      site_number = NA_character_,
      parameter_code = NA_character_,
      poc = NA_integer_,
      latitude = NA_real_,
      longitude = NA_real_,
      datum = NA_character_,
      parameter = NA_character_,
      sample_duration_code = NA_character_,
      sample_duration = NA_character_,
      pollutant_standard = NA_character_,
      date_local = as.Date(NA),
      units_of_measure = NA_character_,
      event_type = NA_character_,
      observation_count = NA_integer_,
      observation_percent = NA_real_,
      validity_indicator = NA_character_,
      arithmetic_mean = NA_real_,
      first_max_value = NA_real_,
      first_max_hour = NA_integer_,
      aqi = NA_integer_,
      method_code = NA_character_,
      method = NA_character_,
      local_site_name = NA_character_,
      site_address = NA_character_,
      state = NA_character_,
      county = NA_character_,
      city = NA_character_,
      cbsa_code = NA_character_,
      cbsa = NA_character_,
      date_of_last_change = as.Date(NA)
    ) %>% 
      slice(0) %>%
      write_csv(path)
  }
  
  # For each pollutant, ...
  for(p in param){
    
    # For 1 county...
    i = geoids
    .state = stringr::str_sub(i, 1,2)
    .county = stringr::str_sub(i, 3,5)
    
    # For each site in that county...
    for(s in site){
      
      req = request("https://aqs.epa.gov") %>%
        req_method("GET") %>%
        #req_url_path("data/api/dailyData/byCounty")  %>%
        req_url_path("data/api/dailyData/bySite")  %>%
        req_headers("Accept" = "application/json") %>%
        req_url_query(email = Sys.getenv("AQIEMAIL"), key = Sys.getenv("AQIKEY")) %>%
        req_url_query(state = .state, county = .county) %>%
        req_url_query(param = p) %>%
        req_url_query(site = s) %>%
        req_url_query(bdate = bdate, edate = edate) 
      
      resp = req %>% req_perform()
      
      content = resp %>% resp_body_json(simplifyVector = TRUE) 
      valid_result = length(content$Data) > 0
      if(valid_result){
        content$Data %>% write_csv(path, append = TRUE)    
      }
      
      # Message
      cat("\n---geoid: ", i, ", param: ", p, ", site: ", s, "\n")
      
    }
    
  }
  return(path)
}

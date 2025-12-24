# FUNCTIONS ##########################################


#' @name pm25_aqi
#' @title Convert PM2.5 Concentration to Air Quality Index
#' @description Converts PM2.5 concentration values (in μg/m³) to Air Quality Index (AQI) values using the EPA's standard conversion formula. The function uses linear interpolation between AQI breakpoints.
#' @param pm25 numeric: PM2.5 concentration in micrograms per cubic meter (μg/m³)
#' @param digits integer: Number of decimal places for rounding (default: 0)
#' @return numeric: Air Quality Index value (0-500 scale)
#' @author Tim Fraser
#' @name pm25_aqi
#' @title Convert PM2.5 Concentration to Air Quality Index
#' @description Converts PM2.5 concentration values (in μg/m³) to Air Quality Index (AQI) values using the EPA's standard conversion formula. The function uses linear interpolation between AQI breakpoints.
#' @param pm25 numeric: PM2.5 concentration in micrograms per cubic meter (μg/m³)
#' @param digits integer: Number of decimal places for rounding (default: 0)
#' @return numeric: Air Quality Index value (0-500 scale)
#' @author Tim Fraser
pm25_aqi <- function(pm25, digits = 0) {
  library(dplyr)

  lerp <- function(aqi_low, aqi_high, conc_low, conc_high, conc) {
    (aqi_high - aqi_low) / (conc_high - conc_low) * (conc - conc_low) + aqi_low
  }
  
    c <- floor(10 * pm25) / 10
    
    case_when(
      c < 0      ~ 0,
      c < 12.1   ~ lerp(0,   50,   0.0,   12.0,  c),
      c < 35.5   ~ lerp(51,  100,  12.1,  35.4,  c),
      c < 55.5   ~ lerp(101, 150,  35.5,  55.4,  c),
      c < 150.5  ~ lerp(151, 200,  55.5,  150.4, c),
      c < 250.5  ~ lerp(201, 300,  150.5, 250.4, c),
      c < 350.5  ~ lerp(301, 400,  250.5, 350.4, c),
      c < 500.5  ~ lerp(401, 500,  350.5, 500.4, c),
      TRUE       ~ 500
    ) %>% round(digits = digits)
  
}

#' @name cor_tidy
#' @title Create Tidy Correlation Matrix
#' @description Converts a correlation matrix into a tidy long-format data frame with pairwise correlations. Removes the lower triangle and diagonal elements to avoid redundancy.
#' @param data data.frame or matrix: Input data containing numeric variables for correlation calculation
#' @return tibble: Data frame with columns `from`, `to`, and `cor` containing pairwise correlations
#' @author Tim Fraser
#' @name cor_tidy
#' @title Create Tidy Correlation Matrix
#' @description Converts a correlation matrix into a tidy long-format data frame with pairwise correlations. Removes the lower triangle and diagonal elements to avoid redundancy.
#' @param data data.frame or matrix: Input data containing numeric variables for correlation calculation
#' @return tibble: Data frame with columns `from`, `to`, and `cor` containing pairwise correlations
#' @author Tim Fraser
cor_tidy = function(data){
  data %>%
    cor(use = "pairwise.complete.obs") %>%
    # Get rid of bottom triangle of matrix
    { data = .; data[lower.tri(data)] <- NA; data  } %>%
    # Convert to tibble
    {data = .; data %>% as_tibble() %>% mutate(from = rownames(data)) } %>%
    tidyr::pivot_longer(cols = -from, names_to = "to", values_to = "cor") %>%
    # Filter out NAs
    filter(!is.na(cor)) %>%
    # Filter out diagonal (which is always r = 1)
    filter(from != to)
}

#' @name tidier
#' @title Tidy Model Output with Significance Stars
#' @description Formats model output using broom::tidy() and adds significance stars based on p-values. Renames standard error and p-value columns for consistency.
#' @param m model object: Fitted model object (typically from lm() or similar) that can be processed by broom::tidy()
#' @return tibble: Tidy data frame with model coefficients, standard errors, p-values, and significance stars
#' @author Tim Fraser
#' @name tidier
#' @title Tidy Model Output with Significance Stars
#' @description Formats model output using broom::tidy() and adds significance stars based on p-values. Renames standard error and p-value columns for consistency.
#' @param m model object: Fitted model object (typically from lm() or similar) that can be processed by broom::tidy()
#' @return tibble: Tidy data frame with model coefficients, standard errors, p-values, and significance stars
#' @author Tim Fraser
tidier = function(m){
  # m = m[[1]]
  m %>% broom::tidy() %>%
    rename(se = std.error) %>%
    rename(p_value = p.value) %>%
    mutate(stars = gtools::stars.pval(p_value))

}

#' @name get_vif
#' @title Calculate Variance Inflation Factors
#' @description Calculates Variance Inflation Factors (VIF) for all terms in a regression model to detect multicollinearity. Handles both simple VIF vectors and generalized VIF matrices.
#' @param m model object: Fitted linear model object (from lm() or similar)
#' @return tibble: Data frame with columns `term` (variable name) and `vif` (VIF value)
#' @author Tim Fraser

#' @name get_vif
#' @title Calculate Variance Inflation Factors
#' @description Calculates Variance Inflation Factors (VIF) for all terms in a regression model to detect multicollinearity. Handles both simple VIF vectors and generalized VIF matrices.
#' @param m model object: Fitted linear model object (from lm() or similar)
#' @return tibble: Data frame with columns `term` (variable name) and `vif` (VIF value)
#' @author Tim Fraser
get_vif = function(m){
  myvif = car::vif(m)
  if(is.matrix(myvif)){
    tibble(term = rownames(myvif), vif = myvif[,3]^2)
  }else{ tibble(term = names(myvif), vif = myvif) }
  
}
}

#' @name get_gof
#' @title Extract Goodness-of-Fit Statistics
#' @description Extracts comprehensive goodness-of-fit statistics from a model including R-squared, RMSE, MAE, VIF, and observed value ranges. Calculates metrics in original units (back-transformed from square root scale if applicable).
#' @param m model object: Fitted linear model object (from lm() or similar)
#' @return tibble: Data frame containing goodness-of-fit statistics including rsq, sigma, statistic, p.value, df, nobs, vifmax, ymin, ymax, range, rmse, mae, maevsrange, tr (treated count), ct (control count)
#' @author Tim Fraser
#' @name get_gof
#' @title Extract Goodness-of-Fit Statistics
#' @description Extracts comprehensive goodness-of-fit statistics from a model including R-squared, RMSE, MAE, VIF, and observed value ranges. Calculates metrics in original units (back-transformed from square root scale if applicable).
#' @param m model object: Fitted linear model object (from lm() or similar)
#' @return tibble: Data frame containing goodness-of-fit statistics including rsq, sigma, statistic, p.value, df, nobs, vifmax, ymin, ymax, range, rmse, mae, maevsrange, tr (treated count), ct (control count)
#' @author Tim Fraser
get_gof = function(m){
  # m = m[[1]]
  gof = m %>% broom::glance()
  
  gof$vifmax = m %>% get_vif() %>% with(vif) %>% max()
  
  gof = gof %>% 
    select(rsq = r.squared, sigma, statistic, p.value, df, nobs, vifmax)

  # Get root mean squared error in original units
  rmse = sqrt(mean((m$model[,1] - m$fitted.values)^2))
  # Get mean average error in original units
  mae = mean(abs(m$model[,1] - m$fitted.values))
  
  # Observed range
  extra = m$model %>%
    rename(y = 1) %>%
    summarize(
      ymin = min(y, na.rm = TRUE),
      ymax = max(y, na.rm = TRUE),
      range = ymax - ymin,
      rmse = rmse, mae = mae,
      maevsrange = mae / range,
      tr = sum(treated == TRUE),
      ct = sum(treated == FALSE)
    )
  
  result = gof %>%
    bind_cols(extra)
  return(result)
}

#' @name sig_html_to_latex
#' @title Convert HTML Significance Symbols to LaTeX
#' @description Helper function that converts HTML-formatted significance symbols (***, **, *, .) to LaTeX math mode format for use in LaTeX documents.
#' @param sig_html character: String containing HTML significance symbols (***, **, *, ., or empty)
#' @return character: LaTeX-formatted significance symbols in math mode
#' @author Tim Fraser
#' @name sig_html_to_latex
#' @title Convert HTML Significance Symbols to LaTeX
#' @description Helper function that converts HTML-formatted significance symbols (***, **, *, .) to LaTeX math mode format for use in LaTeX documents.
#' @param sig_html character: String containing HTML significance symbols (***, **, *, ., or empty)
#' @return character: LaTeX-formatted significance symbols in math mode
#' @author Tim Fraser
sig_html_to_latex <- function(sig_html) {
  sig_html %>%
    str_replace_all("\\*\\*\\*", "\\$^{***}\\$") %>%
    str_replace_all("\\*\\*", "\\$^{**}\\$") %>%
    str_replace_all("\\*", "\\$^{*}\\$") %>%
    str_replace_all("\\.", "\\$^{\\cdot}\\$")
}

#' @name get_latex
#' @title Convert HTML Table Rows to LaTeX Format
#' @description Converts HTML table rows (<tr> elements) containing table cells (<td>) to LaTeX table row format. Extracts coefficients, standard errors, and significance stars, formatting them for LaTeX makecell environment.
#' @param tr_rows character vector: Vector of HTML table row strings containing <td> elements
#' @return character: LaTeX-formatted table rows ready for use in LaTeX tables
#' @author Tim Fraser
#' @name get_latex
#' @title Convert HTML Table Rows to LaTeX Format
#' @description Converts HTML table rows (<tr> elements) containing table cells (<td>) to LaTeX table row format. Extracts coefficients, standard errors, and significance stars, formatting them for LaTeX makecell environment.
#' @param tr_rows character vector: Vector of HTML table row strings containing <td> elements
#' @return character: LaTeX-formatted table rows ready for use in LaTeX tables
#' @author Tim Fraser
get_latex <- function(tr_rows) {
  
  library(stringr)
  library(purrr)
  
  # Process each <tr> row
  latex_rows <- map_chr(tr_rows, function(row) {
    
    # Extract all <td> contents
    tds <- str_match_all(row, "<td[^>]*>(.*?)</td>")[[1]][,2]
    
    # First td is the row label (Week XX)
    row_label <- str_trim(tds[1])
    
    # Process each subsequent td (values)
    values <- tds[-1] %>% map_chr(function(cell) {
      
      # Extract number with optional bold tags <b>...</b>
      val <- str_match(cell, "(<b>)?(-?\\d+\\.?\\d*)?(</b>)?")[,3]
      val_bold <- !is.na(str_detect(cell, "<b>"))
      
      # Extract significance stars from the cell text (e.g., "***", "**", "*", ".")
      sig_raw <- str_extract(cell, "\\*\\*\\*|\\*\\*|\\*|\\.")
      sig_latex <- ifelse(!is.na(sig_raw), sig_html_to_latex(sig_raw), "")
      
      # Extract standard error from parentheses (e.g., (0.04))
      se <- str_match(cell, "\\(([^)]+)\\)")[,2]
      
      # Compose LaTeX makecell content
      val_fmt <- ifelse(val_bold, paste0("\\textbf{", val, "}", sig_latex), paste0(val, sig_latex))
      se_fmt <- ifelse(is.na(se), "", paste0("\\\\(", se, ")"))
      
      paste0("\\makecell{", val_fmt, se_fmt, "}")
    })
    
    # Join full row
    paste0(row_label, " & ", paste(values, collapse = " & "), " \\\\")
  })
  
  paste(latex_rows, collapse = "\n")
}

#' @name get_table
#' @title Create Formatted HTML Table from Model Results
#' @description Creates a formatted HTML table from a list of model objects, including coefficients with standard errors, significance stars, and goodness-of-fit statistics. Can generate either coefficient tables or fixed effects tables.
#' @param modellist list: Named list of fitted model objects (typically from get_many_models())
#' @param fe logical: If TRUE, returns fixed effects table (week and day-of-week coefficients). If FALSE (default), returns coefficient and goodness-of-fit table.
#' @return tibble: Wide-format data frame with model names as columns and terms/statistics as rows, formatted for HTML display
#' @author Tim Fraser
#' @name get_table
#' @title Create Formatted HTML Table from Model Results
#' @description Creates a formatted HTML table from a list of model objects, including coefficients with standard errors, significance stars, and goodness-of-fit statistics. Can generate either coefficient tables or fixed effects tables.
#' @param modellist list: Named list of fitted model objects (typically from get_many_models())
#' @param fe logical: If TRUE, returns fixed effects table (week and day-of-week coefficients). If FALSE (default), returns coefficient and goodness-of-fit table.
#' @return tibble: Wide-format data frame with model names as columns and terms/statistics as rows, formatted for HTML display
#' @author Tim Fraser
get_table = function(modellist, fe = FALSE){
  # modellist = m; fe = TRUE
  
  # Extract model coefficients
  beta = modellist %>% 
    map_dfr(~tidier(.), .id = "model") %>%
    # format coefficients
    mutate(estimate = scales::number(estimate, accuracy = 0.01, scale_cut = scales::cut_short_scale())) %>%
    mutate(estimate = case_when(
      estimate == "0.00" ~ "<0.01", 
      estimate == "-0.00" ~ ">-0.01", 
      TRUE ~ estimate)) %>%
    mutate(se = scales::number(se, accuracy = 0.01, scale_cut = scales::cut_short_scale())) %>%
    mutate(statistic = scales::number(statistic, accuracy = 0.01, scale_cut = scales::cut_short_scale())) %>%
    # If statistically significant, bold it.
    mutate(estimate = case_when(
      p_value < 0.1 ~ paste0("<b>", estimate, "</b>"),
      TRUE ~ estimate
    )) %>%
    mutate(p_value = scales::number(p_value, accuracy = 0.001, scale_cut = scales::cut_short_scale())) %>%
    mutate(estimate = paste0(estimate, stars, "<br>(", se, ")")) %>%
    select(model, term, estimate)
  
  gof = modellist %>% 
    map_dfr(~get_gof(.), .id = "model") %>%
    mutate(
      across(
        .cols = c("statistic", "vifmax", "ymin", "ymax"),
        .fns = ~scales::number(.x, accuracy = 0.1, scale_cut = scales::cut_short_scale())
      ),
      across(
        .cols = c("rsq", "sigma", 
                  "rmse", "mae"),
        .fns = ~scales::number(.x, accuracy = 0.01, scale_cut = scales::cut_short_scale())
      ),
      across(
        .cols = c( "maevsrange"),
        .fns = ~scales::percent(maevsrange, accuracy = 1)
      )
    ) %>%
    mutate(
      statistic = paste0(statistic, gtools::stars.pval(p.value), " (", df, ")"),
      # Get treatment versus control
      trvsct = paste0(tr, "|", ct),
    ) %>%
    mutate(yrange = paste0(ymin, "-", ymax)) %>%
    select(model, 
           rsq, rmse, mae, maevsrange, yrange, statistic, vifmax,
           nobs, trvsct) %>%
    mutate(
      across(.cols = everything(), .fns = ~as.character(.x))
    ) %>%
    pivot_longer(cols = -c(model), names_to = "term", values_to = "estimate") 
  
  
  tab = bind_rows(beta, gof) %>%
    tidyr::pivot_wider(
      id_cols = c(term),
      names_from = model,
      values_from = estimate
    ) 
  
  if(fe == FALSE){
    tab = tab %>%
      filter(stringr::str_detect(term, "factor[(]week[)]", negate = TRUE)) %>%
      filter(stringr::str_detect(term, "factor[(]day[)]", negate = TRUE)) %>%
      #with(term)
      # Reclassify terms into ordered factor labels
      mutate(term = term %>% dplyr::recode_factor(
        "I(treated * daysafter)" = "Daily Treatment Effect",
        "treatedTRUE" = "Treatment",
        "bgmean" = "Background Concentration",
        "sqrt(bgmean)" = "&radic;Background Concentration",
        "log(distcrz + 1)" = "log(Distance from CRZ)",
        "log(distmin + 1)" = "log(Distance from Nearest Highway)",
        "log(temp)" = "log(Temperature)",
        "humidity" = "% Humidity",
        "windspeed" = "Windspeed m/s",
        "precip" = "Precipitation",
        "cloudcover" = "% Cloud Cover",
        "log(pop_density + 1)" = "log(Pop. Density)",
        "log(median_income + 1)" = "log(Median Income)",
        "nonwhite" = "% Non-White",
        "hisplat" = "% Hispanic/Latino",
        "(Intercept)" = "Constant",
        "rsq" = "R<sup>2</sup>",
        "rmse" = "RMSE (ug/m<sup>3</sup>)",
        "mae" = "Mean Avg Error (MAE)  (ug/m<sup>3</sup>)",
        "maevsrange" = "%MAE/Range",
        "yrange" = "Range (ug/m<su>3</sup>)",
        "statistic" = "F (df)",
        "vifmax" = "Max VIF",
        "nobs" = "N Obs",
        "trvsct" = "N Treated|N Control"
      )) %>%
      # arrange by factor order
      arrange(term)
    
  }else if(fe == TRUE){
    tab = tab %>%
      filter(stringr::str_detect(term, "(factor[(]week[)]|factor[(]day[)])", negate = FALSE)) %>%
      mutate(term = term %>% stringr::str_replace_all(
        pattern =  c(
        "factor[(]week[)]" = "Week ",
        "factor[(]day[)]1" = "Monday",
        "factor[(]day[)]2" = "Tuesday",
        "factor[(]day[)]3" = "Wednesday",
        "factor[(]day[)]4" = "Thursday",
        "factor[(]day[)]5" = "Friday",
        "factor[(]day[)]6" = "Saturday",
        "factor[(]day[)]7" = "Sunday"
        ))) %>%
      mutate(num = stringr::str_extract(term, "[0-9]+") %>% as.integer()) %>%
      mutate(isnum = nchar(num) > 0) %>%
      arrange(isnum, desc(num)) %>%
      select(-isnum, -num)
  }
  
  # Clear the NAs to show empty cells
  tab = tab %>% mutate(across(.cols = everything(), 
                              .fns = ~if_else(condition = is.na(.x), true = "", false = .x)))
  return(tab)

}


#' @name get_lines
#' @title Generate Time Series Plot of Air Quality Data
#' @description Creates a ggplot2 time series visualization showing individual sensor trajectories, group means by treatment status, grand means, and linear trend lines. Includes vertical line marking the treatment cutoff date.
#' @param data data.frame: Panel dataset containing columns `date`, `value`, `aqs_id_full`, and `treated`
#' @return ggplot: ggplot2 object with time series plot
#' @author Tim Fraser
get_lines = function(data){
  
  stat = data %>%
    group_by(treated, date) %>%
    summarize(mu = mean(value, na.rm = TRUE),
              se = sd(value) / sqrt(n()), .groups = "drop")
  
  grand = stat %>%
    group_by(treated) %>%
    summarize(mu = mean(mu,na.rm = TRUE))
  
  
  grandlines = grand %>% 
    left_join(by = "treated", y = read_csv("../data/datetimes.csv", show_col_types = FALSE) %>% 
                filter(current == TRUE) %>%
                group_by(treated) %>%
                reframe(date = c(min(date), max(date)))
    )
  
  cutoff = lubridate::date("2025-01-05")
  
  gg = ggplot() +
    geom_line(data = data, mapping = aes(x = date, y = value, group = aqs_id_full), color = "lightgrey", alpha = 0.25) +
    geom_line(data = stat, mapping = aes(x = date, y = mu, group = treated, color = treated), alpha = 1) +
    geom_vline(xintercept = lubridate::as_date("2025-01-05"), linetype = "dashed", color = "black") +
    geom_line(data = grandlines, mapping = aes(x = date, y = mu, group = treated, color = treated)) +
    geom_smooth(data = data %>% filter(treated == FALSE), 
                mapping = aes(x = date, y = value, color = treated),
                method = "lm") +
    geom_smooth(data = data %>% filter(treated == TRUE), 
                mapping = aes(x = date, y = value, color = treated),
                method = "lm") +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si(""))) +
    #breaks = c(0.1, 0.3, 1, 3, 10,30,100, 300))
    theme_bw(base_size = 14)
  
  return(gg)
}




# Create a table containing basic model,
# then model with environmental controls
# then model with population controls

# grid$value %>% sqrt() %>% hist()

#' @name get_many_models
#' @title Fit Multiple Regression Models
#' @description Fits 9 regression models across three geographic scopes (CBSA, NYC, CRZ) and three specification levels (basic, +weather, +demographics). Models use square root transformation of the outcome variable.
#' @param data data.frame: Panel dataset containing all necessary variables for modeling (value, treated, bgmean, distmin, temp, humidity, windspeed, precip, cloudcover, pop_density, median_income, nonwhite, hisplat, name, within, week, day)
#' @return list: Named list containing 9 fitted model objects: cbsa1-3, nyc1-3, crz1-3
#' @author Tim Fraser
#' @name get_many_models
#' @title Fit Multiple Regression Models
#' @description Fits 9 regression models across three geographic scopes (CBSA, NYC, CRZ) and three specification levels (basic, +weather, +demographics). Models use square root transformation of the outcome variable.
#' @param data data.frame: Panel dataset containing all necessary variables for modeling (value, treated, bgmean, distmin, temp, humidity, windspeed, precip, cloudcover, pop_density, median_income, nonwhite, hisplat, name, within, week, day)
#' @return list: Named list containing 9 fitted model objects: cbsa1-3, nyc1-3, crz1-3
#' @author Tim Fraser
get_many_models = function(data){
  # testing data
  # data = read_rds("../descriptives/panel_daily_nyc.rds")
  
  # Create models
  formula1 =  sqrt(value) ~ 
    treated + I(treated * daysafter ) + factor(week) + factor(day) +
    sqrt(bgmean) +
    #sqrt(bgmean) + #log(distcrz + 1) + 
    log(distmin + 1)
  
  formula2 =  sqrt(value) ~ 
    treated + I(treated * daysafter ) + factor(week) + factor(day) +
    #sqrt(bgmean) +
    sqrt(bgmean) + 
    #log(distcrz + 1) + 
    log(distmin + 1) +
    log(temp) + humidity + windspeed + precip + cloudcover
  
  formula3 =   sqrt(value)  ~ 
    treated + I(treated * daysafter ) + factor(week) + factor(day) +
    #bgmean +
    sqrt(bgmean) + 
    #log(distcrz + 1) + 
    log(distmin + 1) +
    log(temp) + humidity + windspeed + precip + cloudcover +
    log(pop_density + 1 ) + log(median_income + 1) + nonwhite + hisplat
  
  # Create models edited for use within congestion relief zone
  formula1b =   sqrt(value)   ~ 
    treated + I(treated * daysafter ) + factor(week) + factor(day) +
    #bgmean 
    sqrt(bgmean) #+ log(distcrz + 1) #+ log(distmin + 1)
  
  formula2b =  sqrt(value)   ~ 
    treated + I(treated * daysafter ) + factor(week) + factor(day) +
    #bgmean +
    sqrt(bgmean) + #log(distcrz + 1) + #log(distmin + 1) +
    log(temp) + humidity + windspeed + precip + cloudcover
  
  formula3b =   sqrt(value)   ~ 
    treated + I(treated * daysafter ) + factor(week) + factor(day) +
    #bgmean +
    sqrt(bgmean) + #log(distcrz + 1) + #log(distmin + 1) +
    log(temp) + humidity + windspeed + precip + cloudcover +
    log(pop_density + 1 ) + log(median_income + 1) #+ nonwhite + hisplat
  
  
  # Model entire NYC metro area
  m1 = data %>% filter(!name %in% c("Beyond")) %>% lm(formula = formula1)
  m2 = data %>% filter(!name %in% c("Beyond")) %>% lm(formula = formula2)
  m3 = data %>% filter(!name %in% c("Beyond")) %>% lm(formula = formula3)
  
  # Model NYC boroughs
  m4 = data %>% filter(name %in% c("Bronx", "Queens", "Kings", "New York", "Richmond")) %>% lm(formula = formula1)
  m5 = data %>% filter(name %in% c("Bronx", "Queens", "Kings", "New York", "Richmond")) %>% lm(formula = formula2)
  m6 = data %>% filter(name %in% c("Bronx", "Queens", "Kings", "New York", "Richmond")) %>% lm(formula = formula3)

  # Model CRZ
  m7 = data %>% filter(within == 1) %>% lm(formula = formula1b)
  m8 = data %>% filter(within == 1) %>% lm(formula = formula2b)
  m9 = data %>% filter(within == 1) %>% lm(formula = formula3b)
  
  m = list(cbsa1 = m1,cbsa2 = m2,cbsa3 = m3, 
        nyc1 = m4, nyc2 = m5, nyc3 = m6,
        crz1 = m7, crz2 = m8, crz3 = m9) 
  return(m)
}


#' @name get_impute_cloudcover
#' @title Impute Cloudcover
#' @description Impute cloudcover using a linear model
#' @param data data.frame: Data frame containing the data to impute
#' @return data.frame: Data frame containing the imputed cloudcover
#' @author Tim Fraser
get_impute_cloudcover = function(data){

  # Construct a model to impute cloudcover
  m = data %>% lm(formula = cloudcover ~ temp + humidity + windspeed + precip + poly(date, 3) + factor(month) + factor(name) )
  result = data %>% 
    predict(object = m, newdata = .) %>%
    as_tibble() %>%
    rename(imp = value) %>%
    bind_cols(data) %>%
    mutate(cloudcover = if_else(is.na(cloudcover) | is.nan(cloudcover), true = imp, false = cloudcover)) %>%
    select(-imp)

  return(result)
}


#' @name get_yhat
#' @title Generate Predicted Values for Treatment and Counterfactual
#' @description Generates predicted values (yhat) and standard errors for both the treated scenario (yhat1) and counterfactual scenario (yhat0) using a fitted model. Used to estimate treatment effects by comparing predictions with and without treatment.
#' @param m model object: Fitted linear model object (from lm() or similar)
#' @param path_data character: Path to RDS file containing panel dataset (default: "../descriptives/panel_daily_nyc.rds")
#' @param useobs logical: Whether to use observed values (default: FALSE). The original study used predictions for the counterfactual and treated (so `useobs = FALSE`), then calculated the difference.
#' @param impute logical: Whether to impute cloudcover using a statistical model trained on temp, humidity, windspeed, precip, month, date, and county (default: FALSE)
#' @return tibble: Data frame containing original data plus columns yhat0, se0 (counterfactual predictions), yhat1, se1 (treated predictions), and id (row identifier)
#' @author Tim Fraser
get_yhat = function(m, path_data = "../descriptives/panel_daily_nyc.rds", useobs = FALSE, impute = FALSE){
  
  # Load grid  
  data = read_rds(path_data)

  # If impute is TRUE, impute cloudcover using a statistical model trained on temp, humidity, windspeed, precip, month, date, and county
  if(impute == TRUE){  data = get_impute_cloudcover(data) }

  # To show treatment effects per site,
  # we need to use our model to predict 
  # what would our data look like WITH the treatment vs. WITHOUT
  grid = data %>%
    mutate(
      # Get base predictions for every point in the grid...
      # assuming the treatment effect never happened...
      predict(object = m, newdata = tibble(.) %>% mutate(treated = FALSE), se.fit = TRUE) %>%
        as_tibble() %>%
        select(yhat0 = fit, se0 = se.fit),
      # Now get real predictions for every point in the grid...
      # reflecting the predictions based on real observed data
      predict(object = m, newdata = ., se.fit = TRUE) %>%
        as_tibble() %>%
        select(yhat1 = fit, se1 = se.fit)
    ) %>%
    # select(date, treated, aqs_id_full, area, name, value,
    #        yhat0, se0, yhat1, se1 ) %>%
    mutate(id = 1:n()) 

  # There are several ways you could calculate the treatment effect downstream.
  # One way is to use the observed values for the treated period - this could yield a little missing data.
  # One way is to use the observed values for the treated period, filling in missing with the predicted values
  # One way is to use the predicted values for the treated period as a whole.
  # In each case, you use predicted values for the counterfactual.
  if(useobs == TRUE){

    grid = grid %>%
       # If the observed value is available, use it instead of the predicted value. 
       # (Remember to square root the observed value so its on the same scale as the predicted values)
       mutate(yhat1 = if_else(!is.na(value), true = sqrt(value), false = yhat1),
       # If the observed value is available, set the standard error to 0
              se1 = if_else(!is.na(value), true = 0, false = se1))

  }


  return(grid)
}

#' @name get_simeffects
#' @title Simulate Treatment Effects with Uncertainty
#' @description Simulates treatment effects by drawing from the predicted value distributions (accounting for prediction uncertainty) and back-transforming from square root scale. Calculates mean difference and standard error of the difference through simulation.
#' @param grid tibble: Data frame containing yhat0, se0, yhat1, se1 columns (typically from get_yhat())
#' @param start character: Start date of treatment period in "YYYY-MM-DD" format (default: "2025-01-05")
#' @param end character: End date of treatment period in "YYYY-MM-DD" format (default: "2025-06-01")
#' @param n integer: Number of simulations to run (default: 10000)
#' @return tibble: Data frame containing simulated treatment effects with columns diff (mean difference), sediff (standard error of difference), yhat1, yhat0, plus original columns from grid
#' @author Tim Fraser
get_simeffects = function(grid, start = "2025-01-05", end = "2025-06-01", n = 10000){
  grid %>%
    # Calculate treatment effects in the treated period
    filter(date >= start & date <= end) %>%
    # for each sensor-date pair,
    group_by(id) %>%
    # simulate (and back transform)
    reframe(
      ysim1 = rnorm(n = n, mean = yhat1, sd = se1)^2,
      ysim0 = rnorm(n = n, mean = yhat0, sd = se0)^2,
      diff = ysim1 - ysim0,
    ) %>%
    # Now return the mean estimated difference and the standard error of the difference
    group_by(id) %>%
    summarize(
      sediff = sd(diff),
      diff = mean(diff),
      yhat1 = mean(ysim1),
      yhat0 = mean(ysim0),
      # Make a new standard errors for each of the mean sims
      se1 = sd(ysim1),
      se0 = sd(ysim0)
      yhat0 = mean(ysim0),
      # Make a new standard errors for each of the mean sims
      se1 = sd(ysim1),
      se0 = sd(ysim0)
    ) %>%
    # Join back in key traits
    left_join(
      by = "id",
      y = grid %>%
        select(any_of(c("id", "date", "aqs_id_full", "treated", "area", "name")))
    )
  
  
}

#' @name get_effects
#' @title Calculate Treatment Effects
#' @description Calculates treatment effects as the difference between treated and counterfactual predictions, along with standard errors. Filters to the treatment period and computes difference and standard error of difference.
#' @param grid tibble: Data frame containing yhat0, se0, yhat1, se1 columns (typically from get_yhat())
#' @param start character: Start date of treatment period in "YYYY-MM-DD" format (default: "2025-01-05")
#' @param end character: End date of treatment period in "YYYY-MM-DD" format (default: "2025-06-01")
#' @return tibble: Data frame containing treatment effects with columns diff (difference), sediff (standard error), yhat1, yhat0, plus date, aqs_id_full, and other identifying columns
#' @author Tim Fraser
#' @name get_effects
#' @title Calculate Treatment Effects
#' @description Calculates treatment effects as the difference between treated and counterfactual predictions, along with standard errors. Filters to the treatment period and computes difference and standard error of difference.
#' @param grid tibble: Data frame containing yhat0, se0, yhat1, se1 columns (typically from get_yhat())
#' @param start character: Start date of treatment period in "YYYY-MM-DD" format (default: "2025-01-05")
#' @param end character: End date of treatment period in "YYYY-MM-DD" format (default: "2025-06-01")
#' @return tibble: Data frame containing treatment effects with columns diff (difference), sediff (standard error), yhat1, yhat0, plus date, aqs_id_full, and other identifying columns
#' @author Tim Fraser
get_effects = function(grid, start = "2025-01-05", end = "2025-06-01"){
  grid %>%
    # Calculate treatment effects in the treated period
    filter(date >= start & date <= end) %>%
    mutate(
      # calculate treatment effect on the treated
      diff = yhat1 - yhat0,
      sediff = sqrt(se1^2 + se0^2)
    ) %>%
    select(date, aqs_id_full, any_of(c("treated", "area", "name")), 
           diff, sediff, yhat1, yhat0) 
  
}




#' @name get_pooled_sd
#' @title Pool Standard Deviations
#' @description 
#' Pools standard deviations.
#' If the sample sizes are equal, we can pool the standard deviations by averaging the variances,
#' @param sds:[dbl] vector: Vector of standard deviations
#' @param ns:[int] vector: Vector of sample sizes
#' @return numeric: Pooled standard deviation
#' @author Tim Fraser
get_pooled_sd = function(sds, ns){
  # Testing values
  # sds = c(0.5, 0.7, 0.3, 0.4)
  # ns = c(10000, 10000, 10000, 10000)
  
  # Do they have equal sample sizes?
  equal_sample_size = all(ns == ns[1])
  

  # The standard deviation of a sampling distribution is the standard error.

  
  # IF it has equal sample sizes ...
  if(equal_sample_size){
    # So, we can pool the standard deviations by averaging the variances,
    # and then taking the square root.
    # If equal sample sizes....
    varp = sum(sds^2) / length(ns)
    sdp = sqrt(varp)
    return(sdp)
  }else if(!equal_sample_size){
    # If sample sizes are not equal, we need to weight the variances by the sample sizes.
    varp = sum(sds^2 * (ns - 1)) / (sum(ns) - length(ns))
    sdp = sqrt(varp)
    return(sdp)
  }
}


# Let's just computationally handle this...

#' @name get_pooled_se_mc
#' @title Pool Standard Errors with Monte Carlo Simulation
#' @description 
#' Pools standard errors with Monte Carlo simulation.
#' A computational solution to the problem of pooling standard errors
#' when sample sizes are different.
#' @param ses:[dbl] vector: Vector of standard errors
#' @param ns:[int] vector: Vector of sample sizes
#' @return numeric: Pooled standard error
#' @author Tim Fraser
get_pooled_se_mc = function(ses, ns){
  # Testing values
  # ses = c(0.5, 0.7, 0.3, 0.4)
  # ns = c(3000, 2000, 2000, 500)
  
  output = tibble(id = 1:length(ses), ses = ses, ns = ns) %>%
    group_by(id) %>%
    # For each standard error, recreate the sampling distribution at the size of the sample of interest
    reframe(
      sim = rnorm(n = ns, mean = 0, sd = ses)
    ) %>%
    # The total distribution will be the full distribution of errors.
    # The pooled standard error is really trying to capture the average variation in this total distribution,
    # up or down weighting by sample size to make sure we don't give too little / too much weight to any on group.
    summarize(sd = sd(sim))
    # The standard deviation of a sampling distribution is a standard error.
    # Therefore, this is the pooled standard error, achieved with monte carlo simulation.

    # This technique hinges more on the shape of each simulated distribution than the typical strategy of averaging variances.
    return(output)
} 


#' @name get_pooled_se
#' @title Pool Standard Errors using Variances
#' @description Pools standard errors using variances and sample sizes.
#' This is the standard strategy for pooling standard errors.
#' @param ses:[dbl] vector: Vector of standard errors
#' @param ns:[int] vector: Vector of sample sizes
#' @return numeric: Pooled standard error
#' @author Tim Fraser
get_pooled_se = function(ses, ns){
#  Convert standard errors to variances, average them, and then take the square root

  # ses = c(0.5, 0.7, 0.3, 0.4)
  # ns = c(3000, 2000, 2000, 500)
  vars = ses*ns
  varp = sum(vars*ns-1)/(sum(ns) - length(ns))
  sep = sqrt(varp / sum(ns) )

  return(sep)
}

# This strategy is essentially the same as the pooled standard deviation.
# But it generally makes sense, looking at the input vector of standard errors.
# get_pooled_se_mc(ses = c(0.5, 0.7, 0.3, 0.4), ns = c(3000, 2000, 2000, 500))
# get_pooled_sd(sds = c(0.5, 0.7, 0.3, 0.4), ns = c(3000, 2000, 2000, 500))
# get_pooled_se(ses = c(0.5, 0.7, 0.3, 0.4), ns = c(3000, 2000, 2000, 500))


#' @name get_att
#' @title Compute Average Treatment Effect on the Treated
#' @description Calculates the Average Treatment Effect on the Treated (ATT) by averaging individual treatment effects. Computes standard error, t-statistic, degrees of freedom, p-value, and significance stars.
#' @param effects tibble: Data frame containing treatment effects with columns diff (difference) and sediff (standard error of difference), typically from get_effects() or get_simeffects()
#' @return tibble: Single-row data frame containing ATT statistics: yhat1, yhat0, att (average treatment effect), se_att (standard error), t (t-statistic), df (degrees of freedom), p_value, stars (significance indicators)
#' @author Tim Fraser



#' @name get_pooled_sd
#' @title Pool Standard Deviations
#' @description 
#' Pools standard deviations.
#' If the sample sizes are equal, we can pool the standard deviations by averaging the variances,
#' @param sds:[dbl] vector: Vector of standard deviations
#' @param ns:[int] vector: Vector of sample sizes
#' @return numeric: Pooled standard deviation
#' @author Tim Fraser
get_pooled_sd = function(sds, ns){
  # Testing values
  # sds = c(0.5, 0.7, 0.3, 0.4)
  # ns = c(10000, 10000, 10000, 10000)
  
  # Do they have equal sample sizes?
  equal_sample_size = all(ns == ns[1])
  

  # The standard deviation of a sampling distribution is the standard error.

  
  # IF it has equal sample sizes ...
  if(equal_sample_size){
    # So, we can pool the standard deviations by averaging the variances,
    # and then taking the square root.
    # If equal sample sizes....
    varp = sum(sds^2) / length(ns)
    sdp = sqrt(varp)
    return(sdp)
  }else if(!equal_sample_size){
    # If sample sizes are not equal, we need to weight the variances by the sample sizes.
    varp = sum(sds^2 * (ns - 1)) / (sum(ns) - length(ns))
    sdp = sqrt(varp)
    return(sdp)
  }
}


# Let's just computationally handle this...

#' @name get_pooled_se_mc
#' @title Pool Standard Errors with Monte Carlo Simulation
#' @description 
#' Pools standard errors with Monte Carlo simulation.
#' A computational solution to the problem of pooling standard errors
#' when sample sizes are different.
#' @param ses:[dbl] vector: Vector of standard errors
#' @param ns:[int] vector: Vector of sample sizes
#' @return numeric: Pooled standard error
#' @author Tim Fraser
get_pooled_se_mc = function(ses, ns){
  # Testing values
  # ses = c(0.5, 0.7, 0.3, 0.4)
  # ns = c(3000, 2000, 2000, 500)
  
  output = tibble(id = 1:length(ses), ses = ses, ns = ns) %>%
    group_by(id) %>%
    # For each standard error, recreate the sampling distribution at the size of the sample of interest
    reframe(
      sim = rnorm(n = ns, mean = 0, sd = ses)
    ) %>%
    # The total distribution will be the full distribution of errors.
    # The pooled standard error is really trying to capture the average variation in this total distribution,
    # up or down weighting by sample size to make sure we don't give too little / too much weight to any on group.
    summarize(sd = sd(sim))
    # The standard deviation of a sampling distribution is a standard error.
    # Therefore, this is the pooled standard error, achieved with monte carlo simulation.

    # This technique hinges more on the shape of each simulated distribution than the typical strategy of averaging variances.
    return(output)
} 


#' @name get_pooled_se
#' @title Pool Standard Errors using Variances
#' @description Pools standard errors using variances and sample sizes.
#' This is the standard strategy for pooling standard errors.
#' @param ses:[dbl] vector: Vector of standard errors
#' @param ns:[int] vector: Vector of sample sizes
#' @return numeric: Pooled standard error
#' @author Tim Fraser
get_pooled_se = function(ses, ns){
#  Convert standard errors to variances, average them, and then take the square root

  # ses = c(0.5, 0.7, 0.3, 0.4)
  # ns = c(3000, 2000, 2000, 500)
  vars = ses*ns
  varp = sum(vars*ns-1)/(sum(ns) - length(ns))
  sep = sqrt(varp / sum(ns) )

  return(sep)
}

# This strategy is essentially the same as the pooled standard deviation.
# But it generally makes sense, looking at the input vector of standard errors.
# get_pooled_se_mc(ses = c(0.5, 0.7, 0.3, 0.4), ns = c(3000, 2000, 2000, 500))
# get_pooled_sd(sds = c(0.5, 0.7, 0.3, 0.4), ns = c(3000, 2000, 2000, 500))
# get_pooled_se(ses = c(0.5, 0.7, 0.3, 0.4), ns = c(3000, 2000, 2000, 500))


#' @name get_att
#' @title Compute Average Treatment Effect on the Treated
#' @description Calculates the Average Treatment Effect on the Treated (ATT) by averaging individual treatment effects. Computes standard error, t-statistic, degrees of freedom, p-value, and significance stars.
#' @param effects tibble: Data frame containing treatment effects with columns diff (difference) and sediff (standard error of difference), typically from get_effects() or get_simeffects()
#' @return tibble: Single-row data frame containing ATT statistics: yhat1, yhat0, att (average treatment effect), se_att (standard error), t (t-statistic), df (degrees of freedom), p_value, stars (significance indicators)
#' @author Tim Fraser
get_att = function(effects){

  # Testing values
  # effects = read_csv("qi_by_sensordate_obs.csv", show_col_types = FALSE) %>%  filter(area == "crz")

  output = effects %>%
    # Some of the values are non-estimatable because of missing factor levels (?)
    filter(!is.na(diff)) %>%

      # If any of the standard errors are zero, meaning that yhat1 is known exactly as an observed value,
      # We still need to calculate the average yhat1, but we need to adjust the formula,
      # since weighted average by standard errors will yield divide by zero errors.
    mutate(
      # So, we're going to... impute a relatively low standard error based on the distribution of standard errors
      # This is a bit of a hack, but it's a way to get around the divide by zero errors.
      # It will only come into play if there are any standard errors that are zero.
      # And it is, practically speaking, more conservative than the alternative of using the mean of the standard errors.
      # At least this way it uses the distribution of standard errors to inform the imputation,
      # and assumes that even the observed values probably have some error, just not enough to be zero.
      se1 = if_else(
        se1 == 0,
        true = quantile(se1[se1 > 0], prob = 0.1, na.rm = TRUE),
        false = se1) 
    ) %>%
    # Calculate AVERAGE treatment effect
    summarize(
      # For Treated values...
      # Average of the estimates
      # Previously, we used this...
      # yhat1 = mean(yhat1),
      # pool standard errors of the estimates
      # se1 = sqrt(  sum(se1^2) ) / n(),
      yhat1 = sum(yhat1 / se1^2) / sum(1/se1^2),
      # now get a new standard error for the average
      yhatse1 = sqrt(1 / sum(1/se1^2)),
      # Keep previous unweighted version for reference
      # results are pretty much identical
      yhat1b = mean(yhat1),
      yhatse1b = sqrt(  sum(se1^2) ) / n(),

      # For counterfactual values...
      # Previously, we used this...
      # Average of the estimates
      # yhat0 = mean(yhat0),
      # pool standard errors of the estimates
      # se0 = sqrt(  sum(se0^2) ) / n(),

      # But it is more accurate to weight by the standard errors
      # given that the standard errors vary much by sensor-date pair
      yhat0 = sum(yhat0 / se0^2) / sum(1/se0^2),
      # now get a new standard error for the average
      yhatse0 = sqrt(1 / sum(1/se0^2)),

      # Keep previous unweighted version for reference
      # results are pretty much identical
      yhat0b = mean(yhat0),
      yhatse0b = sqrt(  sum(se0^2) ) / n(),


      # Average of the differences
      att = mean(diff),
      # pool standard errors of the differences estimates
      # pool standard errors of the differences estimates
      se_att = sqrt(  sum(sediff^2) ) / n(),
      t = att / se_att,
      df = n() - 1,
      p_value = 2 * (1 - pt(q = abs(t), df = df)),
      stars = gtools::stars.pval(p_value),
      # In a few cases, the standard errors are zero, meaning that yhat1 is known exactly as an observed value.
      # This messes up the calculation of yhat1 in this stage, because we divide by the standard error.
      # It does not affect the calculation of yhat1 in previous steps, so att is unaffected.
      # We can calculate the correct yhat1 and correct percent change by subtracting the att.
      # Percentage change in PM2.5 concentration
      # Relies on yhat0 and att estimates
      percentchange = 1 - (yhat0 + att) / yhat0
    ) 

  return(output)
}


#' @name get_qis
#' @title Generate All Treatment Effect Estimates
#' @description Main function that generates comprehensive treatment effect estimates at multiple aggregation levels: overall, per area, per sensor, per week, and per month. Combines get_yhat(), get_simeffects(), and get_att() to produce the complete effects dataset.
#' @param m model object: Fitted linear model object (from lm() or similar)
#' @param areas character vector: Geographic areas to include in analysis (default: c("cbsa", "nyc", "crz"))
#' @param path_data character: Path to RDS file containing panel dataset (default: "../descriptives/panel_daily_nyc.rds")
#' @param start character: Start date of treatment period in "YYYY-MM-DD" format (default: "2025-01-05")
#' @param end character: End date of treatment period in "YYYY-MM-DD" format (default: "2025-06-01")
#' @return tibble: Data frame containing treatment effects with type column indicating aggregation level ("overall", "per_area", "per_sensor", "per_week", "per_month") and all ATT statistics
#' @author Tim Fraser
get_qis = function(m, areas = c("cbsa", "nyc", "crz"), path_data=  "../descriptives/panel_daily_nyc.rds", start = "2025-01-05", end = "2025-06-01"){
  
  # Testing values
  # m = read_rds("models.rds")[[1]]; areas = c("cbsa", "nyc", "crz"); path_data=  "../descriptives/panel_daily_nyc.rds"; start = "2025-01-05"; end = "2025-06-01"
  
  # Get predictions for treatment and counterfactual
  grid = get_yhat(m = m, path_data = path_data) %>% filter(area %in% areas)
  
  # Get (backtransformed) treatment effects on the treated
  # grid %>% get_simeffects(start = start, end = end)
  
  # Get (backtransformed) average treatment effects on the treated
  stat1 = grid %>% get_simeffects(start = start, end = end) %>% get_att()
  
  # Get ATT per area
  stat2 = grid %>% get_simeffects(start = start, end = end) %>% group_by(area) %>% get_att()
  
  # Get ATT per sensor
  stat3 = grid %>% get_simeffects(start = start, end = end) %>% group_by(aqs_id_full) %>% get_att()
  
  # Get ATT per week
  stat4 = grid %>% get_simeffects(start = start, end = end) %>% 
    mutate(week = lubridate::week(date)) %>%
    group_by(week) %>% get_att()
  
  # Get ATT per month
  stat5 = grid %>% get_simeffects(start = start, end = end) %>% 
    mutate(month = lubridate::month(date)) %>%
    group_by(month) %>% get_att()
  
  
  output = bind_rows(
    stat1 %>% mutate(type = "overall"),
    stat2 %>% mutate(type = "per_area"), 
    stat3 %>% mutate(type = "per_sensor"),
    stat4 %>% mutate(type = "per_week"),
    stat5 %>% mutate(type = "per_month")
  )
  
  return(output)
}

#' @name get_missing_obs
#' @title Get Missing Observations
#' @description Get the number of missing observations for the treated period
#' @param grid tibble: Data frame containing the grid of predictions
#' @return tibble: Data frame containing the number of missing observations for the treated period
#' @author Tim Fraser
get_missing_obs = function(grid){

  
  # Remind the reader how many observed values are available for the treated period
  stat = grid %>% 
      filter(treated == TRUE) %>% 
      summarize(
        treated = TRUE,
        date_min = min(date),
        date_max = max(date),
        n_missing = sum(is.na(value)),
        n_available = sum(!is.na(value)),
        n_total = n()
      )

  cat("\n", "Counting missing data for the treated period: ", stat$n_missing, "/", stat$n_total, " (", round(stat$n_missing / stat$n_total * 100, 2), "%)",
   "[", as.character(stat$date_min), " to ", as.character(stat$date_max), "]", "\n")
  
  return(stat)
}

#' @name get_qi_by_sensordate
#' @title Get QIs by Sensor-Date Pair
#' @description 
#' Get QIs by sensor-date pair for the CRZ, NYC, and CBSA models.
#' Includes treatment effects on the treated, counterfactual predictions, and observed values.
#' @param path_data character: Path to the panel daily NYC data
#' @param start character: Start date of the treatment period
#' @param end character: End date of the treatment period
#' @param useobs logical: Whether to use observed values (default: FALSE). The original study used predictions for the counterfactual and treated (so `useobs = FALSE`), then calculated the difference.
#' @param impute logical: Whether to impute cloudcover using a statistical model trained on temp, humidity, windspeed, precip, month, date, and county (default: FALSE)
#' @param controls logical: Whether to use the models with fully specified controls (TRUE) or basic controls (FALSE) (default: TRUE)
#' @return tibble: Data frame containing QIs by sensor-date pair
#' @author Tim Fraser
get_qi_by_sensordate = function(path_data =  "../descriptives/panel_daily_nyc.rds", start = "2025-01-05", end = "2025-06-01", useobs = FALSE, impute = FALSE, controls = TRUE){

  # Testing values
  # path_data =  "../descriptives/panel_daily_nyc.rds"; start = "2025-01-05"; end = "2025-06-01"; useobs = TRUE;setwd("C:/Users/tmf77/nyc_congestion_pricing/descriptives")

  # Requires working directory to be the descriptives folder
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(gtools)

  # source("00_functions.R")

  # Select the appropriate model for each area based on whether you want to use fully specified controls or basic controls
  if(controls == TRUE){ 
    modelid_crz = 9 
    modelid_nyc = 6 
    modelid_cbsa = 3 
  }else{ 
    modelid_crz = 7 
    modelid_nyc = 4 
    modelid_cbsa = 1 
  }
  
  # Use THIS MODEL for CRZ -----------------------
  m = read_rds("models.rds")[modelid_crz][[1]]
  areas = "crz"

  # Get predictions for treatment and counterfactual
  grid = get_yhat(m = m, path_data = path_data, useobs = useobs, impute = impute) %>% filter(area %in% areas)
  # Print the number of missing observations for the treated period
  if(useobs == TRUE){ get_missing_obs(grid) }
  # Get (backtransformed) outcomes and treatment effects on the treated
  stat1 = grid %>% get_simeffects(start = start, end = end) 

  # Use THIS MODEL for NYC -----------------------
  m = read_rds("models.rds")[modelid_nyc][[1]]
  areas = "nyc"
  # Get predictions for treatment and counterfactual
  grid = get_yhat(m = m, path_data = path_data, useobs = useobs, impute = impute) %>% filter(area %in% areas)
  # Print the number of missing observations for the treated period
  if(useobs == TRUE){ get_missing_obs(grid) }
  # Get (backtransformed) outcomes and treatment effects on the treated
  stat2 = grid %>% get_simeffects(start = start, end = end) 
  if(useobs == TRUE){ get_missing_obs(grid) }



  # Use THIS MODEL for CBSA -----------------------
  m = read_rds("models.rds")[modelid_cbsa][[1]]
  areas = "cbsa"
  # Get predictions for treatment and counterfactual
  grid = get_yhat(m = m, path_data = path_data, useobs = useobs, impute = impute) %>% filter(area %in% areas)  
  # Print the number of missing observations for the treated period
  if(useobs == TRUE){ get_missing_obs(grid) }
  # Get (backtransformed) outcomes and treatment effects on the treated
  stat3 = grid %>% get_simeffects(start = start, end = end) 

  # Bundle Results
  stat = bind_rows(
    stat1 %>% mutate(model = paste0("M", modelid_crz)), # CRZ
    stat2 %>% mutate(model = paste0("M", modelid_nyc)), # NYC
    stat3 %>% mutate(model = paste0("M", modelid_cbsa)) # CBSA
  ) %>%
    # Calculate percentage change in PM2.5 concentration
    mutate(percentchg = diff / yhat0) 

  # output = stat %>% 
  #   # Join in observed values, where available
  #   left_join(
  #     by = c("aqs_id_full", "date"),
  #     y = grid %>% select(date, aqs_id_full, observed = value, bgmean), relationship = "many-to-many") 

  return(stat)
}

# get_qi_by_sensordate(path_data =  "../descriptives/panel_daily_nyc.rds", start = "2025-01-05", end = "2025-06-01") %>%
#   write_csv("../descriptives/qi_by_sensordate.csv")


# 
# 
# get_diff = function(m, start = "2025-01-04", end = "2025-06-01", path_data=  "../descriptives/panel_daily_nyc.rds"){
#   
#   # Here's my data...
#   grid = read_rds(path_data) %>%
#     # Get base predictions for every point in the grid...
#     mutate(
#       predict(object = m, newdata = ., se.fit = TRUE) %>%
#         as_tibble() %>%
#         select(yhat = fit, se = se.fit)
#     ) %>%
#     select(date, treated, aqs_id_full, area, name, value, yhat, se) %>%
#     mutate(id = 1:n()) 
#   
#   
#   qis = grid %>%
#     # Now for each one,
#     group_by(id) %>%
#     reframe(
#       # simulate and back-transform from square root scale
#       ysim = rnorm(n = 1000, mean = yhat, sd = se)^2
#     ) %>%
#     group_by(id) %>%
#     summarize(
#       lower = quantile(ysim, prob = 0.025, na.rm = TRUE),
#       upper = quantile(ysim, prob = 0.975, na.rm = TRUE),
#       estimate = mean(ysim),
#       se = sd(ysim), 
#       .groups = "drop"
#     ) 
#   
#   manydiffs = qis %>%
#     # join in traits
#     left_join(by = "id", y = grid %>% select(id, date, treated, aqs_id_full, area, name)) %>%
#     # Create a comparison...
#     # Get the beginning and end of the treatment period
#     # filter(treated == TRUE) %>%
#     mutate(split = case_when(date == start ~ 0, date == end ~ 1, TRUE ~ NA)) %>%
#     filter(split %in% c(0,1)) %>%
#     group_by(area, name, aqs_id_full) %>%
#     reframe(
#       y0 = estimate[split == 0],
#       se0 = se[split == 0],
#       y1 = estimate[split == 1],
#       se1 = se[split == 1],
#     ) %>%
#     # Estimate the change between these two periods
#     mutate(
#       # actual change
#       diff = y1 - y0,
#       # estimate a z-statistic
#       z = (y1 - y0) / sqrt(se0 + se1),
#       # two-tailed p-value
#       p_value = 2 * (1 - pnorm(abs(z))),
#       # estimate a standard error
#       se = diff / z
#     )
#   
#   # Estimate the mean difference  
#   output = bind_rows(
#     manydiffs %>% mutate(area = "overall"),
#     manydiffs
#   ) %>%
#     group_by(area) %>%
#     summarize(
#       mu = mean(diff),
#       se_pooled = sqrt(  sum(se^2) / n()^2),
#       t = mu / se_pooled,
#       # grab all the differences, and subtract 1 (because this style of difference of means reduces the number of parameters)
#       df = n() - 1,
#       p_value = 2 * (1 - pt(q = abs(t), df = df)),
#       stars = gtools::stars.pval(p_value)
#     ) 
# 
#   return(output)
#   
# }
# 
# gc()
# read_rds("models.rds") %>% 
#   purrr::map_dfr(~get_diff(.x), .id = "model") %>%
#   saveRDS("effects.rds")


#' @name get_att_many
#' @title Get Many Average Treatment Effects on the Treated
#' @description Get many average treatment effects on the treated for the CRZ, NYC, and CBSA models
#' @param path_qi character: Path to the qi_by_sensordate.csv file
#' @param start character: Start date of the treatment period
#' @param end character: End date of the treatment period
#' @param controls logical: Whether to use the models with fully specified controls (TRUE) or basic controls (FALSE) (default: TRUE)
#' @return tibble: data frame containing the treatment effects on the treated
#' @author Tim Fraser
get_att_many = function(path_qi = "../descriptives/qi_by_sensordate.csv", start = "2025-01-05", end = "2025-06-01", controls = TRUE){
  
  cat("\n", "Calculating treatment effects on the treated...", "\n")

  # Select the appropriate model for each area based on whether you want to use fully specified controls or basic controls
  if(controls == TRUE){ modelid_crz = 9; modelid_nyc = 6; modelid_cbsa = 3 }else{ modelid_crz = 7; modelid_nyc = 4; modelid_cbsa = 1 }

  # Load in simulated effects for each sensor-date pair
  data = read_csv(path_qi, show_col_types = FALSE) %>%
     # Filter to stated time period
     filter(date >= start & date <= end)

  # CRZ
  att1 = data %>% 
    filter(area == "crz") %>%
    get_att()

  # NYC
  att2 = data %>% 
    filter(area == "nyc") %>%
    get_att()

  # CBSA
  att3 = data %>% 
    filter(area == "cbsa") %>%
    get_att()



  # Compilation of most appropriate values from entire area
  stat2 = bind_rows(
    data %>% 
      filter(model == paste0("M", modelid_crz), area == "crz"),
    data %>% 
      filter(model == paste0("M", modelid_nyc), area == "nyc"),
    data %>% 
      filter(model == paste0("M", modelid_cbsa), area == "cbsa")
  )

  # Overall estimate for entire NYC metro area, using best model for each relevant area
  att4 = stat2 %>% get_att()

  # County-specific estimates for entire NYC metro area, using best model for each given county
  # eg. Bronx, Queens, Kings, Richmond use NYC model, Manhattan uses CRZ model, others use CBSA model
  att5 = stat2 %>%
    group_by(name) %>%
    get_att()

  # Long Island counties
  att6 = stat2 %>%
    filter(name %in% c("Kings", "Queens", "Nassau", "Suffolk")) %>%
    get_att()

  # Long Island - just Nassau & Suffolk
  att7 = stat2 %>%
    filter(name %in% c("Nassau", "Suffolk")) %>%
    get_att()

  # Bundle all results
  myatt = bind_rows(
    att1 %>% mutate(area = "CRZ", model = paste0("M", modelid_crz)), 
    att2 %>% mutate(area = "NYC", model = paste0("M", modelid_nyc)), 
    att3 %>% mutate(area = "CBSA", model = paste0("M", modelid_cbsa)),
    att4 %>% mutate(area = "Overall", model = "Combined"),
    att5 %>% mutate(area = name, model = "Mixed"),
    att6 %>% mutate(area = "Long Island", model = "Mixed"),
    att7 %>% mutate(area = "Long Island (Nassau & Suffolk)", model = "M3")
  ) %>%
    select(
      area, model, att, se_att, stars, yhat1, yhatse1, yhat0, yhatse0, percentchange
    ) 
  return(myatt)
}

#' @name report_att
#' @title Report Average Treatment Effects on the Treated (ATT)
#' @description Report treatment effects for the CRZ, NYC, and CBSA models
#' @param path_att character: Path to the output file for the treatment effects
#' @param path_qi character: Path to the input file for the treatment effects
#' @param path_data character: Path to the data file
#' @param start character: Start date of the treatment period
#' @param end character: End date of the treatment period
#' @param useobs logical: Whether to use observed values (TRUE) or predicted values (FALSE) for the treated group when computing treatment effects
#' @return path to data frame containing the treatment effects
#' @author Tim Fraser
report_att = function(path_att = "../descriptives/att.csv", start = "2025-01-05", end = "2025-06-01", useobs = FALSE, impute = FALSE, path_qi = "../descriptives/qi_by_sensordate.csv", path_data =  "../descriptives/panel_daily_nyc.rds", controls = TRUE){
  # This function expects to be run from the descriptives folder
  # Testing values
  # path_att = "../descriptives/att_obs.csv"; start = "2025-01-05"; end = "2025-06-01"; useobs = TRUE; impute = TRUE; path_qi = "../descriptives/qi_by_sensordate_obs.csv"; path_data =  "../descriptives/panel_daily_nyc.rds"

  # read_rds("panel_daily_nyc.rds") %>% with(date) %>% range()

  # Repeat the simulation for just the final models, and return results and observed values for each sensor-date pair
  if(!file.exists(path_qi)){
    cat("\n", path_qi, " does not yet exist. Generating QIs by sensor-date pair...", "\n")
    get_qi_by_sensordate(path_data =  path_data, start = start, end = end, useobs = useobs, impute = impute, controls = controls) %>%
      write_csv(path_qi)
  }
  # read_csv("../descriptives/qi_by_sensordate.csv") %>% head()

  # What do these variables mean?
  # id - unique id for the sensor-date-pair, purely for simulation purposes
  # sediff - standard error of the difference between the treated and counterfactual predictions
  # diff - mean difference between the treated and counterfactual predictions
  # yhat1 - treated predicted value
  # yhat0 - counterfactual predicted value
  # se1 - standard error of the treated predicted value
  # se0 - standard error of the counterfactual predicted value (what would have happened if the treatment HAD NOT been implemented)
  # percentchg - percentage change in PM2.5 concentration compared to the counterfactual prediction
  # observed - observed value PM2.5 concentration (ug/m3)
  # bgmean - background PM2.5 estimates (eg. non-transportation related)

  output = get_att_many(path_qi = path_qi, start = start, end = end, controls = controls)

  output %>%
    write_csv(path_att)

    return(path_att)
}

#' @name get_att_by_time
#' @title Get Treatment Effects by Time
#' @description Get treatment effects by time
#' @param start:[date] vector of start dates
#' @param end:[date] vector of end dates
#' @param path_qi:character path to the qi_by_sensordate.csv file
#' @param controls logical: Whether to use the models with fully specified controls (TRUE) or basic controls (FALSE) (default: TRUE)
#' @return tibble: data frame containing the treatment effects by time
#' @author Tim Fraser
get_att_by_time = function(path_qi, start, end, controls = TRUE){

  # start = rep("2025-01-05", 5)
  # end = lubridate::make_date(year = 2025, month = 2:6, day = 1)

  times1 = tibble(
    start = start,
    end = end
  ) %>%
    mutate(id = 1:n()) 


  result1 = times1 %>%
    split(.$id) %>%
    map_dfr(~get_att_many(path_qi = path_qi, start = .x$start, end = .x$end, controls = controls), .id = "id")

  result1 = result1 %>%
    mutate(id = as.integer(id)) %>%
    left_join(y = times1, by = "id")

  return(result1)
}

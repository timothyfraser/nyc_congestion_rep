# FUNCTIONS ##########################################



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

tidier = function(m){
  # m = m[[1]]
  m %>% broom::tidy() %>%
    rename(se = std.error) %>%
    rename(p_value = p.value) %>%
    mutate(stars = gtools::stars.pval(p_value))

}
get_vif = function(m){
  myvif = car::vif(m)
  if(is.matrix(myvif)){
    tibble(term = rownames(myvif), vif = myvif[,3]^2)
  }else{ tibble(term = names(myvif), vif = myvif) }
  
}  

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

# Helper to convert significance symbols from HTML to LaTeX
sig_html_to_latex <- function(sig_html) {
  sig_html %>%
    str_replace_all("\\*\\*\\*", "\\$^{***}\\$") %>%
    str_replace_all("\\*\\*", "\\$^{**}\\$") %>%
    str_replace_all("\\*", "\\$^{*}\\$") %>%
    str_replace_all("\\.", "\\$^{\\cdot}\\$")
}

# Main function
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


get_yhat = function(m, path_data = "../descriptives/panel_daily_nyc.rds"){
  # To show treatment effects per site,
  # we need to use our model to predict 
  # what would our data look like WITH the treatment vs. WITHOUT
  grid = read_rds(path_data) %>%
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
  return(grid)
}

get_simeffects = function(grid, start = "2025-01-05", end = "2025-06-01"){
  
  grid %>%
    # Calculate treatment effects in the treated period
    filter(date >= start & date <= end) %>%
    # for each sensor-date pair,
    group_by(id) %>%
    # simulate (and back transform)
    reframe(
      ysim1 = rnorm(n = 1000, mean = yhat1, sd = se1)^2,
      ysim0 = rnorm(n = 1000, mean = yhat0, sd = se0)^2,
      diff = ysim1 - ysim0,
    ) %>%
    # Now return the mean estimated difference and the standard error of the difference
    group_by(id) %>%
    summarize(
      sediff = sd(diff),
      diff = mean(diff),
      yhat1 = mean(ysim1),
      yhat0 = mean(ysim0)
    ) %>%
    # Join back in key traits
    left_join(
      by = "id",
      y = grid %>%
        select(any_of(c("id", "date", "aqs_id_full", "treated", "area", "name")))
    )
  
  
}


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

get_att = function(effects){
  effects %>%
    # Some of the values are non-estimatable because of missing factor levels (?)
    filter(!is.na(diff)) %>%
    # Calculate AVERAGE treatment effect
    summarize(
      yhat1 = mean(yhat1),
      yhat0 = mean(yhat0),
      att = mean(diff),
      se_att = sqrt(  sum(sediff^2) ) / n(),
      t = att / se_att,
      df = n() - 1,
      p_value = 2 * (1 - pt(q = abs(t), df = df)),
      stars = gtools::stars.pval(p_value)
    )
}


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
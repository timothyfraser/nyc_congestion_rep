
# # Calculate the expected effect of increased overnight vehicle count on weekly effects
# # Calculate the expected effect of decreased peak vehicle count on weekly effects
# 
# data %>%
#   pivot_wider(id_cols = c(vehicle_class, week, att), names_from = time_period, values_from = crz_entries) %>%
#   group_by(vehicle_class) %>%
#   reframe(
#     lm(formula = att ~ log(Overnight) + log(Peak)) %>% broom::glance()
#   )
# 
# 
# sims = data %>% 
#   group_by(time_period, vehicle_class) %>%
#   reframe( 
#     {
#       m = lm(formula = att ~ log(crz_entries) )
#       tibble(
#         crz_entries = c(
#           median(crz_entries),
#           if_else(time_period == "Overnight", 
#                   # If overnight, increase
#                   true = median(crz_entries) + 1  ,
#                   # If peak, decrease,
#                   false = median(crz_entries) - 1)
#         )
#       ) %>%
#         # Predict average weekly change based on a 100 car decrease
#         mutate(predict( object = m, newdata = ., se.fit = TRUE) %>% 
#                  as_tibble() %>% select(yhat = 1, se = 2))
#     }
#   ) %>%
#   group_by(time_period, vehicle_class) %>%
#   mutate(id = 1:n()) %>%
#   group_by(time_period, vehicle_class, id) %>%
#   reframe(
#     # Simulate a backtransform
#     ysim = rnorm(n = 1000, mean = yhat, sd = se)
#   ) %>%
#   group_by(time_period, vehicle_class) %>%
#   reframe(
#     ysim1 = ysim[id == 2],
#     ysim0 = ysim[id == 1],
#     diff = ysim1 - ysim0
#   ) %>%
#   group_by(time_period, vehicle_class) %>%
#   summarize(
#     estimate = mean(diff),
#     se = sd(diff),
#     z = estimate / se,
#     p_value = 2 * (1 - pnorm(q = abs(z))),
#     stars = gtools::stars.pval(p_value)
#   )
# 
# sims
# 
# 
# 
# sims = data %>%
#   pivot_wider(id_cols = c(vehicle_class, week, att), names_from = time_period, values_from = crz_entries) %>%
#   group_by(vehicle_class) %>%
#   reframe( 
#     {
#       m = lm(formula = att ~ log(Overnight) + log(Peak) )
#       newdata = bind_rows(
#         tibble(
#           id = 1,
#           Overnight = Overnight,
#           Peak = Peak
#         ),
#         # Suppose for that week, we added...
#         # Overnight is c(21,22,23,24,1,2,3,4) = 8 hours
#         # Peak is c(5:20) = 16 hours
#         # Suppose we remove 1 car per hour during peak and have them do their traffic Overnight
#         tibble(
#           id = 1,
#           Overnight = Overnight + 1*16*7,
#           Peak = Peak - 1*16*7
#         )
#       )
#       output = newdata %>% 
#         mutate(predict(object = m, newdata = newdata, se.fit = TRUE) %>%
#                  as_tibble() %>% select(yhat = 1, se = 2))
#       output
#     }
#   ) %>%
#   group_by(vehicle_class) %>%
#   mutate(id = 1:n()) %>%
#   group_by(vehicle_class, id) %>%
#   reframe(
#     # Simulate a backtransform
#     ysim = rnorm(n = 1000, mean = yhat, sd = se)
#   ) %>%
#   group_by(vehicle_class) %>%
#   reframe(
#     ysim1 = ysim[id == 2],
#     ysim0 = ysim[id == 1],
#     diff = ysim1 - ysim0
#   ) %>%
#   group_by(vehicle_class) %>%
#   summarize(
#     estimate = mean(diff),
#     se = sd(diff),
#     z = estimate / se,
#     p_value = 2 * (1 - pnorm(q = abs(z))),
#     stars = gtools::stars.pval(p_value)
#   )
# 
# 
# sims
# 
# 
# 
# 
# 
# # Reduction in air quality was closely associated with 
# 
# 
# 
# # Tile plot ###############################################################
# 
# tiles = data %>%
#   pivot_wider(id_cols = c(vehicle_class, week, att), names_from = time_period, values_from = crz_entries) %>%
#   group_by(vehicle_class) %>%
#   reframe(
#     {m  = lm(formula = att ~ log(Overnight) + log(Peak))
#     
#     d = expand_grid(
#       Overnight = seq(from = min(Overnight, na.rm = TRUE),
#                       to = max(Overnight, na.rm = TRUE), length.out = 10),
#       Peak = seq(from = min(Peak, na.rm = TRUE),
#                  to = max(Peak, na.rm = TRUE), length.out = 10)
#     )
#     d %>%
#       mutate(yhat = predict(object = m, newdata = d))
#     }
#   )
# 
# ggplot() +
#   geom_tile(
#     data = tiles, mapping = aes(x = factor(Peak), y = factor(Overnight), fill = yhat)
#   ) +
#   scale_fill_gradient2(low = "#648FFF", midpoint = 0, mid = "white", high = "#DC267F")  +
#   geom_contour(
#     data = tiles, mapping = aes(x = factor(Peak), y = factor(Overnight), z = yhat, group = vehicle_class)
#   ) +
#   #scale_y_discrete(labels = scales::label_number(scale_cut = scales::cut_si(""))) +
#   #  scale_x_continuous(trans = "log") +
#   facet_wrap(~vehicle_class, scales = "free")
# 
# ggplot() +
#   geom_tile(
#     data = tiles, mapping = aes(x = Peak, y = Overnight, fill = yhat)
#   ) +
#   scale_y_continuous(trans = "log") +
#   scale_x_continuous(trans = "log") +
#   facet_wrap(~vehicle_class, scales = "free")
# 
# ggplot() +
#   geom_contour(
#     data = tiles, mapping = aes(x = Peak, y = Overnight, z = yhat)
#   ) +
#   facet_wrap(~vehicle_class, scales = "free")
# 
# 
# 
# ggplot() +
#   geom_point(
#     data = data %>% pivot_wider(id_cols = c(vehicle_class, week, att), names_from = time_period, values_from = crz_entries),
#     mapping = aes(x = Overnight, y = Peak, fill = att, color = att),
#     size = 3
#   ) +
#   facet_wrap(~vehicle_class, scales = "free")
# 
# 


# Hourly Vehicle Activity ##############################
# 
# 
# library(dplyr)
# library(readr)
# library(tidyr)
# library(purrr)
# library(stringr)
# library(ggplot2)
# library(viridis)
# library(sf)
# library(ggtext)
# library(shadowtext)
# 
# setwd(paste0(rstudioapi::getActiveProject(), "/descriptives"))
# 
# data = read_rds("../data/zone_vehicle_entries.rds") %>%
#   filter(time_period == 'Peak') %>%
#   select(datetime = toll_10min_block, vehicle_class, detection_group, detection_region, time_period, crz_entries) %>%
#   mutate(date = lubridate::floor_date(datetime, unit = 'days')) %>%
#   group_by(date, vehicle_class) %>%
#   summarize(crz_entries = sum(crz_entries, na.rm = TRUE), .groups = "drop")  %>%
#   left_join(
#     by = c("date"),
#     y = read_rds("../descriptives/panel_daily_nyc.rds") %>%
#       filter(within == 1) %>% 
#       select(date, aqs_id_full, value, bgmean, distmin) %>%
#       group_by(date) %>%
#       summarize(value = mean(value, na.rm = TRUE),
#                 bgmean = mean(bgmean, na.rm = TRUE),
#                 distmin = mean(distmin, na.rm = TRUE))
#   ) %>%
#   mutate(week = lubridate::week(date),
#          day = lubridate::wday(date, week_start = 1))
# 
# data %>%
#   split(.$vehicle_class) %>%
#   map_dfr(~lm(formula = sqrt(value) ~ log(crz_entries + 1) + 
#                 log(distmin + 1) + sqrt(bgmean) + 
#                 factor(week) + factor(day),  data = .x) %>% broom::tidy(), .id = "vehicle_class") %>%
#   group_by(vehicle_class) %>%
#   filter(term == "log(crz_entries + 1)")
# 
# 
# data %>%
#   group_by(vehicle_class) %>%
#   summarize(
#     r = cor(x = log(crz_entries + 1), y = value - bgmean, use = "pairwise.complete.obs")
#   )
# 
# 



# 
# ids = (read_rds("../data/sites.rds") %>%
#          select(aqs_id_full, within) %>%
#          filter(within == 1) %>% as_tibble() %>% with(aqs_id_full))
# 
# read_rds("../data/zone_vehicle_entries.rds") %>%
#   filter(time_period == 'Peak') %>%
#   select(datetime = toll_10min_block, vehicle_class, detection_group, detection_region, time_period, crz_entries) %>%
#   mutate(hour = lubridate::floor_date(datetime, unit = 'hours')) %>%
#   mutate(hour = lubridate::hour(hour)) %>%
#   select(hour) %>%
#   distinct()

# 
# # REMEMBER UTC - so I think you
# 
# # show vehicle time trend
# data = read_rds("../data/zone_vehicle_entries.rds") %>%
#   filter(time_period == 'Peak') %>%
#   select(datetime = toll_10min_block, vehicle_class, detection_group, detection_region, time_period, crz_entries) %>%
#   mutate(datetime = lubridate::floor_date(datetime, unit = 'hours')) %>%
#   group_by(datetime, vehicle_class) %>%
#   summarize(crz_entries = sum(crz_entries, na.rm = TRUE), .groups = "drop") %>%
#   # join in average hourly concentration within CRZ sites
#   left_join(
#     by = c("datetime" = "datetime"), 
#     y = read_csv("../data/air_quality.csv") %>%
#       filter(datetime > "2024-01-01") %>%
#       filter(!is.na(value) & value > 0) %>%
#       filter(unit == "UG/M3" & pollutant %in% c("PM2.5", "PM25") ) %>%
#       filter(aqs_id_full %in%  ids) %>%
#       mutate(hour = datetime %>% lubridate::hour()) %>%
#       # Filter to peak
#       #filter(hour %in% 5:20) %>%
#       group_by(datetime) %>%
#       summarize(value = mean(value, na.rm = TRUE))
#   ) %>%
#   mutate(hour = lubridate::hour(datetime),
#          month = lubridate::month(datetime))
# 
# ggplot() +
#   geom_point(
#     data = data,
#     mapping = aes(x = crz_entries, y = value)
#   ) +
#   facet_wrap(~vehicle_class, scales = "free")
# 
# 
# # data %>% 
# #   split(.$vehicle_class) %>%
# #   purrr::map_dfr(~lm(formula = value ~ crz_entries + factor(hour), data = .x) %>% broom::tidy())
# 
# 
# data %>%
#   group_by(vehicle_class) %>%
#   summarize(r = cor(x = log(crz_entries + 1), y = sqrt(value), use = "pairwise.complete.obs"))
# 
# 

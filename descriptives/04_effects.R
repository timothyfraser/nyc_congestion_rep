# 04_effects.R

# visualize and report our key effects

## Bar plot of ATT Effects  #################################

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(viridis)
library(sf)
library(ggtext)
library(shadowtext)
library(ggpubr)

setwd(paste0(rstudioapi::getActiveProject(), "/descriptives"))

# Overall effects
e = read_csv("effects.csv") %>%
  mutate(lower = att - se_att * qnorm(0.975),
         upper = att + se_att * qnorm(0.975)) %>%
  filter(type == "overall") %>%
  mutate(area = stringr::str_remove(model, pattern = "[0-9]{1}")) %>%
  mutate(area = area %>% dplyr::recode_factor(
    "cbsa" = "<b>New York City Metropolitan Area</b><br>a.k.a. <i>Core-Based Statistical Area</i> (CBSA)",
    "nyc" = "<b>New York City 5 Boroughs</b> (NYC)",
    "crz" = "<b>Congestion Relief Zone</b> (CRZ)"    
  )) %>%
  mutate(spec = spec %>% dplyr::recode_factor(
    "3" = "+Demographics",
    "2" = "+Weather",
    "1" = "Basic Model"
  )) %>%
  mutate(label = paste0(scales::number(att, accuracy = 0.1), stars))


gg = ggplot() +
  geom_col(
    data = e,
    mapping = aes(x = spec, y = att, group = model, fill = att)
  ) +
  geom_linerange(
    data = e,
    mapping = aes(x = spec, ymin = lower, ymax = upper, group = model)
  ) +
  geom_shadowtext(
    data = e,
    mapping = aes(
      x = spec, y = att, label = label, color = att
    ), nudge_x = 0.25, nudge_y = -0.1,
    bg.r = 0.1, bg.color = "white", fontface = "bold"
  ) +
  facet_wrap(~area, scales = "free_y", ncol = 1) +
  coord_flip() +
  labs(y = "<b>Average Treatment Effect on the Treatment Group (ATT)</b><br>Change in PM<sub>2.5</sub></b> in μg/m<sup>3</sup> with 95% Confidence Intervals</i>",
       x = "Model") +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_markdown(size = 16),
    plot.subtitle = element_markdown(size = 14),
    strip.text = ggtext::element_markdown(size = 12, color = "white"),
    legend.box = "vertical",
    legend.position = "bottom",
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_markdown(size = 12),
    strip.background = element_rect(fill = "#373737"),
    axis.title.x = element_markdown(size = 12)
  ) +
  guides(color = "none") +
  guides(fill = guide_colorsteps(show.limits = TRUE, barwidth = 20, barheight = 1)) +
  scale_color_gradient2(low = "#648FFF", high = "#DC267F", mid = "darkgrey", midpoint = 0, limits = c(-4, 4)) +
  scale_fill_gradient2(low = "#648FFF", high = "#DC267F", mid = "white", midpoint = 0, limits = c(-4, 4)) +
  scale_y_continuous(expand = expansion(c(0.1,0))) +
  labs(  fill = "<b>Change in PM<sub>2.5</sub></b><br><i>Δμg/m<sup>3</sup>",
         title = "<b>Average Policy Effects on NYC Air Quality</b>",
         subtitle = "Jan 6, 2025 - June 1, 2025")


ggsave(plot = gg, filename = "../descriptives/fig_att.png", dpi = 500, width = 7, height = 8)  
browseURL("../descriptives/fig_att.png")


## Weekly Effects #####################################
# Show me the predicted overall effects
# read_csv("effects.csv") %>% 
#   filter(type == "overall") 

e = read_csv("effects.csv") %>%
  mutate(lower = att - se_att * qnorm(0.975),
         upper = att + se_att * qnorm(0.975)) %>%
  filter(model %in% c("cbsa3", "nyc3", "crz3")) %>%
  filter(type == "per_week") %>%
  mutate(area = stringr::str_remove(model, pattern = "[0-9]{1}")) %>%
  mutate(area = area %>% dplyr::recode_factor(
    "cbsa" = "<b>New York City Metropolitan Area</b><br>a.k.a. <i>Core-Based Statistical Area</i> (CBSA)",
    "nyc" = "<b>New York City 5 Boroughs</b> (NYC)",
    "crz" = "<b>Congestion Relief Zone</b> (CRZ)"    
  )) %>%
  mutate(spec = spec %>% dplyr::recode_factor(
    "3" = "+Demographics",
    "2" = "+Weather",
    "1" = "Basic Model"
  )) %>%
  mutate(label = paste0(scales::number(att, accuracy = 0.1), stars))

l = tibble(
  model = c("cbsa3", "nyc3", "crz3"),
  label = c("NYC Metropolitan Area (CBSA)", "NYC 5 Boroughs", "Congestion Relief Zone (CRZ)"),
  x = c(2.5,     10,  5),
  y = c(-0.25,  -1.5, -4 )
)

# Effect is increasing over time (though that could just be seasonal)
gg = ggplot() +
  geom_ribbon(
    data = e,
    mapping = aes(
      x = week,
      #y = att,
      ymin = lower,
      ymax = upper,
      group = model,
      fill = model
    ),
    alpha = 0.5
  ) +
  geom_shadowtext(
    data = e %>% filter(week %in% c(1, 5, 10,15, 20, max(week) )),
    mapping = aes(
      x = week,
      y = att,
      label = label,
      color = model
    ),
    bg.r = 0.1,
    bg.color = "white", check_overlap = TRUE
  ) +
  geom_shadowtext(
    data = l,
    mapping = aes(
      x = x,
      y = y,
      label = label,
      color = model
    ),
    bg.r = 0.1,
    bg.color = "white", check_overlap = TRUE,
    fontface = "bold", size = 5, hjust = 0
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "darkgrey") +
  scale_fill_manual(
    breaks = c("crz3", "nyc3", "cbsa3"),
    values = c("#648FFF",  "#FE6100", "#FFB000")
  ) +
  scale_color_manual(
    breaks = c("crz3", "nyc3", "cbsa3"),
    values = c("#648FFF",  "#FE6100", "#FFB000"),
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  theme(
    panel.border = element_rect(fill = NA, color = "#373737"),
    axis.ticks = element_blank(),
    axis.title.x = element_markdown(size = 12),
    axis.title.y = element_markdown(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_markdown(size = 16),
    plot.subtitle = element_markdown(size = 14)
  ) +
  labs(title = "<b>Weekly Average Treatment Effects <br>on Air Quality</b>",
       subtitle = "January 6, 2025 to June 1, 2025",
       y = "Weekly Average Treatment Effects on PM<sub>2.5</sub><br>as 95% Confidence Intervals",
       x = "Weeks since Congestion Relief Policy Started")

ggsave(plot = gg, filename = "../descriptives/fig_att_weekly.png", dpi = 500, width = 6, height = 7)
browseURL("../descriptives/fig_att_weekly.png")



# Vehicles

# show vehicle time trend
data = read_rds("../data/zone_vehicle_entries.rds") %>%
  select(datetime = toll_10min_block, vehicle_class, detection_group, detection_region, time_period, crz_entries) %>%
  mutate(week = lubridate::floor_date(datetime, unit = 'weeks')) %>%
  group_by(week, vehicle_class) %>%
  summarize(crz_entries = sum(crz_entries, na.rm = TRUE), .groups = "drop")

gg2 = ggplot() +
  geom_line(data = data, position = "identity",
            mapping = aes(x = week, y = crz_entries, group = vehicle_class),
            color = "#648FFF",
            linewidth = 1.5) +
  scale_y_continuous(
    trans = "log", 
    #breaks = c(1, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000, 1000000, 3000000),                 
    labels = scales::label_number(accuracy = 1, scale_cut = scales::cut_si(""))) +
  facet_wrap(~vehicle_class, scales = "free_y") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  theme(
    panel.border = element_rect(fill = NA, color = "#373737"),
    strip.background = element_rect(fill = "#373737"),
    strip.text = element_text(color = "white", size = 9),
    axis.ticks = element_blank(),
    axis.title.x = element_markdown(size = 12),
    axis.title.y = element_markdown(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_markdown(size = 16),
    plot.subtitle = element_markdown(size = 14)
  ) +
  labs(title = "<b>Weekly Vehicle Activity in Congestion Relief Zone</b>",
       subtitle = "January 6, 2025 to June 1, 2025",
       y = "Weekly Vehicle Entries",
       x = "Congestion Relief Policy Period (weeks)")

ggcombo = ggarrange(
  plotlist = list(gg, gg2), ncol =2, widths = c(0.4, 0.6), labels = c("A", "B")
)  

ggsave(ggcombo, filename = "../descriptives/fig_weekly_vehicles.png", dpi = 500, width = 12, height = 6)

browseURL("../descriptives/fig_weekly_vehicles.png")



# Relationship between Weekly Vehicle Entries and Change in PM2.5 ######

data = read_rds("../data/zone_vehicle_entries.rds") %>%
  select(datetime = toll_10min_block, vehicle_class, detection_group, detection_region, time_period, crz_entries) %>%
  mutate(week = lubridate::floor_date(datetime, unit = 'weeks')) %>%
  group_by(time_period, week, vehicle_class) %>%
  summarize(crz_entries = sum(crz_entries, na.rm = TRUE), .groups = "drop") %>%
  mutate(week = lubridate::week(week)) %>%
  # Join in weekly effects
  left_join(by = "week",
            y = read_csv("effects.csv", show_col_types = FALSE) %>%
              filter(type == "per_week") %>%
              filter(model == "crz3") %>%
              select(model, week, att, se_att)
  ) %>%
  # Calculate the hourly rate of entries
  mutate(
    crz_entries = case_when(
      # if overnight, 8 hours for 7 days
      time_period == "Overnight" ~ crz_entries / (8 * 7),
      time_period == "Peak" ~ crz_entries / (16 * 7)
    )
  ) %>%
  mutate(time_period = factor(time_period, levels = c("Peak", "Overnight")))

# Correlations
stat = data %>% 
  group_by(time_period, vehicle_class) %>%
  reframe(
    lm(formula = att ~ log(crz_entries) ) %>% broom::glance()  %>% select(rsq = r.squared, nobs),
    r = cor(x = att, y = log(crz_entries), use = "pairwise.complete.obs"),
    xmin = min(crz_entries),
    xmax = max(crz_entries),
    ymax = max(att),
    lm(formula = att ~ log(crz_entries) ) %>% broom::tidy()  %>% 
      filter(term == "log(crz_entries)") %>%
      select(estimate, se = std.error, p_value = p.value)
  ) %>%
  mutate(rsq = scales::percent(rsq, accuracy = 1),
         r = scales::number(r, accuracy = 0.01),
         label = paste0(
           scales::number(estimate, accuracy = 0.01, style_positive = "plus", style_negative = "minus"),
           gtools::stars.pval(p_value)
           ) 
  )


# What are the demonstrated associations between vehicle activity and aqi?
gg3 = ggplot() +
  geom_point(
    data = data,
    mapping = aes(x = crz_entries, y = att),
    color = "#648FFF", size = 2
  ) +
  geom_smooth(
    data = data,
    mapping = aes(x = crz_entries, y = att, group = time_period),
    method = "lm", formula = y ~ log(x), color = "white", fill = "#648FFF33") +
  geom_shadowtext(
    data = stat, mapping = aes(x = xmin, y = -5, label = paste0("r = ", r)), 
    hjust = 0, bg.color = "white", color = "#373737", bg.r = 0.2, vjust = 0
  ) +
  geom_shadowtext(
    data = stat, mapping = aes(x = xmax, y = 0, label = paste0("β = ", label)), 
    hjust = 1, bg.color = "white", color = "#373737", bg.r = 0.2, vjust = 1
  ) +
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si(""))) +
  scale_y_continuous(limits = c(-5,0), oob = scales::squish) +
  facet_wrap(time_period~vehicle_class, nrow = 2, scales = "free_x") +
  theme_bw(base_size = 14) +
  theme(panel.border = element_rect(fill = NA, color = "#373737"),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white", size = 9),
        panel.grid = element_blank(), axis.ticks = element_blank(),
        axis.title.x = element_markdown(size = 12),
        axis.title.y = element_markdown(size = 12),
        axis.text.x = element_text(size = 8),
        plot.title = element_markdown(size = 16),
        plot.subtitle = element_markdown(size = 14)
        ) +
  labs(
    x = "Hourly Rate of Vehicle Entries each Week (n = 20 weeks)",
    y = "Weekly Average Change in PM<sub>2.5</sub>",
    title = "<b>Impacts of Vehicle Entries to Congestion Relief Zone on Air Pollution</b>",
    subtitle = "Janury 6, 2025 to June 1, 2025"
  )
ggsave(plot = gg3, filename = "fig_att_vehicle_entries.png", dpi = 500, width = 12, height = 6) 
browseURL("fig_att_vehicle_entries.png")






ggcombo = ggarrange(
  plotlist = list(gg, gg2), ncol =2, widths = c(0.4, 0.6), labels = c("A", "B")
)  
ggall = ggarrange(
  plotlist = list(ggcombo, gg3), nrow =2, labels = c("", "C")
)  

ggsave(ggall, filename = "../descriptives/fig_att_vehicle_entries_all.png", dpi = 500, width = 12, height = 12)

browseURL("../descriptives/fig_att_vehicle_entries_all.png")

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

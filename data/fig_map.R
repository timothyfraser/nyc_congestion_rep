

# map - study region ##################################
counties = read_rds("data/counties.rds")
bbox = read_rds("data/bbox.rds")
gg = ggplot() +
  geom_sf(data = bbox, fill = "pink", alpha = 0.5) +
  geom_sf(data = counties, mapping = aes(fill = state), color = "#373737") +
  geom_sf_label(data = counties, mapping = aes(label = name), size = 2) +
  theme_classic() +
  theme(legend.position = "bottom") 
ggsave(plot = gg, filename = "data/study_region.png", dpi = 300, width = 5, height = 5)
browseURL("data/study_region.png")

# map - sensors ######################3
gg = ggplot() + 
  geom_sf(data = read_rds("data/bbox.rds"), fill = "pink", alpha = 0.5) +
  geom_sf(data = read_rds("data/counties.rds"), fill = "red4", alpha = 0.5, color = "white") +
  geom_sf(data = read_rds("data/zone.rds"), fill = "red") +
  geom_sf(data = read_rds("data/sites.rds")) +
  theme_void(base_size = 14) 
ggsave(gg, filename = "data/map_sensors.png")

# 03_maps.R

# Make maps of treatment effects

library(dplyr)
library(readr)
library(stringr)
library(sf)
library(viridis)
library(ggspatial)
library(ggpubr)
library(lubridate)
library(ggtext)

setwd(paste0(rstudioapi::getActiveProject(), "/descriptives"))
source("00_functions.R")

### CRZ #################################

# Create a bounding box around the congestion pricing zone
z = read_rds("../data/zone.rds")
# Get bounding box extent
b = st_bbox(z)
# Formalize the bounding box as a polygon
bbox = b %>% st_as_sfc() %>% tibble(geometry = .) %>% st_as_sf(crs = 4326)

# Load in water data and crop to bounding box
w = read_sf("../data/water.geojson") %>%
  st_crop(y = bbox)

# Load in county metadata
g = read_csv("../data/metro.csv") %>% 
  filter(county %in% c("Kings County", "Queens County", "New York County", 
                       "Bronx County", "Richmond County", "Hudson County"))

# Filter roads to just roads in these counties, and crop
r = read_rds("../data/roads.rds") %>%
  filter(geoid %in% g$geoid) %>%
  st_crop(y = bbox)

# Filter highways to just highways in the bounding box and crop
h = read_rds("../data/highways.rds") %>%
  st_crop(y = bbox)

# Load in AQI sites  
s = read_rds("../data/sites.rds") %>% select(aqs_id_full, geometry)

# Get treatment effects, filting to the right set
e = read_csv("effects.csv") %>%
  # treatment effects per sensor
  filter(type == "per_sensor") %>%
  # fully specified model
  filter(spec == 3)  %>%
  # congetsion relief zone model 3
  filter(model == "crz3") %>%
  # Join in polygons
  inner_join(by = "aqs_id_full", y = s) %>%
  st_as_sf(crs = 4326) %>%
  # Format labels
  mutate(label = scales::number(att, accuracy = 0.1, style_positive = "plus", style_negative = "minus")) %>%
  mutate(st_coordinates(geometry) %>% as_tibble() %>% select(x = 1, y = 2))

# Load in county shapefiles, and crop to bounding box
c = read_rds("../data/counties.rds") %>%
  filter(geoid %in% g$geoid) %>%
  st_crop(y = bbox)

# Load in census block groups by population density
bg = read_sf("../data/bg.geojson") %>%
  filter(county %in% g$geoid) %>%
  st_transform(crs = 4326) %>%
  st_crop(y = bbox) %>%
  left_join(by = c("geoid" = "geoid"),
            y = read_csv("../data/census.csv") %>%
              mutate(geoid = stringr::str_pad(geoid, width = 12, side = "left", pad = "0")) %>%
              filter(year == max(year, na.rm = TRUE)) %>%
              select(geoid, total_population)) %>%
  # Get population density per sq.km.
  mutate(pop_density = total_population / (area_land / 1e6)) %>%
  select(geoid, pop_density, area_land, geometry) %>%
  # filter out the ones with no land area
  filter(area_land > 0) 


# water_color = "#0047FF"
# water_color = "#648FFF"
water_color = "#B3C8FF"
# CRZ visual
gg1 = ggplot() +
  # Plot county outlines
  geom_sf(data = c, fill = NA, color = "#373737")  +
  # Plot blockgroups
  geom_sf(data = bg, mapping = aes(fill = pop_density), color = "lightgrey", linewidth = 0.05) +
  # Plot water area
  geom_sf(data = w, fill = water_color, color = "#648FFF", linewidth = 1) +
  # Plot congestion pricing zone
  geom_sf(data = z, fill = NA, mapping = aes(linetype = "Congestion Relief Zone"), 
          color = "#373737", linewidth = 2) +
  # Plot roads
  geom_sf(data = r, color = "grey", linewidth = 0.1) +
  # Plot highways
  geom_sf(data = h, color = "black", linewidth = 1.5) +
  geom_sf(data = h, color = "white", linewidth = 0.5) +
  # Plot sites (white background layer)
  geom_sf(data = s, size = 15, color = "white") +
  # Plot sites (white background layer)
  geom_sf(data = s, size = 13, color = "grey") +
  # Plot sites by treatment effect
  geom_sf(data = e, size = 13, 
          mapping = aes(color = att)) +
  # Plot sites (white background layer)
  # geom_sf(data = s, size = 13, color = "white") +
  # Plot effect size by treatment effect
  shadowtext::geom_shadowtext(
    data = e, 
    mapping = aes(
      x = x,
      y = y,
      color = att, label = label), size = 6, bg.r = 0.1, bg.color = "white",
      fontface = "bold") +
  #geom_sf_text(data = e, mapping = aes(label = label), size = 5) +
  # Crop visual extent to bounding box
  coord_sf(xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")], clip = "on", expand = FALSE)


gg1 = gg1 +
  # Add theming
  theme_void(base_size = 14) +
  # Put fill legend on bottom
  theme(legend.position = "bottom", legend.box = "vertical", panel.border = element_rect(color = "#373737", fill = NA, linewidth = 1)) +
  guides(color = guide_colorsteps(barwidth = 20, barheight = 1, show.limits = TRUE),
         fill = guide_colorsteps(barwidth = 20, barheight = 1, show.limits = TRUE)) +
  # Add color scale for population density
  scale_fill_gradient(
    low = "white", high = "#FFB000", na.value = "#373737", trans = "log", 
    labels = scales::label_number(accuracy = 0.1, scale_cut = scales::cut_si(""))) +
  scale_color_gradient2(
    low = "#648FFF", 
    mid = "white", midpoint = 0,                   
    high = "#DC267F", 
    breaks = seq(from = -4, to = 4, by = 1),
    limits = c(-4, 4)) +
  labs(fill = "<b>Population Density</b><br><i>residents/km<sup>2</sup></i>",
       color = "<b>Change in PM<sub>2.5</sub></b><br><i>Δμg/m<sup>3</sup>",
       title = "<b>Policy Effects on Air Quality per Sensor</b><br>in Congestion Relief Zone",
       subtitle = "Jan 6, 2025 - June 1, 2025",
       linetype = NULL) +
  theme(legend.title = ggtext::element_markdown(size = 12),
        plot.title = ggtext::element_markdown(size = 16),
        plot.subtitle = ggtext::element_markdown(size = 14)) +
  ggspatial::annotation_scale()


# Color scheme
# - water: blue
# - roads: white
# - highways: white, black outline
# - pop density: white-to-yellow

ggsave(filename = "descriptives/fig_map_att_crz.png", plot = gg1, dpi = 500, width = 6.5, height = 8)

browseURL("descriptives/fig_map_att_crz.png")



# NYC ######################################################

# Get congestion pricing zone
z = read_rds("../data/zone.rds")
# Create a bounding box around the 5 boroughs
m = read_rds("../data/counties.rds") %>%
  filter(name %in% c("Kings", "Queens", "New York", 
                     "Bronx", "Richmond"))
# Get bounding box extent
b = st_bbox(m)
# Formalize the bounding box as a polygon
bbox = b %>% st_as_sfc() %>% tibble(geometry = .) %>% st_as_sf(crs = 4326)

# Load in water data and crop to bounding box
w = read_sf("../data/water.geojson") %>%
  st_crop(y = bbox)

superw = w %>% st_collection_extract("POLYGON") %>% summarize(geometry = st_union(geometry))

# Load in county metadata
g = read_csv("../data/metro.csv") %>% 
  filter(county %in% c("Kings County", "Queens County", "New York County", 
                       "Bronx County", "Richmond County", "Hudson County"))

# Filter roads to just roads in these counties, and crop
r = read_rds("../data/roads.rds") %>%
  filter(geoid %in% g$geoid) %>%
  st_crop(y = bbox)

# Filter highways to just highways in the bounding box and crop
h = read_rds("../data/highways.rds") %>%
  st_crop(y = bbox)

# Load in AQI sites  
s = read_rds("../data/sites.rds") %>% select(aqs_id_full, geometry)

# Get treatment effects, filting to the right set
e = read_csv("effects.csv") %>%
  # treatment effects per sensor
  filter(type == "per_sensor") %>%
  # fully specified model
  filter(spec == 3)  %>%
  # NYC model 3
  filter(model == "nyc3") %>%
  # Join in polygons
  inner_join(by = "aqs_id_full", y = s) %>%
  st_as_sf(crs = 4326) %>%
  # Format labels
  mutate(label = scales::number(att, accuracy = 0.1, style_positive = "plus", style_negative = "minus")) %>%
  mutate(st_coordinates(geometry) %>% as_tibble() %>% select(x = 1, y = 2))

# Load in county shapefiles, and crop to bounding box
c = read_rds("../data/counties.rds") %>%
  filter(geoid %in% g$geoid) %>%
  st_crop(y = bbox)

# Load in census block groups by population density
bg = read_sf("../data/bg.geojson") %>%
  # filter(county %in% g$geoid) %>%
  st_transform(crs = 4326) %>%
  # st_crop(y = bbox) %>%
  left_join(by = c("geoid" = "geoid"),
            y = read_csv("../data/census.csv") %>%
              mutate(geoid = stringr::str_pad(geoid, width = 12, side = "left", pad = "0")) %>%
              filter(year == max(year, na.rm = TRUE)) %>%
              select(geoid, total_population)) %>%
  # Get population density per sq.km.
  mutate(pop_density = total_population / (area_land / 1e6)) %>%
  select(geoid, pop_density, area_land, geometry) %>%
  # filter out the ones with no land area
  filter(area_land > 0) 

nyc = c %>% filter(!name %in% c("Hudson") )

# water_color = "#0047FF"
# water_color = "#648FFF"
water_color = "#B3C8FF"
# visual
gg2 = ggplot() +
  # Plot county outlines
  geom_sf(data = nyc, fill = NA, color = "#373737")  +
  # Plot blockgroups
  geom_sf(data = bg, mapping = aes(fill = pop_density), color = "white", linewidth = 0.01) +
  # Plot water area
  geom_sf(data = superw, fill = water_color, color = "#648FFF",linewidth = 1) +
  # Plot congestion pricing zone
  geom_sf(data = z, fill = NA, mapping = aes(linewidth = "Congestion Relief Zone"), 
          color = "#373737") +
  # Plot roads
  # geom_sf(data = r, color = "grey", linewidth = 0.01) +
  # Plot highways
  geom_sf(data = h, color = "black", linewidth = 0.6) +
  geom_sf(data = h, color = "white", linewidth = 0.5) +
  # Plot county boundaries
  geom_sf(data = nyc, fill = NA, mapping = aes(linewidth = "NYC 5 Boroughs"),
          color = "#373737")  +
  
  # Plot sites (white background layer)
  geom_sf(data = s, size = 15, color = "white") +
  # Plot sites (white background layer)
  geom_sf(data = s, size = 13, color = "grey") +
  # Plot sites by treatment effect
  geom_sf(data = e, size = 13, 
          mapping = aes(color = att)) +
  # Plot sites (white background layer)
  # geom_sf(data = s, size = 13, color = "white") +
  # Plot effect size by treatment effect
  # shadowtext::geom_shadowtext(
  #   data = e, 
  #   mapping = aes(
  #     x = x,
  #     y = y,
  #     color = att, label = label), size = 6, bg.r = 0.1, bg.color = "white",
  #   fontface = "bold") +
  #geom_sf_text(data = e, mapping = aes(label = label), size = 5) +
  # Crop visual extent to bounding box
  coord_sf(xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")], clip = "on", expand = FALSE)

gg2 = gg2 +
  # Add theming
  theme_void(base_size = 14) +
  # Put fill legend on bottom
  theme(legend.position = "bottom", legend.box = "vertical", panel.border = element_rect(color = "#373737", fill = NA, linewidth = 1)) +
  guides(color = guide_colorsteps(barwidth = 20, barheight = 1, show.limits = TRUE),
         fill = guide_colorsteps(barwidth = 20, barheight = 1, show.limits = TRUE)) +
  # Add color scale for population density
  scale_fill_gradient(
    low = "white", high = "#FFB000", na.value = "#373737", trans = "log", 
    labels = scales::label_number(accuracy = 0.1, scale_cut = scales::cut_si(""))) +
  scale_color_gradient2(
    low = "#648FFF", 
    mid = "white", midpoint = 0,                   
    high = "#DC267F",
    breaks = seq(from = -4, to = 4, by = 1),
    limits = c(-4, 4)) +
  scale_linewidth_manual(breaks = c("Congestion Relief Zone","NYC 5 Boroughs"),
                         values = c(0.5, 1.5)) +
  labs(fill = "<b>Population Density</b><br><i>residents/km<sup>2</sup></i>",
       color = "<b>Change in PM<sub>2.5</sub></b><br><i>Δμg/m<sup>3</sup>",
       title = "<b>Policy Effects on Air Quality per Sensor</b><br>in NYC 5 Boroughs",
       subtitle = "Jan 6, 2025 - June 1, 2025",
       linewidth = "") +
  theme(legend.title = ggtext::element_markdown(size = 12),
        plot.title = ggtext::element_markdown(size = 16),
        plot.subtitle = ggtext::element_markdown(size = 14)) +
  ggspatial::annotation_scale()


ggsave(filename = "descriptives/fig_map_att_nyc.png", plot = gg2, dpi = 500, width = 11, height = 8)

browseURL("descriptives/fig_map_att_nyc.png")

# CBSA ####################################################

# Get congestion pricing zone
z = read_rds("../data/zone.rds")
# Create a bounding box around the full area
cbsa = read_rds("../data/counties.rds") 

# Get bounding box extent
b = st_bbox(cbsa)
# Formalize the bounding box as a polygon
bbox = b %>% st_as_sfc() %>% tibble(geometry = .) %>% st_as_sf(crs = 4326)
# Load NYC area
nyc = read_rds("../data/counties.rds") %>%
  filter(name %in% c("Kings", "Queens", "New York", 
                     "Bronx", "Richmond"))

# Load in water data and crop to bounding box
w = read_sf("../data/water.geojson") %>%
  st_crop(y = bbox)

superw = read_rds("../data/water_cbsa.rds")

# Load in county metadata
g = read_csv("../data/metro.csv") 

# Filter roads to just roads in these counties, and crop
r = read_rds("../data/roads.rds") %>%
  filter(geoid %in% g$geoid) %>%
  st_crop(y = bbox)

# Filter highways to just highways in the bounding box and crop
h = read_rds("../data/highways.rds") %>%
  st_crop(y = bbox)

# Load in AQI sites  
s = read_rds("../data/sites.rds") %>% select(aqs_id_full, geometry)
states = read_rds("../data/states.rds")

# Get treatment effects, filting to the right set
e = read_csv("effects.csv") %>%
  # treatment effects per sensor
  filter(type == "per_sensor") %>%
  # fully specified model
  filter(spec == 3)  %>%
  # CBSA model 3
  filter(model == "cbsa3") %>%
  # Join in polygons
  inner_join(by = "aqs_id_full", y = s) %>%
  st_as_sf(crs = 4326) %>%
  # Format labels
  mutate(label = scales::number(att, accuracy = 0.1, style_positive = "plus", style_negative = "minus")) %>%
  mutate(st_coordinates(geometry) %>% as_tibble() %>% select(x = 1, y = 2))

# Load in county shapefiles, and crop to bounding box
c = read_rds("../data/counties.rds") %>%
  # filter(geoid %in% g$geoid) %>%
  st_crop(y = bbox)

# Load in census block groups by population density
bg = read_sf("../data/bg.geojson") %>%
  # filter(county %in% g$geoid) %>%
  st_transform(crs = 4326) %>%
  # st_crop(y = bbox) %>%
  left_join(by = c("geoid" = "geoid"),
            y = read_csv("../data/census.csv") %>%
              mutate(geoid = stringr::str_pad(geoid, width = 12, side = "left", pad = "0")) %>%
              filter(year == max(year, na.rm = TRUE)) %>%
              select(geoid, total_population)) %>%
  # Get population density per sq.km.
  mutate(pop_density = total_population / (area_land / 1e6)) %>%
  select(geoid, pop_density, area_land, geometry) %>%
  # filter out the ones with no land area
  filter(area_land > 0) 


coast = read_rds("../data/coastline.rds")


# water_color = "#0047FF"
# water_color = "#648FFF"
water_color = "#B3C8FF"
# visual
gg3 = ggplot() +
  geom_sf(data = coast, color = "#648FFF", linewidth = 1) +
  geom_sf(data = states, fill = "lightgrey", color = "#373737", linewidth = 0.1) +
  # Plot county outlines
  geom_sf(data = cbsa, fill = NA, color = "#373737")  +
  # Plot blockgroups
  geom_sf(data = bg, mapping = aes(fill = pop_density), color = "white", linewidth = 0.01) +
  # Plot water area
  geom_sf(data = superw, fill = "lightgrey", color = "#648FFF",linewidth = 1) +
  # Plot congestion pricing zone
  geom_sf(data = z, fill = NA, mapping = aes(linewidth = "Congestion Relief Zone"), 
          color = "#373737") +
  # Plot roads
  # geom_sf(data = r, color = "grey", linewidth = 0.01) +
  # Plot highways
  geom_sf(data = h, color = "black", linewidth = 0.6) +
  geom_sf(data = h, color = "white", linewidth = 0.5) +
  # Plot county boundaries
  geom_sf(data = nyc, fill = NA, mapping = aes(linewidth = "NYC 5 Boroughs"),
          color = "#373737")  +
  
  # Plot sites (white background layer)
  geom_sf(data = s, size = 5, color = "white") +
  # Plot sites (white background layer)
  geom_sf(data = s, size = 4, color = "grey") +
  # Plot sites by treatment effect
  geom_sf(data = e, size = 4, 
          mapping = aes(color = att)) +
  # Plot sites (white background layer)
  # geom_sf(data = s, size = 13, color = "white") +
  # Plot effect size by treatment effect
  # shadowtext::geom_shadowtext(
  #   data = e, 
  #   mapping = aes(
  #     x = x,
  #     y = y,
  #     color = att, label = label), size = 6, bg.r = 0.1, bg.color = "white",
  #   fontface = "bold") +
  #geom_sf_text(data = e, mapping = aes(label = label), size = 5) +
  # Crop visual extent to bounding box
  coord_sf(xlim = b[c("xmin", "xmax")], ylim = b[c("ymin", "ymax")], clip = "on", expand = FALSE)

gg3 = gg3 +
  # Add theming
  theme_void(base_size = 14) +
  # Put fill legend on bottom
  theme(legend.position = "bottom", legend.box = "vertical", 
        #plot.background = element_rect(fill = "lightgrey"),
        panel.border = element_rect(color = "#373737", fill = NA, linewidth = 1)) +
  guides(color = guide_colorsteps(barwidth = 20, barheight = 1, show.limits = TRUE),
         fill = guide_colorsteps(barwidth = 20, barheight = 1, show.limits = TRUE)) +
  # Add color scale for population density
  scale_fill_gradient(
    low = "white", high = "#FFB000", na.value = "#373737", trans = "log", 
    labels = scales::label_number(accuracy = 0.1, scale_cut = scales::cut_si(""))) +
  scale_color_gradient2(
    low = "#648FFF", 
    mid = "white", midpoint = 0,                   
    high = "#DC267F",
    breaks = seq(from = -4, to = 4, by = 1),
    limits = c(-4, 4)) +
  scale_linewidth_manual(breaks = c("Congestion Relief Zone","NYC 5 Boroughs", "NYC Core-Based Statistical Area"),
                         values = c(0.1, 0.5, 1.5)) +
  labs(fill = "<b>Population Density</b><br><i>residents/km<sup>2</sup></i>",
       color = "<b>Change in PM<sub>2.5</sub></b><br><i>Δμg/m<sup>3</sup>",
       title = "<b>Policy Effects on Air Quality per Sensor</b><br>in NYC Metropolitan Area",
       subtitle = "Jan 6, 2025 - June 1, 2025",
       linewidth = "") +
  theme(legend.title = ggtext::element_markdown(size = 12),
        plot.title = ggtext::element_markdown(size = 16),
        plot.subtitle = ggtext::element_markdown(size = 14)) +
  ggspatial::annotation_scale()


ggsave(filename = "descriptives/fig_map_att_cbsa.png", plot = gg3, dpi = 500, width = 11, height = 8)

browseURL("descriptives/fig_map_att_cbsa.png")

# Combo ######################################

ggside = ggarrange(
  plotlist = list(
    gg2 + labs(subtitle = NULL, title = "in NYC 5 Boroughs") + guides(fill = "none", color = "none", linewidth = "none"),
    gg3 + labs(subtitle = NULL, title = "in NYC Metropolitan Area") + guides(fill = "none", color = "none", linewidth = "none")
  ),
  nrow = 2
)


gg = ggarrange(
  plotlist = list(
    gg1 + labs(subtitle = NULL),
    ggside
    ), ncol = 2, common.legend = TRUE, legend = "bottom"
)
ggsave(plot = gg, filename = "fig_map_att_all.png", dpi = 500, width = 8, height = 8)

browseURL("../descriptives/fig_map_att_all.png")

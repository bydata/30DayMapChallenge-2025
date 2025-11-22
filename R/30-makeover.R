library(tidyverse)
library(sf)
library(osmdata)
library(here)
library(ggtext)

# Adapted version of Day 14 OSM to show the full city of Brussels

city <- getbb("Brussels-Capital Region, Belgium", format_out = "sf_polygon")
city_bbox <- st_bbox(city)

# Streets
streets <- opq(bbox = city_bbox) |>
  add_osm_feature(key = "highway") |>
  osmdata_sf()

street_linewidth_mapping <- c(
  "motorway" = 0.8,
  "trunk" = 0.6,
  "primary" = 0.6,
  "secondary" = 0.4,
  "tertiary" = 0.2,
  "residential" = 0.1,
  "service" = 0.1,
  "other" = 0.1
)

streets_lines <- streets$osm_lines |> 
  mutate(
    street_type = case_when(
      highway %in% c("motorway", "motorway_link") ~ "motorway",
      highway %in% c("trunk", "trunk_link") ~ "trunk",
      highway %in% c("primary", "primary_link") ~ "primary",
      highway %in% c("secondary", "secondary_link") ~ "secondary",
      highway %in% c("tertiary", "tertiary_link") ~ "tertiary",
      highway %in% c("residential", "living_street") ~ "residential",
      highway %in% c("service", "track") ~ "service",
      TRUE ~ "other"
    ),
    street_linewidth = street_linewidth_mapping[street_type]
  )

# Water bodies
water <- opq(bbox = city_bbox) |>
  add_osm_feature(key = "natural", value = "water") |>
  osmdata_sf()

# Green spaces
urban_green <- opq(bbox = city_bbox) |>
  add_osm_feature(key = "leisure", 
                  value = c(
                    "park", "garden", "nature_reserve", 
                    "common", "dog_park", "playground", 
                    "recreation_ground")) |>
  osmdata_sf()
vegetation <- opq(bbox = city_bbox) |>
  add_osm_feature(key = "natural", 
                  value = c(
                    "forest", "wood", "scrub", 
                    "grassland", "heath", "wetland")) |>
  osmdata_sf()
green_landuse <- opq(bbox = city_bbox) |>
  add_osm_feature(key = "landuse", 
                  value = c(
                    "forest", "meadow", "grass", 
                    "village_green", "allotments")) |>
  osmdata_sf()

# Railways
railways <- opq(bbox = city_bbox) |>
  add_osm_feature(key = "railway") |>
  osmdata_sf()


# Limit to the city shape
streets_lines_filtered <- st_intersection(streets_lines, city)
water_filtered <- st_intersection(water$osm_polygons, city)
urban_green_polygons_filtered <- st_intersection(urban_green$osm_polygons, city)
urban_green_multipolygons_filtered <- st_intersection(st_make_valid(urban_green$osm_multipolygons), city)
vegetation_polygons_filtered <- st_intersection(vegetation$osm_polygons, city)
vegetation_multipolygons_filtered <- st_intersection(st_make_valid(vegetation$osm_multipolygons), city)
green_landuse_polygons_filtered <- st_intersection(green_landuse$osm_polygons, city)
green_landuse_multipolygons_filtered <- st_intersection(st_make_valid(green_landuse$osm_multipolygons), city)
railways_lines_filtered <- st_intersection(railways$osm_lines, city)
railways_polygons_filtered <- st_intersection(railways$osm_polygons, city)

bgcolor <- "#F6F6F6"
p <- ggplot() +
  geom_sf(data = urban_green_polygons_filtered,
    fill = "#a3e089",
    color = NA,
    alpha = 0.4) +
  geom_sf(data = urban_green_multipolygons_filtered,
    fill = "#a3e089",
    color = NA,
    alpha = 0.4) +
  geom_sf(data = vegetation_polygons_filtered,
    fill = "#92cf78",
    color = NA,
    alpha = 0.4) +
  geom_sf(data = vegetation_multipolygons_filtered,
    fill = "#92cf78",
    color = NA,
    alpha = 0.4) +
  geom_sf(data = green_landuse_polygons_filtered,
    fill = "#92cf78",
    color = NA,
    alpha = 0.4) +
  geom_sf(data = green_landuse_multipolygons_filtered,
    fill = "#92cf78",
    color = NA,
    alpha = 0.4) +
  geom_sf(data = railways_lines_filtered,
          color = "#8f3111",
          linewidth = 0.3,
          alpha = 0.3) +
  geom_sf(data = railways_polygons_filtered,
          color = "#8f3111",
          alpha = 0.3) +
  geom_sf(
    data = streets_lines_filtered,
    aes(linewidth = street_linewidth),
    color = "gray30",
    alpha = 0.6) +
  geom_sf(data = water_filtered,
    fill = "lightblue",
    color = NA,
    alpha = 0.5) +
  scale_linewidth_identity() +
  coord_sf(clip = "off") +
  labs(
    title = "Brussels-Capital Region",
    caption = "<span style='font-family: \"Cormorant Garamond Semibold\"'>Source:</span> OpenStreetMap.
      <span style='font-family: \"Cormorant Garamond Semibold\"'>Visualization:</span> Ansgar Wolsing"
  ) +
  theme_void(base_family = "Cormorant Garamond", paper = bgcolor) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    text = element_text(color = "grey10"),
    plot.title = element_text(
      family = "Cormorant Garamond Semibold", hjust = 0.5, size = 24),
    plot.caption = element_markdown(hjust = 0.5, size = 10),
    plot.margin = margin(4, 4, 4, 4)
  )
ggsave(here("plots", "30-makeover.png"), width = 6, height = 6, dpi = 500)
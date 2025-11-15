library(tidyverse)
library(sf)
library(osmdata)
library(here)
library(ggtext)


city <- getbb("Brussels, Belgium", format_out = "sf_polygon")
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

# Parks
parks <- opq(bbox = city_bbox) |>
  add_osm_feature(key = "leisure", value = c("park", "garden", "nature_reserve")) |>
  osmdata_sf()

# Forests
forests <- opq(bbox = city_bbox) |>
  add_osm_feature(key = "natural", value = "wood") |>
  osmdata_sf()

# Railways
railways <- opq(bbox = city_bbox) |>
  add_osm_feature(key = "railway") |>
  osmdata_sf()


# Limit to the city shape
streets_lines_filtered <- st_intersection(streets_lines, city)
water_filtered <- st_intersection(water$osm_polygons, city)
parks_polygons_filtered <- st_intersection(parks$osm_polygons, city)
parks_multipolygons_filtered <- st_intersection(st_make_valid(parks$osm_multipolygons), city)
forests_polygons_filtered <- st_intersection(forests$osm_polygons, city)
forests_multipolygons_filtered <- st_intersection(st_make_valid(forests$osm_multipolygons), city)
railways_lines_filtered <- st_intersection(railways$osm_lines, city)
railways_polygons_filtered <- st_intersection(railways$osm_polygons, city)

bgcolor <- "#F6F6F6"
p <- ggplot() +
  geom_sf(data = parks_polygons_filtered,
    fill = "#a3e089",
    color = NA,
    alpha = 0.4) +
  geom_sf(data = parks_multipolygons_filtered,
    fill = "#a3e089",
    color = NA,
    alpha = 0.4) +
  geom_sf(data = forests_polygons_filtered,
    fill = "#92cf78",
    color = NA,
    alpha = 0.4) +
  geom_sf(data = forests_multipolygons_filtered,
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
    title = "Bruxelles \U2022 Brussel \U2022 Brussels",
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
    plot.caption = element_markdown(size = 10),
    plot.margin = margin(4, 4, 4, 4)
  )
ggsave(here("plots", "14-osm.png"), width = 5, height = 7.5, dpi = 500)

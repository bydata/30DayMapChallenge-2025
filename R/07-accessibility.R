library(tidyverse)
library(sf)
library(openrouteservice)
library(osmdata)
library(ggtext)
library(here)


#' https://giscience.github.io/openrouteservice-r/articles/openrouteservice.html#isochrones

city <- getbb("Cologne, Germany", format_out = "sf_polygon")
city_bbox <- st_bbox(city)
city_center <- c(6.9557065, 50.9412784)

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
  "residential" = 0.2,
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
  ) |> 
  select(osm_id, name, highway, street_type, street_linewidth)

# Water bodies
water <- opq(bbox = city_bbox) |>
  add_osm_feature(key = "natural", value = "water") |>
  osmdata_sf()

# Limit to the city shape
streets_lines_filtered <- st_intersection(streets_lines, city)
water_filtered <- st_intersection(water$osm_polygons, city)



iso_intervals <- seq(0, 30, 5)

#' Use the {openrouteservice} package
#' Create account and API key at 
#' Set API via ors_api_key('<your-api-key>')

# Available profiles
ors_profile()
profiles <- c("cycling-regular", "driving-car", "foot-walking" )

df_isochrones <- map(
  profiles, 
  function(x) {
    ors_isochrones(
      locations = city_center,
      profile = x,
      range = 15 * 60, interval = 3 * 60,
      smoothing = 5,
      output = "sf"
    ) |> 
      arrange(-value) |> 
      mutate(
        value = value / 60, 
        value = factor(value))
  }
) |> 
  set_names(profiles) |> 
  bind_rows(.id = "profile")

levels(df_isochrones$value)[length(levels(df_isochrones$value))] <- 
  paste(levels(df_isochrones$value)[length(levels(df_isochrones$value))], "minutes")

# Calculate isochrone areas
df_isochrones |> 
  transmute(profile, value, area = st_area(st_make_valid(geometry))) |> 
  st_drop_geometry()

df_isochrones <- df_isochrones |> 
  mutate(
    profile_label = case_match(
      profile,
      "cycling-regular" ~ "Bicycle", 
      "driving-car"     ~ "Car",
      "foot-walking"    ~ "Foot"
    )
  )

p <- ggplot() +
  ggfx::with_shadow(
    geom_sf(
      data = city,
      fill = "grey70"
    ),
    sigma = 4, colour = "grey4"
  ) +
  geom_sf(
    # data = filter(
    #   streets_lines_filtered, 
    #   street_type %in% c("motorway", "trunk", "primary",
    #     "secondary",  "tertiary", "residential" 
    #   )
    data = streets_lines_filtered, 
    aes(linewidth = street_linewidth),
    color = "gray50",
    alpha = 0.6) +
  geom_sf(data = water_filtered,
    fill = "grey90",
    color = NA,
    alpha = 0.5) +
  geom_sf(
    data = df_isochrones,
    aes(fill = value),
    alpha = 0.4, col = NA
  ) +
  scale_fill_viridis_d(option = "rocket", direction = -1) +
  scale_linewidth_identity() +
  facet_wrap(vars(profile_label)) +
  guides(
    fill = guide_legend(
      override.aes = list(alpha = 0.8, color = "grey2", linewidth = 0.1)
    )
  ) +
  labs(
    title = "How far can you get in 15 minutes?",
    subtitle = "Reachable areas from the Cologne Cathedral...
      not during rush hours, obviously",
    caption = "Data: Openrouteservice, OpenStreetMap contributors.
      Visualization: Ansgar Wolsing",
    fill = "Travel time"
  ) +
  theme_void(
    base_family = "Inter 18pt", paper = "#F6F6F6", ink = "grey30") +
  theme(
    plot.title = element_text(
      family = "Inter 18pt SemiBold", color = "grey2",
       size = 16, hjust = 0.5),
    plot.subtitle = element_textbox(
      hjust = 0.5, margin = margin(t = 4, b = 12)),
    plot.caption = element_textbox(
      hjust = 0.5, margin = margin(t = 12)),
    strip.text = element_text(
      family = "Inter 18pt SemiBold", size = 12, color = "grey40"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.height = unit(2, "mm"),
    plot.margin = margin(0, 4, 2, 4)
  )
ggsave(here("plots", "07-accessibility.png"), width = 7.5, height = 4.5)

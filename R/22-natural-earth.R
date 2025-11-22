library(tidyverse)
library(sf)
library(ggtext)
library(here)
library(rnaturalearth)
library(rnaturalearthdata)

europe <- ne_countries(scale = 10, continent = "Europe", returnclass = "sf")
europe <- select(europe, name, geometry)
germany <- filter(europe, name == "Germany")

# Find neighbor countries (must be 9)
neighbors <- st_touches(germany, europe, sparse = FALSE)
df_neighbor_countries <- europe[as.logical(neighbors), ]
nrow(df_neighbor_countries)

# Extract the border to each neighbor country
extract_border <- function(country, neighbor_country) {
  country_boundary <- st_boundary(country)
  neighbor_boundary <- st_boundary(neighbor_country)
  border <- st_intersection(country_boundary, neighbor_boundary)
  border
}

borders <- map_dfr(
  seq_len(nrow(df_neighbor_countries)), 
  function(i) extract_border(germany, st_geometry(df_neighbor_countries[i, ]))
) |> 
  select(-name) |> 
  add_column(name = df_neighbor_countries$name)


ggplot(borders) +
  geom_sf(aes(col = name))


## Create grid
germany_bbox <- st_bbox(germany)
grid <- st_make_grid(germany, cellsize = 0.075, what = "polygons", square = FALSE)
germany_grid <- st_intersection(grid, germany)
# germany_grid <- st_as_sf(germany_grid)

ggplot(germany_grid) +
  geom_sf(size = 0.1)

# Find the closest country to each cell
find_closest_country <- function(cell, borders) {
  distances <- st_distance(cell, borders)
  min_idx <- which.min(distances)
  closest_country <- borders$name[min_idx]
  closest_country
}

library(mirai)
daemons(6)

closest_country <- map_chr(
  seq_along(germany_grid),
  in_parallel(
    function(i) {
      distances <- sf::st_distance(germany_grid[i, ], borders)
      min_idx <- which.min(distances)
      closest_country <- borders$name[min_idx]
      closest_country
    },
    germany_grid = germany_grid,
    borders = borders
  )
)
daemons(0)


grid_results <- germany_grid |> 
  st_as_sf() |> 
  rename(geometry = x) |> 
  add_column(neighbor = closest_country)
write_rds(grid_results, here("data", "grid-results.rds"))

df_regions <- grid_results |> 
  group_by(neighbor) |> 
  summarize(geometry = st_union(geometry))
nrow(df_regions)

ggplot() +
  geom_sf(
    data = filter(europe, name %in% df_neighbor_countries$name),
    aes(fill = name),
    col = "white", alpha = 0.15
  ) +
  geom_sf(
    data = df_regions,
    aes(fill = neighbor),
  col = "white", linewidth = 0.1) +
  geom_sf(
    data = germany,
    fill = NA, col = "white", linewidth = 0.2
  ) +
  geom_sf_label(
    data = df_regions,
    aes(label = toupper(neighbor), fill = neighbor,
    col = ifelse(neighbor %in% c("Belgium", "Czechia"), "black", "white")
    ),
    label.size = 0.1,
    family = "Source Sans Pro SemiBold"
  ) +
  scale_color_identity() +
  # paletteer::scale_fill_paletteer_d("MoMAColors::Rattner") +
  paletteer::scale_fill_paletteer_d("ggsci::light_uchicago") +
  coord_sf(xlim = c(5.5, 15.5), ylim = c(47.5, 55)) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Which neighboring country is closest to you?",
    subtitle = "",
    caption = "**Source:** Natural Earth. **Visualization:** Ansgar Wolsing (original idea by @JulesGrandin on Twitter Nov 03, 2022)"
  ) +
  theme_void(base_family = "Source Sans Pro", paper = "#F5F5F5") +
  theme(
    plot.title = element_text(family = "Source Sans Pro SemiBold", hjust = 0.5, size = 20),
    plot.subtitle = element_textbox(width = 0.95, hjust = 0.5),
    plot.caption = element_textbox(width = 0.95, hjust = 0.5, size = 8.5),
    plot.margin = margin(4, 4, 4, 4)
  )
ggsave(here("plots", "22-natural-earth.png"), width = 6, height = 8)


#' ggsci::light_uchicago
#' ggthemes::few_Medium


library(tidyverse)
library(sf)
library(ggtext)
library(here)
library(rnaturalearth)

europe <- ne_countries(scale = 10, continent = "Europe", returnclass = "sf")
europe <- select(europe, name, geometry) |> 
  st_transform(crs = 25832)
germany <- filter(europe, name == "Germany")

# Find neighbor countries (must be 9)
neighbors <- st_touches(germany, europe, sparse = FALSE)
df_neighbor_countries <- europe[neighbors, ]
nrow(df_neighbor_countries)

# Create a fine grid of POLYGONS within Germany (not points)
grid_spacing <- 2000  # 2km spacing

grid_cells <- st_make_grid(
  germany,
  cellsize = grid_spacing,
  what = "polygons"
) |> 
  st_as_sf() |> 
  st_filter(germany) |> 
  mutate(cell_id = row_number())

# For each grid cell, calculate distance to each country's border
grid_with_country <- map_dfr(
  unique(df_neighbor_countries$name),
  function(country_name) {
  country <- filter(df_neighbor_countries, name == country_name)
  
  # Get the shared border
  border <- st_intersection(st_boundary(germany), st_boundary(country))
  
  # Calculate distance from each grid cell centroid to this border
  distances <- st_distance(st_centroid(grid_cells), border)
  
  grid_cells |> 
    mutate(
      country = country_name,
      distance = as.numeric(distances)
    )
}) |> 
  group_by(cell_id) |> 
  slice_min(distance, n = 1) |> 
  ungroup()

# Dissolve grid cells by country
df_regions <- grid_with_country |> 
  group_by(country) |> 
  summarize(geometry = st_union(x)) |> 
  st_intersection(germany) 

# Transform back to EPSG:4326
df_regions <- st_transform(df_regions, crs = 4326)
germany <- st_transform(germany, crs = 4326)
europe <- st_transform(europe, crs = 4326)

country_colors <- c(
  "France" = "#80CBC4",      # Light teal
  "Denmark" = "#FF7F66",  # Coral
  "Belgium" = "#B2DFDB",      # Very light teal
  "Luxembourg" = "#FF9E8A",   # Light coral
  "Austria" = "#4DB6AC",       # Medium teal
  "Switzerland" = "#E85D3F",  # Dark coral
  "Netherlands" = "#00796B",      # Dark teal
  "Czechia" = "#FFA99A",      # Medium-light coral
  "Poland" = "#26A69A"        # Medium-dark teal
)

country_colors <- c(
  "Denmark" = "#80CBC4",      # Light teal
  "Netherlands" = "#B08EA2",  # Mauve
  "Belgium" = "#B2DFDB",      # Very light teal
  "Luxembourg" = "#C9B3C0",   # Light mauve
  "France" = "#4DB6AC",       # Medium teal
  "Switzerland" = "#8B6B82",  # Dark mauve
  "Austria" = "#00796B",      # Dark teal
  "Czechia" = "#DCC9D5",      # Pale mauve
  "Poland" = "#26A69A"        # Medium-dark teal
)

ggplot() +
  geom_sf(
    data = filter(europe, name %in% df_neighbor_countries$name),
    aes(fill = name),
    col = "white", alpha = 0.15
  ) +
  geom_sf(
    data = df_regions,
    aes(fill = country),
    col = "white", linewidth = 0.2) +
  geom_sf(
    data = germany,
    fill = NA, col = "white", linewidth = 0.2
  ) +
  geom_sf_label(
    data = df_regions,
    aes(label = toupper(country), fill = country,
    ),
    fill = "black", col = "white", label.size = 0,
    family = "Source Sans Pro SemiBold", size = 3
  ) +
  scale_color_identity() +
  # paletteer::scale_fill_paletteer_d("ggsci::light_uchicago") +
  scale_fill_manual(values = country_colors) +
  coord_sf(xlim = c(5.5, 15.5), ylim = c(47.5, 55)) +
  guides(fill = "none", color = "none") +
  labs(
    title = "Nearest Neighbors",
    subtitle = "Each region shows the closest neighboring country border",
    caption = "**Source:** Natural Earth.
    **Visualization:** Ansgar Wolsing (original idea by @JulesGrandin on Twitter Nov 03, 2022)"
  ) +
  theme_void(base_family = "Source Sans Pro", base_size = 11) +
  theme(
    plot.background = element_rect(fill = "#F5F5F5", color = NA),
    plot.title = element_text(
      family = "Source Sans Pro SemiBold", hjust = 0.5, size = 24),
    plot.subtitle = element_textbox(hjust = 0.5, width = 0.95, halign = 0.5),
    plot.caption = element_textbox(
      width = 0.95, hjust = 0.5, size = 8.5, lineheight = 1.15,
      margin = margin(t = 10)),
    plot.margin = margin(0, 0, 0, 0)
  )
ggsave(here("plots", "22-natural-earth.png"), width = 4, height = 6)

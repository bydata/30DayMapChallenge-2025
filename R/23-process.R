library(tidyverse)
library(terra)
library(sf)
library(ggtext)
library(rnaturalearth)
library(here)

data_path <- here("data", "HDX")

#' Manually download the raster data from 
#' https://data.humdata.org/dataset/germany-high-resolution-population-density-maps-demographic-estimates

raster_paths <- here(
  data_path, c("population_deu_2019-07-01.tif", "DEU_youth_15_24.tif"))
rasters <- map(raster_paths, terra::rast) 
raster_crs <- terra::crs(rasters[[1]])


# Downsample the rasters
downsample_raster <- function(raster, factor = 25) {
  terra::aggregate(raster, fact = factor, fun = sum, na.rm = TRUE)
}
raster_downsampled_pop_full <- downsample_raster(rasters[[1]])
raster_downsampled_pop_group <- downsample_raster(rasters[[2]])

# Combine the rasters: calculate the share of the subgroup 
raster_downsampled_combined <- raster_downsampled_pop_group / raster_downsampled_pop_full


# Create a hex grid of Germany
shp <- ne_countries(scale = 10, country = "Germany", returnclass = "sf")
shp <- select(shp, sovereignt, geometry)
st_crs(shp)
shp <- st_transform(shp, 25832)
hex_grid <- st_make_grid(
    shp,
    cellsize = 15000, 
    square = FALSE,
    what = "polygons"
  ) |>
  st_as_sf() |>
  st_intersection(shp) 

hex_grid$hex_id <- 1:nrow(hex_grid)
hex_grid <- st_transform(hex_grid, crs = raster_crs)


# Extract raster values for each hexagon and calculate the mean
hex_means <- extract(
  raster_downsampled_combined,
  vect(hex_grid),
  fun = mean,
  na.rm = TRUE,
  ID = TRUE
)
hex_grid$mean_value <- hex_means$`Population Count`


#++++ Step-by-step plots +++++++++++++++++++++++++++++

bbox <- st_bbox(hex_grid)
width_to_height <- (bbox["xmax"] - bbox["xmin"]) / (bbox["ymax"] - bbox["ymin"])
plot_height <- 4
plot_width <- width_to_height * plot_height

file_path <- here("plots", "23-process")
if (!dir.exists(file_path)) dir.create(file_path)


# Population 15-24
ragg::agg_png(here(
  file_path, "23-process-step01.png"), 
  width = plot_width, height = plot_height, res = 300, units = "in", bg = "transparent")
plot(
  raster_downsampled_pop_group, 
  legend = FALSE, axes = FALSE, box = FALSE)
dev.off()

# Population total
ragg::agg_png(here(
  file_path, "23-process-step02.png"), 
  width = plot_width, height = plot_height, res = 300, units = "in", bg = "transparent")
plot(
  raster_downsampled_pop_full, 
  legend = FALSE,axes = FALSE,
  box = FALSE)
dev.off()

# Share 15-24
ragg::agg_png(
  here(file_path, "23-process-step03.png"), 
  width = plot_width, height = plot_height, res = 300, units = "in", bg = "transparent")
plot(
  raster_downsampled_combined, 
  legend = FALSE,axes = FALSE,
  box = FALSE)
dev.off()


# Grid
ggplot(hex_grid) +
  geom_sf(fill = NA) +
  theme_void()
ggsave(here(file_path, "23-process-step04.png"), width = plot_width, height = plot_height)

# 
p <- ggplot(hex_grid) +
  ggfx::with_shadow(
    geom_sf(data = shp),
    colour = "grey10", sigma = 4, x_offset = 3, y_offset = 3) +
  geom_sf(aes(fill = mean_value)) +
  guides(fill = guide_colorbar(title.position = "top")) +
  theme_void(base_family = "Fira Sans", ink = "grey6") +
  theme(
    plot.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position = "bottom",
    legend.key.width = unit(3.5, "mm"),
    legend.key.height = unit(2, "mm"),
    legend.title = element_markdown(size = 7, lineheight = 1.1),
    legend.text = element_text(size = 6),
    plot.margin = margin(4, 4, 4, 4),
    plot.title = element_text(hjust = 0.5, family = "Fira Sans SemiBold", size = 18),
    plot.subtitle = element_markdown(hjust = 0.5, size = 9, lineheight = 1.1),
    plot.caption = element_markdown(
      hjust = 0.5, size = 6, margin = margin(t = 10, b = 2))
  )
ggsave(here(file_path, "23-process-step05.png"), width = plot_width, height = plot_height)


p <- ggplot(hex_grid) +
  ggfx::with_shadow(
    geom_sf(data = shp),
    colour = "grey10", sigma = 4, x_offset = 3, y_offset = 3) +
  geom_sf(aes(fill = mean_value)) +
  paletteer::scale_fill_paletteer_c(
    labels = scales::label_percent(), na.value = "grey90", "grDevices::Purple-Yellow") +
  guides(fill = guide_colorbar(title.position = "top")) +
  theme_void(base_family = "Fira Sans", ink = "grey6") +
  theme(
    plot.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position = "bottom",
    legend.key.width = unit(3.5, "mm"),
    legend.key.height = unit(2, "mm"),
    legend.title = element_markdown(size = 7, lineheight = 1.1),
    legend.text = element_text(size = 6),
    plot.margin = margin(4, 4, 4, 4),
    plot.title = element_text(hjust = 0.5, family = "Fira Sans SemiBold", size = 18),
    plot.subtitle = element_markdown(hjust = 0.5, size = 9, lineheight = 1.1),
    plot.caption = element_markdown(
      hjust = 0.5, size = 6, margin = margin(t = 10, b = 2))
  )
ggsave(here(file_path, "23-process-step06.png"), width = plot_width, height = plot_height)

p <- ggplot(hex_grid) +
  ggfx::with_shadow(
    geom_sf(data = shp),
    colour = "grey10", sigma = 4, x_offset = 3, y_offset = 3) +
  geom_sf(aes(fill = mean_value)) +
  paletteer::scale_fill_paletteer_c(
    labels = scales::label_percent(), na.value = "grey90", "grDevices::Purple-Yellow") +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(
    title = "Where's the Youth?",
    subtitle = "Share of people aged 15 to 24 among the population",
    caption = "Source: Data for Good at Meta via The Humanitarian Data Exchange.
    Visualization: Ansgar Wolsing",
    fill = "Share of population aged 15-24 (%)</i>"
  ) +
  theme_void(base_family = "Fira Sans", ink = "grey6") +
  theme(
    plot.background = element_rect(color = "#F6F6F6", fill = "#F6F6F6"),
    legend.position = "bottom",
    legend.key.width = unit(3.5, "mm"),
    legend.key.height = unit(2, "mm"),
    legend.title = element_markdown(size = 7, lineheight = 1.1),
    legend.text = element_text(size = 6),
    plot.margin = margin(4, 4, 4, 4),
    plot.title = element_text(hjust = 0.5, family = "Fira Sans SemiBold", size = 18),
    plot.subtitle = element_markdown(hjust = 0.5, size = 9, lineheight = 1.1),
    plot.caption = element_markdown(
      hjust = 0.5, size = 6, margin = margin(t = 10, b = 2))
  )
ggsave(here(file_path, "23-process-step07.png"), width = width_to_height * 6, height = 6)

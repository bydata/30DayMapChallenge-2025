library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(here)

#' Source: EMODnet Map Viewer
#' https://emodnet.ec.europa.eu/geoviewer/

download_urls <- c(
  "https://downloads.emodnet-bathymetry.eu/v12/D4_2024.nc.zip",
  "https://downloads.emodnet-bathymetry.eu/v12/D5_2024.nc.zip",
  "https://downloads.emodnet-bathymetry.eu/v12/E4_2024.nc.zip",
  "https://downloads.emodnet-bathymetry.eu/v12/E5_2024.nc.zip"
)

local_path <- here("data", "EMODnet")
download_and_unzip <- function(url, local_path) {
  local_filename <- here(
    local_path,
    str_extract(url, "/([A-Z]\\d+_\\d{4}\\.nc\\.zip)", group = 1))
  download.file(url, destfile = local_filename)
  unzip(local_filename, exdir = local_path)
}

if (!dir.exists(local_path)) dir.create(local_path)
walk(download_urls, function(x) download_and_unzip(x, local_path))

local_filenames <- here(
  local_path,
  str_extract(download_urls, "/([A-Z]\\d+_\\d{4}.nc)\\.zip", group = 1))


# Load and combine all necessary areas
rasters_list <- map(local_filenames, rast)
# Extract elevation information
raster_elev_list <- map(rasters_list, `[[`, 1)

# rasters_combined <- do.call(merge, rasters_list)
rasters_elev_combined <- do.call(merge, raster_elev_list)
summary(rasters_elev_combined)

depth_breaks <- c(0, -20, -50, -100, -200, -500, -750)
depth_labels <- c(
  "Less than 20", "20-49", "50-99", "100-199",
  "200-499", "500-750", "More than 750"
)

p <- ggplot() +
  geom_spatraster_contour_filled(
    data = rasters_elev_combined,
    breaks = depth_breaks,
    maxcell = 10e6, na.rm = TRUE
  ) +
  scale_fill_manual(
    values = c("#D4E4EC", "#B0CEDA", "#83ABBF", "#578599",
        "#3B6577", "#234456", "#132530"),
    labels = depth_labels
  ) +
  coord_sf(xlim = c(-2.1, 9.4), ylim = c(50, 60)) +
  labs(
    title = "North Sea Bathymetry",
    caption = "Data: EMODnet. Visualization: Ansgar Wolsing",
    fill = "Depth (in meters)"
  ) +
  theme_void(base_family = "Source Sans Pro", paper = "grey8", ink = "grey92") +
  theme(
    legend.position = c(0.82, 0.14),
    legend.direction = "vertical",
    legend.key.height = unit(5, "mm"),
    legend.key.spacing.y = unit(0.5, "mm"),
    panel.background = element_rect(color = "black"),
    plot.title = element_text(
      family = "Source Sans Pro Light", size = 26, hjust = 0.5, color = "white",
      margin = margin(b = 4)),
    plot.caption = element_text(
      hjust = 0.5, margin = margin(t = 12)),
    plot.margin = margin(2, 0, 2, 0)
  )
ggsave(here("plots", "20-water.png"), width = 4, height = 7)

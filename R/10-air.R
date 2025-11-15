library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(here)

# Read a GeoTIFF
raster_wind <- rast("https://www.opengeodata.nrw.de/produkte/umwelt_klima/energie/windatlas/WA-Windgeschwindigkeit-100m_2025_EPSG25832.tif")
raster_wind

ggplot() +
  geom_spatraster(data = raster_wind) +
  scale_fill_viridis_c(
    option = "plasma", na.value = "transparent") +
  coord_sf() +
  theme_void()


# Remove NAs
global(raster_wind, fun = "isNA")
mask_raster <- !is.na(raster_wind)
raster_wind_no_na <- mask(raster_wind, mask_raster, maskvalues = FALSE)


# Locations to add
df_locations <- tribble(
  ~name, ~lon, ~lat,
  "Cologne", 6.8025157, 50.9575869,
  "Bielefeld", 8.355803, 52.0146965,
  "Düsseldorf", 6.6495436, 51.238339,
  "Münster", 7.4592907, 51.9499704,
  "Dortmund", 7.4492825, 51.4925888,
  "Kahler Asten", 8.4725642, 51.1804948
) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)



bgcolor <- "#F6F6F6"

p <- ggplot() +
  geom_spatraster(
    data = raster_wind_no_na) +
  geom_sf(
    data = df_locations,
    shape = 21, col = "white", fill = "grey2", size = 3
  ) +
  # scale_fill_continuous(na.value = NA) +
  # scale_fill_viridis_c(
  #  option = "plasma", na.value = "transparent") +
  paletteer::scale_fill_paletteer_c(
    "grDevices::Purple-Yellow", 
    na.value = NA) +
  coord_sf() +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(
    title = "Anywhere the wind blows",
    subtitle = "Average annual wind speed in North Rhine-Westphalia",
    caption = "Source: OpenGeodata.NRW. Visualization: Ansgar Wolsing",
    fill = "Wind speed in 100m height (in m/s)"
  ) +
  theme_void(base_family = "Inter 18pt", paper = bgcolor) +
  theme(
    legend.position = c(0.7, 0.1),
    legend.direction = "horizontal",
    legend.key.height = unit(2.5, "mm"),
    legend.key.width = unit(9, "mm"),
    legend.title = element_text(size = 8),
    plot.title = element_text(family = "Inter 18pt SemiBold"),
    plot.margin = margin(4, 4, 4, 4)
  )
ggsave(here("plots", "10-air-pre.png"), width = 6, height = 6)

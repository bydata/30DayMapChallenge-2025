library(tidyverse)
library(sf)
library(here)
library(tmap)
library(osmdata)
# library(tmaptools)

#' Source: Unfallatlas, Statistisches Bundesamt
#' https://unfallatlas.statistikportal.de/
#' Metadata description: https://www.opengeodata.nrw.de/produkte/transport_verkehr/unfallatlas/DSB_Unfallatlas_EN.pdf

# Read shapefile for 2024
shpfile_url <- "https://www.opengeodata.nrw.de/produkte/transport_verkehr/unfallatlas/Unfallorte2024_EPSG25832_Shape.zip"
local_path <- here("data", "unfallatlas")
local_filepath <- here(local_path, "unfallorte-2024.zip")
if (!dir.exists(local_path)) {
  dir.create(local_path)
  download.file(shpfile_url, destfile = local_filepath)
  unzip(local_filepath, exdir = local_path)
}
df_unfallorte <- st_read(here(local_path, "shp", "Unfallorte_2024_LR_BasisDLM.shp"))
st_crs(df_unfallorte)

#' Limit dataframe to a specific municipality
#' Check the "Description of the regional and territorial units" document.

# Select Cologne: 05	3	15
df_unfallorte_filtered <- df_unfallorte |> 
  filter(ULAND == "05", UREGBEZ == "3", UKREIS == "15", IstRad == 1)
nrow(df_unfallorte_filtered)

# City boundaries
boundaries <- getbb("Köln, Deutschland", format_out = "sf_polygon")
# City center
city_center_coords <- data.frame(lon = 6.9557065, lat = 50.9412784)
city_center <- st_as_sf(city_center_coords, coords = c("lon", "lat"), crs = 4326)

boundaries <- st_transform(boundaries, 25832)
city_center <- st_transform(city_center, 25832)

# Circle of 5 km radius around the Dom
city_center_area <- st_buffer(city_center, dist = 2500)

df_unfallorte_filtered <- df_unfallorte_filtered |> 
  # filter(IstPKW == 1 & IstRad == 1) |> 
  # 8 = Leaving the carriageway to the right
  # filter(UART %in% c(5, 8)) |> 
  mutate(Severity = factor(UKATEGORIE, levels = 1:3, labels = c("Deadly", "Severe injuries", "Light injuries")))

# Filter the accidents within the city center area
df_unfallorte_filtered_innercity <- st_filter(st_zm(df_unfallorte_filtered), city_center_area,
    .predicate = st_within)
st_crs(city_center_area) == st_crs(df_unfallorte_filtered)

df_unfallorte_filtered_innercity |> 
  st_drop_geometry() |> 
  mutate(across(starts_with("Ist"), as.numeric)) |> 
  summarize(across(starts_with("Ist"), sum))

# Create a mask to hide basemap
mask_bbox <- st_as_sfc(st_bbox(boundaries) + c(-0.05, -0.05, 0.05, 0.05)) # expand a bit
mask <- st_difference(mask_bbox, city_center_area)

# Create a grid to aggregate close accident locations
nrow(df_unfallorte_filtered_innercity)
hex_grid <- st_make_grid(df_unfallorte_filtered_innercity, 
                         cellsize = 50,
                         square = FALSE)
hex_counts <- st_intersects(hex_grid, df_unfallorte_filtered_innercity)
hex_df <- st_sf(
  geometry = hex_grid,
  count = lengths(hex_counts)
) %>%
  filter(count > 0)
# Check number of accidents in both dataframes
sum(hex_df$count) == nrow(df_unfallorte_filtered_innercity)

bgcolor <- "#F6F6F6"
m1 <- tm_basemap("CartoDB.PositronNoLabels") +
  tm_shape(city_center_area) +
  tm_shape(hex_df) +
  tm_dots(
    fill = "purple", size = "count", fill_alpha = 0.4,
  size.legend = tm_legend(
    title = "# of accidents", text.size = 0.7, title.size = 0.7,
    position = c(0.95, 1.0), bg.color = NA, frame = FALSE)) +
  tm_shape(mask) +
  tm_fill(col = bgcolor, fill_alpha = 1, size = 1, border.col = "grey20") +
  tm_title_out(
    text = "Bike accidents in Cologne 2024",
    size = 2.25, 
    position = tm_pos_out("center", "top"),
    just = "left", padding.left = 0
  ) +
  tm_title_out(
    text = "\nEach dot represents the number of accidents with personal injuries involving\nat least one bicycle at this location, radius of 2.5 km around the city center",
    size = 0.8,
    position = tm_pos_out("center", "top"),
    just = "left", padding.left = 0
  ) +
  tm_credits(
    text = "Data: Stadt Köln, OpenStreetMap, CartoDB. Visualization: Ansgar Wolsing",
    position = c(0.5, 0),
    size = 0.6, just = "left"
  ) +
  tm_layout(
    frame = FALSE, 
    outer.bg.color = bgcolor,
    text.fontfamily = "Source Sans Pro",
    inner.margins = c(0, 0, 0.1, 0), 
    outer.margins = c(0.03, 0, 0, 0))
tmap_save(m1, here("plots", "01-points.png"), width = 2000, height = 2000, dpi = 300)

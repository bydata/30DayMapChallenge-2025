library(tidyverse)
library(sf)
library(here)
library(tmap)
library(osmdata)

#' Source: Stadt Köln
#' Green Space Cadastre Cologne
#' https://offenedaten-koeln.de/dataset/gruenflaechenkataster-koeln-flaechentypen

# Read shapefile
shpfile_url <- "https://offenedaten-koeln.de/sites/default/files/Gruenobjekte_Koeln.zip"
local_path <- here("data", "gruenflaechenkataster-koeln")
local_filepath <- here(local_path, "gruenobjekte.zip")
if (!dir.exists(local_path)) {
  dir.create(local_path)
  download.file(shpfile_url, destfile = local_filepath)
  unzip(local_filepath, exdir = local_path)
}
df_objects <- st_read(here(local_path, "objekte.shp"))
st_crs(df_objects)

df_objects |> 
  st_drop_geometry() |> 
  count(Objekttyp_)

df_objects |> 
  ggplot() +
  geom_sf(fill = "green")

# Translate green area types
object_type_mapping <- c(
  "Bio_Flächen" = "Biotope areas",
  "ForsteigeneF" = "Forested areas",
  "Friedhof" = "Cemeteries",
  "Grünanlage" = "Green spaces",
  "Kleingärten" = "Allotment gardens",
  "Sonderflächen" = "Special green areas",
  "Spielplatz" = "Playgrounds"
)

df_objects <- df_objects |> 
  mutate(
    object_type_en = object_type_mapping[Objekttyp_],
    object_type_en_cat = ifelse(
      object_type_en %in% c("Biotope areas", "Forested areas", "Allotment gardens"),
      object_type_en,
      "Other"
    ),
    object_type_en_cat = factor(object_type_en_cat, levels = c("Biotope areas", "Forested areas", "Allotment gardens", "Other"))
  )


# City boundaries
# Circle of 5 km radius around the Dom
city_center_coords <- data.frame(lon = 6.9557065, lat = 50.9412784)
city_center <- st_as_sf(city_center_coords, coords = c("lon", "lat"), crs = 4326)
city_center <- st_transform(city_center, 25832)
city_center_area <- st_buffer(city_center, dist = 8000)
# Create a mask to hide basemap
boundaries <- getbb("Köln, Deutschland", format_out = "sf_polygon")
boundaries <- st_transform(boundaries, 25832)
# mask_bbox <- st_as_sfc(st_bbox(boundaries) + c(-0.05, -0.05, 0.05, 0.05))
# mask <- st_difference(mask_bbox, boundaries)
mask_bbox <- st_as_sfc(st_bbox(boundaries) + c(-0.05, -0.05, 0.05, 0.05))
mask <- st_difference(mask_bbox, city_center_area)
df_objects_filtered_innercity <- st_filter(st_zm(df_objects), city_center_area,
    .predicate = st_within)
st_crs(city_center_area) == st_crs(df_objects)


color_pal <- c("#8B4789", "#2D5016", "#61A0AF", "#7CB342")
bgcolor <- "#F6F6F6"

m <-
  tm_shape(df_objects_filtered_innercity) +
  tm_polygons(
    fill = "object_type_en_cat",
    col = "object_type_en_cat",
    fill.scale = tm_scale(values = color_pal),
    col.scale = tm_scale(values = color_pal),
    fill_alpha = 0.7,
    col_alpha = 0.3,
    fill.legend = tm_legend(
      title = "Area types",
      position = c(0.9, 0.98),
      frame.lwd = 0,
      bg.color = NA
    ),
    col.legend = tm_legend(show = FALSE)
  ) +
  tm_shape(mask) +
  tm_fill(fill = bgcolor, col = "grey80") +
  tm_basemap("CartoDB.PositronNoLabels") +
  tm_title_out(
    text = "Cologne's Green Spaces",
    size = 2, 
    position = tm_pos_out("center", "top"),
    just = "left", padding.left = 0
  ) +
  tm_title_out(
    text = "\nClassification of green space area types 8 km around the city center",
    size = 0.9,
    position = tm_pos_out("center", "top"),
    just = "left", padding.left = 0
  ) +
  tm_credits(
    text = "Other area types cover parks, cemeteries, special green areas, and playgrounds.\nData: Stadt Köln, OpenStreetMap, CartoDB. Visualization: Ansgar Wolsing",
    position = c(0, 0),
    size = 0.7, just = "left"
  ) +
  tm_layout(
    frame = FALSE, 
    outer.bg.color = bgcolor,
    text.fontfamily = "Source Sans Pro",
    inner.margins = c(0.05, 0, 0.1, 0), 
    outer.margins = c(0.05, 0, 0, 0)
  )
tmap_save(m, here("plots", "05-earth.png"), width = 2000, height = 2000, dpi = 300)

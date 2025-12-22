library(tidyverse)
library(sf)
library(here)
library(tmap)
library(osmdata)

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
  filter(ULAND == "05", UREGBEZ == "3", UKREIS == "15", IstRad == 1) |>
  st_zm()
st_crs(df_unfallorte_filtered)

hex_grid <- st_make_grid(st_union(df_unfallorte_filtered), 
                         cellsize = 50,
                         square = FALSE)
hex_counts <- st_intersects(hex_grid, df_unfallorte_filtered)
hex_df <- st_sf(
  geometry = hex_grid,
  count = lengths(hex_counts)
  ) |> 
  filter(count > 0) |> 
  st_transform(crs = 4326) |> 
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[, "X"],
    lat = st_coordinates(centroid)[, "Y"]) |> 
  st_drop_geometry() |> 
  select(-centroid) |> 
  arrange(-count)
write_csv(hex_df, here("data", "unfallorte-koeln-rad-agg.csv"))

library(tidyverse)
library(sf)
library(here)

#' Source: Stand Wien
#' https://www.data.gv.at/datasets/36a8b9e9-909e-4605-a7ba-686ee3e1b8bf?locale=de
url <- "https://data.wien.gv.at/daten/geo?service=WFS&request=GetFeature&version=1.1.0&typeName=ogdwien:OEFFLINIENOGD&srsName=EPSG:4326&outputFormat=json"
df_lines <- st_read(url)
st_crs(df_lines)
df_lines <- st_transform(df_lines, 3857)

ggplot(df_lines) +
  geom_sf(linewidth = 0.3) +
  theme_void()


df_lines |> 
  st_drop_geometry() |> 
  count(LTYP, LTYPTXT)

ggplot(df_lines) +
  geom_sf(
    aes(col = LTYPTXT), 
    linewidth = 0.3) +
  theme_void()

# Only tram/metro
df_lines |> 
  filter(LTYPTXT %in% c("Straßenbahn", "U-Bahn")) |> 
  ggplot() +
  geom_sf(
    linewidth = 0.3) +
  theme_void()

# All except 
df_lines |> 
  filter(LTYPTXT %in% c("Straßenbahn", "U-Bahn")) |> 
  ggplot() +
  geom_sf(
    linewidth = 0.3) +
  theme_void()





## Create a circular shape
centroid <- df_lines |> 
  st_union() |> 
  st_centroid()
buffered_area <- st_buffer(centroid, dist = 12000, endCapStyle = "ROUND")
df_lines_circular <- st_intersection(df_lines, buffered_area)

# Location of Austria Center Vienna
coords_acv <- data.frame(lon = 16.4111609, lat = 48.2348698)
acv <- st_as_sf(coords_acv, coords = c("lon", "lat"), crs = 4326) |> 
  st_transform(3857)


ggplot() +
  geom_sf(data = df_lines, linewidth = 0.3) +
  geom_sf(data = buffered_area, col = "red") +
  theme_void()

ggplot() +
  geom_sf(data = buffered_area, col = NA, linewidth = 2, fill = "#E8E7FF") +
  geom_sf(data = df_lines_circular, linewidth = 0.2) +
  geom_sf(data = acv, col = "#462346", size = 5) +
  geom_sf(data = buffered_area, col = "#462346", linewidth = 1.2, fill = NA) +
  geom_sf_label(
    data = acv,
    aes(label = "Austria Center"),
    fill = "#462346", col = "white", linewidth = 0,
    size = 4, hjust = 1,
    nudge_x = -600, nudge_y = 1000, family = "Lucida Grande", fontface = "bold"
  ) +
  labs(
    title = "Public Transport Network Vienna",
    caption = "Source: Stadt Wien. Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Lucida Grande", paper = "#F6F6F6") +
  theme(
    text = element_text(color = "#462346"),
    plot.title = element_text(family = "Lucida Grande", face = "bold", hjust = 0.5, size = 16),
    plot.caption = element_text(hjust = 0.5, size = 7),
    plot.margin = margin(4, 4, 4, 4)
  )
ggsave(here("plots", "02-lines.png"), width = 5, height = 5)


#' Font: https://www.wien.gv.at/spezial/cd-manual/grundelemente/typografie/#ersatzschrift-141
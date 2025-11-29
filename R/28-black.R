library(tidyverse)
library(osmdata)
library(sf)
library(ggtext)
library(here)

#' Source:
#' Download Geodatabase manually from
#' https://www.opengeodata.nrw.de/produkte/geologie/geologie/RK/ISRK10/ISRK10KOvektor/

# Get the locations of selected places with coal mines
city_names <- c("Duisburg", "Oberhausen", "Herne", "Dortmund", "Bochum", "Essen", "Gelsenkirchen", 
  "Mülheim a.d. Ruhr", "Bottrop", "Moers", "Kamp-Lintfort", "Hattingen", "Hamm",
  "Castrop-Rauxel", "Recklinghausen")
cities <- map_dfr(
  paste(city_names, "Nordrhein-Westfalen", sep = ","),
  getbb, format_out = "sf_polygon", limit = 1) |> 
  st_centroid()
length(city_names) == nrow(cities)

dput(cities[, c("name", "geometry")])

filepath_geodb <- here("data", "ISRK10KO_EPSG25832_Geodatabase", "ISRK10KO.gdb")
st_layers(filepath_geodb)

df_coalbed <- st_read(filepath_geodb, layer = "KO10_Floez")
glimpse(df_coalbed)

bbox <- st_bbox(df_coalbed)

cities <- st_transform(cities, crs = st_crs(df_coalbed))

# Gradient fill for the map background
fill_dark_gradient <- grid::linearGradient(colours = c("grey15", "black"))

textbox_annotation <- function(x, y, label, width, size = 1.6, fill = NA, ...) {
   annotate(
    GeomTextBox,
    label = label,
    x = x, y = y,
    width = width, fill = fill, box.size = 0, hjust = 0, vjust = 1, color =  "grey90", 
    family = "Roboto Mono", size = size, box.padding = unit(0, "mm"), box.r = unit(0, "mm"),
    ...
  )
}

    
p <- ggplot() + 
  geom_sf(
    data = df_coalbed, 
    col = "white",
    linewidth = 0.05
  ) +
  geom_sf(
    data = cities,
    color = "white"
  ) +
  geom_sf_label(
    data = cities,
    aes(label = name),
    fill = "white", col = "grey2", family = "Roboto Mono SemiBold", size = 1.5,
    alpha = 0.7, linewidth = 0, label.r = unit(0, "mm"),
    position = position_nudge(x = 800, y = -1500)
  ) +
  # Subtitle
  textbox_annotation(
    label = "Coal was mined in numerous locations in the south of the Ruhr region since the 13th century.
    At its peak in 1956, 485 thousand workers were employed in the coal mines,
    but this figure had shrunk by two thirds within the next two decades.
    The last mine, the Prosper Haniel in Bottrop, ceased operations in 2018,
    marking the end of coal mining in the region.",
    x = bbox$xmin, y = bbox$ymax - 15000,
    width = 0.36, fill = "grey15"
  ) +
  # Bottom left annotation
  textbox_annotation(
    label = "A coal seam (*Flöz* in German) is a naturally formed, horizontal layer of coal.
    It is formed over millions of years from plant material that was buried and compressed
    under layers of sediment. The map visualizes the structure of coal seams that were once heavily mined, 
      based on detailed geological mapping of Carboniferous coal layers in the Ruhr region.",
    x = bbox$xmin + 2000, y = bbox$ymin + 9000,
    width = 0.36
  ) +
  # Caption
  textbox_annotation(
    label = "Source: IS RK 10 KO DS - Informationssystem Geologische Karte des
      Rheinisch-Westfälischen Steinkohlengebietes (DL-DE->BY-2.0).<br>
      Locations: OpenStreetMap contributors.<br>
      Visualization: Ansgar Wolsing",
    x = bbox$xmax - 45000, y = bbox$ymin + 4500,
    width = 0.27, size = 1.33
  ) +
  coord_sf(
    xlim = c(bbox$xmin, bbox$xmax - 25000),
    ylim = c(bbox$ymin, bbox$ymax - 15000),
    expand = FALSE) +
  labs(
    title = "COAL SEAMS OF THE RUHR"
  ) +
  theme_void(base_family = "Roboto Mono", ink = "grey90") +
  theme(
    plot.background = element_rect(fill = fill_dark_gradient),
    plot.margin = margin(4, 4, 4, 4),
    plot.title = element_text(
      size = 18, margin = margin(b = 4)),
    plot.title.position = "panel"
  )
ggsave(here("plots", "28-black.png"), width = 6, height = 3)

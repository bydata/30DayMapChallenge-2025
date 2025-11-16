
library(maptiles)
library(sf)
library(tidyverse)
library(tidyterra) 
library(ggtext)
library(here)

tiles <- get_tiles(terra::ext(c(xmin = -2, xmax = 23, ymin = 36, ymax = 55)), 
  provider = "Esri.WorldShadedRelief", crop = TRUE, zoom = 5, forceDownload = TRUE)

revolution_cities <- c("Paris, France", "Palermo, Italy", "Naples, Italy", "Florence, Italy", 
"Torino, Italy", "Bologna, Italy", "Venice, Italy", "Rome, Italy", "Milan, Italy", "Vienna, Austria", 
"Berlin, Germany", "Cologne, Germany", "Prague, Czechia", "Munich, Germany", "Karlsruhe, Germany",
"Frankfurt am Main, Germany", "Potsdam, Germany", "Dresden, Germany", "Budapest, Hungary", "Poznań, Poland")

revolution_cities_geocoded <- map_dfr(
  revolution_cities,
  tidygeocoder::geo
)

revolution_cities_highlight <- c(
  "Berlin, Germany", "Vienna, Austria", "Paris, France", 
  "Milan, Italy", "Budapest, Hungary", "Prague, Czechia")

revolution_cities_geocoded <- revolution_cities_geocoded |> 
  mutate(city = str_extract(address, "(.+)?,", group = 1))|> 
  st_as_sf(coords = c("long", "lat"), crs = 4326)

ggplot() +
  geom_spatraster_rgb(data = tiles, alpha = 0.55) +
  geom_sf(
    data = revolution_cities_geocoded,
    aes(size = ifelse(address %in% revolution_cities_highlight, 4, 2)),
    col = "#c5201aff", shape = 21,
  ) +
  geom_sf(
    data = revolution_cities_geocoded,
    aes(size = ifelse(address %in% revolution_cities_highlight, 4, 2) / 2),
    col = "#c5201aff"
  ) +
  geom_sf_text(
    data = filter(revolution_cities_geocoded, address %in% revolution_cities_highlight),
    aes(label = city),
    size = 3.25, hjust = 0, position = position_nudge(x = 0.5),
    family = "Source Sans Pro SemiBold"
  ) +
  geom_sf_text(
    data = filter(revolution_cities_geocoded, !address %in% revolution_cities_highlight, city != "Potsdam"),
    aes(label = city),
    size = 2.5, hjust = 0, position = position_nudge(x = 0.3),
    family = "Source Sans Pro SemiBold"
  ) +
  geom_sf_text(
    data = filter(revolution_cities_geocoded, city == "Potsdam"),
    aes(label = city),
    size = 2.5, hjust = 1, position = position_nudge(x = -0.3),
    family = "Source Sans Pro SemiBold"
  ) +
  scale_size_identity() +
  coord_sf(expand = FALSE) +
  labs(
    title = "1848 Springtime of the Peoples",
    subtitle = "The most widespread revolutionary wave in European history occurred 
      over the course of one year, from 1848 to 1849. 
      The map shows the locations of the
      <span style='font-family: \"Source Sans Pro SemiBold\"; color: #c5201a'>major uprisings</span>.",
    caption = "Basemap: ESRI. Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Source Sans Pro", paper = "#fbf7f4ff") +
  theme(
    plot.title = element_text(family = "Cormorant Garamond SemiBold",
      size = 24, hjust = 0.5, margin = margin(b = 6)),
    plot.title.position = "panel",
    plot.subtitle = element_textbox(
      width = 0.9, hjust = 0.5, lineheight = 1.2, 
      margin = margin(b = 6), size = 10),
    plot.caption = element_text(margin = margin(t = -15, r = 6), size = 8),
    plot.margin = margin(8, 0, 0, 0)
  )
ggsave(here("plots", "15-fire.png"), width = 4.6, height = 6)

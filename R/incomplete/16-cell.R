library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(here)

#' Source: https://agraratlas.statistikportal.de/#

url_pigs <- "https://www.wcs.nrw.de/stba/agraratlas?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&COVERAGE=K03_Schweinedichte_2020&CRS=EPSG:3035&BBOX=4030000,2680000,4675000,3555000&WIDTH=2000&HEIGHT=2000&FORMAT=GeoTIFF"
raster_pigs <- rast(url_pigs)
describe(raster_pigs)
plot(raster_pigs)
nlyr(raster_pigs)
names(raster_pigs)
sum(values(raster_pigs))
mean(values(raster_pigs))

raster_pigs_clean <- raster_pigs
raster_pigs_clean[raster_pigs_clean == 0] <- NA
names(raster_pigs_clean) <- "class"

url_cattle <- "https://www.wcs.nrw.de/stba/agraratlas?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage&COVERAGE=K04_Rinderdichte_2020&CRS=EPSG:3035&BBOX=4030000,2680000,4675000,3555000&WIDTH=2000&HEIGHT=2000&FORMAT=GeoTIFF"
raster_cattle <- rast(url_cattle)
describe(raster_cattle)
plot(raster_cattle)
nlyr(raster_cattle)
names(raster_cattle)
sum(values(raster_cattle))
mean(values(raster_cattle))

raster_cattle_clean <- raster_cattle
raster_cattle_clean[raster_cattle_clean == 0] <- NA
names(raster_cattle_clean) <- "class"

table(values(raster_pigs_clean))
table(values(raster_cattle_clean))


# Definition of classes
#' Anzahl Schweine je 100 Hektar landwirtschaftlich genutzter Fläche 2020 in einem Raster mit 5 km Weite

pig_classes <- tibble(
  class = 1:5,
  class_center = c(
    12.5,
    162,
    450,
    750,
    1050
  ),
  label = c(
    "\U2264 25",
    "25-299",
    "300-599",
    "600-899",
    "900 or more"
  )
)

summary(raster_pigs_clean)
for (i in seq_along(pig_classes$class)) {
    raster_pigs_clean[raster_pigs_clean == pig_classes$class[i]] <- pig_classes$class_center[i]
}
summary(raster_pigs_clean)
table(values(raster_pigs_clean))

cattle_classes <- tibble(
  class = 1:5,
   class_center = c(
    25,
    75,
    125,
    175,
    250
  ),
  label = c(
    "\U2264 50",
    "50-99",
    "100-149",
    "150-199",
    "200 or more"
  )
)

summary(raster_cattle_clean)
for (i in seq_along(cattle_classes$class)) {
    raster_cattle_clean[raster_cattle_clean == cattle_classes$class[i]] <- cattle_classes$class_center[i]
}
summary(raster_cattle_clean)
table(values(raster_cattle_clean))



# Background shape 
country_shape <- rnaturalearth::ne_countries(scale = 50, country = "Germany")
st_crs(country_shape)
country_shape <- st_transform(country_shape, crs = 3035)
plot_caption <- "Source: Atlas Agrarstatistik Deutschland. Visualization: Ansgar Wolsing"


bgcolor <- "#F6F6F6"
country_bgcolor <- "#E5E5DE"

# Pigs
ggplot() +
  geom_sf(
    data = country_shape,
    fill = country_bgcolor, linewidth = 0
  ) +
  stat_spatraster(
    data = raster_pigs_clean,
    geom = "point",
    aes(size = floor(after_stat(value))),
    fill = "red", col = "white", shape = 21, stroke = 0.2,
    maxcell = 3000
  ) +
  scale_size_continuous(
    range = c(0.75, 2.5),
    breaks = pig_classes$class_center,
    labels = pig_classes$label
    ) +
  labs(
    title = "",
    subtitle = "Number of pigs per 100 hectares of agricultural land in 2020",
    caption = plot_caption,
    size = "Number of pigs"
  ) +
  theme_void(base_family = "Gill Sans", paper = bgcolor) +
  theme(
    legend.key.height = unit(5, "mm")
  )
ggsave(here("plots", "16-cell-pigs.png"), width = 5, height = 5)


# Kettle
ggplot() +
  geom_sf(
    data = country_shape,
    fill = country_bgcolor, linewidth = 0
  ) +
  stat_spatraster(
    data = raster_cattle_clean,
    geom = "point",
    aes(size = ceiling(after_stat(value))),
    fill = "blue", col = "white", shape = 21, stroke = 0.2,
    maxcell = 3000
  ) +
  scale_size_continuous(
    range = c(0.75, 2.5),
    breaks = cattle_classes$class_center,
    labels = cattle_classes$label
    ) +
  labs(
    title = "",
    subtitle = "Number of cattle per 100 hectares of agricultural land in 2020",
    caption = plot_caption,
    size = "Number of cattle"
  ) +
  guides(size = guide_legend()) + 
  theme_void(base_family = "Gill Sans", paper = bgcolor) +
  theme(
    legend.key.height = unit(5, "mm")
  )
ggsave(here("plots", "16-cell-cattle.png"), width = 5, height = 5)

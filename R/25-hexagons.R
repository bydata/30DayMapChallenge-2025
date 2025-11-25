library(tidyverse)
library(sf)
library(xml2)
library(here)


#' Source: https://kvb.koeln/service/open_data.html
#' Try editor: https://mrin9.github.io/OpenAPI-Viewer/#/load/https%3A%2F%2Fraw.githubusercontent.com%2Fnextbike%2Fapi-doc%2Fmaster%2Fmaps%2Fnextbike-maps.openapi.yaml

endpoint_url <- "https://api.nextbike.net/maps/nextbike-live.xml?city=14"
nextbike_xml <- read_xml(endpoint_url)
place_nodes <- xml_find_all(nextbike_xml, "//place")

df_nextbike <- map_dfr(
  place_nodes, function(x) {
  tibble(
    # Attributes are extracted using xml_attr()
    lat = xml_attr(x, "lat"),
    lon = xml_attr(x, "lng"),
    uid = xml_attr(x, "uid"),
    name = xml_attr(x, "name"),
    booked_bikes = xml_attr(x, "booked_bikes"),
    bikes = xml_attr(x, "bikes"),
    terminal_type = xml_attr(x, "terminal_type")
  )
})

df_nextbike <- df_nextbike |> 
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    bikes = as.integer(bikes)
  ) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot(df_nextbike) +
  geom_sf(aes(size = bikes)) +
  theme_void()

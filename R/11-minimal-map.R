library(tidyverse)
library(ggtext)
library(sf)
library(here)

#' Source: 
#' https://itiner-e.org/about
#' 
#' Citation:
#' Brughmans, T., de Soto, P., Pažout, A. and Bjerregaard Vahlstrup, P. (2024)
#' Itiner-e: the digital atlas of ancient roads. https://itiner-e.org/
#' 
#' Nature article:
#' https://www.nature.com/articles/d41586-025-03626-z

url <- "https://itiner-e.org/route-segments/download"
df_roads <- st_read(url)
st_crs(df_roads)

# Which road types?
df_roads |> 
  st_drop_geometry() |> 
  count(type)


df_roads |> 
  filter(type %in% c("Main Road", "Secondary Road")) |> 
  ggplot() +
  geom_sf(linewidth = 0.1, alpha = 0.7) +
  theme_void()

df_roads |> 
  filter(type %in% c("Main Road", "Secondary Road")) |> 
  # filter(lowerDate <= 100 & upperDate >= 0) |> 
  ggplot() +
  geom_sf(
    aes(linewidth = ifelse(type == "Main Road", 0.2, 0.1)), 
      alpha = 0.7) +
  annotate(
    "richtext",
    label = "Roads of the<br>Roman Empire",
    x = 17, y = 54, family = "EB Garamond", size = 8, hjust = 0,
    fill = NA, label.size = 0
  ) +
  scale_linewidth_identity() +
  coord_sf(clip = "off") +
  labs(
    caption = "Source: Brughmans, T., de Soto, P., Pažout, A. and Bjerregaard Vahlstrup, P. (2024):
      Itiner-e: the digital atlas of ancient roads. itiner-e.org.
      Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "EB Garamond", paper = "#f7f7f2") +
  theme(
    plot.title = element_blank(),
    plot.caption = element_textbox(
      width = 1, hjust = 0.5
    ),
    plot.margin = margin(4, 4, 4, 4)
  )
ggsave(here("plots", "11-minimal-map.png"), width = 5, height = 5, dpi = 500)

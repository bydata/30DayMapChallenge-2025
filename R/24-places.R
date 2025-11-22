library(tidyverse)
library(sf)
library(ggtext)
library(here)

#' Source: Federal Agency for Cartography and Geodesy
#' https://gdz.bkg.bund.de/index.php/default/open-data/geographische-namen-1-250-000-gn250.html
#' Documentation (DE): https://sg.geodatenzentrum.de/web_public/gdz/dokumentation/deu/gn250.pdf

url <- "https://daten.gdz.bkg.bund.de/produkte/sonstige/gn250/aktuell/gn250.utm32s.csv.zip"
local_filepath <- here("data", "gn250.utm32s.csv.zip")
local_filepath_extracted <- here("data", "gn250.utm32s.csv", "gn250", "GN250.csv")
if (!file.exists(local_filepath_extracted)) {
  download.file(url, destfile = local_filepath)
  unzip(local_filepath, exdir = here("data"))
}
df_geonames <- read_delim(
  local_filepath_extracted,
  delim = ";", locale = locale(decimal_mark = "."))

df_gemeinden <- df_geonames |> 
  filter(OBA == "AX_Gemeinde") |> 
  st_as_sf(coords = c("RECHTS", "HOCH"), crs = 25832,
  remove = FALSE)

bbox <- st_bbox(df_gemeinden)
min_inhabitants <- min(df_gemeinden$EWZ)
max_inhabitants <- max(df_gemeinden$EWZ)

create_plot <- function(df, density = 0.05) {
  stopifnot(density > 0 & density <= 1)
  df |> 
    slice_max(prop = density, order_by = EWZ) |>
    mutate(NAME = fct_reorder(NAME, -EWZ)) |> 
    ggplot() +
    geom_sf_text(
      aes(label = toupper(NAME), size = EWZ, alpha = EWZ), 
      family = "Roboto Condensed"
    ) +
    scale_size_continuous(
      range = c(1, 10), limits = c(min_inhabitants, max_inhabitants)) +
    scale_alpha_continuous(range = c(0.7, 1)) +
    coord_sf(
      clip = "off", 
      xlim = c(bbox["xmin"], bbox["xmax"]), 
      ylim = c(bbox["ymin"], bbox["ymax"])) +
    guides(size = "none", alpha = "none") +
    labs(
      title = "Germany by Names",
      caption = "The top 5 percent of municipalities by inhabitants. 
        **Source:** Federal Agency for Cartography and Geodesy 
      (Datenlizenz Deutschland). **Visualization:** Ansgar Wolsing."
    ) +
    theme_void(base_family = "Inter 18pt", paper = "#27233A", ink = "white") +
    
    theme(
      plot.title = element_text(
        family = "Roboto Condensed Medium Italic", hjust = 0.5, size = 32),
      plot.caption = element_textbox(
        width = 1, hjust = 0.5, lineheight = 1.2),
      plot.margin = margin(4, 4, 8, 4)
    )
}

# Create folder to store the frames
plot_output_path <- here("plots", "24-places")
if (!dir.exists(plot_output_path)) {
  dir.create(plot_output_path)
}


# Create one frame building up the 10 largest cities

largest_cities_names <- df_gemeinden |> 
  st_drop_geometry() |> 
  slice_max(order_by = EWZ, n = 10) |> 
  pull(NAME)
length(largest_cities_names)

walk(
  seq_along(largest_cities_names),
  function(i) {
    p <- df_gemeinden |> 
      filter(NAME %in% largest_cities_names[1:i]) |> 
      create_plot(density = 1)
    filename <- sprintf("24-places-cities-%d.png", i)
    ggsave(here(plot_output_path, filename), width = 6.5, height = 8)
  }) 


p <- create_plot(df_gemeinden, density = 1)
ggsave(here("plots", "24-places", "24-places-100.png"), width = 6.5, height = 8)

p <- create_plot(df_gemeinden, density = 0.2)
ggsave(here("plots", "24-places-020.png"), width = 6.5, height = 8)

p <- create_plot(df_gemeinden, density = 0.1)
ggsave(here("plots", "24-places-010.png"), width = 6.5, height = 8)

p <- create_plot(df_gemeinden, density = 0.05)
ggsave(here("plots", "24-places-005.png"), width = 6.5, height = 8)

p <- create_plot(df_gemeinden, density = 0.025)
ggsave(here("plots", "24-places-0025.png"), width = 6.5, height = 8)

p <- create_plot(df_gemeinden, density = 0.01)
ggsave(here("plots", "24-places-001.png"), width = 6.5, height = 8)

p <- create_plot(df_gemeinden, density = 0.005)
ggsave(here("plots", "24-places-0005.png"), width = 6.5, height = 8)

p <- create_plot(df_gemeinden, density = 0.001)
ggsave(here("plots", "24-places-0001.png"), width = 6.5, height = 8)

library(tidyverse)
library(sf)
library(ggtext)
library(magick)
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


# Function to create plots with consistent dimensions and scales
create_plot <- function(df, density = 0.05) {
  stopifnot(density > 0 & density <= 1)
  df <- df |> 
    slice_max(prop = density, order_by = EWZ) |>
    mutate(NAME = fct_reorder(NAME, -EWZ)) 
  n_cities <- nrow(df)
  
  df |> 
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
      subtitle = sprintf(
        "<span style='font-size: 40px; font-family:\"Roboto Mono Medium\"'>%d</span>
          <br>municipalities ordered by<br>number of inhabitants",
        n_cities),
      caption = "**Source:** Federal Agency for Cartography and Geodesy 
      (Datenlizenz Deutschland). **Visualization:** Ansgar Wolsing.<br>"
    ) +
    theme_void(base_family = "Roboto Condensed", paper = "#27233A", ink = "white") +
    
    theme(
      plot.title = element_text(
        family = "Roboto Slab Medium", hjust = 0.5, size = 36),
      plot.subtitle = element_markdown(
        size = 14, hjust = 0.5, halign = 0.5, color = "#ADABBD",
        margin = margin(t = 16)),
      plot.caption = element_textbox(
        width = 1, hjust = 0.5, lineheight = 1.2, size = 11,
        margin = margin(b = 8)),
      plot.margin = margin(4, 4, 4, 4)
    )
}

# Create folder to store the frames
plot_output_path <- here("plots", "24-places")
if (!dir.exists(plot_output_path)) {
  dir.create(plot_output_path)
}

plot_width <- 5
plot_height <- 5/6.5 * 8
plot_dpi <- 200

# Create one frame building up the n largest cities
n_cities <- 50
# use NNID to avoid plotting multiple municipalities with the same name
largest_cities_id <- df_gemeinden |> 
  st_drop_geometry() |> 
  slice_max(order_by = EWZ, n = n_cities, with_ties = FALSE) |> 
  pull(NNID)
length(largest_cities_id)

walk(
  seq_along(largest_cities_id),
  function(i) {
    p <- df_gemeinden |> 
      filter(NNID %in% largest_cities_id[1:i]) |> 
      create_plot(density = 1)
    filename <- sprintf("24-places-cities-%04d.png", i)
    ggsave(
      here(plot_output_path, filename), 
      width = plot_width, height = plot_height, dpi = plot_dpi,
      scale = 300 / plot_dpi
    )
  }
) 

# Build the frames based on proportion of largest cities
nrow(df_gemeinden)
cities_prop_steps <- c(seq(0.01, 0.1, 0.0025), seq(0.125, 0.25, 0.025), seq(0.3, 1, 0.1))
floor(cities_prop_steps * nrow(df_gemeinden))

walk(
  cities_prop_steps,
  function(prop) {
    p <- df_gemeinden |> 
      slice_max(order_by = EWZ, prop = prop, with_ties = FALSE) |> 
      create_plot(density = 1)
    filename <- sprintf("24-places-prop-%s.png", prop)
    ggsave(
      here(plot_output_path, filename), 
      width = plot_width, height = plot_height, dpi = plot_dpi,
      scale = 300 / plot_dpi
    )
  }
) 

# Read the frames
png_files_cities <- list.files(
  path = plot_output_path,
  pattern = ".*cities.*\\.png$",
  full.names = TRUE
)
png_files_prop <- list.files(
  path = plot_output_path,
  pattern = ".*prop.*\\.png$",
  full.names = TRUE
)
png_files <- c(png_files_cities, png_files_prop, 
  # repeat the last frame
  rep(png_files_prop[length(png_files_prop)], 10)
)

img_sequence <- image_read(png_files)

# Write the GIF
img_sequence |> 
  image_animate(
    fps = 10,
    loop = 0
  ) |> 
  image_write(path = here("plots", "24-places.gif"))

# Write video
img_sequence |> 
  image_animate(
    fps = 10,
    loop = 0
  ) |> 
  image_write_video(
    here("plots", "24-places.mp4"), framerate = 10)

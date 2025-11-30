library(tidyverse)
library(terra)
library(tidyterra)
library(ggspatial)
library(here)
library(patchwork)
library(ggtext)

# Define WMS parameters
wms_url <- "https://www.wms.nrw.de/umwelt/laerm"

bbox <- c(xmin = 332899,
          ymin = 5633657,
          xmax = 372570,
          ymax = 5653837)
ext_obj <- ext(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"])
crs <- "EPSG:25832"

get_wms_layer <- function(layer_id, wms_url, bbox, crs, width = 2076, height = 1056) {
  wms_request <- paste0(
    wms_url,
    "?SERVICE=WMS",
    "&VERSION=1.3.0",
    "&REQUEST=GetMap",
    "&LAYERS=", layer_id,
    "&STYLES=",
    "&CRS=", crs,
    "&BBOX=", paste(bbox, collapse = ","),
    "&WIDTH=", width,
    "&HEIGHT=", height,
    "&FORMAT=image/png",
    "&TRANSPARENT=true"
  )
  
  temp_file <- tempfile(fileext = ".png")
  download.file(wms_request, temp_file, mode = "wb", quiet = TRUE)
  wms_data <- rast(temp_file)
  
  # Create a properly georeferenced raster
  wms_georef <- rast(nrows = nrow(wms_data), 
                     ncols = ncol(wms_data),
                     nlyrs = nlyr(wms_data),
                     xmin = bbox["xmin"], 
                     xmax = bbox["xmax"],
                     ymin = bbox["ymin"], 
                     ymax = bbox["ymax"],
                     crs = crs)
  values(wms_georef) <- values(wms_data)
  
  # Make white/near-white pixels transparent
  if (nlyr(wms_georef) == 4) {
    vals <- values(wms_georef)
    white_pixels <- vals[,1] > 250 & vals[,2] > 250 & vals[,3] > 250
    vals[white_pixels, 4] <- 0
    values(wms_georef) <- vals
  }

  unlink(temp_file)
  wms_georef
}

# Download the road noise raster
road_noise_raster <- get_wms_layer("STR_DEN", wms_url, bbox, crs)

# Convert to data frame for plotting
rast_df <- as.data.frame(road_noise_raster, xy = TRUE)

# Filter out white/near-white pixels (all RGB > 250)
if (ncol(rast_df) >= 5) { 
  color_cols <- names(rast_df)[3:5]
  rast_df <- rast_df %>%
    filter(!(get(color_cols[1]) > 250 & 
             get(color_cols[2]) > 250 & 
             get(color_cols[3]) > 250))
}


p <- ggplot() +
  annotation_map_tile(type = "cartodark", zoomin = 1, progress = "none") +
  geom_raster(
    data = rast_df, 
    aes(
      x = x, y = y,
      fill = rgb(rast_df[[3]], rast_df[[4]], rast_df[[5]], maxColorValue = 255)),
    interpolate = TRUE,
    alpha = 0.8
  ) +
  scale_fill_identity() +
  coord_sf(crs = crs, expand = FALSE) +
  labs(
    title = "Road Noise Pollution in the Cologne Area",
    subtitle = "24-hour LDEN noise levels from major and urban road traffic, 
      for highways and primary roads with over<br>3 million vehicles per year,
      plus additional noise-relevant streets within urban areas.",
    caption = "Source: GEOPortal.NRW, Landesamt für Natur, Umwelt und Klima Nordrhein-Westfalen 
      (dl-zero-de/2.0).
      Basemap: CartoDB. Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Source Sans Pro", paper = "#303030", ink = "white") +
  theme(
    plot.title = element_text(
      family = "Source Sans Pro SemiBold", size = 18,
      margin = margin(t = 4, l = 4, r = 4, b = 4)),
    plot.subtitle = element_textbox(
      width = 0.9, margin = margin(t = 4, l = 4, r = 4, b = 4)),
    plot.caption = element_textbox(
      width = 1, margin = margin(t = 4, l = 4, r = 4, b = 2))
  )

#' Legend:
#' https://www.wms.nrw.de/umwelt/laerm?format=image%2Fpng&layer=STR_DEN&sld_version=1.1.0&request=GetLegendGraphic&service=WMS&version=1.1.1&styles=
#' 55-59: #E2F3BF 
#' 60-64: #F3C683 
#' 65-69: #CD463F 
#' 70-74: #62074D
#' >= 75: #3F0947


# Legend categories
noise_legend <- tibble(
  x = 1, y = 1,
  class = factor(
    c("55–59 dB(A)", "60–64 dB(A)", "65–69 dB(A)", "70–74 dB(A)", "≥ 75 dB(A)"),
    levels = c("55–59 dB(A)", "60–64 dB(A)", "65–69 dB(A)", "70–74 dB(A)", "≥ 75 dB(A)")
  )
)
noise_colors <- c(
  "55–59 dB(A)" = "#E2F3BF",
  "60–64 dB(A)" = "#F3C683",
  "65–69 dB(A)" = "#CD463F",
  "70–74 dB(A)" = "#62074D",
  "≥ 75 dB(A)"  = "#3F0947"
)

# Add legend to the map
p_legend <- ggplot() +
  geom_point(
    data = noise_legend,
    aes(x = x, y = y, fill = class),
    shape = 22, size = 6
  ) +
  scale_fill_manual(
    name = "L_den / dB(A)",
    values = noise_colors,
    guide = guide_legend(override.aes = list(size = 6))
  ) +
  scale_x_continuous(limits = c(0, 0)) +
  coord_cartesian(xlim = c(0, 0), ylim = c(0, 0), expand = FALSE) +
  theme_void(base_family = "Source Sans Pro", paper = NA) +
  theme(
    legend.position = c(0.5, 0.5),
    legend.background = element_rect(fill = alpha("#303030", 0.4), color = "blue"),
    legend.text = element_text(color = "white", size = 8),
    legend.title = element_text(color = "white", size = 10, face = "bold"),
    legend.key.height = unit(4, "mm")
  )

p + inset_element(p_legend, left = 0.08, bottom = 0.1, right = 0.28, top = 0.3, align_to = "full") +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "#303030", color = "#303030"),
      plot.margin = margin(0, 0, 0, 0)))
ggsave(here("plots", "29-raster.png"), width = 8, height = 5, dpi = 300, bg = "#303030")

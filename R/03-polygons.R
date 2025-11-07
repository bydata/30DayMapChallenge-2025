library(tidyverse)
library(sf)
library(osmdata)
library(osrm)
library(terra)
library(here)
library(ggtext)

endpoint_url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/bildung_und_schule/schulstandorte/MapServer/0/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=pjson"
df_schools <- st_read(endpoint_url)
st_crs(df_schools)

df_schools |> 
  st_drop_geometry() |> 
  count(schulform, sort = TRUE)

df_schools |> 
  ggplot() +
  geom_sf()


# Get city shape
city_name <- "Cologne, Germany"
city_shp <- getbb(city_name, format_out = "sf_polygon")


# Rhine in Cologne
koeln_bbox <- st_bbox(city_shp)  #c(6.77, 50.5, 7.16, 51.08)
rhein_koeln <- opq(bbox = koeln_bbox) |> 
  add_osm_feature(key = "waterway", value = "river") |> 
  add_osm_feature(key = "name", value = "Rhein") |> 
  osmdata_sf()
rhein_lines <- rhein_koeln$osm_lines
rhein_lines_koeln <- st_intersection(rhein_lines, st_buffer(city_shp, dist = 100))
st_crs(rhein_lines_koeln)
st_crs(city_shp)

# Generate a grid of points within the city boundary
buffer <- 1000
grid_points <- st_make_grid(city_shp, cellsize = 0.001, what = "centers") %>%
  st_sf() %>%
  st_intersection(st_buffer(city_shp, dist = buffer))

# Calculate walking times to the nearest supermarket for each grid point
mobility_mode <- "bike"
school_type <- "Realschule" # "Hauptschule" # "Gesamtschule" # "Gymnasium"

path_time_to_school <- here("data", paste0(paste("time-to-school", mobility_mode, school_type, sep = "-"), ".rds"))

if (TRUE) {
  df_schools_filtered <- filter(df_schools, schulform == school_type)
  chunk_size <- 100
  grid_length <- nrow(grid_points)
  chunk_start <- seq(1, grid_length, chunk_size)
  chunk_end <- chunk_start + chunk_size - 1
  chunk_end[which.max(chunk_end)] <- pmin(chunk_end[which.max(chunk_end)], grid_length)
  
  durations <- map2(
    chunk_start, chunk_end,
    function(x, y) {
      routing_table <- osrmTable(
        src = st_geometry(grid_points[x:y, ]),
        dst = st_geometry(df_schools_filtered),
        osrm.profile = mobility_mode
      ) 
      # calculate the minimum per grid cell
      min_durations <- map_dbl(
        seq_len(nrow(routing_table$durations)),
        function(x) min(routing_table$durations[x,]))
      return(min_durations)
    }
  )
  write_rds(durations, path_time_to_school)
} else {
  durations <- read_rds(path_time_to_school)
}


grid_points$path_time <- unlist(durations)

raster_template <- rast(
  extent = st_bbox(st_buffer(city_shp, dist = buffer)), 
  resolution = 0.001,
  crs = st_crs(city_shp)$wkt
)
raster_interpolation <- rasterize(grid_points, raster_template, field = "path_time", 
  method = "bilinear",
  fun = min)
summary(values(raster_interpolation))
raster_interpolation_masked <- mask(raster_interpolation, vect(city_shp))
raster_smoothed <- focal(raster_interpolation_masked, 
                         w = 5,
                         fun = mean, 
                         na.rm = TRUE)


bg_color <- "#F6F6F6"
isochrone_breaks <- c(0, 2, 5, 10, 15, 30, Inf)
isochrone_labels <- c(
  "Less than 2", "2-5", "5-10", "10-15", "15-30", "More than 30"
)


p <- ggplot() +
  ggfx::with_shadow(
    geom_sf(data = city_shp, fill = bg_color, color = bg_color, linewidth = 1.5),
    colour = "#727272", x_offset = 8, y_offset = 8
  ) +
  geom_contour_filled(
    data = as.data.frame(raster_smoothed, xy = TRUE),
    aes(x = x, y = y, z = focal_mean, fill = after_stat(level)),
    breaks = isochrone_breaks, alpha = 0.67) +
  geom_sf(data = rhein_lines_koeln, color = bg_color, size = 1.9) +
  scale_fill_brewer(
    palette = "PuRd", direction = -1, labels = isochrone_labels) +
  guides(
    fill = guide_legend(
      title = "Path time by bicycle (in minutes)", title.position = "top",
    override.aes = list(color = "grey20", linewidth = 0.1))) +
  # labs(
  #   title = "...",
  #   subtitle = "...
  #   The colors indicate how long it takes to get to the nearest school by bike.",
  #   caption = "Source: Stadt Köln, OpenStreetMap contributors.
  #   Visualization: Ansgar Wolsing"
  # ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = NA, fill = NA),
    legend.position = "inside",
    legend.position.inside = c(0.75, 0.9),
    legend.direction = "horizontal",
    plot.title = element_markdown(face = "bold", size = 14),
    plot.title.position = "plot",
    plot.subtitle = element_textbox(width = 1, lineheight = 1.15),
    plot.caption = element_markdown(),
    plot.margin = margin(4, 4, 4, 4)
  )
ggsave(here("plots", "03-polygons", paste0(paste("03-polygons", mobility_mode, school_type, sep = "-"), ".png")), width = 5, height = 5)

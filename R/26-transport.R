library(tidyverse)
library(sf)
library(here)
library(tmap)

# Run 26-transport/parse-xml-nextbike.R to generate df_bikes dataframe

df_bikes <- read_rds(here("data", "26-transport", "df_bikes.rds"))
glimpse(df_bikes)

df_bikes_prep <- df_bikes |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  st_as_sf(
    coords = c("place_lng", "place_lat"), 
    crs = 4326,
    remove = FALSE) |> 
  st_transform(25832)


# Check for the most used bikes (= most different locations)
df_bikes_prep |> 
  st_drop_geometry() |> 
  distinct(bike_number, place_lat, place_lng) |> 
  count(bike_number, sort = TRUE) 

selected_bike <- "224268"
df_bikes_prep |> 
  st_drop_geometry() |> 
  filter(bike_number == selected_bike) 

df_bikes_prep |> 
  filter(bike_number == selected_bike) |> 
  ggplot() +
  geom_sf()

df_bikes_selected_bike <- df_bikes_prep |> 
  filter(bike_number == selected_bike) |> 
  select(bike_number, date_time, place_lng, place_lat) |> 
  # create a new id once the bike is tracked at a different location
  mutate(
    place_lng_lat = sprintf("%s|%s", place_lng, place_lat),
    moved = place_lng_lat != lag(place_lng_lat),
    moved = replace_na(moved, FALSE),
    move_id = cumsum(moved)
  ) 

time_gap_threshold <- duration(30, "minutes")
df_bikes_selected_bike_sessions <- df_bikes_selected_bike |> 
  group_by(bike_number, move_id) |> 
  summarize(
    start_time = min(date_time),
    end_time = max(date_time),
    .groups = "drop"
  ) |> 
  mutate(
    interrupted = difftime(start_time, lag(start_time), units = "mins") > time_gap_threshold,
    interrupted = replace_na(interrupted, FALSE),
    session_id = factor(cumsum(interrupted)),
  ) |> 
  mutate(
    session_label = sprintf(
      "%02d:%02d to %02d:%02d", 
      hour(min(start_time)), minute(min(start_time)),
      # start_time of the last event in a session
      hour(max(start_time)), minute(max(start_time))
    ),
    .by = session_id
  ) |> 
  select(-interrupted) |> 
  # mark first and last event of each session
  group_by(bike_number, session_id) |> 
  mutate(start_end = case_when(
    move_id == min(move_id) ~ "Start",
    move_id == max(move_id) ~ "End",
    TRUE ~ ""
  )) |> 
  ungroup() |> 
  # remove any session that only consists of one (or two) events
  filter(n_distinct(move_id) > 2, .by = session_id)

# How many sessions?
length(unique(df_bikes_selected_bike_sessions$session_id))

# City boundaries
boundaries <- osmdata::getbb("Köln, Deutschland", format_out = "sf_polygon")
# Map center
map_center <- st_union(df_bikes_selected_bike) |> 
  st_centroid()
map_center <- st_transform(map_center, 25832)
boundaries <- st_transform(boundaries, 25832)

# Circle of 5 km radius
map_center_area <- st_buffer(map_center, dist = 4000)
st_crs(map_center_area)

# Create a mask to hide basemap
mask_bbox <- st_as_sfc(st_bbox(boundaries) + c(-0.05, -0.05, 0.05, 0.05))
mask <- st_difference(mask_bbox, map_center_area)

tmap_mode("plot")
marker_icon_path <- "/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/leaflet/htmlwidgets/lib/leaflet/images/marker-icon.png"
bgcolor <- "#F6F6F6"
m <- tm_basemap("Esri.WorldGrayCanvas") +
  tm_shape(map_center_area) +
  tm_shape(df_bikes_selected_bike_sessions) +
  # tm_scale_bar(position = tm_pos_out("center", "bottom")) +
  tm_dots(
    fill = "grey20", fill_alpha = 0.6, size = 0.15,
    size.legend = tm_legend(
      title = "# of accidents", text.size = 0.7, title.size = 0.7,
      position = c(0.95, 1.0), bg.color = NA, frame = FALSE)) +
  tm_facets_wrap(
    by = "session_label",
    ncol = 5, free.coords = FALSE
  ) +
  tm_shape(filter(df_bikes_selected_bike_sessions, start_end %in% c("Start", "End"))) +
  tm_symbols(
    shape = marker_icon_path,
    size = 0.3
  ) +
  tm_facets_wrap(
    by = "session_label",
    ncol = 5, free.coords = FALSE
  ) +
  tm_shape(mask) +
  tm_fill(col = bgcolor, fill_alpha = 1, size = 1, border.col = "grey20") +
  tm_title_out(
    text = "A Day in the Life of a Rented Bike",
    size = 2, 
    position = tm_pos_out("center", "top"),
    just = "left", padding.left = 0
  ) +
  tm_title_out(
    text = "\nIn Cologne, the public transport company KVB offers rental bikes from Nextbike. The locations of the bikes can be queried via an open API.\nThe map sections show the minute-by-minute locations of a bike throughout the day (25 November 2025).",
    size = 1.2,
    position = tm_pos_out("center", "top"),
    just = "left", padding.left = 0
  ) +
  tm_title_out(
    text = "Data: Nextbike, OpenStreetMap, Esri. Visualization: Ansgar Wolsing",
    position = tm_pos_out("center", "bottom"),
    size = 0.5, just = "right"
  ) +
  tm_layout(
    frame = FALSE, 
    outer.bg.color = bgcolor,
    text.fontfamily = "Source Sans Pro",
    panel.label.bg.color = "transparent", 
    panel.label.color = "grey35",
    panel.label.size = 0.8,
    panel.label.fontface = "bold",
    panel.label.frame = FALSE,
    attr.outside = TRUE
  )
tmap_save(m, here("plots", "26-transport.png"), width = 1800, height = 1200, dpi = 300)

# tmap_icons("/Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/leaflet/htmlwidgets/lib/leaflet/images/marker-icon.png")

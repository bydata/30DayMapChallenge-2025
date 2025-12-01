library(tidyverse)
library(sf)
library(jsonlite)
library(gganimate)
library(ggtext)
library(smoothr)
library(here)

filepath <- here("data", "running.gpx")
st_layers(filepath)
df_points <- st_read(filepath, layer = "track_points")
df_points <- df_points |> 
  select(track_seg_point_id, time, geometry)
df_tracks <- st_read(filepath, layer = "tracks")
df_tracks_smooth <- smooth(df_tracks, method = "ksmooth", smoothness = 10)

ggplot() +
  geom_sf(data = df_tracks, color = "blue") +
  geom_sf(data = df_tracks_smooth, color = "red")

# Read actitivity JSON
activities <- read_json("~/Documents/strava-running-activities.json")[[1]]
str(head(activities), max.level = 1)
# Drop tages and create dataframe
df_activities <- map_dfr(activities, function(x) {x$tags <- NULL; x})
df_activities <- df_activities |> 
  mutate(
    start_time = as_datetime(start_time),
    start_date = as_date(start_time)
  ) |> 
  filter(year(start_date) == 2025)

df_activities <- df_activities |> 
  select(
    id,
    start_date,
    start_time
  ) |> 
  arrange(start_time) |> 
  mutate(
    run_number = row_number()
  )
nrow(df_points)

# Select evenly spaced points
nth_event <- 20
df_points_sampled <- df_points[seq(1, nrow(df_points), by = nth_event), ] |> 
  arrange(track_seg_point_id)
nrow(df_points_sampled)

# Combine sampled points and activities - expand the dataframe
df_combined <- df_activities |> 
  mutate(dummy = 1) |> 
  full_join(
    mutate(df_points_sampled, dummy = 1),
    by = join_by(dummy), relationship = "many-to-many"
  ) |> 
  select(-c(dummy, time)) |> 
  mutate(state_id = 10e6 * run_number + track_seg_point_id) |> 
  st_as_sf(crs = st_crs(df_points_sampled))

bbox <- st_bbox(df_tracks)

df_combined_slice <- df_combined[1:(25 * nrow(df_points_sampled)), ]
df_combined_slice <- df_combined


highlight_color <- "#FF8CF0"
bgcolor <- "#0D0121"
fill_linear_gradient <- grid::linearGradient(c("#0D0121", "#A066FF"))

p <- df_combined_slice |> 
  ggplot() +
  geom_sf(
    data = df_tracks_smooth, 
    col = "#A066FF", linewidth = 2.5
  ) +
  geom_richtext(
    aes(
      x = bbox["xmin"], y = bbox["ymin"],
      label = sprintf(
        "<b style='font-size: 36pt'>Run</b> <b style='font-size: 60pt; color:%s'>#%d</b><br>on %s", 
        highlight_color, run_number, format(start_date, "%B %d"))
    ),
    color = "#C6C4E8",
    nudge_x = 0.02 * (bbox["xmax"] - bbox["xmin"]),
    nudge_y = 0.2 * (bbox["ymax"] - bbox["ymin"]),
    hjust = 0, 
    fill = NA, label.size = 0, family = "Fira Sans", lineheight = 1, size = 6
  ) +
  geom_sf(
    col = bgcolor, size = 10
  ) +
  geom_sf(
    col = highlight_color, size = 8
  ) +
  labs(
    title = "My Runs",
    subtitle = "January to November 2025",
    caption = "One exemplary track tracked via the Strava app.<br>Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Fira Sans", ink = "white", base_size = 10) +
  theme(
    plot.background = element_rect(color = NA, fill = fill_linear_gradient),
    plot.title = element_text(
      family = "Fira Sans SemiBold", size = 18 , hjust = 0.5
    ),
    plot.subtitle = element_text(
      hjust = 0.5, margin = margin(t = 0)),
    plot.caption = element_markdown(hjust = 0, lineheight = 1.2),
    plot.margin = margin(4, 4, 4, 4)
  )

p_anim <- p +
  transition_states(state_id) +
  # ease_aes("linear")
  ease_aes("cubic-in-out")

animate(
  p_anim, fps = 10, duration = 120, end_pause = 0, rewind = FALSE,
  width = 400, height = 395, scale = 1.5, units = "px", res = 100, bg = fill_linear_gradient)
anim_save(here("plots", "04-my-data.gif"))

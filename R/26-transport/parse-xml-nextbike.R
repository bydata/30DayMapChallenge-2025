library(tidyverse)
library(xml2)
library(here)

# Read files
input_path <- here("data", "26-transport")
xml_files <- list.files(here(input_path), pattern = "data_.*\\.xml")
xml_docs <- map(here(input_path, xml_files), read_xml)
xml_docs <- set_names(xml_docs, str_extract(xml_files, "data_(\\d{8}_\\d{6})\\.xml", group = 1))

# Parse XML files and generate dataframe
parse_xml <- function(xml_doc) {
  places <- xml_find_all(xml_doc, "//place")
  bikes_data <- list()
  # Iterate through all places elements
  for (place in places) {
    place_uid <- xml_attr(place, "uid")
    place_name <- xml_attr(place, "name")
    place_lat <- xml_attr(place, "lat")
    place_lng <- xml_attr(place, "lng")
    place_number <- xml_attr(place, "number")
    n_bikes <- xml_attr(place, "bikes")
    booked_bikes <- xml_attr(place, "booked_bikes")
    
    bikes <- xml_find_all(place, "./bike")
    
    if (length(bikes) > 0) {
      for (bike in bikes) {
        bike_data <- list(
          # Place information
          place_uid = place_uid,
          place_name = place_name,
          place_lat = as.numeric(place_lat),
          place_lng = as.numeric(place_lng),
          place_number = place_number,
          
          # Bike information
          bike_number = xml_attr(bike, "number"),
          bike_type = xml_attr(bike, "bike_type"),
          lock_types = xml_attr(bike, "lock_types"),
          active = xml_attr(bike, "active"),
          state = xml_attr(bike, "state"),
          electric_lock = xml_attr(bike, "electric_lock"),
          boardcomputer = xml_attr(bike, "boardcomputer"),
          pedelec_battery = xml_attr(bike, "pedelec_battery"),
          booked = booked_bikes == n_bikes
        )
        
        bikes_data[[length(bikes_data) + 1]] <- bike_data
      }
    }
  }
  bind_rows(bikes_data)
}

df_bikes <- map_dfr(xml_docs, parse_xml, .id = "date_time")
write_rds(df_bikes, here("data", "26-transport", "df_bikes.rds"), compress = "gz")

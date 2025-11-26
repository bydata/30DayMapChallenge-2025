library(xml2)
library(dplyr)
library(here)

xml_content <- here("data", "26-transport", "data_20251126_000739.xml")
xml_doc <- read_xml(xml_content)

# Find all place nodes
places <- xml_find_all(xml_doc, "//place")


bikes_data <- list()

# Loop through each place
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

df_bikes <- bind_rows(bikes_data)

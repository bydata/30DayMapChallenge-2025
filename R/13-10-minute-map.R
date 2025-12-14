library(tidyverse)
library(sf)
library(ggtext)
library(here)

#' Source: Federal Agency for Cartography and Geodesy
#' https://gdz.bkg.bund.de/index.php/default/wfs-kfz-kennzeichen-1-250-000-wfs-kfz250.html
#' https://en.wikipedia.org/wiki/Vehicle_registration_plates_of_Germany#Constituent_elements

endpoint_url <- "https://sgx.geodatenzentrum.de/wfs_kfz250?service=wfs&version=1.1.0&request=GetFeature&TYPENAME=kfz250&MAXFEATURES=1000"
df <- st_read(endpoint_url)


df <- df |> 
  mutate(
    number_of_area_codes = str_count(str_squish(kfz), " ") + 1L,
    number_of_area_codes_grp = case_when(
      number_of_area_codes == 1 ~ "Just 1",
      number_of_area_codes == 2 ~ "2",
      number_of_area_codes %in% c(3, 4) ~ "3-4",
      number_of_area_codes %in% c(5, 6) ~ "5-6",
      TRUE ~ "7 or more"
    ),
    number_of_area_codes_grp = factor(
      number_of_area_codes_grp,
      levels = c("Just 1", "2", "3-4", "5-6", "7 or more")
    )
  )

ggplot(df) + 
  geom_sf(
    aes(fill = number_of_area_codes_grp),
    linewidth = 0.1) +
  paletteer::scale_fill_paletteer_d("soilpalettes::crait") +
  guides(fill = guide_legend(title = "Number of area codes")) +
  labs(
    title = "Dude, what's my number plate?",
    subtitle = "The first part or the number plate in Germany consists of 
    one to three letters representing the district where the car was registered.
    Since 2013, there can be more than one area code within the same district and 
    long abolished area codes were reintroduced.",
    caption = "Source: Federal Agency for Cartography and Geodesy. Visualization: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Gill Sans", paper = "#F6F6F6") +
  theme(
    plot.subtitle = element_textbox(width = 1.25),
    plot.caption = element_textbox(hjust = 0),
    plot.margin = margin(6, 6, 6, 6)
  )
ggsave(here("plots", "13-10-minute-map.png"), width = 6, height = 6)

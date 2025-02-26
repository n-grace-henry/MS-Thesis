# Load necessary libraries
library(sf)
library(rnaturalearth)  # For basemaps
library(rnaturalearthdata)  # Country data
library(osmdata)
library(tidyverse)


# Load Alaska map
alaska <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "Alaska")

# Load river shapefiles (example: National Hydrography Dataset or custom shapefile)
# If using a shapefile, replace with your file path
rivers <- st_read("path_to_river_shapefile.shp") 

# Filter for Wood, Kvichak, and Egegik Rivers
highlight_rivers <- rivers %>%
  filter(NAME %in% c("Wood River", "Kvichak River", "Egegik River"))  # Ensure names match your dataset

# Plot the map
ggplot() +
  geom_sf(data = alaska, fill = "grey90", color = "black") +  # Alaska outline
  geom_sf(data = highlight_rivers_egeg, color = "blue", size = 1.2) +  # Highlighted rivers
  geom_sf(data = highlight_rivers_wood, color = "blue", size = 1.2) +  # Highlighted rivers
  coord_sf(xlim = c(-162, -156), ylim = c(56.5, 59.5)) +  # Adjust for Bristol Bay
  theme_minimal() +
  labs(title = "Highlighted Rivers in Bristol Bay, Alaska",
       subtitle = "Wood, Kvichak, and Egegik Rivers",
       x = "Longitude", y = "Latitude")


egeg_box <- c(-158, 57.5, -156, 59)  # Bounding box for Egegik River
egeg_rivers <- opq(bbox = egeg_box) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()
egeg_river_lines <- egeg_rivers$osm_lines
colnames(egeg_river_lines)
unique(egeg_river_lines$name)

wood_box <- c(-159, 59, -158, 60)  # Bounding box for Wood River
wood_rivers <- opq(bbox = wood_box) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()
wood_river_lines <- wood_rivers$osm_lines
colnames(wood_river_lines)
unique(wood_river_lines$name)

# Filter for the specific rivers
highlight_rivers_wood <- wood_river_lines %>%
  filter(name %in% c("Wood River"))

highlight_rivers_egeg <- egeg_river_lines %>%
  filter(name %in% c("Egegik River", "Kvichak River"))





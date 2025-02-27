# Load necessary libraries
library(sf)
library(rnaturalearth)  # For basemaps
library(rnaturalearthdata)  # Country data
library(osmdata)
library(tidyverse)
library(grid)


# Load Alaska map
alaska <- ne_states(country = "United States of America", returnclass = "sf") %>%
  filter(name == "Alaska")

# Egegik and Kvichak data
egeg_box <- c(-158, 57.5, -156, 59)  # Bounding box for Egegik River
egeg_rivers <- opq(bbox = egeg_box) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()
egeg_river_lines <- egeg_rivers$osm_lines
colnames(egeg_river_lines)
unique(egeg_river_lines$name)

# Wood river data
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

# Labels to rivers 
river_labels <- data.frame(
  name = c("Egegik", "Wood", "Kvichak"),
  lon = c(-157.0, -159.0, -156.3),  # Adjust as needed
  lat = c(57.9, 59.2, 58.9)  # Adjust as needed
)

# Simplify the Alaska shapefile (reduce details)
alaska_simplified <- st_simplify(alaska, dTolerance = 5000)  # Adjust tolerance as needed

# Create the inset map (entire Alaska)
inset_map <- ggplot() +
  geom_sf(data = alaska_simplified, fill = "grey94", color = "black") +  # Full Alaska outline
  coord_sf(xlim = c(-180, -129), ylim = c(53, 71)) + 
  theme_void() +  # Remove axes, grid, background
  theme(
    plot.background = element_rect(fill = "transparent", color = NA)  # Transparent background
  )
print(inset_map)

# Plot the map
main_map <- ggplot() +
  geom_sf(data = alaska, fill = "grey94", color = "black") +  # Alaska outline
  geom_sf(data = highlight_rivers_egeg, color = "black", size = 1.2) +  # Highlighted rivers
  geom_sf(data = highlight_rivers_wood, color = "black", size = 1.2) +  # Highlighted rivers
  geom_text(data = river_labels, aes(x = lon, y = lat, label = name), 
            color = "black", size = 3) +  # Smaller text, no bold
  coord_sf(xlim = c(-162, -156), ylim = c(56.5, 59.5)) +  # Adjust for Bristol Bay
  theme_minimal() +
  labs(title = "Bristol Bay, Alaska", x = NULL, y = NULL) +  # Remove x and y axis titles
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman")  # Center title, change font
  ) 
print(main_map)

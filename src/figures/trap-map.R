library(sf)
library(tmap)
library(tmaptools)
library(ggplot2)

source("src/data-aggregation-I.R")

#### Map traps as points ####
# read a basemap in from OSM
bm <- read_osm(
  points_d,
  type = "osm", # for satellite image, "esri-imagery",
  zoom = 10,
  ext = 1
)

# plot the points
p <- tm_shape(bm,
              unit = "km") +
  tm_rgb() +
  tm_shape(points_d) +
  tm_dots(size = 0.2) 

tmap_save(p, filename = paste0("out/stat-trap-map.pdf"))

#### Map traps as grid ####

grid_agg <- grid_d %>%
  group_by(geometry) %>%
  summarise(count = n())

p <- ggplot() +
  geom_sf(data = grid_agg, aes(fill = count)) +
  geom_sf(data = points_d, color = "red", size = 0.5) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Aggregated Spatial Points",
       fill = "Count")
ggsave("out/grid-trap-map.pdf", plot = p)

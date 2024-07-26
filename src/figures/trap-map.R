library(sf)
library(tmap)

source("src/data-aggregation-I.R")

d <- st_as_sf(od, coords = c("lon", "lat"), CRS = 4326)

# read a basemap in from ESRI
bm <- read_osm(
  d,
  type = "osm", # for satellite image, "esri-imagery",
  zoom = 8,
  ext = 1
)


p <- tm_shape(bm,
              unit = "km") +
  tm_rgb() +
  tm_shape(d) +
  tm_dots(size = 0.2) 

tmap_save(p, filename = paste0("out/stat-trap-map.pdf"))
library(dplyr)
library(lubridate)
library(stringr)
library(sf)


# define the data directory
data_dir <- file.path(Sys.getenv("DATA_PATH"), "PSHB")

# read the data
ts <- read.csv(file.path(data_dir,"PSHB_Trap_Sample_29022024.csv"))
names(ts) <- make.names(names(ts))
ts <- ts %>% select(trapID = CaseTrap.FindTrapID, 
                    date = InspectedDate, 
                    sampleID = SampleID, 
                    abundance = Abundance) %>%
              mutate(date = dmy(date))

ct <- read.csv(file = file.path(data_dir,"PSHB_CaseTrap_29022024.csv")) 
names(ct) <- make.names(names(ct))
ct <- ct %>% select(spatial = Spatial, trapID = TrapID) %>%
              filter(trapID != "Unknown") %>%
              mutate(lat = as.numeric(str_extract(spatial, "-[\\S\\d.,]+?(?=(\\s|,|\\)))|$")),
                     lon = as.numeric(str_extract(spatial, "\\d\\S+"))) %>%
              select(-spatial) %>%
              distinct(trapID, .keep_all = TRUE)

# merge the two datasets
od <- left_join(ts, ct) %>%
      filter(!is.na(abundance) & !is.na(lat) & !is.na(lon)) %>%
      filter(lat > -32.161 & lat < -31.804 & lon > 115.6 & lon < 116.018)

#tidy up
rm(ct, ts)

##### Spatial aggregation ####
# make into simple points spatial feature
points_d <- st_as_sf(od, coords = c("lon", "lat"), crs = 4326)

# set up a grid
bbox <- st_bbox(points_d)
cell_size <- 0.1  # in degrees
grid <- st_make_grid(points_d, cellsize = cell_size, square = TRUE)
sf_grid <- st_sf(geometry = st_sfc(grid)) # Convert grid to sf object
# spatial join
grid_d <- st_join(sf_grid, points_d, join = st_intersects) %>%
  filter(!is.na(trapID))




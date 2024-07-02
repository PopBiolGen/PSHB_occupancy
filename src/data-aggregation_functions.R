## Define data directory
data_dir <- file.path(Sys.getenv("DATA_PATH"), "PSHB")

library(plyr)          # load before dplyr - else will give problems - use another package rather
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)
library(cowplot)

##========================##
#### Tailored functions ####
##========================##

st <- function(x, ...) { 
  if(!is.data.frame(x)) stop("No data frame!") 
  st1 <- gsub(" \\$", "", capture.output(str(x, ...))) # grab str() output and remove dollar signs
  ind <- prettyNum(1:ncol(x), width = nchar(ncol(x)) + 1) # create a numeric column index; the nchar(ncol(x)) + 1 bit ensures neat vertical alignment of items
  st1[-1] <- paste(ind, st1[-1]) # add the numeric column index to the str() output (leave out the 1st line saying it is a data frame...)
  cat(paste0(st1, "\n")) # get rid of quotes, escape characters, etc.
}

## Function makes summary df, that shows how many rows contain na, NULL and NULL as a percentage
calculate_null_info <- function(df) {
  na_count <- colSums(is.na(df))
  null_count <- sapply(df, function(x) sum(is.character(x) & x == "NULL"))
  null_count_per <- null_count / nrow(df) * 100
  formatted_null_count_per <- ifelse(null_count_per < 1, "< 1", round(null_count_per, 2))
  space_count <- sapply(df, function(x) sum(is.character(x) & x == ""))
  space_count_per <- space_count / nrow(df) * 100
  space_count_per <- ifelse(space_count_per < 1, "< 1", round(space_count_per, 2))
  new_df <- data.frame(Vector_Name = names(df), NA_count = na_count, 
                       NULL_count = null_count, 
                       NULL_percentage = formatted_null_count_per, 
                       space_count = space_count,
                       space_count_per = space_count_per
  )
  rownames(new_df) <- NULL
  
  return(new_df)
}

## Finding analysis
finding_analysis <- function(col_analyze) {
  fa_df <- data.frame(table(col_analyze))
  fa_df$per <- ifelse(fa_df$Freq / nrow(df) * 100 <= 1, "< 1", round(fa_df$Freq / nrow(df) * 100, 2))
  return(fa_df)
}

## Function to retrieve latitude and longitude from OpenStreetMap for multiple addresses and suburbs
get_lat_lon <- function(addresses, suburbs) {
  full_addresses <- paste(addresses, suburbs, "Western Australia", sep = ", ")
  coords_list <- lapply(full_addresses, function(full_address) {
    tryCatch(
      {
        geocode_result <- geocode_OSM(full_address)
        if (!is.na(geocode_result$coords[1])) {
          return(data.frame(Lat = geocode_result$coords[2], Lon = geocode_result$coords[1]))
        } else {
          return(data.frame(Lat = 0, Lon = 0))
        }
      },
      error = function(e) {
        return(data.frame(Lat = 0, Lon = 0))
      }
    )
  })
  do.call(rbind, coords_list)
}

## Infestation clumper function 
library(geosphere)
infestation_clumper <- function(dataframe, dist_clump){ ## note, dataframe must be "lon", "lat"; dist_clump in km
  dm <- distm(as.matrix(dataframe)) ## make distance matrix, df[,c("lon","lat")]
  dm <- which(dm <= (dist_clump * 1000) + 0.00001, arr.ind = T) ## note that less than or equal does not work, therefore have to assign +1
  df <- data.frame(dm)
  df <- df[, c("col", "row")]
  dim(df) 
  
  for(h in 1){
    old <- Sys.time()
    df$clust <- ifelse(df$col == "1", 1, 0)
    counter <- 0
    repeat{
      counter = counter + 1
      before <- dim(df[df$clust == "0", ])[1]
      df$clust = ifelse(df$col %in% df[df$clust == "1", "row"], 1, 0)
      after <- dim(df[df$clust == "0", ])[1]
      print(counter)
      if (after == before)
      {new <- Sys.time() - old
      print(new)
      break
      }}
    for(i in 1:dim(df)[1]){
      x = 1 + i
      df$clust = ifelse(df$col %in% head(df[df$clust == "0", "col"], 1), x, df$clust)
      counter = 0
      repeat{
        counter = counter + 1
        before <- dim(df[df$clust == "0", ])[1]
        df$clust = ifelse(df$col %in% df[df$clust == x, "row"], x, df$clust)
        after <- dim(df[df$clust == "0", ])[1]
        print(paste0(after, "-", counter))  
        if (after == before){
          break
        }}
      if (dim(df[df$clust == "0", ])[1] == 0){
        break
      }}
    new <- Sys.time() - old
    print(new)
    cluster_numbers <- as.numeric(levels(as.factor(df$clust)))
    print("cluster_numbers")
    print(cluster_numbers)
  }
  clumps <- df[, c("col", "clust")]
}

## Polygon maker
library(alphahull)
#library(gissr)
library(sf)

ahpoints <-  function(df, alpha){
  alpha <- alpha
  ap1 <- data.frame(ahull(df, alpha = alpha)$ashape.obj$edges)[, 3:4]
  names(ap1) <- c("lon", "lat")
  ch <- df[chull(df), ]
  rbind(ap1, ch)
} ## required for polygon_maker

remove_close_points <- function(df, threshold) {
  library(geosphere) # Load library for distance calculation
  dist_matrix <- distm(df[,c("lon", "lat")], fun = distHaversine)# Calculate distance matrix
  too_close_indices <- which(dist_matrix < threshold, arr.ind = TRUE)# Get indices of points that are too close
  too_close_indices <- too_close_indices[too_close_indices[,2] > too_close_indices[,1],]# Remove duplicates (keep only the first occurrence of each pair)
  if (nrow(too_close_indices) > 0) { # Remove points that are too close
    df <- df[-too_close_indices[,2],]
  }
  return(df)
}

polygon_maker_2 <- function(df, clum, clumps, distance, dist_clump) {
  # i = 2
  # df = positive_time_list[[i]][, c(24, 23)]
  # # length(positive_time_list)
  # clum = 5
  # dim(clumps)
  # clumps = clumps
  # distance = 0
  # dist_clump = 0.1
  
  # Get the individual clump data out of the polygons
  row.names(df) <- NULL ## Required to reset the row names
  df$id <- row.names(df)
  clumps2 <- unique(clumps[c("col", "clust")])
  df2 <- merge(x = df, y = clumps2, by.x = "id", by.y = "col", all.x = T, sort = F)
  c <- df2[df2$clust == clum, ]
  
  ## Try removing close points
  # c <- remove_close_points(df = c[, c(2:3)], threshold = 10)
  # c <- c[chull(c[, c("lon", "lat")]), ]
  
  ## Single and double clump correction
  options(digits = 12)
  c$lon <- as.numeric(c$lon)
  c$lat <- as.numeric(c$lat)
  c <- distinct(c, lon, lat, .keep_all = TRUE) ## removes two points in the same lat lon for the analysis
  q <- as.numeric(format(x = c$lon)) ## helps with the unique issue when there are hidden decimals
  ifelse(test = length(q) <= 2, yes = lon  <-  c(q, q + (dist_clump/1000), q - (dist_clump/1000)), no = lon  <-  q)
  w <- as.numeric(format(x = c$lat))
  ifelse(length(w) <= 2, lat  <-  c(w + (dist_clump/1000), w, w - (dist_clump/1000)), lat  <-  w)
  c <- data.frame(lon, lat)
  
  ## Four clump correction
  q <- as.numeric(format(x = c$lon))
  fcc <- data.frame(area = c(areaPolygon(c[2:4, ]), areaPolygon(c[c(1, 2:4), ]), areaPolygon(c[c(1:2, 4), ]), areaPolygon(c[1:3, ])), 
                    ah = c("c[2:4, ]", "c[c(1, 2:4), ]", "c[c(1:2, 4), ]",  "c[1:3, ]"))
  maxpoints <- eval(parse(text = fcc[fcc$area == max(fcc$area), "ah"]))
  
  if (is.data.frame(maxpoints) && nrow(maxpoints) == 4) {
    maxpoints <- maxpoints[chull(maxpoints), ]
  }
  ifelse(length(q) == 4, yes = ap <- maxpoints[chull(maxpoints), ], 
         no = ap <- c[chull(c), ])
  t <- seq(0, 355, 5) ## points of the rose
  fun_rp <- function(x, i){destPoint(p = x, b = i, d = distance * 1000)}## Rose point function ## cctk
  rp <- mapply(fun_rp, list(ap), t) ## determine rose points
  
  ## Assemble in meaningful dataframe
  ap <- ap[rep(seq_len(nrow(ap)), each = length(t)), ]
  long <- stack(data.frame(t(rp))[, 1:(nrow(rp)/2)])[1] ## Convert a data frame from the wide format to the long format
  ap$rplong <- long$values
  lat  <- stack(data.frame(t(rp))[, ((nrow(rp)/2)+1):nrow(rp)])[1] ## Convert a data frame from the wide format to the long format
  ap$rplat <- lat$values
  ap <- ap[, 3:4]
  colnames(ap) <- c("lon", "lat")
  ap$com <- paste(ap$lon, ap$lat)
  ap <- ap[!duplicated(ap$com), ]
  ap$com <- NULL
  
  # ## remove points that are too close to each other
  # if(distance == 0){ap <- ap
  # } else {
  #   ap <- remove_close_points(df = ap, threshold = 10)
  # }
  
  ## Don;t do chul for 3 points encase it has a problem
  if(nrow(ap) == 3){ap <- ap } else {
    ap <- ap[chull(ap), ]
  }
  
  ## Sort points so you can make a logical polygon
  ap <- sort_points(ap, y = "lat", x = "lon", clockwise = T) ## cool code to keep cctk
  
  ## Make alpha hull outline polygon
  coords <- ap ## note, this step is crucial
  coords <- matrix(c(lon = coords$lon, lat = coords$lat), ncol = 2) # Convert from df to sfc_polygon CCTK
  coords <- rbind(coords, coords[1, ]) # Bound the box
  coords <- st_polygon(list(coords))
  polygon <- st_sfc(coords, crs = 4326)
  # plot(polygon)
  
  return(polygon)
}

polygon_maker <- function(df, clum, clumps, distance, dist_clump) {
  # df = positive_time_list[[3]][, c(24, 23)]
  # clum = 11
  # clumps = clumps
  # distance = 0
  # dist_clump = 1
  
  row.names(df) <- NULL ## Required to reset the row names
  df$id <- row.names(df)
  clumps2 <- unique(clumps[c("col", "clust")])
  df2 <- merge(x = df, y = clumps2, by.x = "id", by.y = "col", all.x = T, sort = F)
  c <- df2[df2$clust == clum, ]
  
  ## Single and double clump correction
  c <- distinct(c, lon, lat, .keep_all = TRUE) ## removes two points in the same lat lon for the analysis
  q <- as.numeric(format(x = c$lon, digits = 7)) ## helps with the unique issue when there are hidden decimals
  ifelse(test = length(q) <= 2, yes = lon  <-  c(q, q + (dist_clump/1000), q - (dist_clump/1000)), no = lon  <-  q)
  w <- as.numeric(format(x = c$lat, digits = 7))
  ifelse(length(w) <= 2, lat  <-  c(w + (dist_clump/1000), w, w - (dist_clump/1000)), lat  <-  w)
  c <- data.frame(lon, lat)
  
  ## Four clump correction
  q <- as.numeric(format(x = c$lon, digits = 7))
  fcc <- data.frame(area = c(areaPolygon(c[2:4, ]), areaPolygon(c[c(1, 2:4), ]), areaPolygon(c[c(1:2, 4), ]), areaPolygon(c[1:3, ])),
                    ah = c("c[2:4, ]", "c[c(1, 2:4), ]", "c[c(1:2, 4), ]",  "c[1:3, ]"))
  maxpoints <- eval(parse(text = fcc[fcc$area == max(fcc$area), "ah"]))
  
  if (is.data.frame(maxpoints) && nrow(maxpoints) == 4) {
    maxpoints <- maxpoints[chull(maxpoints), ]
  }
  ifelse(length(q) == 4, yes = ap <- ahpoints(df = maxpoints, alpha = 0.474),
         no = ap <- ahpoints(df = unique(c)[chull(unique(c)), ], alpha = 0.474))
  t <- seq(0, 355, 5) ## points of the rose
  fun_rp <- function(x, i){destPoint(p = x, b = i, d = distance * 1000)}## Rose point function ## cctk
  rp <- mapply(fun_rp, list(ap), t) ## determine rose points
  
  ## Assemble in meaningful dataframe
  ap <- ap[rep(seq_len(nrow(ap)), each = length(t)), ]
  long <- stack(data.frame(t(rp))[, 1:(nrow(rp)/2)])[1] ## Convert a data frame from the wide format to the long format
  ap$rplong <- long$values
  lat  <- stack(data.frame(t(rp))[, ((nrow(rp)/2)+1):nrow(rp)])[1] ## Convert a data frame from the wide format to the long format
  ap$rplat <- lat$values
  ap <- ap[, 3:4]
  colnames(ap) <- c("lon", "lat")
  ap$com <- paste(ap$lon, ap$lat)
  ap <- ap[!duplicated(ap$com), ]
  ap$com <- NULL
  
  if(distance == 0){ap <- ap
  } else {
    ## remove points that are too cloe to each other
    ap <- remove_close_points(df = ap, threshold = 250)
  }
  
  ## get alpha hull points again
  ap <- ap[chull(ap), ]
  ap <- ahpoints(df = ap, alpha = 0.474)
  ap <- sort_points(ap, y = "lat", x = "lon", clockwise = T) ## cool code to keep cctk
  
  ## Make alpha hull outline polygon
  coords <- ap ## note, this step is crucial
  coords <- matrix(c(lon = coords$lon, lat = coords$lat), ncol = 2) # Convert from df to sfc_polygon CCTK
  coords <- rbind(coords, coords[1, ]) # Bound the box
  coords <- st_polygon(list(coords))
  polygon <- st_sfc(coords, crs = 7844)
  return(polygon)
}

library(units)
polygon_maker_100m_buffer <- function(df, clumps_specific, clumps_df, distance, single_point_radius) {
  # # i = 2
  # df = positive_time_list[[i]][, c(24, 23)]
  # clumps_specific = 1:unique_clusters
  # # clumps_specific = 2
  # clumps_df = clumps
  # distance = 0
  # single_point_radius = set_units(100, m)
  
  ## Set the units for the distance
  single_point_radius <- set_units(single_point_radius, m)
  distance <- set_units(distance, m)
  utm_crs <- st_crs(7844) # Define the UTM CRS for Zone 50S
  
  # Get the individual clump data out of the polygons
  row.names(df) <- NULL ## Required to reset the row names
  df$id <- row.names(df)
  clumps2 <- unique(clumps_df[c("col", "clust")])
  df2 <- merge(x = df, y = clumps2, by.x = "id", by.y = "col", all.x = T, sort = F)
  c <- df2[df2$clust == clumps_specific, ]
  options(digits = 12)
  c$lon <- as.numeric(c$lon)
  c$lat <- as.numeric(c$lat)
  c <- distinct(c, lon, lat, .keep_all = TRUE) ## removes two points in the same lat lon for the analysis
  c <- c[, c("lon", "lat")]
  
  ## For clumps with 1 point
  if(nrow(c) %in% 1){
    
    # Loop through each row in the points matrix
    matrix <- (as.matrix(c))
    center_point <- st_point(matrix) ## Note, must set the point crs in order to set the units
    center_point <- st_sfc(center_point, crs = utm_crs)
    circle <- st_buffer(center_point, dist = (single_point_radius + distance))
    polygon <- st_sfc(circle, crs = utm_crs)
    
  } else {
    ## For clumps with 2, 3 & 4 points
    if (nrow(c) %in% c(2, 3, 4)) {
      
      polygons_list <- list() ## Make list to add into
      
      # Loop through each row in the points matrix
      for (j in 1:nrow(c)) {
        matrix <- (as.matrix(c[j, ]))
        center_point <- st_point(matrix) ## Note, must set the point crs in order to set the units
        center_point <- st_sfc(center_point, crs = utm_crs)
        circle <- st_buffer(center_point, dist = (single_point_radius + distance))
        polygon_points <- st_sfc(circle, crs = utm_crs)
        polygons_list[[j]] <- polygon_points
      }
      
      # Combine the polygons using st_union
      union_polygon <- st_union(do.call("c", polygons_list))
      union_polygon <- st_make_valid(union_polygon)
      polygon <- st_make_valid(union_polygon)
      
      # get the convex hull, not can only do this for polygons with 2, 3, and 4 points
      polygon <- st_convex_hull(polygon)
      
    } else {
      
      ## For 5 and above polygons
      polygons_list <- list() ## Make list to add into
      
      # Loop through each row in the points matrix
      for (j in 1:nrow(c)) {
        matrix <- (as.matrix(c[j, ]))
        center_point <- st_point(matrix) ## Note, must set the point crs in order to set the units
        center_point <- st_sfc(center_point, crs = utm_crs)
        circle <- st_buffer(center_point, dist = (single_point_radius + distance))
        polygon_points <- st_sfc(circle, crs = utm_crs)
        polygons_list[[j]] <- polygon_points
        
      }
      
      # Combine the polygons using st_union
      union_polygon <- st_union(do.call("c", polygons_list))
      union_polygon <- st_make_valid(union_polygon)
      polygon <- st_make_valid(union_polygon)
      
    }
  }
  return(polygon)
}

## Function combines polygons into one "sfc_MULTIPOLYGON" 
fun_combine <- function(list){st_combine(do.call("c", list))}

# Function to filter overlapping polygons within a target polygon
filter_overlapping_polygons <- function(polygon_list, target_polygon, is_multipolygon = FALSE) {
  target_sfc <- st_sfc(target_polygon)
  overlapping_polygons <- list()
  for (polygon in polygon_list) {
    intersection <- st_intersection(polygon, target_sfc)
    if (!st_is_empty(intersection)) {
      overlapping_polygons <- c(overlapping_polygons, list(intersection))}}
  if (is_multipolygon) {
    return(lapply(overlapping_polygons, st_cast, "MULTIPOLYGON"))
  } else {
    return(lapply(overlapping_polygons, st_cast, "POLYGON"))
  }
  0}
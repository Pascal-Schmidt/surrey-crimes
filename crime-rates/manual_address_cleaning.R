library(tidyverse)
library(cancensus)
library(leaflet)
library(sf)

options(cancensus.api_key = "CensusMapper_f8a7e66b1263cfd8596e73babf6cc6b1")
city_surrey <- get_census(dataset = 'CA16', 
                          regions = list(CSD = "5915004"), 
                          vectors = c(), labels = "detailed", 
                          geo_format = "sf", level = 'DA')

city_surrey %>%
  dplyr::mutate(ID = dplyr::row_number()) -> city_surrey_shape

address_df <- readr::read_csv("lat_lon_postal_added.csv") %>%
  dplyr::mutate(district = ifelse(is.na(district), "NA", district))

# look up shapes on mapmanually and insert number for neighborhood
strawberry_hill <- c(207, 206, 205, 211, 210, 204, 203, 208, 209, 222)
panorama_ridge <- c(317, 319, 188, 187, 189, 186, 185)
fraser_heights <- c(519, 41, 57, 478, 480, 479, 480, 44, 45, 515, 514, 518, 516, 43, 42)
guildford <- c(519, 41, 557, 526, 464, 525, 519, 39, 32, 35, 39, 465, 466, 559, 535, 1, 463,2, 558, 46, 463, 477, 29, 520, 508, 507, 521, 519, 33, 30, 25)
johnson_heights <- c(17, 18, 36, 37, 35, 86, 37, 32, 31, 34, 135, 134, 22, 23, 511, 25, 511, 510, 21, 20, 139, 138, 24, 137, 19, 30, 471, 137, 136, 16, 14, 15, 133)
bridgeview <- c(459, 458, 457, 70, 460, 66, 67, 61, 62, 63, 64)
fleetwood <- c(570, 129, 524, 141, 159, 149, 150, 569, 155, 156, 496, 448, 482, 453, 536, 156, 448, 153, 152, 570, 8, 159, 157, 448, 492, 481,  534, 151, 5, 3, 157, 144, 11, 493, 12, 13, 481, 131, 132, 130, 126, 124, 122, 123, 121, 461, 142, 143, 140, 147, 145, 146, 148, 10, 9, 8, 7, 6, 149, 567, 162, 163, 161, 160, 158, 157, 493)
morgan_heights <- c(589, 588, 581, 586, 587, 583, 582)
sunny_side <- c(513, 415, 414, 413, 426, 411, 424, 422, 423, 434, 427, 441, 417, 410, 442, 412)
crescent_heights <- c(399, 298, 397, 400, 401, 401, 403, 396, 394, 560, 561, 562, 395, 393)
ocean_parl <- c(385, 384, 386, 387, 388, 390, 379, 380, 381, 377, 443, 562)
crescent_beach <- c(392, 476)
grandview <- c(595, 435, 436, 594, 592, 593, 437, 438)
port_kells <- c(529, 528, 527, 444, 530)
ocean_park <- c(383, 382, 387, 389, 378, 379)
hazelmere <- c(365, 366)
sullivan <- c(533, 538, 324, 325, 554)
cloverdale <- c(345, 575, 576, 451, 533, 356, 591, 533, 532, 349, 348, 360, 352, 355, 504, 341, 362, 445, 350, 351, 353, 354, 359, 358, 355, 531, 590, 503)
whalley <- c(571, 97, 99, 572, 100, 101, 104, 102, 126, 524, 227, 249, 252, 74, 69, 248, 281, 245, 60, 74, 524, 522, 239, 71, 72, 69, 249, 251, 250, 64, 63, 260, 226, 229, 228, 273, 270, 271, 267, 473, 266, 278, 277, 279, 280, 266, 102, 287, 285, 225, 283)
brownsville <- c(238, 236, 235, 234, 237, 241, 240, 242, 230, 231, 232)

  
# create district variable
city_surrey_shape %>%
  dplyr::mutate(district = "NA") %>%
  dplyr::mutate(district = ifelse(ID %in% strawberry_hill, "Strawberry Hill", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% panorama_ridge, "Panorame Ridge", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% fraser_heights, "Fraser Heights", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% guildford, "Guildford", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% johnson_heights, "Johnson Heights", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% bridgeview, "Bridgeview", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% fleetwood, "Fleetwood", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% morgan_heights, "Morgan Heights", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% sunny_side, "Sunny Side", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% crescent_heights, "Crescent Heights", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% ocean_parl, "Ocean Parl", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% crescent_beach, "Crescent Beach", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% grandview, "Grandview", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% port_kells, "Port Kells", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% ocean_park, "Ocean Park", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% hazelmere, "Hazelmere", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% sullivan, "Sullivan", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% cloverdale, "Cloverdale", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% whalley, "Whalley", district)) %>%
  dplyr::mutate(district = ifelse(ID %in% brownsville, "Brownsville", district))-> city_surrey_shape


city_surrey_shape %>%
  dplyr::select(geometry, district, ID) -> city_surrey_shape

# prepare for joining
sf_point_df <- st_as_sf(x = address_df %>%
                          dplyr::filter(!is.na(lon)), 
                        coords = c("lon", "lat"),
                        crs = 4326)
sf_point_df <- st_join(sf_point_df, city_surrey_shape, join = st_intersects)

sf_point_df %>%
  dplyr::mutate(district = ifelse(district.x == "NA",
                                  district.y, 
                                  district.x)) %>%
  dplyr::mutate(district = ifelse(district.x != "NA" & (district.x != district.y) & district.y != "NA",
                                  paste0(district.x, " (", 
                                         district.y, ")"),
                                  district)) %>%
  dplyr::select(-c(district.x, district.y)) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(lon = unlist(purrr::map(.$geometry, 1)),
                lat = unlist(purrr::map(.$geometry, 2))) %>%
  dplyr::select(-geometry) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(district = ifelse(lat < 49.08 & district == "NA", 
                                  "South Surrey",
                                  district)) -> sf_point_df


unique_districts <- na.omit(unique(sf_point_df$district))[ !(na.omit(unique(sf_point_df$district)) %in% "NA") ]
all_districts <- st_sf(st_sfc())
for(i in seq_along(unique_districts)) {
  
  print(i)
  sf_point_df %>%
    dplyr::filter(district == unique_districts[i]) %>%
    dplyr::select(lon, lat) -> temp
  
  coords <- as.matrix(temp)
  hpts <- chull(coords)
  hpts <- c(hpts, hpts[1])
  poly <- as_tibble(coords[hpts, ]) 
  poly$all_dists <- unique_districts[i]
  
  sfheaders::sf_polygon(
    obj = poly[, c("lon", "lat")],
    close = FALSE
  ) -> polygon_df
  
  all_districts <- rbind(all_districts, cbind(polygon_df, postal = unique_districts[i]))
  
  
}

# make invalid polygons valid
poly_districts <- st_make_valid(all_districts)

# prepare for joining
sf_point_df2 <- st_as_sf(x = sf_point_df %>%
                          dplyr::filter(!is.na(lon)), 
                        coords = c("lon", "lat"),
                        crs = st_crs(all_districts))

# fill out NA values
sf_point_df3 <- st_join(sf_point_df2, poly_districts, join = st_intersects)

sf_point_df3 %>%
  dplyr::as_tibble() %>%
  dplyr::select(-c(id)) %>%
  dplyr::mutate(district = ifelse(district == "NA" | is.na(district), as.character(postal), district)) %>%
  dplyr::select(-c(postal)) %>%
  dplyr::mutate(lon = unlist(purrr::map(.$geometry, 1)),
                lat = unlist(purrr::map(.$geometry, 2))) %>%
  dplyr::select(-geometry) -> sf_point_df4

sf_point_df4 %>%
  dplyr::distinct(HUNDRED_BLOCK, .keep_all = TRUE) -> sf_point_df5

write.csv(sf_point_df5, "latest.csv")

leaflet(data = sf_point_df5) %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions(),
             popup = ~district) %>%
  addPolygons(data = city_surrey_shape, weight = 2, smoothFactor = 0.5,
              label = city_surrey_shape$ID) %>%
  addProviderTiles(providers$OpenStreetMap)

# testing
# sf_point_df %>%
#   dplyr::filter(HUNDRED_BLOCK == "12900 114A AVE") %>%
#   st_as_sf(coords = c("lon", "lat"),
#            crs = 4326) -> x
# 
# city_surrey_shape %>%
#   dplyr::filter(ID == 458) -> y
# 
# st_join(x, y, join = st_intersects) %>%
# 
# 
#   dplyr::mutate(district = ifelse(district.x == "NA",
#                                   district.y, 
#                                   district.x)) %>%
#   dplyr::mutate(district = ifelse(district.x != "NA" & (district.x != district.y) & district.y != "NA",
#                                   paste0(district.x, " (", 
#                                          district.y, ")"),
#                                   district)) %>%
#   View

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
panorama_ridge <- c(188, 187, 189, 186, 185)
fraser_heights <- c(45, 515, 514, 518, 516)
guildford <- c(29, 520, 508, 507, 521, 519, 33, 30, 25)
johnson_heights <- c(22, 23, 511, 25, 511, 510, 21, 20, 139, 138, 24, 137, 19, 30, 471, 137, 136, 16, 14, 15, 133)
bridgeview <- c(458, 457, 70, 460, 66, 67, 69, 71, 61, 62, 63, 64)
fleetwood <- c(12, 13, 481, 131, 132130, 126, 124, 122, 123, 121, 461, 142, 143, 140, 147, 146, 148, 10, 9, 8, 7, 6, 149, 567, 162, 163, 161, 160, 158, 157, 493)
morgan_heights <- c(513, 589, 588, 581, 586, 587, 583, 582)
sunny_side <- c(415, 414, 413, 426, 411, 424, 422, 423, 434, 427, 441, 417, 410, 442, 412)
crescent_heights <- c(298, 397, 400, 401, 401, 403, 396, 394, 560, 561, 562, 395, 393)
ocean_parl <- c(385, 384, 386, 387, 388, 390, 379, 380, 381, 377, 443, 562)
crescent_beach <- c(392, 476)

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
  dplyr::mutate(district = ifelse(ID %in% crescent_beach, "Crescent Beach", district)) -> city_surrey_shape

city_surrey_shape %>%
  dplyr::select(geometry, district, ID) -> city_surrey_shape

# prepare for joining
sf_point_df <- st_as_sf(x = address_df %>%
                          dplyr::filter(!is.na(lon)), 
                        coords = c("lon", "lat"),
                        crs = st_crs(city_surrey_shape))
sf_point_df <- st_join(sf_point_df, city_surrey_shape, join = st_intersects)

sf_point_df %>%
  dplyr::mutate(district = ifelse(district.x == "NA", district.x, 
                                  paste0(district.x, " (", 
                                         district.y, ")"))) %>%
  dplyr::select(-c(district.x, district.y)) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(lon = unlist(purrr::map(.$geometry, 1)),
                lat = unlist(purrr::map(.$geometry, 2))) %>%
  dplyr::select(-geometry) %>%
  dplyr::as_tibble() -> sf_point_df


leaflet(data = sf_point_df) %>%
  addMarkers(clusterOptions = markerClusterOptions()) %>%
  addTiles() %>%
  addPolygons(data = city_surrey_shape, weight = 2, smoothFactor = 0.5,
              label = city_surrey_shape$ID) %>%
  addProviderTiles(providers$OpenStreetMap)

write.csv(sf_point_df, "latest.csv")


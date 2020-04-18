library(tidyverse)
library(sf)
library(sfheaders)

lat_lon_postal <- readr::read_csv("lat_lon_postal.csv")

# manually imported from the internet
districts <- c("Newton", "Guildford", "Whalley", "Campbell Heights", 
               "Cloverdale", "South Westminster", "Fleetwood", "Rosemary Heights Central",
               "East Panorama Ridge", "Fraser Heights", "Ocean Park", "Grandview Heights",
               "Port Kells", "Crescent Beach", "Aloha Estates", "East Clayton", "Mud Bay",
               "Anniedale", "North Grandview Heights", "West Newton", "East Panorama Ridge",
               "East Newton North", "South Westminster", "Bridgeview", "East Clayton West", 
               "West Cloverdale South", "Clayton", "East Claython North", "Sunnyside", 
               "North Cloverdale East", "Johnson Heights", "Sullivan", "Alluvia", "Hazelmere",
               "Crescent", "West Newton South", "West Cloverdale North", "South Newton",
               "West Newton North", "Elgin", "Rosemary Heights West", "Morgan Heights",
               "North Cloverdale West", "East Newton South", "Kensington Prairie",
               "Saint Helen's Park", "Strwaberry Hill", "South Mann")

# construct district out of street_name, neighborhood, and city
lat_lon_postal %>%
  dplyr::mutate(district = NA) %>%
  dplyr::mutate(district = ifelse(street_name %in% districts & is.na(district), 
                                  street_name, district)) %>%
  dplyr::mutate(district = ifelse(neighborhood %in% districts & is.na(district),
                                  neighborhood, district)) %>%
  dplyr::mutate(district = ifelse(city %in% districts & is.na(district),
                                  city, district)) -> lat_lon_postal


# get unique districts
unique_districts <- na.omit(unique(lat_lon_postal$district))

# get unique postal codes with 6 letters/digits
unique_postal_code <- na.omit(unique(lat_lon_postal$postal_code))
unique_postal_code <- unique_postal_code[nchar(unique_postal_code) == 7]

# initialize sf object
poly_districts <- st_sf(st_sfc())
for(i in seq_along(unique_districts)) {
  
  lat_lon_postal %>%
    dplyr::filter(district == unique_districts[i]) %>%
    dplyr::select(lon, lat) -> temp
  
  # construct the biggest possible polygon
  coords <- as.matrix(temp)
  hpts <- chull(coords)
  hpts <- c(hpts, hpts[1])
  poly <- as_tibble(coords[hpts, ]) 
  poly$dist <- unique_districts[i]
  
  # transform biggest possible polygon into sf polygon object
  sfheaders::sf_polygon(
    obj = poly[, c("lon", "lat")],
    close = FALSE
  ) -> polygon_df
  
  # rbind for every neighborhood 
  poly_districts <- rbind(poly_districts, cbind(polygon_df, dist = unique_districts[i]))
  
}

# make invalid polygons valid
poly_districts <- st_make_valid(poly_districts)

# prepare for joining
sf_point_df <- st_as_sf(x = lat_lon_postal %>%
                          dplyr::filter(!is.na(lon)), 
                        coords = c("lon", "lat"),
                        crs = st_crs(poly_districts))

# fill out missing values in the district column where latitude and 
# longitude points fall into the polygon
sf_point_df <- st_join(sf_point_df, poly_districts, join = st_intersects)

# same thing as above. Construct biggest possible polygon out of available 
# postal codes that are the same and then look if we can fill out 
# NA values in original df with valid postal codes based on the intersection
# of latitude and longitude values in data frame and just constructed polygon
poly_postal <- st_sf(st_sfc())
for(i in seq_along(unique_postal_code)) {
  
  lat_lon_postal %>%
    dplyr::filter(postal_code == unique_postal_code[i]) %>%
    dplyr::select(lon, lat) -> temp
  
  coords <- as.matrix(temp)
  hpts <- chull(coords)
  hpts <- c(hpts, hpts[1])
  poly <- as_tibble(coords[hpts, ]) 
  poly$postal <- unique_postal_code[i]
  
  sfheaders::sf_polygon(
    obj = poly[, c("lon", "lat")],
    close = FALSE
  ) -> polygon_df
  
  poly_postal <- rbind(poly_postal, cbind(polygon_df, postal = unique_postal_code[i]))
  
  
}

# make postal polygons valid if invalid
poly_postal <- st_make_valid(poly_postal)

# fill out NA values
sf_point_df <- st_join(sf_point_df, poly_postal, join = st_intersects)

sf_point_df %>%
  dplyr::as_tibble() %>%
  dplyr::select(-c(id.x, id.y)) %>%
  dplyr::mutate(district = ifelse(is.na(district) & !is.na(dist), as.character(dist), district),
                postal_code = ifelse(is.na(postal_code) & !is.na(postal), as.character(postal), postal_code)) %>%
  dplyr::select(-c(postal, dist)) -> sf_point_df2

sf_point_df2 %>%
  dplyr::as_tibble() %>%
  dplyr::select(postal_code, district, HUNDRED_BLOCK) -> add_postal_district

lat_lon_postal %>%
  dplyr::select(-c(district, postal_code)) %>%
  dplyr::inner_join(add_postal_district, by = "HUNDRED_BLOCK") -> lat_lon_postal


lat_lon_postal %>%
  write.csv("lat_lon_postal_added.csv")








##############################
##### Geocoding Function #####
##############################

library(tidyverse)
library(ggmap)

register_google(key = "###")
api_key = "###"

# get coordinates
coord <- vector(mode = "list", length = nrow(add_coordinates))
for(i in seq_along(add_coordinates$address)) {
  
  coord[[i]] <- ggmap::geocode(add_coordinates$address[[i]])
  Sys.sleep(0.1)
  
}
coord %>%
  do.call(rbind, .) %>% 
  dplyr::bind_cols(add_coordinates) %>%
  write.csv("lat_lon_surrey_df.csv")


final <- readr::read_csv("lat_lon_surrey_df.csv")
parameters <- stringr::str_c("&key=", api_key)
apiRequests <- base::iconv(str_c(
  "https://maps.googleapis.com/maps/api/geocode/json?latlng=",
  final$lat, ",", final$lon, parameters
), "", "UTF-8")

result <- base::vector(mode = "list", length = nrow(final))
for (i in 1:nrow(final)) {
  
  if(is.na(final$lon[[i]]) | is.na(final$lat[[i]])) {
    
    result[[i]] <- NULL
    next
    
  }
  
  # Avoid calling API too often
  Sys.sleep(0.25)
  
  # Call google maps API
  conn <- httr::GET(URLencode(apiRequests[i]))
  
  # Parse the JSON response
  apiResponse <- jsonlite::fromJSON(httr::content(conn, "text"))
  
  # Look at the address_components of the 1st address
  result[[i]] <- apiResponse$results$address_components[[1]]
  
}

# result %>%
#   do.call(rbind, .) %>%
#   as_tibble() %>%
#   dplyr::select(-types) %>%
#   write.csv("reverse_geocoding.csv")
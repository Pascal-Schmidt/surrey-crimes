library(tidyverse)


#########################
##### Data Cleaning #####
#########################

final <- readr::read_csv("lat_lon_surrey_df.csv")
result <- readr::read_csv("reverse_geocoding.csv")

result %>%
  purrr::map(function(x) {
    
    if(is.null(x)) {
      
      data.frame(types = c("street_number", "route", "neighborhood", 
                           "locality", "level1", "level2", 
                           "country", "postal_code"), 
                 long_name = letters[1:8], short_name = rep(NA, 8))
      
    } else {
      
      x
      
    }
    
  }) %>%
  purrr::map(~ dplyr::select(., -short_name)) %>%
  purrr::map(~ tidyr::pivot_wider(., names_from = "types", values_from = "long_name")) %>%
  purrr::map(~ .[, c(1:4, length(.))]) %>%
  purrr::map(~ purrr::set_names(., "street_number", "street_name", "neighborhood", "city", "postal_code")) %>%
  do.call(rbind, .) -> lat_lon_postal

lat_lon_postal %>%
  dplyr::bind_cols(final[1:nrow(.), ]) %>%
  write.csv("lat_lon_postal.csv")


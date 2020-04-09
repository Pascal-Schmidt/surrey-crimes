library(tidyverse)

all_address <- readr::read_csv("latest.csv") %>%
  dplyr::select(HUNDRED_BLOCK, postal_code, district, lon, lat)

crime_2014 <- readr::read_csv("data/crime_2014.csv") %>%
  tidyr::unite("HUNDRED_BLOCK", HUNDRED_BLOCK, ROAD_NAME, sep = " ")

list.files("data")[!(list.files("data") %in% "crime_2014.csv")] %>%
  purrr::map(~ readr::read_csv(paste0("data/", .))) %>%
  do.call(rbind, .) %>%
  rbind(crime_2014) -> df

df %>%
  dplyr::left_join(all_address, by = "HUNDRED_BLOCK") -> df2

df2 <- na.omit(df2)
write.csv(df2, "final_df.csv")

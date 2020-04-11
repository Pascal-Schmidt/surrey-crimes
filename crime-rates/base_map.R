base_map <- function(initial_df, filtered_df) {
  
  pal7 <- RColorBrewer::brewer.pal(7, "Set1")
  leaf_pal <- colorFactor(palette = pal7, domain = initial_df$INCIDENT_TYPE)
  
  base_map <- leafletProxy("map", data = filtered_df) %>%
    clearMarkerClusters() %>%
    clearControls() %>%
    addCircleMarkers(clusterOptions = markerClusterOptions(),
                     stroke = FALSE, fill = TRUE, fillOpacity = .7,
                     color = ~leaf_pal(INCIDENT_TYPE),
                     popup = paste("'<strong>'Neighborhood: '</strong>'", filtered_df$district, "<br/>",
                                   "<strong>Address: </strong>", filtered_df$HUNDRED_BLOCK, "<br/>",
                                   "<strong>Postal Code: </strong>", filtered_df$postal_code, "<br/>", 
                                   "<strong>Date: </strong>", filtered_df$date, "<br/>")) %>%
    addLegend("bottomright",
              pal = leaf_pal,
              values = ~INCIDENT_TYPE, title = "Category")
  
  return(base_map)
  
}
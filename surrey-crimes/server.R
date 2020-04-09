library(shiny)
library(leaflet)
library(tidyverse)

df <- readr::read_csv("final_df.csv") %>%
  tidyr::unite("date", YEAR, MONTH, sep = "-") %>%
  dplyr::mutate(date = lubridate::ymd(paste0(date, "-1")))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    # filter for dates, district, postal code, and incident types
    df[df$date <= input$range[1] & df$date >= input$range[2], ] %>%
      .[df$district %in% input$neighborhoods] %>%
      .[df$postal_code %in% input$postal] %>%
      .[df$INCIDENT_TYPE %in% input$incident]
    
  })
  
  output$map <- renderLeaflet({
    
    # map displayed at beginning
    leaflet(df) %>% 
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions()) %>%
      fitBounds(~min(lon) + 0.1, ~min(lat) + 0.1, ~max(lon) + 0.1, ~max(lat) + 0.1)
    
  })
  
}

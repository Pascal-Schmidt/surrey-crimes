library(shiny)
library(leaflet)
library(RColorBrewer)
df <- readr::read_csv("final_df.csv") %>%
  tidyr::unite("date", YEAR, MONTH, sep = "-") %>%
  dplyr::mutate(date = lubridate::ymd(paste0(date, "-1"))) %>%
  dplyr::filter(lon > -123 & lon < -122 & lat < 49.5 & lat > 48.5)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                
                
                # choose a date range
                shiny::dateRangeInput("range", "Date", 
                                      start = lubridate::ymd("2019-01-01"), end = max(df$date),
                                      min = min(df$date), max = max(df$date),
                                      format = "yyyy/mm/dd",
                                      separator = "-"),
                
                # choose a neighborhood
                shiny::selectInput("neighborhoods", "Neighborhoods",
                                   choices = unique(df$district), multiple = TRUE,
                                   selected = "Whalley"),
                
                # choose the incident type
                shiny::selectInput("incident", "Incident Type",
                                   choices = unique(df$INCIDENT_TYPE), multiple = TRUE,
                                   selected = df %>%
                                     dplyr::filter(district == "Whalley") %>%
                                     dplyr::pull(INCIDENT_TYPE) %>%
                                     { unique(.)[1:3] }),
                
                shiny::checkboxInput("postal_code", "Search for Your Postal Code!"),
                shiny::uiOutput("postal_surrey")))

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    
    if(!input$postal_code) {
      df[df$date >= input$range[1] & df$date <= input$range[2],]
      df <- df[df$district %in% input$neighborhoods, ]
      df <- df[df$INCIDENT_TYPE %in% input$incident, ]
      df
      
    } else {
      
      df[df$date >= input$range[1] & df$date <= input$range[2],]
      df <- df[df$district %in% input$neighborhoods, ]
      df <- df[df$postal_code %in% input$postal, ]
      df <- df[df$INCIDENT_TYPE %in% input$incident, ]
      df
      
    }
  })
  
  output$postal_surrey <- renderUI({
    
    if(input$postal_code) {
      
      # choose a postal code
      shiny::selectInput("postal", "Postal Code",
                         choices = unique(df$postal_code), multiple = TRUE,
                         selected = "")
      
    }
    
    
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(df) %>% addProviderTiles("CartoDB.Positron") %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    if(length(input$incident) != 0 & length(input$postal_code) != 0) {
    
    pal7 <- RColorBrewer::brewer.pal(7, "Set1")
    leaf_pal <- colorFactor(palette = pal7, domain = df$INCIDENT_TYPE)
    
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>%
      clearControls() %>%
      addCircleMarkers(clusterOptions = markerClusterOptions(),
                       stroke = FALSE, fill = TRUE, fillOpacity = .7,
                       color = ~leaf_pal(INCIDENT_TYPE),
                       label = ~district) %>%
      addLegend("bottomright",
                pal = leaf_pal,
                values = ~INCIDENT_TYPE, title = "Category")
    
    } else {
      
      leafletProxy("map", data = filteredData()) %>%
        clearMarkerClusters() %>%
        clearControls()
      
    }
    
  })
  
}

shinyApp(ui, server)

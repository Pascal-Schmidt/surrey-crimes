library(shiny)
library(leaflet)
library(tidyverse)

df <- readr::read_csv("final_df.csv") %>%
  tidyr::unite("date", YEAR, MONTH, sep = "-") %>%
  dplyr::mutate(date = lubridate::ymd(paste0(date, "-1"))) %>%
  dplyr::filter(lon > -123 & lon < -122 & lat < 49.5 & lat > 48.5)

ui <- bootstrapPage(
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(top = 10, right = 10,
                
                # choose a date range
                shiny::sliderInput("range", "Date", min(df$date), max(df$date), 
                                   value = range(df$date)),
                
                # choose a neighborhood
                shiny::selectInput("neighborhoods", "Neighborhoods", 
                                   choices = unique(df$district), multiple = TRUE, 
                                   selected = "Whalley"),
                
                # choose a postal code
                shiny::selectInput("postal", "Postal Code", 
                                   choices = unique(df$postal_code), multiple = TRUE, 
                                   selected = df %>%
                                     dplyr::filter(district == "Whalley") %>%
                                     dplyr::pull(postal_code) %>%
                                     { unique(.)[1:5] }),
                
                # choose the incident type
                shiny::selectInput("incident", "Incident Type", 
                                   choices = unique(df$INCIDENT_TYPE), multiple = TRUE,
                                   selected = df %>%
                                     dplyr::filter(district == "Whalley") %>%
                                     dplyr::pull(INCIDENT_TYPE) %>%
                                     { unique(.)[1:3] })
                
                
  )
)

# -----------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    # filter for dates, district, postal code, and incident types
    df[df$date <= input$range[1] & df$date >= input$range[2], ] %>%
       dplyr::filter(districts %in% input$neighborhood) %>%
       dplyr::filter(postal_codes %in% input$postal) %>%
       dplyr::filter(INCIDENT_TYPE %in% input$incident)
    
  })
  
  output$map <- renderLeaflet({
    
    # map displayed at beginning
    leaflet("map", filtered_data()) %>% 
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions()) 
      setView(lng = -122.5, lat = 49.05, zoom = 10)
    
  })
  
  observe({
    
    leafletProxy("map", data = filtered_data()) %>%
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions()) 
  
  })
  
  
}

shinyApp(ui, server)

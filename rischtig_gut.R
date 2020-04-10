library(shiny)
library(leaflet)
library(highcharter)
library(RColorBrewer)
library(cancensus)
library(lubridate)
library(tidyverse)
source("base_map.R")

# options(cancensus.api_key = "CensusMapper_f8a7e66b1263cfd8596e73babf6cc6b1")
# city_surrey <- get_census(dataset = 'CA16', 
#                           regions = list(CSD = "5915004"), 
#                           vectors = c(), labels = "detailed", 
#                           geo_format = "sf", level = 'CT')

df <- readr::read_csv("final_df.csv") %>%
  tidyr::unite("date", YEAR, MONTH, sep = "-") %>%
  dplyr::mutate(date = lubridate::ymd(paste0(date, "-1"))) %>%
  dplyr::filter(lon > -123 & lon < -122 & lat < 49.5 & lat > 48.5)



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 5, right = 5, draggable = TRUE, fixed = TRUE,
                width = "20%", style = "z-index:500; min-width: 300px;",
                
                highchartOutput("selectstat")),
  
  absolutePanel(top = 60, left = 5, draggable = TRUE, fixed = TRUE,
                width = "20%", style = "z-index:500; min-width: 200px;",
                
                
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
                         choices = unique(df$postal_code), 
                         multiple = TRUE)
      
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
    
    if(length(input$incident) != 0 & 
       length(input$neighborhoods) != 0 & !input$postal_code) {
      
      base_map(df, filteredData())
      
      crime_rank <- filteredData() %>%
        count(INCIDENT_TYPE) %>%
        arrange(desc(n))
      output$selectstat <- renderHighchart({
        
        hchart(crime_rank, "bar", hcaes(INCIDENT_TYPE, n)) %>% 
          hc_colors("SteelBlue") %>% 
          hc_title(text = paste("Number of Incident Types in the Municipality of Surrey")) %>% 
          hc_subtitle(text = "") %>% 
          hc_xAxis(title = list(text = ""), gridLineWidth = 0, minorGridLineWidth = 0) %>% 
          hc_yAxis(title = list(text = "Incidents"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
          hc_legend(enabled = FALSE) %>% 
          hc_tooltip(pointFormat = "Incidents: <b>{point.y}</b>") %>% 
          hc_plotOptions(series = list(cursor = "default")) %>% 
          hc_add_theme(hc_theme_smpl()) %>% 
          hc_chart(backgroundColor = "transparent")
        
      })
      
    } else if(length(input$incident) != 0 & 
              length(input$neighborhoods) != 0 &
              length(input$postal) != 0 & input$postal_code &
              filteredData() %>%
              dplyr::filter(postal_code %in% input$postal) %>%
              dplyr::filter(district %in% input$neighborhoods) %>%
              nrow(.) == 0) {
      
      showModal(modalDialog(title = "Sorry!", 
                            tags$p("Your postal code is not in the selected neighborhood."),
                            tags$p("Give another one a try or expand the number of neighborhoods.")))
      
    } else if(length(input$incident) != 0 & 
              length(input$neighborhoods) != 0 &
              length(input$postal) != 0 & input$postal_code &
              filteredData() %>%
              dplyr::filter(postal_code %in% input$postal) %>%
              dplyr::filter(district %in% input$neighborhoods) %>%
              nrow(.) != 0) {
      
      base_map(df, filteredData())
      
      crime_rank <- filteredData() %>%
        count(INCIDENT_TYPE) %>%
        arrange(desc(n))
      output$selectstat <- renderHighchart({
        
        hchart(crime_rank, "bar", hcaes(INCIDENT_TYPE, n)) %>% 
          hc_colors("SteelBlue") %>% 
          hc_title(text = paste("Number of Incident Types in the Municipality of Surrey")) %>% 
          hc_subtitle(text = "") %>% 
          hc_xAxis(title = list(text = ""), gridLineWidth = 0, minorGridLineWidth = 0) %>% 
          hc_yAxis(title = list(text = "Incidents"), gridLineWidth = 0, minorGridLineWidth = 0) %>%
          hc_legend(enabled = FALSE) %>% 
          hc_tooltip(pointFormat = "Incidents: <b>{point.y}</b>") %>% 
          hc_plotOptions(series = list(cursor = "default")) %>% 
          hc_add_theme(hc_theme_smpl()) %>% 
          hc_chart(backgroundColor = "transparent")
        
      })
      
    } else {
      
      leafletProxy("map", data = filteredData()) %>%
        clearMarkerClusters() %>%
        clearControls()
      
    }
    
  })
  
}

shinyApp(ui, server)

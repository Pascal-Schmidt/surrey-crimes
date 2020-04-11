library(shiny)
library(leaflet)
library(highcharter)
library(RColorBrewer)
library(cancensus)
library(lubridate)
library(tidyverse)
library(DT)
source("base_map.R")
source("highcharter_barplot.R")

options(cancensus.api_key = "CensusMapper_f8a7e66b1263cfd8596e73babf6cc6b1")
city_surrey <- get_census(dataset = 'CA16',
                          regions = list(CSD = "5915004"),
                          vectors = c(), labels = "detailed",
                          geo_format = "sf", level = 'CT')

df <- readr::read_csv("final_df.csv") %>%
  tidyr::unite("date", YEAR, MONTH, sep = "-") %>%
  dplyr::mutate(date = lubridate::ymd(paste0(date, "-1"))) %>%
  dplyr::filter(lon > -123 & lon < -122 & lat < 49.5 & lat > 48.5)



ui <- bootstrapPage(

  navbarPage(title = "City of Surrey Map of Incident Types",
            
             tabPanel("Map", 
                      style = "height:500px;",
                      
                      
                      leafletOutput("map", width = "100%", height = "100%"),
                      absolutePanel(top = 70, right = 20, draggable = TRUE, fixed = TRUE,
                                    width = "20%", style = "z-index:500; min-width: 300px;",
                                    
                                    highchartOutput("selectstat")),
                      
                      absolutePanel(top = 150, left = 20, draggable = TRUE, fixed = TRUE,
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
                                    shiny::uiOutput("postal_surrey"),
                                    
                                    shiny::checkboxInput("boundaries", "Add Boundaries Around Surrey"))),
             
             tabPanel("Table", 
                      
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          shiny::selectInput("groupings", "Group Your Data",
                                             choices =  c("Postal Code", "Incident Type",
                                                          "Address", "Neighborhood"),
                                             multiple = TRUE,
                                             selected = "Incident Type"),
                          
                          shiny::radioButtons("year_month", label = "Crime Over Time",
                                               choices = c("Monthly", "Yearly"),
                                              selected = "")
                        ),
                        
                        mainPanel(
                          
                          DT::dataTableOutput("data")
                          
                        )
                      )
                      
             )
                      #tabPanel("About", includeMarkdown("readme.md"))
             ))

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    
    if(!input$postal_code) {
      df <- df[df$date >= input$range[1] & df$date <= input$range[2],]
      df <- df[df$district %in% input$neighborhoods, ]
      df <- df[df$INCIDENT_TYPE %in% input$incident, ]
      df
      
    } else {
      
      df <- df[df$date >= input$range[1] & df$date <= input$range[2],]
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
      
      # map
      m <- base_map(df, filteredData())
      m
      
      # bar chart
      crime_rank <- filteredData() %>%
        count(INCIDENT_TYPE) %>%
        arrange(desc(n))
      output$selectstat <- renderHighchart({
        
        highchart_barplot(crime_rank)
        
      })
      
      if(input$boundaries) {
        
        bins <- 
        
        m %>%
          addPolygons(data = city_surrey)
        
        
      }
      
      
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
        
        highchart_barplot(crime_rank)
        
      })
      
    } else {
      
      leafletProxy("map", data = filteredData()) %>%
        clearMarkerClusters() %>%
        clearControls()
      
    }
    
  })
  
  ###############################################################################
  
  ##### output table #####
  output$data <- DT::renderDataTable({
    
    if(length(input$groupings) == 0) {
      
      filteredData() %>%
        dplyr::mutate(date = as.character(date)) %>%
        dplyr::select(`Incident Type` = INCIDENT_TYPE, `Postal Code` = postal_code,
                      Address = HUNDRED_BLOCK, Neighborhood = district, Date = date)
      
    } else {
      
      input$year_month %>%
        tolower() %>%
        strsplit(split = "") %>%
        unlist() %>%
        .[1:(length(.) - 2)] %>%
        paste0(collapse = "") -> y_m
      
      filteredData() %>%
        dplyr::select(`Incident Type` = INCIDENT_TYPE, `Postal Code` = postal_code,
                      Address = HUNDRED_BLOCK, Neighborhood = district, Date = date) %>%
        dplyr::mutate(Date = lubridate::floor_date(Date, y_m)) %>%
        dplyr::group_by_at(vars(c(input$groupings, "Date"))) %>%
        dplyr::summarise(count = dplyr::n()) %>%
        dplyr::arrange_at(vars(c(input$groupings, Date))) %>%
        dplyr::mutate(`% closing` = round(c(NA, diff(count)) / count, 2) * 100) -> dat
      
      DT::datatable(
        dat,
        options = list(rowCallback = DT::JS(
          'function(row, data) {
          // Bold cells for those >= 5 in the first column
          if (parseFloat(data[data.length - 1]) < 0)
          $("td", row).last().css("color", "#f00");
          else if (parseFloat(data[data.length - 1]) > 0)
          $("td", row).last().css("color", "#0f0");
    }'))
      ) -> dat
      
      dat %>%
        formatPercentage("% closing")
      
    }
    
    #   DT::datatable(
    #     data,
    #     options = list(rowCallback = DT::JS(
    #       'function(row, data) {
    #       // Bold cells for those >= 5 in the first column
    #       if (parseFloat(data[9]) < 0)
    #       $("td", row).last().css("color", "#f00");
    #       else if (parseFloat(data[9]) > 0)
    #       $("td", row).last().css("color", "#0f0");
    # }'))
    #   )
    
  })
  
}
shinyApp(ui, server)


df %>%
  dplyr::mutate(Date = lubridate::floor_date(date, "year")) %>%
  dplyr::group_by_at("Date") %>%
  summarise(n = n())

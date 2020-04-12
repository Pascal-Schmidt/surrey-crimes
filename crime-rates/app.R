library(shiny)
library(leaflet)
library(highcharter)
library(RColorBrewer)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(DT)
library(sf)

gg_facet_nrow <- function(p){
  num_panels <- length(unique(ggplot_build(p)$data[[1]]$PANEL)) # get number of panels
  num_cols <- ggplot_build(p)$layout$facet$params$ncol # get number of columns set by user
  num_rows <- wrap_dims(num_panels, ncol=num_cols)[1] # determine number of rows
}

data_table_fn <- function(df) {
  
  DT::datatable(
    df,
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
    formatPercentage("% closing") -> dat
  
  return(dat)
  
}

table_fn <- function(df, input_year_month, input_groupings) {
  
  input_year_month %>%
    tolower() %>%
    strsplit(split = "") %>%
    unlist() %>%
    .[1:(length(.) - 2)] %>%
    paste0(collapse = "") -> yy_mm
  
  df %>%
    dplyr::select(`Incident Type` = INCIDENT_TYPE, `Postal Code` = postal_code,
                  Address = HUNDRED_BLOCK, Neighborhood = district, Date = date) %>%
    dplyr::mutate(Date = lubridate::floor_date(Date, yy_mm)) %>%
    dplyr::group_by_at(vars(c("Incident Type", input_groupings, "Date"))) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::arrange_at(vars(c("Incident Type", input_groupings, Date))) -> dat
  
  return(dat)
  
}

base_map <- function(initial_df, filtered_df) {
  
  pal7 <- RColorBrewer::brewer.pal(7, "Set1")
  leaf_pal <- colorFactor(palette = pal7, domain = initial_df$INCIDENT_TYPE)
  
  base_map <- leafletProxy("map", data = filtered_df) %>%
    clearMarkerClusters() %>%
    clearControls() %>%
    addCircleMarkers(clusterOptions = markerClusterOptions(),
                     stroke = FALSE, fill = TRUE, fillOpacity = .7,
                     color = ~leaf_pal(INCIDENT_TYPE),
                     popup = paste("<strong>Neighborhood: </strong>", filtered_df$district, "<br/>",
                                   "<strong>Address: </strong>", filtered_df$HUNDRED_BLOCK, "<br/>",
                                   "<strong>Postal Code: </strong>", filtered_df$postal_code, "<br/>", 
                                   "<strong>Date: </strong>", filtered_df$date, "<br/>")) %>%
    addLegend("bottomright",
              pal = leaf_pal,
              values = ~INCIDENT_TYPE, title = "Category")
  
  return(base_map)
  
}

highchart_barplot <- function(crime_rank) {
  
  bar <- hchart(crime_rank, "bar", hcaes(INCIDENT_TYPE, n)) %>% 
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
  
  return(bar)
  
}

df <- readr::read_csv("final_df.csv") %>%
  tidyr::unite("date", YEAR, MONTH, sep = "-") %>%
  dplyr::mutate(date = lubridate::ymd(paste0(date, "-1"))) %>%
  dplyr::filter(lon > -123 & lon < -122 & lat < 49.5 & lat > 48.5)
city_surrey <- sf::read_sf("city_surrey.shp")



ui <- bootstrapPage(theme = "style.css",
                div(style = "padding: 1px 0px; width: '100%'",
                    titlePanel(
                      title = "",
                      windowTitle = "City of Surrey: Crime and Collision Incidents 2011-2020"
                    )
                ),
  
                navbarPage(
                  
                  # Application title.
                  title = div(span(img(src = "surrey.png", height = 35),
                                   "City of Surrey: Crime and Collision Incidents 2011-2020",
                                   style = "position: relative; top: 50%; transform: translateY(-50%);")),
             
             tabPanel("Map", 
                      style = "height:500px;",
                      
                      
                      leafletOutput("map", width = "100%", height = "100%"),
                      absolutePanel(top = 90, right = 20, draggable = TRUE, fixed = TRUE,
                                    width = "20%", style = "z-index:500; min-width: 300px;",
                                    
                                    highchartOutput("selectstat", height = "325px")),
                      
                      absolutePanel(top = 170, left = 20, draggable = TRUE, fixed = TRUE,
                                    width = "25%", style = "z-index:500; min-width: 200px;",
                                    
                                    
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
                      
                      fluidRow(
                        
                        column(12,
                               
                               shiny::helpText("Group the Surrey crime data set by Incident Rates,
                                               to see how the overall crime rate has been changing over time.
                                               You also have the choice to group by additional variables in the data set.
                                               Whenever you want to update the data you see in the table, go back to the map
                                               tab on top and change the time span, add more neighborhoods of your choosing,
                                               or look up a postal code you are particularily interested in. To explore the data
                                               further, click the checkbox below. Enjoy the data exploration!")
                               
                               ),
                        
                        column(4,
                               
                               shiny::checkboxInput("by_groups", "Explore By Incident Type and More")
                        
                               ),
                        
                        # Sidebar panel for inputs ----
                        column(4,
                               
                               shiny::uiOutput("groupings_ui")
                               
                        ),
                        
                        column(4,  
                               
                               shiny::uiOutput("year_month_ui")
                               
                        ),
                        
                        column(12,
                               
                               DT::dataTableOutput("data")
                               
                        ),
                        
                        column(12,
                               
                               plotOutput("plot")
                               
                        )
                      )
                      
                      
             ),
             tabPanel("About", includeMarkdown("about.md"))
  )
)

server <- function(input, output, session) {
  
  output$groupings_ui <- renderUI({
    
    if(input$by_groups) {
      
      shiny::selectInput("groupings", "Group Your Data",
                         choices =  "Neighborhood",
                         multiple = TRUE,
                         selected = "")
      
    }
    
  })
  
  output$year_month_ui <- renderUI({
    
    if(input$by_groups) {
      
      shiny::radioButtons("year_month", label = "Crime Over Time",
                          choices = c("Monthly", "Yearly"),
                          selected = "Monthly")
      
    }
    
  })
  
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
      base_map(df, filteredData())
      
      # bar chart
      crime_rank <- filteredData() %>%
        count(INCIDENT_TYPE) %>%
        arrange(desc(n))
      output$selectstat <- renderHighchart({
        
        highchart_barplot(crime_rank)
        
      })
      
      if(input$boundaries) {
        
        observe({
          
          leafletProxy("map", data = filteredData()) %>% 
            addPolygons(data = city_surrey, color = "green",
                        fill = FALSE)
          
        })
        
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
    
    if(!input$by_groups) {
      
      filteredData() %>%
        dplyr::mutate(date = as.character(date)) %>%
        dplyr::select(`Incident Type` = INCIDENT_TYPE, `Postal Code` = postal_code,
                      Address = HUNDRED_BLOCK, Neighborhood = district, Date = date)
    
    } else {
      
      table_fn(filteredData(), input$year_month, input$groupings) %>%
        dplyr::mutate(`% Change of # of Incidents` = round(c(NA, diff(count)) / count, 2)) %>%
        data_table_fn()
      
    }
    
  })
  
  p <- reactive({
    
    if(!input$by_groups) {
      
      return()
      
    }
    
    else if(input$by_groups & length(input$groupings) == 0) {
      
      table_fn(filteredData(), input$year_month, input$groupings) %>%
        ggplot(aes(x = Date, y = count,
                   col = `Incident Type`, group = `Incident Type`)) +
        geom_point() +
        geom_line() +
        theme_economist_white() +
        theme(legend.position = "top") 
      
    } else if(input$by_groups & length(input$groupings) == 1) {

      table_fn(filteredData(), input$year_month, input$groupings) %>%
        ggplot(aes(x = Date, y = count,
                   col = `Incident Type`, group = `Incident Type`)) +
        geom_point() +
        geom_line() +
        facet_wrap(~ eval(parse(text = input$groupings)), 
                   ncol = 2,
                   scales = "free_y") +
        theme_economist_white() +
        theme(legend.position = "top",
              plot.title = element_text(hjust = 0.5, size = 18)) +
        ggtitle("Number of Incident Types By Neighborhood Over Time")
      
    }

  })
  
  he <- reactive(gg_facet_nrow(p()))
  
  output$plot <- renderPlot({p() }, height = function(){he()*300})
  
}
shinyApp(ui, server)

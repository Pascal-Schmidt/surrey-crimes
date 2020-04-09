library(shiny)

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
                                   selected = unique(df$postal_code)),
                
                # choose the incident type
                shiny::selectInput("incident", "Incident Type", 
                                   choices = unique(df$INCIDENT_TYPE), multiple = TRUE,
                                   unique(df$INCIDENT_TYPE))
                
                
  )
)

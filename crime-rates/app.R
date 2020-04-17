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
library(feather)
library(data.table)

# function for getting how many time-series plots are going to be plotted
gg_facet_nrow <- function(p) {
  num_panels <- length(unique(ggplot_build(p)$data[[1]]$PANEL)) # get number of panels
  num_cols <- ggplot_build(p)$layout$facet$params$ncol # get number of columns set by user
  num_rows <- wrap_dims(num_panels, ncol = num_cols)[1] # determine number of rows
}

# data table function that colors percentage change red and green
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
    }'
    ))
  ) -> dat

  dat %>%
    formatPercentage("% Change of # of Incidents") -> dat

  return(dat)
}

# function for displaying table (yearly, monthly), by neighborhood or not
table_fn <- function(df, input_year_month, input_groupings) {
  input_year_month %>%
    tolower() %>%
    strsplit(split = "") %>%
    unlist() %>%
    .[1:(length(.) - 2)] %>%
    paste0(collapse = "") -> yy_mm

  df %>%
    dplyr::select(
      `Incident Type` = INCIDENT_TYPE, `Postal Code` = postal_code,
      Address = HUNDRED_BLOCK, Neighborhood = district, Date = date
    ) %>%
    dplyr::mutate(Date = lubridate::floor_date(Date, yy_mm)) %>%
    dplyr::group_by_at(vars(c("Incident Type", input_groupings, "Date"))) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::arrange_at(vars(c("Incident Type", input_groupings, Date))) -> dat

  return(dat)
}

# leaflet map
base_map <- function(initial_df, filtered_df) {
  pal7 <- RColorBrewer::brewer.pal(7, "Set1")
  leaf_pal <- colorFactor(palette = pal7, domain = initial_df$INCIDENT_TYPE)

  base_map <- leafletProxy("map", data = filtered_df) %>%
    clearMarkerClusters() %>%
    clearControls() %>%
    addCircleMarkers(
      clusterOptions = markerClusterOptions(),
      stroke = FALSE, fill = TRUE, fillOpacity = .7,
      color = ~ leaf_pal(INCIDENT_TYPE),
      popup = paste(
        "<strong>Neighborhood: </strong>", filtered_df$district, "<br/>",
        "<strong>Address: </strong>", filtered_df$HUNDRED_BLOCK, "<br/>",
        "<strong>Postal Code: </strong>", filtered_df$postal_code, "<br/>",
        "<strong>Date: </strong>", filtered_df$date, "<br/>"
      )
    ) %>%
    addLegend("bottomright",
      pal = leaf_pal,
      values = ~INCIDENT_TYPE, title = "Category"
    )

  return(base_map)
}

# bar plot
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

# use feather file to read in data faster
df <- feather::read_feather("final_df.feather")

# use data.table for faster filtering (reactive data inputs)
df <- data.table(df)
city_surrey <- sf::read_sf("city_surrey.shp")



ui <- bootstrapPage(
  
  # add city of surrey logo
  theme = "style.css",
  div(
    style = "padding: 1px 0px; width: '100%'",
    titlePanel(
      title = "",
      windowTitle = "City of Surrey: Crime and Collision Incidents 2011-2020"
    )
  ),

  navbarPage(

    # Application title.
    title = div(span(img(src = "surrey.png", height = 35),
      "City of Surrey: Crime and Collision Incidents 2011-2020",
      style = "position: relative; top: 50%; transform: translateY(-50%);"
    )),

    tabPanel("Map",
      style = "height:500px;",

      # keaflet map
      leafletOutput("map", width = "100%", height = "100%"),
      
      # highcharter bar plot
      absolutePanel(
        top = 90, right = 20, draggable = TRUE, fixed = TRUE,
        width = "25%", style = "z-index:500; min-width: 300px;",
        
        highchartOutput("selectstat", height = "325px")
      ),

      absolutePanel(
        top = 170, left = 20, draggable = TRUE, fixed = TRUE,
        width = "25%", style = "z-index:500; min-width: 200px;",


        # choose a date range
        shiny::dateRangeInput("range", "Date",
          start = lubridate::ymd("2019-01-01"), end = max(df$date),
          min = min(df$date), max = max(df$date),
          format = "yyyy/mm/dd",
          separator = "-"
        ),
        

        # choose the incident type
        shiny::selectInput("incident", "Incident Type",
          choices = unique(df$INCIDENT_TYPE), multiple = TRUE,
          selected = df[district == "Whalley"] %>%
            dplyr::pull(INCIDENT_TYPE) %>% {
              unique(.)[1:3]
            }
        ),
        
        # if checkbox for choosing all neighborhoods is not selected,
        # we can choose individual neighborhoods.
        # disappears when checkbox all neighborhood is clicked
        shiny::uiOutput("district"),
        
        # choose to select all neighborhoods at once.

        shiny::checkboxInput("all", "Select all Neighborhoods of Surrey"),
        
        # checkbox for searching for specific neighborhoods
        shiny::checkboxInput("postal_code", "Search for Your Postal Code!"),
        
        # pops up when checkbox is clicked
        shiny::uiOutput("postal_surrey"),
          
        # add boundaries around surrey with shapefile
        shiny::checkboxInput("boundaries", "Add Boundaries Around Surrey")
      )
    ),

    tabPanel(
      "Table",
      
      # helpertext
      fluidRow(
        column(
          12,

          shiny::helpText("Group the Surrey crime data set by Incident Rates,
                                               to see how the overall crime rate has been changing over time.
                                               You also have the choice to group by additional variables in the data set.
                                               Whenever you want to update the data you see in the table, go back to the map
                                               tab on top and change the time span, add more neighborhoods of your choosing,
                                               or look up a postal code you are particularily interested in. To explore the data
                                               further, click the checkbox below. Enjoy the data exploration!")
        ),
        
        # by clicking this checkbox, many more options will appear
        column(
          4,

          shiny::checkboxInput("by_groups", "Explore By Incident Type and More")
        ),

        # group by neighborhood will appear
        column(
          4,

          shiny::uiOutput("groupings_ui")
        ),

        # group by month or year will appear
        column(
          4,

          shiny::uiOutput("year_month_ui")
        ),

        # data table output
        column(
          12,

          DT::dataTableOutput("data")
        ),

        # time series plots will appear
        column(
          12,

          plotOutput("plot")
        )
      )
    ),
    
    # include a short project description
    tabPanel("About", includeMarkdown("about.md"))
  )
)

server <- function(input, output, session) {
  
  # filter for specific neighborhoods
  output$district <- renderUI({
    
    if(!input$all) {
      
      # choose a neighborhood
      shiny::selectInput("neighborhoods", "Neighborhoods",
                         choices = unique(df$district), multiple = TRUE,
                         selected = "Whalley"
      )
    }
    
  })
  
  # group data table by neighborhood
  output$groupings_ui <- renderUI({
    if (input$by_groups) {
      shiny::selectInput("groupings", "Group Your Data",
        choices = "Neighborhood",
        multiple = TRUE,
        selected = ""
      )
    }
  })

  # group by month/year
  output$year_month_ui <- renderUI({
    if (input$by_groups) {
      shiny::radioButtons("year_month",
        label = "Crime Over Time",
        choices = c("Monthly", "Yearly"),
        selected = "Monthly"
      )
    }
  })

  # Reactive expression for the data, subsetted to what the user selected
  filteredData <- reactive({
    
    # filter for dates
    df <- df[date >= input$range[1] & date <= input$range[2]]
    
    # if we do not have include all neighborhoods checkbox checked,
    # then we have to filter for certain neighborhods. Otherwise,
    # every neighborhood is included and we do not have to filter.
    if(!input$all) {
      df <- df[district %in% input$neighborhoods]
    }
    
    # if checked, we can search for postal codes and therefore,
    # have to filter for postal codes. Otherwise we do not.
    if(input$postal_code) {
      df <- df[postal_code %in% input$postal]
    }
    
    # filter for incident type
    df <- df[INCIDENT_TYPE %in% input$incident]
    df
    
  })
  
  output$postal_surrey <- renderUI({
    if (input$postal_code) {

      # choose a postal code
      shiny::selectInput("postal", "Postal Code",
        choices = unique(df$postal_code),
        multiple = TRUE
      )
    }
  })

  output$map <- renderLeaflet({
    
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      fitBounds(~ min(lon), ~ min(lat), ~ max(lon), ~ max(lat))
  })

  
  observe({
    if (length(input$incident) != 0 &
      (length(input$neighborhoods) != 0 | input$all) & !input$postal_code) {

      # map
      base_map(df, filteredData())

      # bar chart
      crime_rank <- filteredData() %>%
        count(INCIDENT_TYPE) %>%
        arrange(desc(n))
      output$selectstat <- renderHighchart({
        highchart_barplot(crime_rank)
        
      })

      # if boundaries checkbox is being clicked, we are observing how 
      # a boundary around surrey is drawn with shapefile
      if (input$boundaries) {
        observe({
          leafletProxy("map", data = filteredData()) %>%
            addPolygons(
              data = city_surrey, color = "green", 
              smoothFactor = 0.2, weight = 1,
              fill = FALSE, opacity = 1
            )
        })
      }
      
      # if postal code is not in neighborhood we have filtered for,
      # a message pops up
    } else if (length(input$incident) != 0 &
      (length(input$neighborhoods) != 0 | input$all) &
      length(input$postal) != 0 & input$postal_code &
      filteredData() %>%
        .[postal_code %in% input$postal] %>%
        .[district %in% input$neighborhoods] %>%
        nrow(.) == 0) {
      showModal(modalDialog(
        title = "Sorry!",
        tags$p("Your postal code is not in the selected neighborhood."),
        tags$p("Give another one a try or expand the number of neighborhoods.")
      ))
      
      # if postal code is in neighborhood,
      # we add crimes to map that occur in filtered neighborhood
    } else if (length(input$incident) != 0 &
      (length(input$neighborhoods) != 0 | input$all) &
      length(input$postal) != 0 & input$postal_code &
      filteredData() %>%
        .[postal_code %in% input$postal] %>%
        .[district %in% input$neighborhoods] %>%
        nrow(.) != 0) {
      base_map(df, filteredData())

      crime_rank <- filteredData() %>%
        count(INCIDENT_TYPE) %>%
        arrange(desc(n))

      output$selectstat <- renderHighchart({
        highchart_barplot(crime_rank)
      })
      
      # else, just draw the base map without anything on it
    } else {
      
      
      leafletProxy("map", data = filteredData()) %>%
        clearMarkerClusters() %>%
        clearControls()
      
    }
  })

  ###############################################################################

  ##### output table #####
  output$data <- DT::renderDataTable({
    
    # if checkbox is not clicked, just display data as seen in map
    if (!input$by_groups) {
      filteredData() %>%
        dplyr::mutate(date = as.character(date)) %>%
        dplyr::select(
          `Incident Type` = INCIDENT_TYPE, `Postal Code` = postal_code,
          Address = HUNDRED_BLOCK, Neighborhood = district, Date = date
        )
    } else {
      
      # if checkbox is clicked, display percentages 
      table_fn(filteredData(), input$year_month, input$groupings) %>%
        dplyr::mutate(`% Change of # of Incidents` = round( c(NA, na.omit(c((diff(count)), NA) / count)), 2)) %>%
        data_table_fn()
    }
  })
  
  # if checkbox is not clicked, display nothing 
  p <- reactive({
    if (!input$by_groups) {
      
      ggplot() +
        theme_void()
      
    }

    # if we do not group by neighborhood, we do not use facet_wrap,
    # and only one plot is shown
    else if (input$by_groups & length(input$groupings) == 0) {
      table_fn(filteredData(), input$year_month, input$groupings) %>%
        ggplot(aes(
          x = Date, y = count,
          col = `Incident Type`, group = `Incident Type`
        )) +
        geom_point() +
        geom_line() +
        theme_economist_white() +
        theme(legend.position = "top")
      
      # if we group by neighborhood, then we facet by neighborhood and time-series
      # plots for each neighborhood are being shown. We alsomake use of facet function
      # to make each plot appear in a readable height without squishing plots when 
      # adding neighborhoods
    } else if (input$by_groups & length(input$groupings) == 1) {
      table_fn(filteredData(), input$year_month, input$groupings) %>%
        ggplot(aes(
          x = Date, y = count,
          col = `Incident Type`, group = `Incident Type`
        )) +
        geom_point() +
        geom_line() +
        facet_wrap(~ eval(parse(text = input$groupings)),
          ncol = 2,
          scales = "free_y"
        ) +
        theme_economist_white() +
        theme(
          legend.position = "top",
          plot.title = element_text(hjust = 0.5, size = 18)
        ) +
        ggtitle("Number of Incident Types By Neighborhood Over Time")
    }
  })

  # get the height of the plot. The more neighborhoods we select, the larger the plot output
  he <- reactive(
    
    if(input$by_groups & length(input$groupings) == 1) {

      return(gg_facet_nrow(p()))
      
    } else {
      
      x = 1
      return(x)
      
    }

  )

  # disply plot
  output$plot <- renderPlot(
    {p()}, height = function() {he() * 300}
  )
}
shinyApp(ui, server)

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
# R Shiny Application of Crime and Collision Data for the City of Surrey

## Data Collection

- The data was collected from the city of [Surrey](https://data.surrey.ca/dataset/rcmp-crime). The data set consists of 10 csv files containing the incident type, the address, the month of the incident, and the year of the incident.

- Additional data augmentation was done by adding the latitude and logitude values for addresses through the Google Maps API. In addition to that, through reverse geocoding, we got the postal codes from the different addresses. 

- On top of that, we downloaded a shapefile with the help of the `cancensus` library in R. The documentation about the file can be found [here](https://mountainmath.github.io/cancensus/articles/cancensus.html).

- The map was constructed with help of the `leaflet` library in R.

## Data Representation

- The app is designed to inform the citizens of Surrey about crime and collision incidents in their municipality. 
- We can zoom into different neighbohoods and postal codes by clicking on the circles.
- A very specific serach can be done by searching for specific postal codes and crimes.
- Besides the visual map with crimes, the app also features a table and time-series plots that show how crimes have been evolved over time in certain neighborhoods.

Link to the (City of Surrey Crime and Collision Shiny Aplication)[https://www.shinyapps.io/admin/#/application/2098944]

## Follow Up

- If you have any questions about the app, have recommendations, find any bugs, or find errors concerning certain address and neighborhoods, then you can email me: schmidtpascal553@googlemail.com. Thank you!

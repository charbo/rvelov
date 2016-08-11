library(shiny)
library(rjson)
library(plyr)
library(rCharts);
library(leaflet);
library(htmltools);
library(RCurl);

path <- 'https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=681e47e9ba5d7f9316f815d7d262382d92cea6c4'
document <- fromJSON(getURL(url=path))
dat <- lapply(document, function(doc) {
  data.frame(num = doc$number, place = doc$available_bike_stands, lat = doc$position$lat, lng = doc$position$lng, bike=doc$available_bikes)
})
pal_green <- colorNumeric(
  palette = "Greens",
  domain = dat$place
)
pal_red <- colorNumeric(
  palette = "Reds",
  domain = dat$bike
)

stations <- rbind.fill(dat)

shinyServer(function(input, output){
  
    dataInput <- reactive({
      if (input$radio == 1) {
        stations$display = stations$place
      } else {
        stations$display = stations$bike
      }
      stations  
    })

    output$mymap <- renderLeaflet({
      leaflet(data = dataInput()) %>% setView(lng = 4.84 , lat = 45.75, zoom = 13) %>%
        addProviderTiles("Stamen.TonerLite",
                         options = providerTileOptions(noWrap = TRUE)
        ) %>% 
        addCircleMarkers(
            lng = ~lng, lat = ~lat, stroke = FALSE, fillOpacity = 1, radius = ~display*0.5, fill = TRUE, color = ~pal_green(display), fillColor = ~pal_green(display)
        ) %>%
        addLegend(pal = pal_green, values = ~display,
                  title = ifelse(input$radio == 1, "Nombre de places disponibles", "Nombre de vélos disponibles"),  opacity = 1,
                  position = c("bottomleft")
        ) 
      
    })   
})
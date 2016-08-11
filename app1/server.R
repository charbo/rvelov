library("shiny")
library("RMySQL")
library("zoo")
library("forecast")
library("leaflet");
library("htmltools");

con <- dbConnect(MySQL(), user = 'root', password = 'root', host = '192.168.0.30', dbname='test')

forecastAll <- function(startDate, endDate) {
  sql <- paste("SELECT s.ID_STATION, IF(s.INTERVALLE < @a, @b:=@b+1, @b) as VALUE_B,  s.INTERVALLE + 288 * @b AS INTERVALLE, MIN(s.DATE) AS DATE, AVG(s.DISPONIBLES) AS DISPONIBLES, @a := s.INTERVALLE as previous FROM station s, (select @a:= 1, @b:=0) as init WHERE s.DATE BETWEEN '",startDate,"' AND '",endDate,"' GROUP BY s.ID_STATION, s.JOUR , s.INTERVALLE ORDER BY s.ID;", sep="")
  datas <- dbGetQuery(conn = con, statement = sql)
  df <- data.frame(datas)
  seq(from = min(df$INTERVALLE), to = max(df$INTERVALLE))
  inte <- data.frame(inte)
  fore <- do.call(rbind, lapply(unique(df$ID_STATION), function(station) forecastOne(station, df[df$ID_STATION == station, ], inte)))
  
}

forecastOne <- function(idStation, datasStation, inte) {
  merge <- merge(inte, datasStation, by.x=names(inte)[1], by.y="INTERVALLE", all.x = TRUE)
  merge <- na.locf(merge)
  s1 <- ts(data = as.numeric(merge$DISPONIBLES), start = as.numeric(min(merge$inte)), end = as.numeric(max(merge$inte)), frequency = 1)
  fit <- auto.arima(s1)
  fcast <- forecast(fit, h=9)
  res <- data.frame(station = idStation, mean1 = fcast$mean[3], mean2 = fcast$mean[6], mean3 = fcast$mean[9])
  res
}

forecastQuarter <- function() {
  today <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  before <- format(Sys.time() - 7*60*60*24, "%Y-%m-%d %H:%M:%S")
  forecastAll(before, today)
}

res <- forecastQuarter()
sqlStat <- "select * from stations"
stations <- dbGetQuery(conn = con, statement = sqlStat)
merge <- merge(stations, res, by.x="ID_STATION", by.y="station", all.x = TRUE)

shinyServer(function(input, output){
  dataInput <- reactive({
    if (input$integer == 15) {
      merge$mean = merge$mean1
    } else if (input$integer == 30) {
      merge$mean = merge$mean2
    } else {
      merge$mean = merge$mean3
    }
    merge  
  })
  
  
  output$mymap <- renderLeaflet({
    leaflet(data = dataInput()) %>% addTiles() %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
      addCircleMarkers(
        lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = ~round(mean), fill = TRUE,
        clusterOptions = markerClusterOptions(),
        label = ~as.character(round(mean))
      ) 
  })
  
  # Show the values using an HTML table
  output$values <- renderTable({
    dataInput()
  })
  
})
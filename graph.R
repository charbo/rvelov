merge <- merge(stations, res, by.x="ID_STATION", by.y="station", all.x = TRUE)

leaflet(data = merge) %>% addTiles() %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
  addCircleMarkers(
    lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = ~mean1, fill = TRUE, clusterOptions = markerClusterOptions()
  ) %>% addProviderTiles("Stamen.Toner")
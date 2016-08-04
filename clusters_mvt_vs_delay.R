library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);

#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')

#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select V_MVT.ID_STATION, V_MVT.MOUVEMENT, V_DELAY.DELAY from (select d.ID_STATION as ID_STATION, sum(abs(d.MOUVEMENT))/max(d.TOTALES) as MOUVEMENT from station d inner join stations stat on stat.ID_STATION = d.ID_STATION group by d.ID_STATION) V_MVT inner join (select d.ID_STATION, avg(delay) from delais d group by d.ID_STATION) V_DELAY on V_MVT.ID_STATION = V_DELAY.ID_STATION ;")

#Création du data frame
stations <- data.frame(datas, row.names = 'ID_STATION')

#ACP
res.pca <- PCA(stations, scale.unit = TRUE, graph = FALSE)

#Cluster
res.hcpc=HCPC(res.pca, graph = FALSE)

#nombre de clusters
res.hcpc$call$t$nb.clust

#Cluster 1
clust1 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 1,]

#Stations du cluster 1
stations1 <- dbGetQuery(conn = con, statement = paste("select * from stations where ID_STATION in (", paste(rownames(clust1), collapse=','), ")"))

#Cluster 1
clust1 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 2,]

#Stations du cluster 1
stations1 <- dbGetQuery(conn = con, statement = paste("select * from stations where ID_STATION in (", paste(rownames(clust1), collapse=','), ")"))

#Cluster 2
clust2 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 1,]

#Stations du cluster 2
stations2 <- dbGetQuery(conn = con, statement = paste("select * from stations where ID_STATION in (", paste(rownames(clust2), collapse=','), ")"))

#Cluster 3
clust3 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 3,]

#Stations du cluster 3
stations3 <- dbGetQuery(conn = con, statement = paste("select * from stations where ID_STATION in (", paste(rownames(clust3), collapse=','), ")"))


ALL1 <- stations1[,4:5]
ALL2 <- stations2[,4:5]
ALL3 <- stations3[,4:5]
ALL1$CLUST <- 1
ALL2$CLUST <- 2
ALL3$CLUST <- 3
ALL <- rbind(ALL1, ALL2)
ALL <- rbind(ALL, ALL3)

pal_green <- colorNumeric(
  palette = "Greens",
  domain = ALL$CUST
)

map <- leaflet(data = ALL) %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
    addCircleMarkers(
        lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = 6, fill = TRUE, color = ~pal_green(CLUST), fillColor = ~pal_green(CLUST)
    )  %>% addProviderTiles("Stamen.Toner")


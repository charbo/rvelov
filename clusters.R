library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);

#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')

#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select d.ID_STATION as ID_STATION, sum(abs(d.MOUVEMENT))/max(d.TOTALES) as MOUVEMENT, sum(d.FULL)/max(d.TOTALES) as FULL from station d inner join stations stat on stat.ID_STATION = d.ID_STATION group by d.ID_STATION;")

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

#MAP 1
#Centre
loc <- c(4.75, 45.7, 4.95, 45.8)
map <- get_map(location=loc, source="osm")
ggmap(map) +
geom_point(data=stations1, aes(x=as.numeric(LONG), y=as.numeric(LAT), size=10, color=10), alpha=.9, na.rm=T)

#MAP2
#Leaflet
leaflet(data = stations1) %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
addCircleMarkers(
lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = 8, fill = TRUE, color = "#FF0000"
)
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);

#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')

#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select d.ID_STATION, s.TOTALES,avg(CASE WHEN s.NUMERO_JOUR = 2 THEN d.delay/1000 ELSE 0 END) as LUNDI, avg(CASE WHEN s.NUMERO_JOUR = 3 THEN d.delay/1000 ELSE 0 END) as MARDI, avg(CASE  WHEN s.NUMERO_JOUR = 4 THEN d.delay/1000 ELSE 0 END) as MERCREDI, avg(CASE WHEN s.NUMERO_JOUR = 5 THEN d.delay/1000 ELSE 0 END) as JEUDI, avg(CASE WHEN s.NUMERO_JOUR = 6 THEN d.delay/1000 ELSE 0 END) as VENDREDI, avg(CASE WHEN s.NUMERO_JOUR = 7 THEN d.delay/1000 ELSE 0 END) as SAMEDI, avg(CASE WHEN s.NUMERO_JOUR = 1 THEN d.delay/1000 ELSE 0 END) as DIMANCHE from delais d inner join station s on s.ID = d.ID and s.ID_STATION = d.ID_STATION inner join stations stat on stat.ID_STATION = s.ID_STATION group by ID_STATION;")

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


#MAP2
#Leaflet
leaflet(data = stations1) %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
addCircleMarkers(
lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = 8, fill = TRUE, color = "#FF0000"
)
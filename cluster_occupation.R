library(RMySQL)
library(FactoMineR)
library(ggmap)
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
ggmap(map) +
geom_point(data=stations1, aes(x=as.numeric(LONG), y=as.numeric(LAT), size=10, color=10), alpha=.9, na.rm=T)
#MAP2
#Leaflet
leaflet(data = stations1) %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
addCircleMarkers(
lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = 8, fill = TRUE, color = "#FF0000"
)
leaflet(data = stations1) %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
addCircleMarkers(
lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = 8, fill = TRUE, color = "#FF0000"
)
library(leaflet);
library(RMySQL)
library(FactoMineR)
library(ggmap)
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
ggmap(map) +
geom_point(data=stations1, aes(x=as.numeric(LONG), y=as.numeric(LAT), size=10, color=10), alpha=.9, na.rm=T)
#MAP2
#Leaflet
leaflet(data = stations1) %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
addCircleMarkers(
lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = 8, fill = TRUE, color = "#FF0000"
)
library(devtools)
library(devtools)
devtools::install_github("rstudio/leaflet")
install.packages("digest")
devtools::install_github("rstudio/leaflet")
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select d.ID_STATION as ID_STATION, sum(abs(d.MOUVEMENT))/max(d.TOTALES) as MOUVEMENT, sum(d.FULL)/max(d.TOTALES) as FULL from station d inner join stations stat on stat.ID_STATION = d.ID_STATION where MOUVEMENT is not null group by d.ID_STATION;")
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
#Cluster 2
clust2 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 2,]
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
)
map
rstudio::viewer(map)
map <- leaflet() %>% addTiles()
map
heures <- dbGetQuery(conn = con, statement = "select s.HEURE, sum(CASE   		WHEN s.MOUVEMENT >= 0 THEN s.MOUVEMENT         ELSE 0  	END) as ARRIVEES,     sum(CASE   		WHEN s.MOUVEMENT <= 0 THEN s.MOUVEMENT         ELSE 0  	END) as DEPART from STATION s group by s.HEURE;")
#Création du data frame
d_heures <- data.frame(datas, row.names = 'HEURE')
#ACP
res.pca <- PCA(d_heures, scale.unit = TRUE, graph = TRUE)
summary(heures)
d_heures <- data.frame(heures, row.names = 'HEURE')
res.pca <- PCA(d_heures, scale.unit = TRUE, graph = TRUE)
install.packages("htmlwidgets")
install.packages("htmlwidgets")
install.packages("htmlwidgets")
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select d.ID_STATION as ID_STATION, sum(abs(d.MOUVEMENT))/max(d.TOTALES) as MOUVEMENT, sum(d.FULL)/max(d.TOTALES) as FULL from station d inner join stations stat on stat.ID_STATION = d.ID_STATION where MOUVEMENT is not null group by d.ID_STATION;")
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
#Cluster 2
clust2 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 2,]
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
)
map
htmlwidgets::saveWidget(map, file="map.html")
options(viewer = NULL)
map
m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()
map %>% addTiles()
map %>% addTiles()
map %>% addProviderTiles("Stamen.Toner")
map %>% addProviderTiles("Stamen.TonerLabels")
map %>% addProviderTiles("Stamen.TonerLines")
map %>% addProviderTiles("CartoDB.Positron")
heures <- dbGetQuery(conn = con, statement = "select s.HEURE, sum(abs(s.MOUVEMENT)) from STATION s group by s.HEURE;")
#Création du data frame
d_heures <- data.frame(heures, row.names = 'HEURE')
#ACP
res.pca <- PCA(d_heures, scale.unit = TRUE, graph = TRUE)
res.pca
res.pca <- PCA(d_heures, scale.unit = TRUE, graph = TRUE)
res.pca <- PCA(d_heures)
heures <- dbGetQuery(conn = con, statement = "select s.HEURE, sum(abs(s.MOUVEMENT)), sum(s.FULL) from STATION s group by s.HEURE;")
d_heures <- data.frame(heures, row.names = 'HEURE')
res.pca <- PCA(d_heures)
res.hcpc=HCPC(res.pca, graph = FALSE)
res.hcpc=HCPC(res.pca, graph = TRUE)
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
datas <- dbGetQuery(conn = con, statement = "select d.ID_STATION, stat.JOUR, stat.HEURE, s.TOTALES, d.delay/1000 from delais d inner join station stat on stat.ID = d.ID inner join stations s on s.ID_STATION = stat.ID_STATION ;")
stations <- data.frame(datas, row.names = 'ID_STATION')
datas <- dbGetQuery(conn = con, statement = "select d.ID, d.ID_STATION, stat.JOUR, stat.HEURE, s.TOTALES, d.delay/1000 from delais d inner join station stat on stat.ID = d.ID inner join stations s on s.ID_STATION = stat.ID_STATION ;")
datas <- dbGetQuery(conn = con, statement = "select d.ID, d.ID_STATION, stat.JOUR, stat.HEURE, s.TOTALES, d.delay/1000 from delais d inner join station stat on stat.ID = d.ID and stat.ID_STATION = d.ID_STATION inner join stations s on s.ID_STATION = stat.ID_STATION ;")
stations <- data.frame(datas, row.names = 'ID')
datas$IDS <- datas$ID + "" + datas$ID_STATION
datas$IDS <- paste(datas$ID, datas$ID_STATION)
stations <- data.frame(datas, row.names = 'IDS')
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE)
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE, quanti.sup = 2)
summary(stations)
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE, quanti.sup = 1)
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE, quali.sup =  2)
res.hcpc=HCPC(res.pca, graph = TRUE)
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
datas <- dbGetQuery(conn = con, statement = "select d.ID_STATION, s.TOTALES,avg(CASE WHEN s.NUMERO_JOUR = 2 THEN d.delay/1000 ELSE 0 END) as LUNDI, avg(CASE WHEN s.NUMERO_JOUR = 3 THEN d.delay/1000 ELSE 0 END) as MARDI, avg(CASE  WHEN s.NUMERO_JOUR = 4 THEN d.delay/1000 ELSE 0END) as MERCREDI, avg(CASE WHEN s.NUMERO_JOUR = 5 THEN d.delay/1000 ELSE 0 END) as JEUDI, avg(CASE WHEN s.NUMERO_JOUR = 6 THEN d.delay/1000 ELSE 0 END) as VENDREDI, avg(CASE WHEN s.NUMERO_JOUR = 7 THEN d.delay/1000 ELSE 0 END) as SAMEDI, avg(CASE WHEN s.NUMERO_JOUR = 1 THEN d.delay/1000 ELSE 0 END) as DIMANCHE from delais d inner join station s on s.ID = d.ID and s.ID_STATION = d.ID_STATION inner join stations stat on stat.ID_STATION = s.ID_STATION group by ID_STATION;")
datas <- dbGetQuery(conn = con, statement = "select d.ID_STATION, s.TOTALES,avg(CASE WHEN s.NUMERO_JOUR = 2 THEN d.delay/1000 ELSE 0 END) as LUNDI, avg(CASE WHEN s.NUMERO_JOUR = 3 THEN d.delay/1000 ELSE 0 END) as MARDI, avg(CASE  WHEN s.NUMERO_JOUR = 4 THEN d.delay/1000 ELSE 0 END) as MERCREDI, avg(CASE WHEN s.NUMERO_JOUR = 5 THEN d.delay/1000 ELSE 0 END) as JEUDI, avg(CASE WHEN s.NUMERO_JOUR = 6 THEN d.delay/1000 ELSE 0 END) as VENDREDI, avg(CASE WHEN s.NUMERO_JOUR = 7 THEN d.delay/1000 ELSE 0 END) as SAMEDI, avg(CASE WHEN s.NUMERO_JOUR = 1 THEN d.delay/1000 ELSE 0 END) as DIMANCHE from delais d inner join station s on s.ID = d.ID and s.ID_STATION = d.ID_STATION inner join stations stat on stat.ID_STATION = s.ID_STATION group by ID_STATION;")
stations <- data.frame(datas, row.names = 'ID_STATION')
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE)
summary(stations)
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE, quali.sup = 1)
res.hcpc=HCPC(res.pca, graph = TRUE)
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
datas <- dbGetQuery(conn = con, statement = "select V_MVT.ID_STATION, V_MVT.MOUVEMENT, V_DELAY.DELAY from (select d.ID_STATION as ID_STATION, sum(abs(d.MOUVEMENT))/max(d.TOTALES) as MOUVEMENT from station d inner join stations stat on stat.ID_STATION = d.ID_STATION group by d.ID_STATION) V_MVT inner join (select d.ID_STATION, avg(delay) from delais d group by d.ID_STATION) V_DELAY on V_MVT.ID_STATION = V_DELAY.ID_STATION ;")
datas <- dbGetQuery(conn = con, statement = "select V_MVT.ID_STATION, V_MVT.MOUVEMENT, V_DELAY.DELAY from (select d.ID_STATION as ID_STATION, sum(abs(d.MOUVEMENT))/max(d.TOTALES) as MOUVEMENT from station d inner join stations stat on stat.ID_STATION = d.ID_STATION group by d.ID_STATION) V_MVT inner join (select d.ID_STATION, avg(delay) as DELAY from delais d group by d.ID_STATION) V_DELAY on V_MVT.ID_STATION = V_DELAY.ID_STATION ;")
stations <- data.frame(datas, row.names = 'ID_STATION')
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE)
res.hcpc=HCPC(res.pca, graph = TRUE)
#Cluster 1
clust1 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 1,]
#Stations du cluster 1
stations1 <- dbGetQuery(conn = con, statement = paste("select * from stations where ID_STATION in (", paste(rownames(clust1), collapse=','), ")"))
#Cluster 2
clust2 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 2,]
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
) %>% addTiles()
map
map %>% addTiles()
map %>% addProviderTiles("Stamen.Toner")
res.hcpc=HCPC(res.pca, graph = TRUE)
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
) %>% addTiles()
rstudio::viewer(map)
browseURL(map)
options(viewer = NULL)
map
#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select d.ID_STATION as ID_STATION, sum(abs(d.MOUVEMENT))/max(d.TOTALES) as MOUVEMENT, sum(d.FULL)/max(d.TOTALES) as FULL from station d inner join stations stat on stat.ID_STATION = d.ID_STATION where MOUVEMENT is not null group by d.ID_STATION;")
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
#Cluster 2
clust2 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 2,]
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
map2 <- leaflet(data = ALL) %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
addCircleMarkers(
lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = 6, fill = TRUE, color = ~pal_green(CLUST), fillColor = ~pal_green(CLUST)
) %>% addTiles()
map2
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select s.ID_STATION,  sum(CASE  WHEN s.HEURE >= 9 AND s.HEURE < 10 THEN d.delay/1000  ELSE 0 END) as NEUF,  sum(CASE  WHEN s.HEURE >= 18 AND s.HEURE < 21 THEN d.delay/1000   ELSE 0 END)/3 as SOIREE,	sum(CASE  WHEN s.HEURE >= 7 AND s.HEURE < 9 THEN d.delay/1000  ELSE 0 END)/2 as MATIN,    sum(CASE  		WHEN s.HEURE >= 10 AND s.HEURE < 18 THEN d.delay/1000        ELSE 0 	END)/8 as JOURNEE,    sum(CASE  		WHEN s.HEURE >= 21 OR s.HEURE < 7 THEN d.delay/1000        ELSE 0 	END) /10 as NUIT    from delais d inner join station s on s.ID = d.ID and s.ID_STATION = d.ID_STATION group by ID_STATION;")
#Création du data frame
stations <- data.frame(datas, row.names = 'ID_STATION')
#ACP
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE)
#Cluster
res.hcpc=HCPC(res.pca, graph = TRUE)
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select s.ID_STATION,  avg(CASE  WHEN s.HEURE >= 9 AND s.HEURE < 10 THEN d.delay/1000  ELSE 0 END) as NEUF,  avg(CASE  WHEN s.HEURE >= 18 AND s.HEURE < 21 THEN d.delay/1000   ELSE 0 END) as SOIREE,	avg(CASE  WHEN s.HEURE >= 7 AND s.HEURE < 9 THEN d.delay/1000  ELSE 0 END) as MATIN,    avg(CASE  		WHEN s.HEURE >= 10 AND s.HEURE < 18 THEN d.delay/1000        ELSE 0 	END) as JOURNEE,    avg(CASE  		WHEN s.HEURE >= 21 OR s.HEURE < 7 THEN d.delay/1000        ELSE 0 	END)  as NUIT    from delais d inner join station s on s.ID = d.ID and s.ID_STATION = d.ID_STATION group by ID_STATION;")
#Création du data frame
stations <- data.frame(datas, row.names = 'ID_STATION')
#ACP
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE)
#Cluster
res.hcpc=HCPC(res.pca, graph = TRUE)
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select s.ID_STATION, avg(CASE WHEN s.HEURE >= 18 AND s.HEURE < 21 THEN d.delay/1000 ELSE 0 	END) as SOIREE,	avg(CASE WHEN s.HEURE >= 8 AND s.HEURE < 10 THEN d.delay/1000 ELSE 0 END) as MATIN, avg(CASE  WHEN s.HEURE >= 10 AND s.HEURE < 18 THEN d.delay/1000 ELSE 0 END) as JOURNEE,  avg(CASE  WHEN s.HEURE >= 21 OR s.HEURE < 8 THEN d.delay/1000 ELSE 0 END) as NUIT  from delais d inner join station s on s.ID = d.ID and s.ID_STATION = d.ID_STATION group by ID_STATION;")
#Création du data frame
stations <- data.frame(datas, row.names = 'ID_STATION')
#ACP
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE)
#Cluster
res.hcpc=HCPC(res.pca, graph = TRUE)
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select s.ID_STATION, avg(CASE WHEN s.HEURE >= 18 AND s.HEURE < 21 THEN d.delay/1000 ELSE 0     END) as SOIREE,    avg(CASE WHEN s.HEURE >= 8 AND s.HEURE < 10 THEN d.delay/1000 ELSE 0 END) as MATIN, avg(CASE  WHEN s.HEURE >= 10 AND s.HEURE < 18 THEN d.delay/1000 ELSE 0 END) as JOURNEE,  avg(CASE  WHEN s.HEURE >= 21 OR s.HEURE < 8 THEN d.delay/1000 ELSE 0 END) as NUIT  from delais d inner join station s on s.ID = d.ID and s.ID_STATION = d.ID_STATION group by ID_STATION;")
#Création du data frame
stations <- data.frame(datas, row.names = 'ID_STATION')
#ACP
res.pca <- PCA(stations, scale.unit = TRUE, graph = FALSE)
res.hcpc=HCPC(res.pca, graph = TRUE)
#Cluster 1
clust1 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 1,]
#Stations du cluster 1
stations1 <- dbGetQuery(conn = con, statement = paste("select * from stations where ID_STATION in (", paste(rownames(clust1), collapse=','), ")"))
#Cluster 2
clust2 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 2,]
#Stations du cluster 2
stations2 <- dbGetQuery(conn = con, statement = paste("select * from stations where ID_STATION in (", paste(rownames(clust2), collapse=','), ")"))
#Cluster 3
clust3 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 3,]
#Stations du cluster 3
stations3 <- dbGetQuery(conn = con, statement = paste("select * from stations where ID_STATION in (", paste(rownames(clust3), collapse=','), ")"))
#Cluster 4
clust4 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 4,]
#Stations du cluster 4
stations4 <- dbGetQuery(conn = con, statement = paste("select * from stations where ID_STATION in (", paste(rownames(clust4), collapse=','), ")"))
#Cluster 5
clust5 <- res.hcpc$data.clust[res.hcpc$data.clust$clust == 5,]
#Stations du cluster 5
stations5 <- dbGetQuery(conn = con, statement = paste("select * from stations where ID_STATION in (", paste(rownames(clust5), collapse=','), ")"))
ALL1 <- stations1[,4:5]
ALL2 <- stations2[,4:5]
ALL3 <- stations3[,4:5]
ALL4 <- stations4[,4:5]
ALL5 <- stations5[,4:5]
ALL1$CLUST <- 1
ALL2$CLUST <- 2
ALL3$CLUST <- 3
ALL4$CLUST <- 4
ALL5$CLUST <- 5
ALL <- rbind(ALL1, ALL2)
ALL <- rbind(ALL, ALL3)
ALL <- rbind(ALL, ALL4)
ALL <- rbind(ALL, ALL5)
pal_green <- colorNumeric(
palette = "Greens",
domain = ALL$CUST
)
map <- leaflet(data = ALL) %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
addCircleMarkers(
lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = 6, fill = TRUE, color = ~pal_green(CLUST), fillColor = ~pal_green(CLUST)
)
map
factpal <- colorFactor(topo.colors(5), ALL$CUST)
map <- leaflet(data = ALL) %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
addCircleMarkers(
lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = 6, fill = TRUE, color = ~factpall(CLUST), fillColor = ~factpall(CLUST)
)
map <- leaflet(data = ALL) %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
addCircleMarkers(
lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = 6, fill = TRUE, color = ~factpal(CLUST), fillColor = ~factpal(CLUST)
)
map
options(viewer = NULL)
map
map %>% addProviderTiles("Stamen.Toner")
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE)
factpal <- colorFactor(c("Red", "Blue", "Grey", "Yellow", "Green"), ALL$CUST)
map <- leaflet(data = ALL) %>% setView(lng = 4.84, lat = 45.75, zoom = 13) %>%
addCircleMarkers(
lng = ~LONG, lat = ~LAT, stroke = FALSE, fillOpacity = 1, radius = 6, fill = TRUE, color = ~factpal(CLUST), fillColor = ~factpal(CLUST)
)
map %>% addProviderTiles("Stamen.Toner")
savehistory("C:/projet/velov/scripts/cluster_occupation.R")

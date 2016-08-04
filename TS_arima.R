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
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select s.NUMERO_JOUR, avg(abs(s.MOUVEMENT)) as MVT, avg(d.DELAY) as DELAY from delais d inner join station s on s.ID = d.ID and s.ID_STATION = d.ID_STATION group by s.NUMERO_JOUR;")
#Création du data frame
jours <- data.frame(datas, row.names = 'NUMERO_JOUR')
#ACP
res.pca <- PCA(jours, scale.unit = TRUE, graph = FALSE)
res.pca <- PCA(jours, scale.unit = TRUE, graph = TRUE)
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE)
res.pca <- PCA(jours, scale.unit = TRUE, graph = TRUE)
res.hcpc=HCPC(res.pca, graph = TRUE)
#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select s.NUMERO_JOUR, avg(IF(s.MOUVEMENT>=0, IF(s.MOUVEMENT=0, 1, s.MOUVEMENT), 0)) as ARRIVEES, avg(IF(s.MOUVEMENT<=0, IF(s.MOUVEMENT=0, -1, s.MOUVEMENT), 0) as DEPARTS, avg(d.DELAY) as DELAY from delais d inner join station s on s.ID = d.ID and s.ID_STATION = d.ID_STATION group by s.NUMERO_JOUR;")
#Création du data frame
jours <- data.frame(datas, row.names = 'NUMERO_JOUR')
#ACP
res.pca <- PCA(jours, scale.unit = TRUE, graph = TRUE)
#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select s.NUMERO_JOUR, avg(IF(s.MOUVEMENT>=0, IF(s.MOUVEMENT=0, 1, s.MOUVEMENT), 0)) as ARRIVEES, avg(IF(s.MOUVEMENT<=0, IF(s.MOUVEMENT=0, -1, s.MOUVEMENT), 0)) as DEPARTS, avg(d.DELAY) as DELAY from delais d inner join station s on s.ID = d.ID and s.ID_STATION = d.ID_STATION group by s.NUMERO_JOUR;")
#Création du data frame
jours <- data.frame(datas, row.names = 'NUMERO_JOUR')
#ACP
res.pca <- PCA(jours, scale.unit = TRUE, graph = TRUE)
res.hcpc=HCPC(res.pca, graph = TRUE)
library(RMySQL)
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select s.INTERVALLE, avg(s.DISPONIBLES) as DISPONIBLES  from station s where s.ID_STATION = '3003' and s.JOUR = 22 and s.MOIS = 2 group by s.INTERVALLE order by s.INTERVALLE;")
summary(datas)
s3003 <- data.frame(datas)
summary(s3003)
vals2 <- expand.grid(YearWeek = 0:288,
ProductID = unique(s3003$DISPONIBLES)))
vals2 <- expand.grid(YearWeek = 0:288,
ProductID = unique(s3003$DISPONIBLES))
vals2
merge(vals2,s3003,all = TRUE)
inte <- 0:288
inte
merge(inte, s3003$DISPONIBLES)
summery(inte)
summary(inte)
inte <- data.frame(0:288)
inte
summery(inte)
summary(inte)
merge(inte, s3003, inte$X0.288 = s3003$INTERVALLE, inte$X0.288 = all)
merge(inte, s3003, by.x="X0.288", by.y="INTERVALLE", all.x = all)
merge(inte, s3003, by.x="X0.288", by.y="INTERVALLE", all.x = al)
merge(inte, s3003, by.x="X0.288", by.y="INTERVALLE", all.x = TRUE)
library(RMySQL)
#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
datas <- dbGetQuery(conn = con, statement = "select s.INTERVALLE, avg(s.DISPONIBLES) as DISPONIBLES  from station s where s.ID_STATION = '3003' and s.JOUR = 22 and s.MOIS = 2  ans s.HEURE <= 17 group by s.INTERVALLE order by s.INTERVALLE;")
datas <- dbGetQuery(conn = con, statement = "select s.INTERVALLE, avg(s.DISPONIBLES) as DISPONIBLES  from station s where s.ID_STATION = '3003' and s.JOUR = 22 and s.MOIS = 2  and s.HEURE <= 17 group by s.INTERVALLE order by s.INTERVALLE;")
summary(datas)
s3003 <- data.frame(datas)
max(s3003$INTERVALLE)
inte <- data.frame(0:max(s3003$INTERVALLE))
inte
names(inte)
names(inte)[0]
names(inte)[]
names(inte)[1]
merge(inte, s3003, by.x=names(inte)[1], by.y="INTERVALLE", all.x = TRUE)
merge <- merge(inte, s3003, by.x=names(inte)[1], by.y="INTERVALLE", all.x = TRUE)
plot(merge)
fit<-arima(merge$DISPONIBLES, order=c(2,0,2))
predict <- predict(fit, n.ahead = 10)
predict
s1 <- ts(data = merge, start = 0, end = 216)
plot(s1)
s1 <- ts(data = merge$DISPONIBLES, start = 0, end = 216)
plot(s1)
s1 <- ts(data = merge$DISPONIBLES, start = 0, end = 216, frequency = 5)
plot(s1)
s1 <- ts(data = merge$DISPONIBLES, start = 0, end = 216, frequency = 1)
plot(s1)
s1 <- ts(data = merge$DISPONIBLES, start = 0, end = 216, frequency = 1/5)
s1 <- ts(data = merge$DISPONIBLES, start = 0, end = 216, frequency = 1/216)
plot(s1)
s1 <- ts(data = merge$DISPONIBLES, start = 0, end = 216*5, frequency = 5)
plot(s1)
s1 <- ts(data = merge$DISPONIBLES, start = 0, end = 216*5, frequency = 1/5)
plot(s1)
s1 <- ts(data = merge$DISPONIBLES, start = 0, end = 216/5, frequency = 1/5)
merge
install.packages("zoo")
na.locf(merge)
library(zoo)
na.locf(merge)
merge <- na.locf(merge)
s1 <- ts(data = merge$DISPONIBLES, start = 0, end = 216, frequency = 1)
plot(s1)
datas <- dbGetQuery(conn = con, statement = "select s.INTERVALLE + 288*s.JOUR as INTERVALLE, avg(s.DISPONIBLES) as DISPONIBLES  from station s where s.ID_STATION = '3003' and s.JOUR in (21,22) and s.MOIS = 2 group by s.INTERVALLE order by s.ID;")
s3003 <- data.frame(datas)
inte <- data.frame(0:max(s3003$INTERVALLE))
merge(inte, s3003, by.x=names(inte)[1], by.y="INTERVALLE", all.x = TRUE)
inte
s3003
inte <- data.frame(min(s3003$INTERVALLE):max(s3003$INTERVALLE))
merge(inte, s3003, by.x=names(inte)[1], by.y="INTERVALLE", all.x = TRUE)
merge <- na.locf(merge)
s1 <- ts(data = merge$DISPONIBLES, start = min(merge$X0.max.s3003.INTERVALLE.), end = max(merge$X0.max.s3003.INTERVALLE.), frequency = 1)
plot(s1)
merge
datas$
datas
s3003
datas <- dbGetQuery(conn = con, statement = "select s.INTERVALLE + 288*s.JOUR as INTERVALLE, avg(s.DISPONIBLES) as DISPONIBLES  from station s where s.ID_STATION = '3003' and s.JOUR in (21,22) and s.MOIS = 2 group by s.JOUR, s.INTERVALLE order by s.ID;")
s3003 <- data.frame(datas)
inte <- data.frame(min(s3003$INTERVALLE):max(s3003$INTERVALLE))
merge <- merge(inte, s3003, by.x=names(inte)[1], by.y="INTERVALLE", all.x = TRUE)
merge
merge <- na.locf(merge)
s1 <- ts(data = merge$DISPONIBLES, start = min(merge$X0.max.s3003.INTERVALLE.), end = max(merge$X0.max.s3003.INTERVALLE.), frequency = 1)
s1 <- ts(data = merge$DISPONIBLES, start = min(merge$min.s3003.INTERVALLE..max.s3003.INTERVALLE.), end = max(merge$min.s3003.INTERVALLE..max.s3003.INTERVALLE.), frequency = 1)
plot(s1)
savehistory("C:/projet/R/TS1.R")
library(RMySQL)
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
datas <- dbGetQuery(conn = con, statement = "select V_DELAY.DT, V_MVT.MVT ,V_DELAY.DELAY FROM (select DATE_FORMAT(s.DATE, '%d-%m-%Y') as DT, sum(abs(s.MOUVEMENT)) as MVT from station s group by DATE_FORMAT(s.DATE, '%d-%m-%Y')) V_MVT inner join (select DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y') as DT, avg(d.DELAY) as DELAY from delais d group by DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y')) V_DELAY on V_DELAY.DT = V_MVT.DT")
summary(datas)
days <- data.frame(datas, row.names = 'DT')
res.pca <- PCA(days, scale.unit = TRUE, graph = TRUE)
library(FactoMineR)
res.pca <- PCA(days, scale.unit = TRUE, graph = TRUE)
res.hcpc=HCPC(res.pca, graph = TRUE)
datas <- dbGetQuery(conn = con, statement = "select V_DELAY.DT, V_MVT.MVT ,V_DELAY.DELAY FROM (select DATE_FORMAT(s.DATE, '%d-%m-%Y') as DT, sum(abs(s.MOUVEMENT)) as MVT from station s where s.JOUR <> 18 group by DATE_FORMAT(s.DATE, '%d-%m-%Y')) V_MVT inner join (select DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y') as DT, avg(d.DELAY) as DELAY from delais d group by DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y')) V_DELAY on V_DELAY.DT = V_MVT.DT")
days <- data.frame(datas, row.names = 'DT')
res.pca <- PCA(days, scale.unit = TRUE, graph = TRUE)
res.hcpc=HCPC(res.pca, graph = TRUE)
datas <- dbGetQuery(conn = con, statement = "select V_DELAY.DT, V_MVT.MVT ,V_DELAY.DELAY FROM (select DATE_FORMAT(s.DATE, '%d-%m-%Y') as DT, sum(abs(s.MOUVEMENT)) as MVT from station s where s.JOUR <> 18 and s.JOUR <> 4 group by DATE_FORMAT(s.DATE, '%d-%m-%Y')) V_MVT inner join (select DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y') as DT, avg(d.DELAY) as DELAY from delais d group by DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y')) V_DELAY on V_DELAY.DT = V_MVT.DT")
days <- data.frame(datas, row.names = 'DT')
res.pca <- PCA(days, scale.unit = TRUE, graph = TRUE)
datas <- dbGetQuery(conn = con, statement = "select V_DELAY.DT, V_MVT.MVT ,V_DELAY.DELAY FROM (select DATE_FORMAT(s.DATE, '%d-%m-%Y') as DT, sum(abs(s.MOUVEMENT)) as MVT from station s where s.JOUR <> 18 or s.JOUR <> 4 group by DATE_FORMAT(s.DATE, '%d-%m-%Y')) V_MVT inner join (select DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y') as DT, avg(d.DELAY) as DELAY from delais d group by DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y')) V_DELAY on V_DELAY.DT = V_MVT.DT")
days <- data.frame(datas, row.names = 'DT')
res.pca <- PCA(days, scale.unit = TRUE, graph = TRUE)
datas <- dbGetQuery(conn = con, statement = "select V_DELAY.DT, V_MVT.MVT ,V_DELAY.DELAY FROM (select DATE_FORMAT(s.DATE, '%d-%m-%Y') as DT, sum(abs(s.MOUVEMENT)) as MVT from station s where s.JOUR <> 18 and s.JOUR <> 7 group by DATE_FORMAT(s.DATE, '%d-%m-%Y')) V_MVT inner join (select DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y') as DT, avg(d.DELAY) as DELAY from delais d group by DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y')) V_DELAY on V_DELAY.DT = V_MVT.DT")
days <- data.frame(datas, row.names = 'DT')
res.pca <- PCA(days, scale.unit = TRUE, graph = TRUE)
res.hcpc=HCPC(res.pca, graph = TRUE)
datas <- dbGetQuery(conn = con, statement = "select V_DELAY.DT, V_MVT.MVT ,V_DELAY.DELAY FROM (select DATE_FORMAT(s.DATE, '%d-%m-%Y') as DT, sum(abs(s.MOUVEMENT)) as MVT from station s where s.JOUR <> 18 and s.JOUR <> 7 group by DATE_FORMAT(s.DATE, '%d-%m-%Y')) V_MVT inner join (select DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y') as DT, sum(d.DELAY)/1000/24 as DELAY from delais d group by DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y')) V_DELAY on V_DELAY.DT = V_MVT.DT")
days <- data.frame(datas, row.names = 'DT')
res.pca <- PCA(days, scale.unit = TRUE, graph = TRUE)
res.hcpc=HCPC(res.pca, graph = TRUE)
datas
pred<-predict(res.hcpc, newdata=matrix(c('07-06-2016', 61912, 164752.3333),nrow=1))
pred<-predict(res.hcpc$data.clust, newdata=matrix(c('07-06-2016', 61912, 164752.3333),nrow=1))
pred<-predict(res.pca, newdata=matrix(c('07-06-2016', 61912, 164752.3333),nrow=1))
pred<-predict.PCA(res.pca, newdata=matrix(c('07-06-2016', 61912, 164752.3333),nrow=1))
pred<-predict.PCA(res.pca, newdata=matrix(c('07-06-2016', 61912, 164752.3333)))
pred<-predict(res.pca, newdata=matrix(c('07-06-2016', 61912, 164752.3333)))
pred<-predict(res.pca, datas[1,]))
pred<-predict(res.pca, datas[1,])
days[1,]
pred<-predict(res.pca, days[1,])
predict(res.pca, days[1,])
help("FactoMineR")
install.packages("FactoMineR")
help("FactoMineR")
library(FactoMineR)
help("FactoMineR")
install.packages("FactoMineR")
install.packages("FactoMineR")
install.packages("FactoMineR")
library(FactoMineR)
help("FactoMineR")
library(RMySQL)
datas <- dbGetQuery(conn = con, statement = "select V_DELAY.DT, V_MVT.MVT ,V_DELAY.DELAY FROM (select DATE_FORMAT(s.DATE, '%d-%m-%Y') as DT, sum(abs(s.MOUVEMENT)) as MVT from station s where s.JOUR <> 18 and s.JOUR <> 7 group by DATE_FORMAT(s.DATE, '%d-%m-%Y')) V_MVT inner join (select DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y') as DT, sum(d.DELAY)/1000/24 as DELAY from delais d group by DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y')) V_DELAY on V_DELAY.DT = V_MVT.DT")
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
datas <- dbGetQuery(conn = con, statement = "select V_DELAY.DT, V_MVT.MVT ,V_DELAY.DELAY FROM (select DATE_FORMAT(s.DATE, '%d-%m-%Y') as DT, sum(abs(s.MOUVEMENT)) as MVT from station s where s.JOUR <> 18 and s.JOUR <> 7 group by DATE_FORMAT(s.DATE, '%d-%m-%Y')) V_MVT inner join (select DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y') as DT, sum(d.DELAY)/1000/24 as DELAY from delais d group by DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y')) V_DELAY on V_DELAY.DT = V_MVT.DT")
days <- data.frame(datas, row.names = 'DT')
res.pca <- PCA(days, scale.unit = TRUE, graph = TRUE)
res.hcpc=HCPC(res.pca, graph = TRUE)
predict(res.pca, days[1,])
install.packages("C:/Users/LKBN0116/Downloads/FactoMineR_1.33.tar.gz", repos = NULL, type = "source")
library(FactoMineR)
help("FactoMineR")
library(FactoMineR)
help("FactoMineR")
library(RMySQL)
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
datas <- dbGetQuery(conn = con, statement = "select V_DELAY.DT, V_MVT.MVT ,V_DELAY.DELAY FROM (select DATE_FORMAT(s.DATE, '%d-%m-%Y') as DT, sum(abs(s.MOUVEMENT)) as MVT from station s where s.JOUR <> 18 and s.JOUR <> 7 group by DATE_FORMAT(s.DATE, '%d-%m-%Y')) V_MVT inner join (select DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y') as DT, sum(d.DELAY)/1000/24 as DELAY from delais d group by DATE_FORMAT(FROM_UNIXTIME(d.ID/1000), '%d-%m-%Y')) V_DELAY on V_DELAY.DT = V_MVT.DT")
days <- data.frame(datas, row.names = 'DT')
res.pca <- PCA(days, scale.unit = TRUE, graph = TRUE)
predict(res.pca, days[1,])
res.hcpc=HCPC(res.pca, graph = TRUE)
predict(res.hcpc, days[1,])
library(RMySQL)
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
datas <- dbGetQuery(conn = con, statement = "select s.INTERVALLE + 288*s.JOUR as INTERVALLE, avg(s.DISPONIBLES) as DISPONIBLES  from station s where s.ID_STATION = '3003' and s.JOUR in (21,22) and s.MOIS = 2 group by s.JOUR, s.INTERVALLE order by s.ID;")
s3003 <- data.frame(datas)
inte <- data.frame(min(s3003$INTERVALLE):max(s3003$INTERVALLE))
merge <- merge(inte, s3003, by.x=names(inte)[1], by.y="INTERVALLE", all.x = TRUE)
merge <- na.locf(merge)
library(zoo)
merge <- na.locf(merge)
s1 <- ts(data = merge$DISPONIBLES, start = min(merge$min.s3003.INTERVALLE..max.s3003.INTERVALLE.), end = max(merge$min.s3003.INTERVALLE..max.s3003.INTERVALLE.), frequency = 1)
plot(s1)
library(forecast)
fit <- auto.arima(s1)
fcast <- forecast(fit)
plot(fcast)
grid()
fcast
fit<-arima(s1, order=c(2,0,2))
predict <- predict(fit, n.ahead = 10)
predict
fcast <- forecast(fit)
plot(fcast)
predict <- predict(fit, n.ahead = 100)
predict
fcast
help(forecast)
fcast <- forecast(fit, h = 20)
plot(fcast)
fcast <- forecast(fit, h = 200)
plot(fcast)
fit<-arima(s1, order=c(0,0,1))
fcast <- forecast(fit, h = 50)
plot(fcast)
fit<-arima(s1, order=c(0,0,2))
fcast <- forecast(fit, h = 50)
plot(fcast)
fit<-arima(s1, order=c(2,1,2))
fcast <- forecast(fit, h = 50)
plot(fcast)
fit<-arima(s1, order=c(2,0,2))
fcast <- forecast(fit, h = 10)
plot(fcast)
fcast
setwd("C:/projet/velov/scripts")
saveHistory(file = "TS_arima.R")
savehistory(file = "TS_arima.R")

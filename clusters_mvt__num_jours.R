library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);

#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')

#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select s.NUMERO_JOUR, avg(IF(s.MOUVEMENT>=0, IF(s.MOUVEMENT=0, 1, s.MOUVEMENT), 0)) as ARRIVEES, avg(IF(s.MOUVEMENT<=0, IF(s.MOUVEMENT=0, -1, s.MOUVEMENT), 0)) as DEPARTS, avg(d.DELAY) as DELAY from delais d inner join station s on s.ID = d.ID and s.ID_STATION = d.ID_STATION group by s.NUMERO_JOUR;")

#Création du data frame
jours <- data.frame(datas, row.names = 'NUMERO_JOUR')

#ACP
res.pca <- PCA(jours, scale.unit = TRUE, graph = TRUE)

res.hcpc=HCPC(res.pca, graph = TRUE)
library(RMySQL)
library(FactoMineR)
library(ggmap)
library(leaflet);

#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')

#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select d.TYPE_JOUR as TYPE_JOUR, sum(abs(d.MOUVEMENT)) as MOUVEMENT, sum(d.FULL) as FULL from station d group by d.TYPE_JOUR;")

#Création du data frame
stations <- data.frame(datas, row.names = 'TYPE_JOUR')

#ACP
res.pca <- PCA(stations, scale.unit = TRUE, graph = TRUE)

#Cluster
res.hcpc=HCPC(res.pca, graph = TRUE)

#Il y a bien une distinction entre les jours "normaux", les week-end et les congés
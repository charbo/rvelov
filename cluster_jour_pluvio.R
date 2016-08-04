library(RMySQL)
library(FactoMineR)

#Connection BDD
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')

#RQT
#Nb mouvements par nombre de places dispos
datas <- dbGetQuery(conn = con, statement = "select DATE_FORMAT(sh.DATE, '%Y-%m-%d') as DATE, sum(abs(sh.MOUVEMENT)) as MOUVEMENT, sum(sh.FULL) as FULL, min(sh.TYPE_JOUR) as TYPE_JOUR, sum(ph.PLUVIO) as PLUVIO from STATION_HORAIRE sh inner join PLUVIO_HORAIRE ph on ph.ANNEE = sh.ANNEE and ph.MOIS = sh.MOIS and ph.JOUR = sh.JOUR and ph.HEURE = sh.HEURE group by DATE_FORMAT(sh.DATE, '%Y-%m-%d');")

#Création du data frame
dates <- data.frame(datas, row.names = 'DATE')

#ACP
res.pca <- PCA(dates, scale.unit = TRUE, graph = TRUE)

#Cluster
res.hcpc=HCPC(res.pca, graph = TRUE)
heures <- dbGetQuery(conn = con, statement = "select s.HEURE, sum(abs(s.MOUVEMENT)), sum(s.FULL) from STATION s group by s.HEURE;")
#Création du data frame
d_heures <- data.frame(heures, row.names = 'HEURE')

#ACP
res.pca <- PCA(d_heures, scale.unit = TRUE, graph = TRUE)

#Cluster
res.hcpc=HCPC(res.pca, graph = TRUE)


heures <- dbGetQuery(conn = con, statement = "select V_MVT.HEURE as HEURE, V_MVT.MOUVEMENT as MOUVEMENT, V_DELAY.DELAY as DELAY  from (select s.HEURE, sum(abs(s.MOUVEMENT)) as MOUVEMENT from STATION s group by s.HEURE) V_MVT inner join (select FROM_UNIXTIME(d.ID/1000,'%H') as HEURE, avg(d.DELAY) as DELAY from DELAIS d group by FROM_UNIXTIME(d.ID/1000,'%H')) V_DELAY on V_DELAY.HEURE = V_MVT.HEURE;")
#Création du data frame
d_heures <- data.frame(heures, row.names = 'HEURE')

#ACP
res.pca <- PCA(d_heures, scale.unit = TRUE, graph = TRUE)

#Cluster
res.hcpc=HCPC(res.pca, graph = TRUE)

---- BOF
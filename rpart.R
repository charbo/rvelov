library(psych)
library(caret)
library(rattle)
datas <- dbGetQuery(conn = con, statement = "SELECT * FROM STATION s where s.ID_STATION in('6028', '3087', '3083');")
datas$MIN <- datas$HEURE*60 + datas$MINUTE
studies <- datas[c("ID_STATION", "MIN", "JOUR", "MOIS", "TOTALES", "FULL")]
studies$test <- ifelse(studies$FULL == 1, "A", "B")
samples <- studies[c("ID_STATION", "MIN", "JOUR", "MOIS", "TOTALES", "test")]
inTrain <- createDataPartition(y = samples$test, p=0.8, list=FALSE)
training <- studies[inTrain,]
testing <- studies[-inTrain,]
modFit <- train(test ~ ., method="rpart", data=samples)
fancyRpartPlot(modFit$finalModel)
predict(modFit, newdata=testing)
df <- data.frame(c("3087"), 780, 1, 1, 20)
names(df) <- c("ID_STATION", "MIN", "JOUR", "MOIS", "TOTALES")
predict(modFit, newdata=df)
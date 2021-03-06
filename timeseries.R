library(forecast)
library(RMySQL)
library(zoo)
con <- dbConnect(MySQL(), user = 'root', password = 'root', host = 'localhost', dbname='test')
datas <- dbGetQuery(conn = con, statement = "select s.INTERVALLE + min(s.ID) - 1459199200000 as INTERVALLE, avg(s.DISPONIBLES) as DISPONIBLES  from station s where s.ID_STATION = '3003' and s.DATE between '2016-03-28' and '2016-04-07' group by s.JOUR, s.INTERVALLE order by s.ID;")
s3003 <- data.frame(datas)
inte <- data.frame(min(s3003$INTERVALLE):max(s3003$INTERVALLE))
merge <- merge(inte, s3003, by.x=names(inte)[1], by.y="INTERVALLE", all.x = TRUE)
merge <- na.locf(merge)
s1 <- ts(data = merge$DISPONIBLES, start = min(merge$min.s3003.INTERVALLE..max.s3003.INTERVALLE.), end = max(merge$min.s3003.INTERVALLE..max.s3003.INTERVALLE.), frequency = 1)
plot(s1)
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

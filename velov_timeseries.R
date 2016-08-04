library("RMySQL")
library("zoo")
library("forecast")


forecastAll <- function(startDate, endDate) {
  sql <- paste("SELECT s.ID_STATION, IF(s.INTERVALLE < @a, @b:=@b+1, @b) as VALUE_B,  s.INTERVALLE + 288 * @b AS INTERVALLE, MIN(s.DATE) AS DATE, AVG(s.DISPONIBLES) AS DISPONIBLES, @a := s.INTERVALLE as previous FROM station s, (select @a:= 1, @b:=0) as init WHERE s.DATE BETWEEN '",startDate,"' AND '",endDate,"' GROUP BY s.ID_STATION, s.JOUR , s.INTERVALLE ORDER BY s.ID;", sep="")
  datas <- dbGetQuery(conn = con, statement = sql)
  df <- data.frame(datas)
  inte <- seq(from = 0, to = 1728)
  inte <- data.frame(inte)
  fore <- do.call(rbind, lapply(unique(df$ID_STATION), function(station) forecastOne(station, df[df$ID_STATION == station, ], inte)))

}

forecastOne <- function(idStation, datasStation, inte) {
  merge <- merge(inte, datasStation, by.x=names(inte)[1], by.y="INTERVALLE", all.x = TRUE)
  merge <- na.locf(merge)
  s1 <- ts(data = as.numeric(merge$DISPONIBLES), start = as.numeric(min(merge$inte)), end = as.numeric(max(merge$inte)), frequency = 1)
  fit <- auto.arima(s1)
  fcast <- forecast(fit, h=3)
  res <- data.frame(station = idStation, mean1 = fcast$mean[1], mean2 = fcast$mean[2], mean3 = fcast$mean[3])
  res
}

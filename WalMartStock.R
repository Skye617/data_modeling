library(readxl)

walmart<-read_excel("WalMartStock.xls")

fit <-Arima(walmart$Close, order=c(1,0,0))

fit2 <-Arima(diff(walmart$Close, 1), order=c(1,0,0))

summary(fit)

summary(fit2)

Acf(walmart$Close, lag.max = 12, main = 'fit')

Acf(diff(walmart$Close), lag.max = 12, main = 'fit2')

#######################################################

sales.df <- read_excel("SouvenirSales.xls")

sales.ts <- ts(sales.df$Sales, start = c(1995, 1), end = c(2001, 12), freq = 12)

nValid <- 12

nTrain <- length(sales.ts) - nValid

train.ts <- window(sales.ts, start = c(1995, 1), end = c(1995, nTrain))

valid.ts <- window(sales.ts, start = c(1995, nTrain + 1), end = c(1995, nTrain + nValid))

# 1
train.lm.B<- tslm(train.ts ~ trend + season, lambda = 0)
train.lm.B.pred <- forecast(train.lm.B, h = nValid, level = 0)
accuracy(train.lm.B.pred$mean, valid.ts)

# 2
train.arima <-auto.arima(train.ts)
train.arima.pred <- forecast(train.arima, h = nValid, level = 0)
accuracy(train.arima.pred$mean, valid.ts)

# 3
train.arima.log <-auto.arima(train.ts, lambda=0)
train.arima.log.pred <- forecast(train.arima.log, h = nValid, level = 0)
accuracy(train.arima.log.pred$mean, valid.ts)

# 4
lm.B<- tslm(sales.ts ~ trend + season, lambda = 0)
lm.B.pred <- forecast(lm.B, h = nValid, level = 0)

# 5
arima.log <- auto.arima(sales.ts, lambda = 0)
arima.log.pred <- forecast(arima.log, h = nValid, level = 0)
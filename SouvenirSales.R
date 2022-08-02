# set up
library(readxl)
hw2 <- read_excel("SouvenirSales.xls")
# set as a ts data
hw.ts <- ts(hw2$Sales, start = c(1991, 1), end = c(2001, 12), freq = 12)
# partition data
ahead <- 12
ntrain <- length(hw.ts) - ahead
train.ts <- window(hw.ts, start = c(1995, 1), end = c(1995, ntrain))
valid.ts <- window(hw.ts, start = c(1995, ntrain+1), end = c(1995, ntrain + ahead))


ridership.ts <- ts(hw2$Sales, start = c(1995, 1), end = c(2001, 12), freq = 12)
nValid <- 12
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1995, 1), end = c(1995, nTrain))
valid.ts <- window(ridership.ts, start = c(1995, nTrain + 1), end = c(1995, nTrain + nValid))

#library(forecast)
seasonal<-snaive(train.ts, h=frequency(train.ts))
seasonal_fc = forecast(seasonal)
accuracy(seasonal_fc$fitted, train.ts)
accuracy(seasonal_fc$mean, valid.ts)

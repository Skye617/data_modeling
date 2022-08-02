#########################################
################ SET UP #################
#########################################

# Read in data
library(readxl)
library(forecast) # for ma()
library(zoo) # for rollmean()

# Transfer to ts data
df <- read_excel("Amtrak data.xls")
ridership.ts <- ts(df$Ridership, start = c(1991,1), end =c(2004,3) , frequency = 12)

# Data Partition
nValid <- 36
nTrain <- length(ridership.ts) - nValid
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + nValid))

# Visualization: Plain 
plot(ridership.ts, xlab = "Year", ylab = "Ridership", ylim = c(1300, 2300))

# Visualization: Zoom In
ridership.zoom <- window(ridership.ts, start = c(1991,1), end = c(2000,12))
plot(ridership.zoom, xlab = "Year", ylab = "Ridership", ylim = c(1300, 2300))

# Visualization: Monthly Trendlines
library(forecast) 
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2)) 
plot(ridership.ts, xlab = "Year", ylab = "Ridership (in 000s)", ylim = c(1300, 2300))
lines(ridership.lm$fitted, lwd = 2)

# Visualization: Annual Trendlines
annual.ridership.ts <- aggregate(ridership.ts, FUN = mean)
plot(annual.ridership.ts, xlab = "Year", ylab = "Average Ridership",
     ylim = c(1300, 2300))

# Visualization: forecast
plot(ridership.ts)
lines(ridership.lm.pred$mean, col = "pink")
lines(naive.pred$mean, col = "blue")
lines(snaive.pred$mean, col = "red")

# Visualization: Differencing
plot(ridership.ts, 
     ylab = "Ridership", xlab = "Time", 
     bty = "l", xlim = c(1991, 2004.25), main = "Ridership")
plot(diff(ridership.ts, lag = 12), 
     ylab = "Lag-12", xlab = "Time", 
     bty = "l", xlim = c(1991,2004.25), main = "Lag-12 Difference")
plot(diff(ridership.ts, lag = 1), 
     ylab = "Lag-1", xlab = "Time", 
     bty = "l", xlim = c(1991,2004.25), main = "Lag-1 Difference")
plot(diff(diff(ridership.ts, lag = 12), lag = 1), 
     ylab = "Lag-12, then Lag-1", xlab = "Time", 
     bty = "l", xlim = c(1991,2004.25), main = "Twice-Differenced (Lag-12, Lag-1)")


#########################################
################ MODELS #################
#########################################

# Linear Trend
train.lm.linear.trend <- tslm(train.ts ~ trend, lambda = 1)
summary(train.lm.linear.trend)
train.lm.linear.trend.pred <- forecast(train.lm.linear.trend, h = nValid, level = 0)

# Exponential Trend 
train.lm.expo.trend <- tslm(train.ts ~ trend, lambda = 0)

# Polynomial/Quadratic trend
train.lm.poly.trend <- tslm(train.ts ~ trend + I(trend^2))

# Seasonality
train.lm.season <- tslm(train.ts ~ season)

# Polynomial trend + seasonality 
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# Naive
naive.pred <- naive(train.ts, h = nValid)
snaive.pred <- snaive(train.ts, h= nValid)

# Moving Averages
ma.centered <- ma(ridership.ts, 
                  order = 12)
ma.trailing <- rollmean(train.ts, 
                        k = 12, align = "right")
last.ma <- tail(ma.trailing, 1)
ma.trailing.pred <- ts(rep(last.ma, nValid), 
                       start = c(1991, nTrain + 1), 
                       end = c(1991, nTrain + nValid), 
                       freq = 12)

# Deseasonalized
ridership.deseasonalized <- diff(ridership.ts, lag = 12)
summary(tslm(ridership.deseasonalized ~ trend))

# ACF - ridership's autocorrelation
ridership.24.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, 24))
Acf(ridership.24.ts, lag.max = 12, main = "")
# ACF - errors' autocorrelation
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)
Acf(train.lm.trend.season$residuals, lag.max = 12, main = "")

# AR
train.lm.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season) 
train.lm.trend.season.pred <- forecast(train.lm.trend.season, h = nValid, level = 0)
train.res.arima <- Arima(train.lm.trend.season$residuals, order = c(1,0,0)) 
train.res.arima.pred <- forecast(train.res.arima, h = nValid) 
accuracy(train.lm.trend.season.pred$mean + train.res.arima.pred$mean, valid.ts)

plot(train.lm.trend.season.pred)
lines(valid.ts)
lines(train.lm.trend.season.pred$mean + train.res.arima.pred$mean, col = 'red')

# ARIMA
train.arima <- auto.arima(train.ts)
train.arima.pred <- forecast(train.arima, h = nValid)
lines(train.arima.pred$mean, col = 'green')

# Holt-winter
train.hw <- ets(train.ts, model = 'MAA')
train.hw.pred <- forecast(train.hw, h = nValid)
lines(train.hw.pred$mean, col = 'purple')

#########################################
############## Evaluation ###############
#########################################

# Manually
train.res <- ridership.lm.pred$residuals
valid.res <- valid.ts - ridership.lm.pred$mean
train.MAE <- mean(abs(train.res))
train.RMSE <- sqrt(mean(train.res^2))
train.MAPE <- mean(abs(train.res/train.ts)*100)
valid.MAE <- mean(abs(valid.res))
valid.RMSE <- sqrt(mean(valid.res^2))
valid.MAPE <- mean(abs(valid.res/valid.ts)*100)
data.frame(train.MAE, train.RMSE, train.MAPE, valid.MAE, valid.RMSE, valid.MAPE)
# Automatically
accuracy(ridership.lm.pred$mean, valid.ts)
accuracy(ridership.lm.pred$fitted, train.ts)
accuracy(ridership.lm.pred, ridership.ts)

# Error Distribution
hist(ridership.lm.pred$residuals,  ylab = "Frequency", xlab = "Forecast Error", main = "")
shapiro.test(ridership.lm.pred$residuals)

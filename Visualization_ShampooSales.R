#### Figure 3.1
library(readxl)
ShampooSales <- read_excel("ShampooSales.xls")
#Amtrak.df <- read.csv("Amtrak data.csv")
head(ShampooSales)

# use time series analysis
library(forecast)
ridership.ts <- ts(ShampooSales$`Shampoo Sales`, start = c(1995,1), end = c(1997,12), freq = 12)#Ridership data from Jan 1991 to March 2004
# 12 datapoints in a year
head(ridership.ts)
plot(ridership.ts, xlab = "Year", ylab = "Ridership (in 000s)", ylim = c(110, 700))

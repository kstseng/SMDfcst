load("rawData\\order_2010_201506.RData")
colnames(raw_data) <- c("orderNo", "orderDate", "firstDate", "cmtDate1st", "efftiveDate", "customerID",
                        "itemNo", "factEntity", "factZone", "pg", "edivision", "salesID", "sector", 
                        "Qty", "US.Amt")

rawData.correctDT <- raw_data[-which(raw_data$orderDate > raw_data$efftiveDate), ]
rawData.used <- rawData.correctDT[, c("orderNo", "orderDate", "efftiveDate", "customerID",
                                      "factEntity", "factZone", "pg", "edivision", "sector", 
                                      "Qty", "US.Amt")]

#max(rawData.used$orderDate)
monSeq <- strptime(seq(as.Date("2010/1/1"), by = "month", length.out = 67), "%Y-%m-%d")

usamt.month <- 0
for(i in 1:(length(monSeq) - 1)){
  print(i/(length(monSeq) - 1))
  monIndex <- which(rawData.used$orderDate >= monSeq[i] & rawData.used$orderDate < monSeq[i + 1])
  monOrder <- rawData.used[monIndex, ]
  smdOrder <- monOrder[monOrder$efftiveDate >= monSeq[i] & monOrder$efftiveDate < monSeq[i + 1], ]
  #rec[i] <- nrow(smdOrder)
  usamt.month[i] <- sum(smdOrder$US.Amt)
}

########################################################################
##
## model building
##
########################################################################
usamt.month.ts <- ts(usamt.month, frequency = 12, start = c(2010, 1))
plot.ts(usamt.month.ts)

########################################################################
##
## decomposing seasonal data
##
########################################################################
usamt.month.ts.component <- decompose(usamt.month.ts)
usamt.month.ts.component$seasonal
plot(usamt.month.ts.component)

########################################################################
##
## exponential smoothing forecast
##
########################################################################
usamt.month.ts.forecasts <- HoltWinters(usamt.month.ts, beta=FALSE, gamma=FALSE, l.start = 18177068, b.start = usamt.month.ts[2] - usamt.month.ts[1])
usamt.month.ts.forecasts <- HoltWinters(usamt.month.ts, beta=FALSE, l.start = 18177068, b.start = usamt.month.ts[2] - usamt.month.ts[1])
usamt.month.ts.forecasts <- HoltWinters(usamt.month.ts, l.start = 18177068, b.start = usamt.month.ts[2] - usamt.month.ts[1])
library(forecast)
usamt.month.ts.forecasts2 <- forecast.HoltWinters(usamt.month.ts.forecasts, h=8)
plot.forecast(usamt.month.ts.forecasts2)
acf(usamt.month.ts.forecasts2$residuals, lag.max=20)
Box.test(usamt.month.ts.forecasts2$residuals, lag=20, type="Ljung-Box")

plotForecastErrors <- function(forecasterrors){
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(usamt.month.ts.forecasts2$residuals)
plot.ts(usamt.month.ts.forecasts2$residuals) 

plot(usamt.month.ts.forecasts2)
plot(usamt.month.ts.forecasts)

########################################################################
##
## ARIMA model
##
########################################################################
usamt.month.ts.diff2 <- diff(usamt.month.ts, differences = 2)
plot.ts(usamt.month.ts.diff2)
# library(fUnitRoots) # for testing stationality
acf(usamt.month.ts.diff2, lag.max=20) # plot a correlogram
acf(usamt.month.ts.diff2, lag.max=20, plot=FALSE) # get the autocorrelation values
pacf(usamt.month.ts.diff2, lag.max=20) # plot a correlogram
pacf(usamt.month.ts.diff2, lag.max=20, plot=FALSE) # get the autocorrelation values
usamt.month.ts.arima <- arima(usamt.month.ts, order=c(0,1,1)) 
usamt.month.ts.arima.fcst <- forecast.Arima(usamt.month.ts.arima, h=5)
plot.forecast(usamt.month.ts.arima.fcst)
acf(usamt.month.ts.arima.fcst$residuals, lag.max=20)
Box.test(usamt.month.ts.arima.fcst$residuals, lag=20, type="Ljung-Box")
# auto arima
usamt.month.autoArima <- auto.arima(usamt.month.ts)
summary(usamt.month.autoArima)
plot(usamt.month.autoArima)
plot(forecast(usamt.month.autoArima))
plot(1:length(usamt.month.ts),fitted(usamt.month.autoArima), col="green", type = "l")
lines(1:length(usamt.month.ts), usamt.month.ts)

#
usamt.month.ts.ets <- ets(usamt.month.ts)
accuracy(usamt.month.ts.ets)
fcast <- forecast(usamt.month.ts.ets)
plot(fcast)

# neural network 
fit <- nnetar(usamt.month.ts)
plot(forecast(fit,h=30))
points(1:length(usamt.month.ts),fitted(fit),type="l",col="green")


########################################################################
##
## validation
##
########################################################################
# auto arima
usamt.month.ts.train <-  ts(usamt.month[1:54], frequency = 12, start = c(2010, 1))
usamt.month.autoArima.train <- auto.arima(usamt.month.ts.train)
summary(usamt.month.autoArima)
plot(forecast(usamt.month.autoArima.train, h = 15))
plot(1:length(usamt.month.ts),fitted(usamt.month.autoArima), col="green", type = "l")
lines(1:length(usamt.month.ts), usamt.month.ts)
#
fcst <- forecast(usamt.month.autoArima.train, h = 12)
plot(c(usamt.month[1:54], fcst$mean), type = "l", col = "red")
lines(usamt.month, col = "blue")

########################################################################
##
## Make prediction
##
########################################################################
usamt.08 <- c(usamt.month, MonthRevenue(rev201507, "2015/07/01", "2015/07/31"), MonthRevenue(rev201508, "2015/08/01", "2015/08/31"))
usamt.month.ts <-  ts(usamt.08, frequency = 12, start = c(2010, 1))
usamt.month.autoArima <- auto.arima(usamt.month.ts)
plot(forecast(usamt.month.autoArima, h = 12))
forecast(usamt.month.autoArima, h = 10)
# plot(1:length(usamt.month.ts),fitted(usamt.month.autoArima), col="green", type = "l")
# lines(1:length(usamt.month.ts), usamt.month.ts)
#
fcst <- forecast(usamt.month.autoArima, h = 1)
plot(c(usamt.month, fcst$mean), type = "l", col = "red")
lines(usamt.08, col = "blue")

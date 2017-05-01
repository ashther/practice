
library(magrittr)
library(forecast)
library(tseries)
library(TTR)

plotForecastErrors <- function(forecasterrors) {
  forecasterrors <- na.omit(as.numeric(forecasterrors))
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}

forecastErros <- function(forecasterrors) {
  require(forecast)
  old_par_mfrow <- par()$mfrow
  par(mfrow = c(3, 1))
  on.exit(par(mfrow = old_par_mfrow))
  
  # check if there is auto-correlation between predict errors
  # if yes, model can be improved with arima method
  print(Box.test(forecasterrors, lag = 20, type = 'Ljung-Box')) # use statistic
  Acf(forecasterrors, lag.max = 20) # use plot
  plot.ts(forecasterrors) # constant errors
  plotForecastErrors(forecasterrors) # check if mean = 0 and normal distribute
}

acfPlot <- function(ts) {
  require(forecast)
  old_par_mfrow <- par()$mfrow
  par(mfrow = c(1, 2))
  on.exit(par(mfrow = old_par_mfrow))
  
  Acf(ts, lag.max = 20)
  Pacf(ts, lag.max = 20)
}

# ========================== EDA ==============================================
# non-seasonal ts
kingsTs <- scan('http://robjhyndman.com/tsdldata/misc/kings.dat', skip = 3) %>% 
  ts()
plot.ts(kingsTs)
# decomposing non-seasonal ts
kingsTs %>% SMA(n = 8) %>% plot.ts() # simple moving average

# seasonal ts
birthsTs <- scan('http://robjhyndman.com/tsdldata/data/nybirths.dat') %>% 
  ts(frequency = 12, start = c(1946, 1))
plot.ts(birthsTs)
# decomposing seasonal ts
birthsTs %>% decompose() %>% plot()

# sasonal ts with changing trend
souvenirTs <- scan('http://robjhyndman.com/tsdldata/data/fancy.dat') %>% 
  ts(frequency = 12, start = c(1987, 1))
plot.ts(souvenirTs)
# convent to seasonal ts with contant trend
souvenirTsLog <- log(souvenirTs) 
plot.ts(souvenirTsLog)
souvenirTsLog %>% decompose() %>% plot()

# =================forecast with exponential smoothing ========================
# non-stationary ts with confirmed reason, and no correlation between predict errors
# 
# forecast with simple exponential smoothing(no trend, non-seasonal)
rain <- scan('http://robjhyndman.com/tsdldata/hurst/precip1.dat', skip = 1) %>% 
  ts(start = 1813)
plot.ts(rain)
rainEsModel <- HoltWinters(rain,
                           beta = FALSE, # no trend
                           gamma = FALSE, # non-seasonal
                           l.start = 23.56) 
plot(rainEsModel)
rainEsModel_2 <- forecast.HoltWinters(rainEsModel, h = 8, level = c(80, 95))
forecastErros(rainEsModel_2$residuals)

# forecast with holt's exponential smoothing(with incresing or decreasing trend
# and non-seasonal)
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip = 5) %>% 
  ts(start = 1866)
plot.ts(skirts)
skirtsHoltEsModel <- HoltWinters(skirts, 
                                 gamma = FALSE, 
                                 l.start = 608, 
                                 b.start = 9)
plot(skirtsHoltEsModel)
skirtsHoltEsModel_2 <- forecast.HoltWinters(skirtsHoltEsModel, h = 19)
plot(skirtsHoltEsModel_2)

forecastErros(skirtsHoltEsModel_2$residuals)

# forecast with holt-winter's exponetial smoothing(with increasing or decreasing
# trend and seasonal)
souvenirTsLogHWModel <- HoltWinters(souvenirTsLog)
plot(souvenirTsLogHWModel)
souvenirTsLogHWModel_2 <- forecast.HoltWinters(souvenirTsLogHWModel, h = 48)
plot(souvenirTsLogHWModel_2)

forecastErros(souvenirTsLogHWModel_2$residuals)

# =================forecast with ARIMA ========================
# non-stationary ts with uncertain reason, and correlation between predict errors
skirtsDiff <- diff(skirts, differences = 2)
kpss.test(skirtsDiff) # check if stationary ts
plot.ts(skirtsDiff)

# Acf(skirtsDiff, lag.max = 20)
# Pacf(skirtsDiff, lag.max = 20)
acfPlot(skirtsDiff)
auto.arima(skirts, trace = TRUE)
# ARIMA(2,2,2)                    : Inf
# ARIMA(0,2,0)                    : 393.6216
# ARIMA(1,2,0)                    : 391.6212
# ARIMA(0,2,1)                    : 392.0664
# ARIMA(2,2,0)                    : 393.9273
# ARIMA(1,2,1)                    : 393.9276
# ARIMA(2,2,1)                    : Inf
skirtsARIMA <- Arima(skirts, order = c(1, 2, 0))
skirtsForecast <- forecast.Arima(skirtsARIMA, h = 8)
forecastErros(skirtsForecast$residuals)

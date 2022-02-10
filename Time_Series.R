library(fpp2)
library(ggplot2)
library(tseries)
data_csv <- read.csv("C:/Project DBs/Statistics/eComm_US.csv")
time_series <- ts(data_csv$ECOMNSA,start = c(1999,4),frequency = 4)
autoplot(time_series,ts.colour='red')
autoplot(decompose(time_series))
start(time_series)
end(time_series)
frequency(time_series)
autoplot(decompose(time_series),ts.colour='red')
autoplot(time_series)+autolayer(ma(time_series,3))


#Exponential smoothing
ses.fit <- ses(time_series,h=3)
ses.fit
ses.fit$model #optimized for lowest AIC
round(accuracy(ses.fit),2)
                           
#holt
holt.fit <- holt(time_series,h=3)
summary(holt.fit)

holt.winters.fit <- hw(time_series,h=3)
summary(holt.winters.fit)

# Using ets() no trend no seasonality a for adiitive errors or m for multiplicative errors
ets.fit <- ets(time_series,model='ANN')
ets.fit <- ets(time_series,model='ZZZ') #Using all similar to best subsets

#arima
ndiffs(time_series)
dtime_series <- diff(time_series)
plot(dtime_series)
adf.test(dtime_series)
dtime_series2 <- diff(time_series,2)
plot(dtime_series2)
adf.test(dtime_series2)

acf(time_series)
pacf(time_series)
ggtsdisplay(time_series)


summary(auto.arima(time_series))
sarima.fit <- arima(time_series,c(1,1,0),seasonal=c(1,1,0))
summary(sarima.fit)
checkresiduals(sarima.fit)


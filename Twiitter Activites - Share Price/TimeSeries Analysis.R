library(xts)
library(tseries)
library(TSA)
library(forecast)
data1 <- read.zoo(file="C:\\Users\\Srikanth\\Desktop\\old\\Datamining\\MSFT.csv",header=TRUE,format="%m/%d/%Y",sep=",");
index(data1) = as.yearmon(index(data1))
msft = ts(data1$Close,start=start(data1), end=end(data1),frequency = 12)
time_series = msft
#plot the data, assuming time_series is a ts object
plot(time_series ,col="black", lwd=3)
plot(decompose(time_series))

#create the ARIMA models (see lecture notes)
arima_fit <- Arima(time_series, order= c(1,1,1), seasonal = c(0,0,0))
auto_arima_fit <- auto.arima(time_series, stepwise=FALSE, seasonal=FALSE, approximation=FALSE, D=1)

ets_fit <- ets(time_series)

#We can force a seasonal ets by explictly specifying the model type as shown below
#ets_fit <- ets(time_series, model="ZZM")

#plot the models. red is the hard-coded arima model, blue is the auto-fit arima model, and green is the exponential smoothing model
plot(arima_fit$x,col="black", lwd=3)
lines(fitted(arima_fit),col="red")
lines(fitted(auto_arima_fit),col="blue")
lines(fitted(ets_fit),col="green")

#create the acf plots to determine the level of differencing
acf(time_series)

#create the differenced time series to account for the seasonal pattern
diff_time_series <- diff(time_series, 4)

#create the acf and pacf plots to determine the order of the AR and MA terms

acf(diff_time_series)
pacf(diff_time_series)

#error analysis of the models
summary(arima_fit)

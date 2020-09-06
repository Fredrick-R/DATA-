####TIME SERIES ANALYSIS###

##LOAD DATASETS & LIBRARY/IES##
library(MASS)
library(tseries)
library(forecast)


###load datasets###
Airpass <- AirPassengers
print(Airpass)
View(Airpass)

###verify the class,structure,frequency,summary,head,tail,start,end & dim
class(AirPassengers)
str(AirPassengers)
dim(AirPassengers)
summary(AirPassengers)
frequency(AirPassengers)
head(AirPassengers)
tail(AirPassengers)
start(AirPassengers)
end(AirPassengers)
plot(AirPassengers)



####linearity of the datasets
reg. <- lm(AirPassengers ~ time(AirPassengers))
reg.

###abline: this shows the mean of the series
abline(reg.)

###obtain the cycle of the series
cycle(AirPassengers)

###plot the series via aggregrate
plot(aggregate(AirPassengers, FUN = mean))
?aggregate

###boxplot graph
boxplot(AirPassengers ~ cycle(AirPassengers))
plot(AirPassengers)

##take the ln(log) of the series##
plot(log(AirPassengers))

##acf & pacf graph
acf(log(AirPassengers))
pacf(log(AirPassengers))

##differencing the seires to remove stationarity###
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

###ARIMA MODELLING COMPARING THE DIFFERENT MODELS###
Arima(log(AirPassengers), c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period =
                                                        12))
Arima(log(AirPassengers), c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period =
                                                        12))

###autoarima model for best model###
auto.arima(AirPassengers)

###predicting the future values##
fit = Arima(log(AirPassengers), c(2, 1, 1), seasonal = list(order = c(2, 1, 1), period =
                                                              12))
pred = predict(fit, n.head = 10 * 12)
pred

###ADF TEST
adf.test(log(AirPassengers))
adf.test(diff(log(AirPassengers)))

###plot the graph###
ts.plot(AirPassengers, 2.718, pred$pred)
pred1 <- 2.718
pred1

forecast(Arima(log(AirPassengers), c(2, 1, 1), seasonal = list(
  order = c(2, 1, 1), period = 12
)))
Forecastplot <- forecast(Arima(log(AirPassengers)))
Forecastplot

####box.test for the squred value####
Box.test(log(AirPassengers))
forecast1 <- forecast(fit)
forecast1
plot(forecast1)

##plotting the forecast using the auto.arima model
auto.fore<-forecast(auto.arima(AirPassengers))
plot(auto.fore) ##best fitted forecast

###obtaining the accuracy of the model
accuracy(fit)

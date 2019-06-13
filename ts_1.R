#dublin weather temp data
data_p <- read.csv("C:/Users/PRATEEK PARASHER/Downloads/Compressed/web_scrap-master/repeat_exam_2019/csv_files/TS_1.csv")

#select value from excel file and copy the data in console 
#blank vector imported into R, this is just data not time stamp
data_p = scan()


#view vector data
plot.ts(data_p)
# now we have data now we converting into time-series,our data start from 1998
# end in 2018 , we select the min & max temp value from data 
# because this is monthy data our frequency is 12
data_p =runif(n=251 , min= -12 , max= 24)
datats = ts(data = data_p,
            c(start = 1998,1) , c(end = 2018,10) , frequency = 12)
#view time series data
plot(datats)

# This tells you that the data series is in a time series format
class(datats)
# This is the start of the time series
start(datats)
# This is the end of the time series
end(datats)
# The cycle of this time series is 12 months in a year
frequency(datats)
# use type = "additive" for additive components
deadd<- decompose(datats, type = "additive")
plot(deadd)
# use type = "multiplicative" for multiplicative components
demul<- decompose(datats, type = "multiplicative")
plot(demul)
#splits the time series into seasonality, trend and error components
seasonal_trend_error <- stl(datats, s.window = "periodic")
plot(seasonal_trend_error)

# Test if a time series is stationary

library(tseries)
# p-value < 0.05 indicates the TS is stationary

adf.test(datats)

# Computes the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test 
# for the null hypothesis that x is level or trend stationary

kpss.test(datats)

#autocorrelatioin plot

Acf(datats) #p 3

# partial autocorrelation plot

pacf(datats) #q 3

# Fitting an ARIMA model 
library(forecast)
fit <- Arima(datats, order = c(3, 0, 3))

fit

#Accuracy measures
accuracy(fit)


# qqnorm produces a normal QQ plot of the values in y. 
# qqline adds a line to a "theoretical", quantile-quantile plot 
# which passes through the probs quantiles, 
# by default the first and third quartiles

help("qqnorm")
qqnorm(fit$residuals)
qqline(fit$residuals)

#The Box.test() function tests that the autocorrelations are all zero
Box.test(fit$residuals, type = "Ljung-Box")

#making a forecast
forecast(fit, 12)

#
arimafore <- forecast(fit, h= 12)
#
plot(arimafore, xlim = c(2015,2022))


#AUTOMATED ARIMA FORECASTING

#
fit_1 <- auto.arima(datats)
fit_1


#
accuracy(fit_1)
qqnorm(fit_1$residuals)
qqline(fit_1$residuals)


#
forecast(fit, 12)
forecast(fit_1, 12)



#
arimafore_1 <- forecast(fit_1, h= 12)
#
plot(arimafore_1, xlim = c(2015,2022))


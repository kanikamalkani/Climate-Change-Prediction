

```{r}
#time series data
#climate change
library(readxl)
DailyDelhiClimateTrain <- read_excel("C:/Users/kanik/Downloads/DailyDelhiClimateTrain.xlsx",col_types = c("date", "numeric", "numeric","numeric", "numeric"))
View(DailyDelhiClimateTrain)
```


```{r}
#Temperature
dataset <- ts(DailyDelhiClimateTrain$meantemp, frequency = 365, start=c(2013,1))
dataset
#DailyDelhiClimateTrain <- head(DailyDelhiClimateTrain, - 1)   
dataset
plot.ts(dataset)
#log transform
datasetlog <- log(dataset)
plot.ts(datasetlog)
#decomposition
decompose1 <- decompose(dataset)
plot(decompose1)
#removing seasonality so only the trend and irregular component remain
decompose1adjusted <- dataset - decompose1$seasonal
plot(decompose1adjusted)
#forecasts
tempfor <- HoltWinters(dataset)
tempfor
plot(tempfor)
```



```{r}


library(tseries)
adf.test(humidity)
#since the data is stationary, we go for ARMA model
acf(dataset, lag.max=48)
pacf(dataset)
#auto.arima(dataset)




```
```{r}
#wind speed
wspeed <- ts(DailyDelhiClimateTrain$wind_speed,frequency = 365)
plot.ts(wspeed)
decomposedwspeed <- decompose(wspeed)
plot(decomposedwspeed)
#pressure
pressure <- ts(DailyDelhiClimateTrain$meanpressure,frequency = 365, start=c(2013,1))
plot.ts(pressure)
decomposedpressure <- decompose(pressure)
plot(decomposedpressure)

#removing seasonality so only the trend and irregular component remain
decomposedadjusted <- pressure - decomposedpressure$seasonal
plot(decomposedadjusted)

pressfor <- HoltWinters(pressure)
pressfor
plot(pressfor)

adf.test(pressure)
#since the data is stationary, we go for ARMA model
acf(pressure, lag.max=48)
pacf(pressure)
#auto.arima(pressure)
pressureMA <- arima(pressure,order=c(0,0,1))
pressureMA
```


```{r}
#Humidity
humidity <- ts(DailyDelhiClimateTrain$humidity, frequency = 365, start=c(2013,1))
humidity
plot.ts(humidity)
decomposedhumidity <- decompose(humidity)
plot(decomposedhumidity)
decompose1adjusted <- humidity - decomposedhumidity$seasonal
plot(decompose1adjusted)
#forecasts
humfor <- HoltWinters(humidity)
humfor
plot(humfor)
install.packages("forecast")
library(forecast)
#humfutfor <- hw(y=humidity, h=48)
library(tseries)
adf.test(humidity)
#since the data is stationary, we go for ARMA model
acf(humidity, lag.max=48)
pacf(humidity)
auto.arima(humidity)
```



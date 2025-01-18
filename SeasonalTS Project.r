---
title: "MA641 Project"
output:
  pdf_document: default
  word_document: default
---
ADIDAS AG is a multinational corporation, founded and head-quartered in Herzogenaurach, Germany, that designs and manufactures shoes, clothing and accessories. It is the largest sportswear manufacturer in Europe, and the second largest in the world, after Nike.
In the fiscal year of 2016, ADIDAS generated a total revenue of $19,068 million, increased by 18 percent. In its annual report, ADIDAS projects an annual growth in sale between 11 percent and 13 percent
For this paper, I used the past quarterly sales data of ADIDAS to forecast its future sales from Q2 of 2017 to Q4 of 2018, applying Time Series Theories to predict whether the goal of ADIDAS can be reached.



```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(fpp)
library(knitr)
library(vars)
library(forecastHybrid)
library(rmarkdown)
library(TSA)
library(CADFtest)
```

```{r message=FALSE, warning=FALSE}
adidas_revenue <- read.csv("C:/Users/manal/Desktop/adidas_revenue1.csv")

### rename data frame
df=adidas_revenue

### define data as time series
df<-ts(df, start=c(2000,1), end=c(2017,1), frequency = 4)

### extract Adidas revenue data
y=df[,2]
y
```
Methodology.
Exploratory Data Analysis: Here, we plot the time series plot, auto-correlation function(ACF) plot, partial auto-correlation function(PACF) plot, and EACF of the time series.
```{r message=TRUE, warning=FALSE, out.width="75%"}
plot(y,main="Adidas Revenue", xlab="year", ylab="$millions", lwd=3)
seasonplot(y,main="Seasonal plot:   revenue",year.labels = TRUE, 
           year.labels.left = TRUE,col=1:20, pch=19,lwd=2)
monthplot(y, main="Seasonal plot: Adidas revenue", xlab="quarter", ylab = "$million")
```

The quarterly sales of ADIDAS have strong growing trends throughout the years except for a visible decrease in the year of 2004. There are also some fluctuations every year which indicates a possible seasonal pattern that increases in size as the level of the series increase. The centered moving average plot removes the influence of seasonality and makes the increasing trend more obvious These pattern indicate that a good forecast of this series would need to capture both the trend and seasonality.

Looking at the two plots above, ADIDAS performs differently in different seasons. It is clear that there is a large jump in sales in January each year. The Seasonal graph also shows that there is a big gap between 2005 and 2006, meaning there was a huge increase between these two years.

```{r}
acf(y, lwd=5, main="Adidas QUarterly Revenue", lag.max = 100)
pacf(y, lwd=5, main="Adidas QUarterly Revenue", lag.max = 100)
eacf(y)
```
The ACF graph indicates a strong trend component of the dataset. Although the tend pattern is getting weak as the number of lags increases, it is still significant when the lag is as large as 20. Additionally, r4, r8, r12 and r16 is slightly higher than their neighbors. This is because the seasonal pattern of the data: the peaks tend to be four quarters apart (which is the Q3 of each year)
Stationarity: Here, we test for stationarity of the TS by Augmented Dicky Fuller test. If the TS is
stationary we proceed to the next step, otherwise if the TS is non-stationary, we apply differencing,
transforming, and detrending to remove stationarity from the TS and then use Augmented Dicky Fuller test to test for stationarity again.
```{r message=TRUE, warning=FALSE, out.width="75%"}
plot(stl(y, s.window = "periodic"))
trend = stl(y, s.window = "periodic")$time.series[,2]
detrend_ts = y - (trend - trend[1])
plot(detrend_ts)
train<-window(detrend_ts,start=c(2000,1), end=c(2013,4))
test<-window(detrend_ts,start=c(2014,1))
adf.test(train)
plot(train)
adf.test(diff(log(train)))
plot(diff(log(train)))
auto.arima(log(train))
acf(diff(log(train)), lag.max=100)
pacf(diff(log(train)), lag.max=100)
eacf(log(train))

```
```{r message=TRUE, warning=FALSE, out.width="75%"}
qtr_forecast <- Arima(log(train), order=c(1,0,0), 
                      seasonal=list(order=c(1,1,0), period=4), method="CSS-ML")
summary(qtr_forecast)
qtr_forecast <- Arima(log(train), order=c(2,0,0), 
                      seasonal=list(order=c(2,1,0), period=4), method="CSS-ML")
summary(qtr_forecast)
qtr_forecast <- Arima(log(train), order=c(1,0,2), 
                      seasonal=list(order=c(1,1,2), period=4), method="CSS-ML")
summary(qtr_forecast)
qtr_forecast <- Arima(log(train), order=c(2,0,2), 
                      seasonal=list(order=c(2,1,2), period=4), method="CSS-ML")
summary(qtr_forecast)
```
Here, we find the models by checking the number of bars in ACF plot to find theMA(q) and number of bars in PACF to find the AR(p) and then cross check it with EACF to findARMA(p,q) model.
Parameter Estimation : we compare multiple models based on the AIC and BIC scores, log
likelihood values, Adjusted Pearson Goodness-of-Fit Test, Ljung-Box Test, ARCH LM Tests etc. and
select the best model from them.
On comparing the plot of train and diff(log(train)) we can see that the latter plot is centered around 0 with +-0.4 variance. Hence we are choosing this transformation.
```{r message=TRUE, warning=FALSE, out.width="75%"}
qtr_forecast <- Arima(log(train), order=c(2,0,2), 
                      seasonal=list(order=c(2,1,2), period=4), method="CSS-ML")
plot(residuals(qtr_forecast))
abline(h=0)
hist(residuals(qtr_forecast))
acf(residuals(qtr_forecast), lag.max=100)
qqnorm(residuals(qtr_forecast));qqline(residuals(qtr_forecast))
shapiro.test(residuals(qtr_forecast))

Box.test(residuals(qtr_forecast), type='Ljung')
```

According to shapiro-wilk test p value is less than 0.05 hence it is normal, but qq plot says that it has alot of outliers.
Here, we use ARIMA models here which are selected by taking difference
of log of TS and then using this new TS to find the best model based on ACF, PACF and EACF plots.
Residual Analysis : Here, we analyse the residuals of the bets model by making ACF plot, Histogram,
QQ plot, Residual plot and check for Shapiro-Wilk test and Ljung-Box test.
```{r message=TRUE, warning=FALSE, out.width="75%"}
library(ggplot2)
qtr_forecast <- Arima(log(train), order=c(2,0,2), 
                      seasonal=list(order=c(2,1,2), period=4), method="CSS-ML")
summary(qtr_forecast)
qtr_forecast.new <- forecast(qtr_forecast, h=20, level=c(90,95))
autoplot(qtr_forecast.new,) + autolayer(log(test))
summary(qtr_forecast.new)
```


```{r}
reg <- tslm(train ~ trend)
fit.tslm=forecast(reg, h=h,level=c(80,95))
summary(fit.tslm)
plot(fit.tslm, ylab="Adidas Quarterly Sales",
     xlab="t")
lines(fitted(reg),col="blue")
lines(y)

tps <- tslm(train ~ trend + season)
fit.lmts = forecast(tps, h=h)
plot(fit.lmts)
lines(y)
```
Based on Linear Trend Model, the forecasted quarterly sales = 1040.73 + 47.06t. It forms a straight line which perfectly catches the increasing trend pattern. By adding seasonality to the model, the forecasting looks even batter in the plot as it is almost overlapping with the test data.

```{r}
fit.stl <- forecast(y.stl, method="rwdrift", h=h)
summary(fit.stl)
plot(fit.stl)
lines(y)
````
The model uses STL and random work drift to forecast.Forecasts of STL objects are obtained by applying a non-seasonal forecasting method to the seasonally adjusted data and re-seasonalizing using the last year of the seasonal component.The plot shows that the forecast follows the seasonal component pretty well but does not include trend component.
Prediction : Here, we use the best model to forecast the revenue of Adidas
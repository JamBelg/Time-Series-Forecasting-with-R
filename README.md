# Time-Series-Forcasting-with-R
with Kaggle data

## About me
#### [Jamel Belgacem](https://www.linkedin.com/in/jamel-belgacem-289606a7/)
#### Application Engineer, R&Python developper
I have been coding with R and Python since 2015.


## Introduction
In this tutorial, we will talk about time series forecasting. As mentioned on the title, I will use [R](https://www.rstudio.com/) (or Posit from October 2022).
Time series forecasting is a prediction technic for data involving time component. The idea is to use historical data to predict the future.
I will use the data of Store Sales-Time Series Forecasting in Kaggle competition. It is about sales in Ecuador between 2013 and 2017. You can download data from [Kaggle](https://www.kaggle.com/competitions/store-sales-time-series-forecasting/overview) or directly from [my github](https://github.com/JamBelg/Time-Series-Forcasting-with-R).
Time series forecasting can be applied in various domains such as stock/price prediction or weather prediction.

I will use these libraries in this tutorial:
```
library(dplyr)
library(ggplot2)
library(forecast)
library(stats)
library(TTR)
library(tseries)
library(astsa)
library(MLmetrics)
library(zoo)
library(scales)
library(tidymodels)
library(modeltime)
library(timetk)   
library(tidyverse)
library(lubridate)
library(stringr)
```

## Data analysis

### Reading

```
df_train=read.csv(file="data/train.csv", 
                  header=TRUE, sep=",", 
                  colClasses=c("integer","Date","factor","factor","numeric","numeric"))

df_test=read.csv(file="data/test.csv", 
                 header=TRUE, sep=",",
                 colClasses=c("integer","Date","factor","factor","numeric"))

df_oil = read.csv(file="data/oil.csv", 
                  header=TRUE, sep=",",
                  colClasses=c("Date","numeric"))

df_holidays = read.csv(file="data/holidays_events.csv", 
                       header=TRUE, sep=",",
                       colClasses=c("Date","factor","factor","factor","character","factor"))

df_stores = read.csv(file="data/stores.csv",
                     header=TRUE, sep=",",
                     colClasses =c("factor","factor","factor","factor","integer"))

df_transactions = read.csv(file="data/transactions.csv", 
                           header=TRUE, sep=",",
                           colClasses=c("Date","factor","numeric"))
```

### Summary
Before jumping into model creation, it is always good to take a look into data.
```
summary(df_train)
str(df_train)
summary(df_oil)
str(df_oil)
summary(df_holidays)
str(df_holidays)
summary(df_stores)
str(df_stores)
summary(df_transactions)
str(df_transactions)
```
We have 43 missing values (NA) in df_oil:</br>
![image1](https://github.com/JamBelg/Time-Series-Forcasting-with-R/blob/main/pics/Oil_NA.jpg?raw=true)

There is a lot of technics to deal with missing value, one of them is simply delete them. In this tutorial I will take the last non NA value to replace the missing values.
```
df_oil$oil_NNA<-df_oil$dcoilwtico
df_oil[1,3]=df_oil[2,3]
for(i in 2:nrow(df_oil)){
  if(is.na(df_oil[i,3])){
    df_oil[i,3]=prev_val
  }else{
    prev_val=df_oil[i,3]
  }
}
```

Data are separated in 6 csv files, for better understanding let's join them into one table.
```
df_train <- left_join(x=df_train, y=df_stores, by="store_nbr")
df_train <- left_join(x=df_train, y=df_transactions, by=c("store_nbr","date"))
df_train <- left_join(x=df_train, y=df_holidays, by="date")
df_train <- left_join(x=df_train, y=df_oil, by="date")
head(df_train,n=20)
```

Daily sales plot
```
plot1<-df_train %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales)
  ) %>%
  ggplot(aes(x=date,y=daily_sales,groups=1))+geom_line()+geom_smooth()+
  labs(title="Sales",subtitle="Ecuador (2013-2017)")+
  xlab("Date")+ylab("Sales")
ggsave("pics/plot1.png")
```
![image1](https://github.com/JamBelg/Time-Series-Forcasting-with-R/blob/main/pics/plot1.png?raw=true)

### Correlation
#### Oil dependency
#### Holidays/events
#### Promotions

### Periodicity
Is our data variable seasonal. It is a very important aspect as our data involves time. So we have to control the variables variation in time to see any frequency. 
####

## Forecasting
There is a lot of time series forecasting models,
### ARIMA
ARIMA is ... of auto rgressive integrative moving average. It is a combination of a moving average and autoregressive model.


### SARIMA
S is for Seasonal, it is ARIMA model with seasonal option.


### Prophet


### TBATS
Trigonometric seasonality, Box-Cox transformation, ARMA errors, Trend and Seasonal components

### Seasonal Naive


### Elastic Net


### Random forest


### Prophet with boost


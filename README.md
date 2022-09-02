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
Oil price fluctuation have a big impact on economie, and Ecuador have a high dependency on oil.
```
plot_salesvsoil <-df_train %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales,na.rm=TRUE),
    daily_oil=mean(oil_NNA,na.rm=TRUE)
  ) %>%
  ggplot(aes(x=daily_oil,y=daily_sales))+geom_point()+geom_smooth()+
  ylim(c(300000,1200000))+
  labs(title="Impact of oil price",subtitle="Ecuador (2013-2017)")+
  xlab("Oil Price")+ylab("Daily sales")
ggsave("pics/plot_oil.png")
```
![image2](https://github.com/JamBelg/Time-Series-Forcasting-with-R/blob/main/pics/plot_oil.png?raw=true)
Sales volume is bigger with a small oil price.
#### Holidays/events
There is a national (approx. 14), regional and local holidays in Ecuador.
```
plot_holidays <-df_train %>%
  mutate(holidays_fact=ifelse(is.na(locale) | locale!="National","No","Yes")) %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales,na.rm=TRUE),
    holidays_fact=min(holidays_fact,na.rm=TRUE)
  )%>%
  mutate(holidays_fact=factor(holidays_fact,levels=c("No","Yes"))) %>%
  ggplot(aes(x=holidays_fact,y=daily_sales,fill=holidays_fact,group=holidays_fact))+geom_boxplot()+
  labs(title="Average sales",subtitle="Ecuador (2013-2017)")+xlab("Holidays ?")+ylab("Average daily sales")
ggsave("pics/plot_holidays.png")
```
![image3](https://github.com/JamBelg/Time-Series-Forcasting-with-R/blob/main/pics/plot_holidays.png?raw=true)
Between 2013 and 2017, there was more sales during holidays.
#### Promotions

### Periodicity
Is our data variable seasonal. It is a very important aspect as our data involves time. So we have to control the variables variation in time to see any frequency.
```
# Seasonal decomposition
max_date=max(df_train$date)
min_date=min(df_train$date)

dat_ts <- df_train %>%
  # Earth quake filter
  filter(!grepl("Terremoto", description, fixed = TRUE)) %>%
  
  select(date,sales) %>%
  group_by(date) %>%
  summarise(
    value=sum(sales,na.rm=TRUE)
  )

dat_ts <-ts(dat_ts$value,end=c(year(max_date), month(max_date)),
            start=c(year(min_date), month(min_date)),
            frequency = 30)

# Seasonal Decomposition Plot
png(file="pics/stl_plot.png",
    width = 960, height = 960, units = "px", pointsize = 20,
    bg="azure")
plot(stl(dat_ts,s.window = "periodic"), 
     main="Seasonal Decomposition of Time Series by Loess")
dev.off()
```
<img src="https://github.com/JamBelg/Time-Series-Forcasting-with-R/blob/main/pics/stl_plot.png?raw=true" width="700" height="700">

# Forecasting
There is a lot of time series forecasting models,
## Automatic models
### ARIMA
ARIMA is abbreviation of Auto Regressive Integrative Moving Average. It is a combination of a moving average and autoregressive model.

```
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splits))
```

### Prophet
Prophet is open source software developped by Meta and available on R and Python. It is a additive technique that combains trend component, seasonality, holidays and residuals.
```
model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE,seasonality_weekly =TRUE) %>%
  set_engine("prophet",holidays=df_holidays) %>%
  fit(value ~ date, training(splits))
```

### TBATS
TBATS is an abbreviation of Trigonometric seasonality, Box-Cox transformation, ARMA errors, Trend and Seasonal components
```
model_fit_tbats<-seasonal_reg(mode="regression",
                              seasonal_period_1= "auto",
                              seasonal_period_2= "1 year",
                              seasonal_period_3= "1 month") %>%
  set_engine("tbats") %>%
  fit(value ~ date, training(splits))
```

### Seasonal Naive
```
model_fit_snaive <- naive_reg(seasonal_period="1 year") %>%
  set_engine("snaive") %>%
  fit(value ~ date, training(splits))
```
## Machine learning models
### Elastic Net
```
library(glmnet)
# 5.1 Set Engine
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

# 5.2 Fit the workflow
workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))
```

### Random forest
```
library(randomForest)
# 6.1 Set engine
model_spec_rf <- rand_forest(mode="regression",trees = 500, min_n = 50) %>%
  set_engine("randomForest")

# 6.2 Fit workflow
workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))
```

### Prophet with boost
```
model_spec_prophet_boost <- prophet_boost() %>%
  set_engine("prophet_xgboost") 
workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost
```


### Results
```
model_table <- modeltime_table(
  model_fit_arima,
  model_fit_prophet,
  model_fit_tbats,
  model_fit_snaive,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_prophet_boost,
)
# Models accuracy
calibration_table %>%
  modeltime_accuracy() %>%
  arrange(mae) %>%
  table_modeltime_accuracy(.interactive = FALSE)

```
<img src="https://github.com/JamBelg/Time-Series-Forcasting-with-R/blob/main/pics/Accuracy_table.png?raw=true" width="700" height="700">
The table is in ascending order by MAE.
Definition of columns:
- MAE: Mean absolute error
- MAPE: Mean absolute percentage error
- MASE: Mean absolute scaled error
- SMAPE: Symmetric mean absolute percentage error
- RMSE: Root mean squared error
RSQ: R-squared

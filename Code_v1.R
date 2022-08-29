# 
rm(list=ls())
# setwd("/Users/Shared/Dev Jamel/Store Sales - Time Series Forecasting")
# setwd("C:/tmp/Store Sales - Time Series Forecasting")
setwd("U:/R scripts/Project_Directory/Kaggle/Store Sales - Time Series Forecasting")

# Load libraries
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


mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}


# Read datasets
df_train=read.csv("data/train.csv", sep=",")
df_test=read.csv("data/test.csv", sep=",")
df_oil = read.csv("data/oil.csv", sep=",")
df_holidays = read.csv("data/holidays_events.csv", sep=",")
df_stores = read.csv("data/stores.csv",sep=",")
df_transactions = read.csv("data/transactions.csv", sep=",")


# Oil factor
# Plot oil price (model1 if oil <=63 else model2)
df_oil$oil_NNA<-df_oil$dcoilwtico
df_oil[1,3]=df_oil[2,3]
for(i in 2:nrow(df_oil)){
  if(is.na(df_oil[i,3])){
    df_oil[i,3]=prev_val
  }else{
    prev_val=df_oil[i,3]
  }
}
df_oil %>%
  mutate(mod=ifelse(oil_NNA<=63,1,2)) %>%
  ggplot(aes(x=date,y=oil_NNA,col=as.factor(mod)))+geom_point()


# Join train with stores
df_train <- left_join(x=df_train, y=df_stores, by="store_nbr")
# Join train with transactions
df_train <- left_join(x=df_train, y=df_transactions, by=c("store_nbr","date"))
# Join train with holidays
df_train <- left_join(x=df_train, y=df_holidays, by="date")
# Join train with oil
df_train <- left_join(x=df_train, y=df_oil, by="date")

head(df_train,n=20)
df_train_copy <- df_train


# Sales family
levels(as.factor(df_train_copy$family))


# Relation between sales and oil price
df_train %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales,na.rm=TRUE),
    daily_oil=mean(oil_NNA,na.rm=TRUE)
  ) %>%
  ggplot(aes(x=daily_oil,y=daily_sales))+geom_line()+geom_smooth()+
  ylim(c(300000,1200000))+labs(title="Impact of oil price")

# Weekly sales
df_train %>%
  mutate(date=as.Date(date)) %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales,na.rm=TRUE)
  ) %>%
  mutate(sales_weekly=zoo::rollmean(daily_sales,k=7,fill=NA)) %>%
  ggplot(aes(x=date,y=sales_weekly,group=1)) + geom_line()+geom_smooth()+
  scale_x_date(date_breaks="3 month")+
  theme(axis.text.x=element_text(angle=90))


# Monthly Sales
df_train %>%
  mutate(date=as.Date(date)) %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales,na.rm=TRUE)
  ) %>%
  mutate(sales_monthly=zoo::rollmean(daily_sales,k=30,fill=NA)) %>%
  ggplot(aes(x=date,y=sales_monthly,group=1)) + geom_line()+geom_smooth()+
  scale_x_date(date_breaks="3 month")+
  theme(axis.text.x=element_text(angle=90))

# city Guayaquil
df_train <- df_train %>%
  mutate(date=as.Date(date)) %>%
  filter(city=="Guayaquil") %>%
  select(date,sales) %>%
  group_by(date) %>%
  summarise(
    value=sum(sales,na.rm=TRUE)
  )



max_date=max(df_train$date)
min_date=min(df_train$date)
dat_ts <- df_train

dat_ts <-ts(dat_ts$value,end=c(year(max_date), month(max_date)),
            start=c(year(min_date), month(min_date)),
            frequency = 52)


# Seasonal Decomposition Plot
plot(stl(dat_ts,s.window = "periodic"))

# Augmented Dickey-Fuller(ADF) Test
# p-value <0.05 -> data is "stationary"
print(adf.test(dat_ts))








# split data in two
# last 3 months for validation
splits <- df_train %>%
  time_series_split(assess = "3 months", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)

# Last 3 months
df_train %>%
  ggplot(aes(x=date,y=value,groups=1))+
  geom_line()+scale_x_date(limits=c(as.Date("2017-05-01"),as.Date("2017-08-05")))+
  labs(Title="",subtitle="Last 3 months")

# Automatic Models
# Auto Arima
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splits))
model_fit_arima

# Prophet
model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
  set_engine("prophet") %>%
  fit(value ~ date, training(splits))
model_fit_prophet



# TBATS
model_fit_tbats<-seasonal_reg(mode="regression",
                              seasonal_period_1 = "auto",
                              seasonal_period_2="1 year",
                              seasonal_period_3="1 month") %>%
  set_engine("tbats") %>%
  fit(value ~ date, training(splits))
model_fit_tbats

# 4.2 Fit workflow
workflow_fit_tbats <- workflow() %>%
  add_model(model_spec_TBATS) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))


# Machine learning models
# 1- make a recipe
recipe_spec <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 365, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()

# 2- Elastic Net
library(glmnet)
# 2.1 Set Engine
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

# 2.2 Fit the workflow
workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))

# 3- Random forest
library(randomForest)
# 3.1 Set engine
model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
  set_engine("randomForest")

# 3.2 Fit workflow
workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))


# Hybrid ML
# 4 Prophet boost
model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE) %>%
  set_engine("prophet_xgboost") 
workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost



# Modeltime table
model_table <- modeltime_table(
  model_fit_arima,
  model_fit_prophet,
  model_fit_tbats,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_prophet_boost
) 
model_table

model_table %>%
  modeltime_calibrate(new_data=testing(splits)) %>%
  modeltime_forecast(
    new_data=testing(splits),
    actual_data=df_train
  ) %>%
  plot_modeltime_forecast(.interactive = FALSE.smooth=FALSE)


# Calibration table
calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))
calibration_table


calibration_table %>%
  modeltime_forecast(actual_data = df_train) %>%
  plot_modeltime_forecast(.interactive = TRUE,.smooth=FALSE)


calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)


calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id != 1) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(df_train) %>%
  modeltime_forecast(h = "3 months", actual_data = df_train) %>%
  plot_modeltime_forecast(.interactive = TRUE,.smooth=FALSE)
























# Daily sales
df_train %>%
  filter(family=="MAGAZINES",type.y!="Holiday") %>%
  group_by(date) %>%
  summarise(
    sales_daily=sum(sales,na.rm=TRUE)
  ) %>%
  ggplot(aes(x=date,y=sales_daily,group=1))+geom_line()+geom_smooth()


df_train %>%
  filter(family=="BEAUTY") %>%
  mutate(date=as.Date(date)) %>%
  group_by(date) %>%
  summarise(
    sales_daily=sum(sales,na.rm=TRUE)
  ) %>%
  mutate(sales_weekly=zoo::rollmean(sales_daily, k = 7, fill = NA)) %>%
  ggplot(aes(x=date,y=sales_weekly,group=1))+geom_line()+geom_smooth()+
  labs(title="BEAUTY")+
  scale_x_date(date_breaks = "6 month")


df_train_1 <- df_train %>%
  filter(store_nbr==1,family=="AUTOMOTIVE")
df_train_1 %>%
  ggplot(aes(x=date,y=sales,group=1))+geom_line()+geom_smooth()


# Data transformation to time-series object
max_date=max(df_train_1$date)
min_date=min(df_train_1$date)
# dat_ts <- ts(df_train_1[,5])
# train_ts <- dat_ts[1:1600]

dat_ts <- ts(df_train_1[, 5],
             end=c(year(max_date), month(max_date)),
             start=c(year(min_date), month(min_date)),
             frequency = 52)
train_ts <- window(dat_ts, start = c(2013,1), end = c(2017,2))
test_ts <- window(dat_ts, start = c(2017,3))

# Seasonal Decomposition Plot
plot(stl(dat_ts,s.window = "periodic"))

# Augmented Dickey-Fuller(ADF) Test
# p-value <0.05 -> data is "stationary"
print(adf.test(dat_ts))



# ARIMA model
arima_model <- auto.arima(dat_ts)
summary(arima_model)
accuracy(forecast(arima_model))
checkresiduals(arima_model) 

arima_model$fitted


holt_model <- holt(train_ts, h = 6)
summary(holt_model)
df_holt = as.data.frame(holt_model)
d_test$ho = df_holt$`Point Forecast`
mape(c(0,2,2,12,0,2), df_holt$`Point Forecast`) 



model_tbats <- tbats(train_ts)
summary(model_tbats)
for_tbats <- forecast::forecast(model_tbats, h = 6)
df_tbats = as.data.frame(for_tbats)
dat_test$tbats = df_tbats$`Point Forecast`
mape(dat_test$unemploy, dat_test$tbats) 


#Train a SARIMA model
sarima.forecast <- sarima.for(train_ts, n.ahead=length(test_ts),
                              p=1,d=1,q=1,P=0,D=0,Q=0,S=5)
#Check the MAPE
MAPE(sarima.forecast$pred, test_ts) * 100


#plot a line graph with the actual values 
plot(dat_ts,col="blue",xlab="Date", ylab="Sales",main="SARIMAForecast", type='l',lwd=1.5)
#plot the predicted values
lines(sarima.forecast$pred,col="orange",lwd=1.5)
#add a legend
legend("topleft",inset=.03,legend=c("actual", "predicted"),col=c("blue","orange"),lty=1:1,cex=0.8)





















# -----------------------------------------------------------------------
# To DO
Sales_daily <- df_train %>%
  group_by(date) %>%
  summarise(
    sales_day=sum(sales,na.rm=T)
  )
for(i in 1:nrow(Sales_daily)){
  
}

ggplot(aes(x=date,y=sales_day))+geom_point()


# Holidays factor
df_holidays %>%
  filter(date>="2013-01-01",date<="2013-12-31") %>%
  group_by(type, locale) %>%
  summarise(
    count=n()
  )


df_train %>%
  filter(date>="2013-03-01",date<="2013-03-10") %>%
  group_by(store_nbr,family) %>%
  summarise(
    count=n()
  )


# Levels
## family levels
levels(as.factor(df_train$family))

## onpromotion levels
levels(as.factor(df_train$onpromotion))

## store_nbr levels
levels(as.factor(df_train$store_nbr))

# Date min & max
min(df_train$date)
max(df_train$date)

str(df_train)


# Prepare data
dat_ts <- df_train %>%
  filter(family=="LINGERIE",onpromotion==0,store_nbr==2) %>%
  select(sales) %>%
  ts()

plot.ts(dat_ts)

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

# Naive forecasting model
naive_mod <- naive(dat_ts)
summary(naive_mod)


# Holt method
holt_mod <- holt(dat_ts)
summary(holt_mod)


# ggplot
df_train %>%
  filter(family=="AUTOMOTIVE",onpromotion!=0) %>%
  ggplot(aes(x=date,y=sales))+geom_line()
df_train %>%
  filter(family=="AUTOMOTIVE",onpromotion==0) %>%
  ggplot(aes(x=date,y=sales))+geom_line()
df_train %>%
  mutate(Promo=ifelse(onpromotion==0,0,1)) %>%
  filter(family=="AUTOMOTIVE") %>%
  ggplot(aes(x=date,y=sales,color=as.factor(Promo)))+geom_point()
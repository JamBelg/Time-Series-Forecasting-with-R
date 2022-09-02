# Remove objects
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
library(stringr)


# Add link to github (make it public)
# temp <- tempfile()
# download.file("http://www.newcl.org/data/zipfiles/a1.zip",temp)
# data <- read.table(unz(temp, "a1.dat"))
# unlink(temp)


# Read datasets

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
  mutate(date=as.Date(date)) %>%
  mutate(mod=ifelse(oil_NNA<=63,1,2)) %>%
  ggplot(aes(x=date,y=oil_NNA,col=as.factor(mod)))+geom_point()+
  scale_x_date(date_breaks="1 year")+ylab("Oil price")+xlab("date")+
  labs(title="Oil courses in Ecauador")



# Join tables
df_train <- left_join(x=df_train, y=df_stores, by="store_nbr")
df_train <- left_join(x=df_train, y=df_transactions, by=c("store_nbr","date"))
df_train[is.na(df_train$transactions),11] <- 0
df_train <- left_join(x=df_train, y=df_holidays, by="date")
df_train <- left_join(x=df_train, y=df_oil, by="date")

head(df_train,n=20)
df_train_copy <- df_train

# Daily sales
plot1<-df_train %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales)
  ) %>%
  ggplot(aes(x=date,y=daily_sales,groups=1))+geom_line()+geom_smooth()+
  labs(title="Sales",subtitle="Ecuador (2013-2017)")+
  xlab("Date")+ylab("Sales")
ggsave("pics/plot1.png")




# Holidays factor
df_holidays %>%
  distinct(date) %>%
  mutate(Year=year(date)) %>%
  group_by(Year,locale) %>%
  summarise(
    count=n()
  )
plot_holidays <-df_train %>%
  mutate(holidays_fact=ifelse(is.na(locale) | locale!="National","No","Yes")) %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales,na.rm=TRUE),
    holidays_fact=min(holidays_fact,na.rm=TRUE)
  )%>%
  mutate(holidays_fact=factor(holidays_fact,levels=c("No","Yes"))) %>%
  ggplot(aes(x=holidays_fact,y=daily_sales,fill=holidays_fact,group=holidays_fact))+geom_boxplot()+
  labs(title="Average sales",subtitle="Ecuador (2013-2017)")+xlab("National holidays (Yes / No)")+ylab("Average daily sales")
ggsave("pics/plot_holidays.png")




# Products
plot_products <-df_train %>%
  group_by(date,family) %>%
  summarise(
    daily_sales=sum(sales,na.rm=TRUE)
  ) %>%
  filter(family %in% levels(family)[c(11:12,30,33)]) %>%
  ggplot(aes(x=date,y=daily_sales,color=family))+geom_line()+
  facet_grid(rows=vars(family))+labs(title="Daily sales for some products",subtitle="Ecuador (2013-2017)")+
  xlab("Date")+ylab("Daily sales")
ggsave("pics/plot_products.png")

Products <- df_train %>%
  group_by(family) %>%
  summarise(
    sales_total=sum(sales,na.rm=TRUE),
    sales_mean=mean(sales,na.rm=TRUE),
    sales_median=median(sales,na.rm=TRUE),
  ) %>%
  arrange(desc(sales_total)) %>%
  as_tibble()



# Cities
plot_cities <- df_train %>%
  group_by(date,city) %>%
  summarise(
    sales=sum(sales,na.rm=TRUE)
  ) %>%
  ggplot(aes(x=city,y=sales,color=city))+geom_boxplot()+
  labs(title="Daily sales by city",subtitle="Ecuador (2013-2017)")+
  xlab("City")+ylab("Sales")
ggsave("pics/plot_city1.png")


plot_cities2 <- df_train %>%
  group_by(date,city) %>%
  summarise(
    sales=sum(sales,na.rm=TRUE)
  ) %>%
  filter(city %in% levels(city)[c(1:4)]) %>%
  ggplot(aes(x=date,y=sales,color=city))+geom_line()+
  facet_grid(rows=vars(city))+
  labs(title="Daily sales for some cities",subtitle="Ecuador (2013-2017)")+
  xlab("Date")+ylab("Daily sales")
ggsave("pics/plot_city2.png")





# Relation between sales and oil price
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


# Augmented Dickey-Fuller(ADF) Test
# p-value <0.05 -> data is "stationary"
print(adf.test(dat_ts))








# Store_nbr 51
data<- df_train %>%
  filter(!grepl("Terremoto", description, fixed = TRUE)) %>%
  filter(store_nbr==51) %>%
  group_by(date) %>%
  summarise(
    value=mean(sales,na.rm=TRUE)
  )



# split data in two
# last 3 months for validation
splits <- data %>%
  time_series_split(assess = "3 months", cumulative = TRUE)
# training(splits): dataset to create model
# testing(splits): dataset to test/validate model

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)


# Automatic Models
# 1- Auto Arima
model_fit_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splits))
model_fit_arima



# 2- Prophet
model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE,seasonality_weekly =TRUE) %>%
  set_engine("prophet",holidays=df_holidays) %>%
  fit(value ~ date, training(splits))




# 3- TBATS
model_fit_tbats<-seasonal_reg(mode="regression",
                              seasonal_period_1= "auto",
                              seasonal_period_2= "1 year",
                              seasonal_period_3= "1 month") %>%
  set_engine("tbats") %>%
  fit(value ~ date, training(splits))



# 4- Seasonal Naive
model_fit_snaive <- naive_reg(seasonal_period="1 year") %>%
  set_engine("snaive") %>%
  fit(value ~ date, training(splits))




# Machine learning models
# make a recipe
recipe_spec <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_fourier(date, period = c(365, 91.25, 30.42), K = 5) %>%
  step_dummy(all_nominal(),all_predictors())

recipe_spec %>% prep() %>% juice()

# 5- Elastic Net
library(glmnet)
# 5.1 Set Engine
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

# 5.2 Fit the workflow
workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))



# 6- Random forest
library(randomForest)
# 6.1 Set engine
model_spec_rf <- rand_forest(mode="regression",trees = 500, min_n = 50) %>%
  set_engine("randomForest")

# 6.2 Fit workflow
workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))


# Hybrid ML
# 7 Prophet boost
model_spec_prophet_boost <- prophet_boost() %>%
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
  model_fit_snaive,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_prophet_boost
) 


# model_table %>%
#   modeltime_calibrate(new_data=testing(splits)) %>%
#   modeltime_forecast(
#     new_data=testing(splits),
#     actual_data=df_train
#   ) %>%
#   plot_modeltime_forecast(.y_lab="Sales",.x_lab="Date",.title="Sales forecasting",
#                           .interactive = TRUE,.smooth=FALSE)

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))
calibration_table %>%
  modeltime_forecast(actual_data = tail(data,120)) %>%
  plot_modeltime_forecast(.interactive = FALSE,.conf_interval_show = FALSE)



# Calibration table
calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))


calibration_table %>%
  modeltime_forecast(actual_data = data) %>%
  plot_modeltime_forecast(.interactive = TRUE,.smooth=FALSE)

# Models accuracy
calibration_table %>%
  modeltime_accuracy() %>%
  arrange(desc(rsq)) %>%
  table_modeltime_accuracy(.interactive = FALSE)


# 3 months prediction
calibration_table %>%
  # Take Random forest model
  filter(.model_id == 6) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(data) %>%
  modeltime_forecast(h = "12 month", actual_data = tail(data,600)) %>%
  plot_modeltime_forecast(.y_lab="Sales",.x_lab="Date",.title="Sales forecasting",
                          .interactive = FALSE,.smooth=TRUE)
























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








# city Guayaquil, store number 24
df_train_Guayaquil_24 <- df_train_copy %>%
  
  # Convert to date type
  mutate(date=as.Date(date)) %>%
  
  # filter by city
  filter(city=="Guayaquil") %>%
  
  # filter by store
  filter(store_nbr==24) %>%
  
  # Earth quake filter
  filter(!grepl("Terremoto", description, fixed = TRUE)) %>%
  
  select(date,sales) %>%
  group_by(date) %>%
  summarise(
    value=sum(sales,na.rm=TRUE)
  )

# city Quito, store number 44
df_train_Quito_44 <- df_train_copy %>%
  
  # Convert to date type
  mutate(date=as.Date(date)) %>%
  
  # filter by city
  filter(city=="Quito") %>%
  
  # filter by store
  filter(store_nbr==48) %>%
  
  # Earth quake filter
  filter(!grepl("Terremoto", description, fixed = TRUE)) %>%
  
  select(date,sales) %>%
  group_by(date) %>%
  summarise(
    value=sum(sales,na.rm=TRUE)
  )



# Weekly
dayofweek <- list(day=c("7-Sunday","1-Monday","2-Tuesday","3-Wednesday","4-Thursday","5-Friday","6-Saturday"))
df_train_Guayaquil_24 %>%
  mutate(week_day=dayofweek$day[as.POSIXlt(date)$wday+1]) %>%
  ggplot(aes(x=week_day,y=value,fill=week_day))+geom_boxplot()+
  labs(title="Sales by weekday",subtitle="City Guayaquil - Store 24 (2013-2017)")+
  xlab("Weekday")+ylab("Daily sales")
df_train_Quito_44 %>%
  mutate(week_day=dayofweek$day[as.POSIXlt(date)$wday+1]) %>%
  ggplot(aes(x=week_day,y=value,fill=week_day))+geom_boxplot()+
  labs(title="Sales by weekday",subtitle="City Quito - Store 44 (2013-2017)")+
  xlab("Weekday")+ylab("Daily sales")

df_train %>%
  mutate(week_day=dayofweek$day[as.POSIXlt(date)$wday+1]) %>%
  group_by(week_day) %>%
  summarise(
    count=n(),
    avg=mean(value,na.rm=TRUE),
    sd=sd(value,na.rm=TRUE),
    min=min(value,na.rm=TRUE),
    max=max(value,na.rm=TRUE),
    median=median(value,na.rm=TRUE)
  )















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
  theme(axis.text.x=element_text(angle=90))+
  xlab("Date")+ylab("Weekly sales")+labs(title="Weekly sales",subtitle="Ecuador (2013-2017)")


# Monthly Sales
df_train %>%
  mutate(date=as.Date(date)) %>%
  group_by(date) %>%
  summarise(
    daily_sales=sum(sales,na.rm=TRUE)
  ) %>%
  mutate(sales_monthly=zoo::rollmean(daily_sales,k=30,fill=NA)) %>%
  ggplot(aes(x=date,y=sales_monthly,group=1)) + geom_line()+geom_smooth()+
  scale_x_date(date_breaks="3 month",date_labels = "%b-%Y")+
  theme(axis.text.x=element_text(angle=90))+
  xlab("Date")+ylab("Monthly sales")+labs(title="Monthly sales",subtitle="Ecuador (2013-2017)")


# Holidays
data_H <- df_train %>%
  mutate(date=as.Date(date)) %>%
  filter(!is.na(type.y)) %>%
  group_by(family,type.y,locale) %>%
  summarise(
    sum_H=sum(sales,na.rm=TRUE),
    mean_H=mean(sales,na.rm=TRUE)
  )

# Food products
df_train_copy %>%
  
  # Convert to date type
  mutate(date=as.Date(date)) %>%
  
  # Earth quake filter
  filter(!grepl("Terremoto", description, fixed = TRUE)) %>%
  
  select(date,sales,family) %>%
  group_by(date,family) %>%
  summarise(
    value=sum(sales,na.rm=TRUE)
  ) %>%
  filter(family %in% c("EGGS","FROZEN FOODS","MEATS","SEAFOOD","PREPARED FOODS","BREAD/BAKERY")) %>%
  ggplot(aes(x=date,y=value,groups=family,color=family))+geom_line()+
  labs(title="Food products sales", subtitle="Ecuador (2013.2015)")+
  xlab("Date")+ylab("Daily sales")

# Liquors
df_train_copy %>%
  
  # Convert to date type
  mutate(date=as.Date(date)) %>%
  
  # Earth quake filter
  filter(!grepl("Terremoto", description, fixed = TRUE)) %>%
  
  select(date,sales,family) %>%
  group_by(date,family) %>%
  summarise(
    value=sum(sales,na.rm=TRUE)
  ) %>%
  filter(family %in% c("LIQUOR,WINE,BEER","BEVERAGES")) %>%
  ggplot(aes(x=date,y=value,groups=family,color=family))+geom_line()+
  labs(title="Beverage products sales", subtitle="Ecuador (2013.2015)")+
  xlab("Date")+ylab("Daily sales")







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
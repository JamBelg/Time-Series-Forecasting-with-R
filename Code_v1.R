# Remove objects
rm(list=ls())
setwd("/Users/Shared/Dev Jamel/Store Sales - Time Series Forecasting")

# Load libraries
library(tidymodels)
library(modeltime)
library(tseries)
library(stats)
library(zoo)
library(lubridate)
library(timetk)




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


# Promotions
Plot_promotions <- df_train %>%
  group_by(date) %>%
  summarise(
    sales=mean(sales,na.rm=TRUE),
    onprom=sum(onpromotion,na.rm=TRUE)
  ) %>%
  mutate(Promotions=ifelse(onprom==0,"No","Yes")) %>%
  ggplot(aes(x=Promotions,y=sales,fill=Promotions))+geom_boxplot()+
  labs("Influence of promotions on daily sales",subtitle="Ecuador (2013-2017)")+
  xlab("Promotions ?")+ylab("Daily sales")
ggsave("pics/plot_promotions.png")


df_oil %>%
  ggplot(aes(x=date,y=oil_NNA,groups=1)) + geom_line()


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

# Store_nbr 51, family MEATS
data_1<- df_train %>%
  filter(!grepl("Terremoto", description, fixed = TRUE)) %>%
  filter(store_nbr==51,family=="MEATS") %>%
  group_by(date) %>%
  summarise(
    value=mean(sales,na.rm=TRUE)
  )
# Store_nbr 51, family PRODUCE
data_2<- df_train %>%
  filter(!grepl("Terremoto", description, fixed = TRUE)) %>%
  filter(store_nbr==51,family=="PRODUCE") %>%
  group_by(date) %>%
  summarise(
    value=mean(sales,na.rm=TRUE)
  )

# split data in two
# last 3 months for validation
splits <- data %>%
  time_series_split(assess = "3 months", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)


# Automatic Models
# 1- Auto Arima
model_arima <- arima_reg() %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, training(splits))
model_arima



# 2- Prophet
model_prophet <- prophet_reg(seasonality_yearly = TRUE,seasonality_weekly =TRUE) %>%
  set_engine("prophet",holidays=df_holidays) %>%
  fit(value ~ date, training(splits))




# 3- TBATS
model_tbats<-seasonal_reg(mode="regression",
                          seasonal_period_1= "auto") %>%
  set_engine("tbats") %>%
  fit(value ~ date, training(splits))



# 4- Seasonal Naive
model_snaive <- naive_reg() %>%
  set_engine("snaive") %>%
  fit(value ~ date, training(splits))




# Machine learning models
# make a recipe
recipe <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_fourier(date, period = c(365, 91.25, 30.42), K = 5) %>%
  step_dummy(all_nominal(),all_predictors())

recipe %>% 
  prep() %>%
  juice()


# 5- Elastic Net
library(glmnet)
# 5.1 Set Engine
engine_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

# 5.2 Fit the workflow
model_glmnet <- workflow() %>%
  add_model(engine_glmnet) %>%
  add_recipe(recipe %>% step_rm(date)) %>%
  fit(training(splits))



# 6- Random forest
library(randomForest)
# 6.1 Set engine
engine_rf <- rand_forest(mode="regression",trees = 50, min_n = 5) %>%
  set_engine("randomForest")

# 6.2 Fit workflow
model_rf <- workflow() %>%
  add_model(engine_rf) %>%
  add_recipe(recipe %>% step_rm(date)) %>%
  fit(training(splits))


# Hybrid ML
# 7 Prophet boost
engine_prophet_boost <- prophet_boost() %>%
  set_engine("prophet_xgboost") 
model_prophet_boost <- workflow() %>%
  add_model(engine_prophet_boost) %>%
  add_recipe(recipe) %>%
  fit(training(splits))




# Modeltime table
models_table <- modeltime_table(
  model_arima,
  model_prophet,
  model_tbats,
  model_snaive,
  model_glmnet,
  model_rf,
  model_prophet_boost
) 


# Calibrate models for accuracy and forecast
calib_table <- models_table %>%
  modeltime_calibrate(testing(splits))

# Models accuracy
calib_table %>%
  modeltime_accuracy() %>%
  arrange(desc(rsq)) %>%
  table_modeltime_accuracy(.interactive = FALSE)


# Plot prohet + prophet+boost
calib_table %>%
  filter(.model_id %in% c(2,7)) %>%
  modeltime_forecast(actual_data = tail(data,120)) %>%
  plot_modeltime_forecast(.interactive = FALSE,.conf_interval_show = FALSE)



# Plot all models
calib_table %>%
  modeltime_forecast(actual_data = data) %>%
  plot_modeltime_forecast(.interactive = TRUE,.smooth=FALSE)
calib_table %>%
  modeltime_forecast(actual_data = tail(data,120)) %>%
  plot_modeltime_forecast(.y_lab="Sales",.x_lab="Date",
                          .title="Models testing",
                          .interactive = FALSE,.smooth=FALSE)



# 3 months prediction
calib_table %>%
  # Take Random forest model
  filter(.model_id == 6) %>%
  # Refit
  modeltime_refit(data) %>%
  modeltime_forecast(h = "3 month", actual_data = tail(data,600)) %>%
  plot_modeltime_forecast(.y_lab="Sales",.x_lab="Date",
                          .title="Sales forecasting-Sales_nbr 51-PRODUCE product",
                          .interactive = FALSE,.smooth=FALSE)


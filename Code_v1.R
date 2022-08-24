# Load libraries
library(dplyr)
library(ggplot2)
library(forecast)
library(stats)
library(TTR)

# Read datasets
df_train=read.csv("data/train.csv",sep=",")
df_test=read.csv("data/test.csv", sep=",")
df_oil = read.csv("data/oil.csv",sep=",")
df_holidays = read.csv("data/holidays_events.csv",sep=",")
df_stores = read.csv("data/stores.csv",sep=",")
df_transactions = read.csv("data/transactions.csv",sep=",")


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


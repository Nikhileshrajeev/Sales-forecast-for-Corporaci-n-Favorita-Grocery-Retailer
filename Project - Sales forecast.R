install.packages("plotly")
library(ggplot2)
library(data.table)
library(lubridate)
library(plotly)
library(shiny)
library(tidyverse)
library(DT)
library(data.table)
library(lubridate)
library(caret)
library(knitr)
library(kableExtra)
library(forecast)
library(prophet)

train        <- fread("/Users/nikra/OneDrive/documents/train.csv") #About 4.5GB
transactions <- fread("/Users/nikra/OneDrive/documents/transactions.csv")
items        <- fread("/Users/nikra/OneDrive/documents/items.csv")
holidays_events <- fread("/Users/nikra/OneDrive/documents/holidays_events.csv")
oil          <- fread("/Users/nikra/OneDrive/documents/oil.csv")
stores       <- fread("/Users/nikra/OneDrive/documents/stores.csv")
test        <- fread("/Users/nikra/OneDrive/documents/test.csv")

#Top 10 Selling Stores
train %>%
  group_by("store_nbr") %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(store_nbr = reorder(store_nbr,Count)) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = store_nbr,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = "aliceblue") +
  geom_text(aes(x = store_nbr, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Store Number', 
       y = 'Count', 
       title = 'Store Number and Count') +
  coord_flip() + 
  theme_bw()

#Trend of oil prices
oil %>%
  filter(!is.na(dcoilwtico)) %>%
  mutate(year = year(ymd(date)) ) %>%
  mutate(month = month(ymd(date)) ) %>%
  filter(!is.na(year)) %>%
  filter(!is.na(month)) %>%
  group_by(year,month) %>%
  summarise(MedianOilPrice = median(dcoilwtico)) %>%
  mutate(YearMonth = make_date(year=year,month=month) ) %>%
  
  ggplot(aes(x=YearMonth,y=MedianOilPrice)) +
  geom_line(size=1, color="red")+
  geom_point(size=3, color="red") +
  
  labs(x = 'Time', y = 'Median',title = 'Trend of Oil Prices') +
  theme_bw() 

#Fivefold cross validation and predictions on the test data set using XGBoost model
TransformColumn = function(x)
{
  if (is.na(x))
  {
    return (0)
  }
  else
  {
    return (x)
  }
}

GetHighestSoldItem = function(ds,ItemNumber,StoreNumber)
{
  
  HighestSoldItem = ds %>%
    filter(store_nbr == StoreNumber) %>%
    filter(item_nbr == ItemNumber) %>%
    mutate(year = year(ymd(date)))  %>%
    mutate(month = month(ymd(date)))  %>%
    mutate(dayOfWeek = wday(date))  %>%
    mutate(day = day(ymd(date))) 
  
  HighestSoldItem = left_join(HighestSoldItem,oil)
  
  HolidaysNational = holidays_events %>%
    filter(type != "Work Day") %>%
    filter(locale == "National")
  
  HighestSoldItem = left_join(HighestSoldItem,HolidaysNational, by = "date") 
  
  HighestSoldItem = HighestSoldItem %>%
    select( -locale,-locale_name,-description,-transferred)
  
  HighestSoldItem = HighestSoldItem %>%
    select(-id,-store_nbr,-item_nbr,-date,-onpromotion) 
  
  HighestSoldItem$type = sapply(HighestSoldItem$type,TransformColumn)
  
  features <- colnames(HighestSoldItem)
  
  for (f in features) {
    if ((class(HighestSoldItem[[f]])=="factor") || (class(HighestSoldItem[[f]])=="character")) {
      levels <- unique(HighestSoldItem[[f]])
      HighestSoldItem[[f]] <- as.numeric(factor(HighestSoldItem[[f]], levels=levels))
    }
  }
  
  
  
  return(HighestSoldItem)
  
  
}

HighestSoldItem = GetHighestSoldItem(train,329362,44)

HighestSoldItemTest = GetHighestSoldItem(test,329362,44)


formula = unit_sales ~ .

fitControl <- trainControl(method="cv",number = 5)


PlotImportance = function(importance)
{
  varImportance <- data.frame(Variables = row.names(importance[[1]]), 
                              Importance = round(importance[[1]]$Overall,2))
  
  # Create a rank variable based on importance
  rankImportance <- varImportance %>%
    mutate(Rank = paste0('#',dense_rank(desc(Importance))))
  
  rankImportancefull = rankImportance
  
  ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                             y = Importance)) +
    geom_bar(stat='identity',colour="white", fill = "gray90") +
    geom_text(aes(x = Variables, y = 1, label = Rank),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'Variables', title = 'Relative Variable Importance') +
    coord_flip() + 
    theme_bw()
  
  
}


xgbGrid <- expand.grid(nrounds = 500,
                       max_depth = 4,
                       eta = .05,
                       gamma = 0,
                       colsample_bytree = .5,
                       min_child_weight = 1,
                       subsample = 1)

set.seed(13)

XGBModel = train(formula, data = HighestSoldItem,
                 method = "xgbTree",trControl = fitControl,
                 tuneGrid = xgbGrid,na.action = na.pass,metric="RMSE")


importance = varImp(XGBModel)

PlotImportance(importance)

#Forecasting time series using prophet procedure which is the best for missing data  and shifts in the trend, and typically handles outliers well.

HighestSoldItem = train %>%
  filter(store_nbr == 44) %>%
  filter(item_nbr == 329362) %>%
  arrange(desc(date)) %>%
  select(date,unit_sales) %>% head(60)

colnames(HighestSoldItem) = c("ds","y")

m <- prophet(HighestSoldItem,changepoint.prior.scale = 0.1)

future <- make_future_dataframe(m, periods = 16,freq = "day")

forecast <- predict(m, future)

predictions = tail(round(forecast$yhat),16)

plot(m, forecast)

cat("The predictions are ","\n")

predictions

prophet_plot_components(m, forecast)

# Team 16 improved Favorita approaches

######
# Import libraries

require(data.table)
require(tidyverse)
require(lubridate)
require(xts)
require(leaflet)
require(ggfortify)
require(tidyquant)
require(microbenchmark)


library(DT)
library(caret)
library(kableExtra)
library(forecast)
library(prophet)








#####
# Import the data
oil <- fread('oil.csv', showProgress = TRUE)
items <- fread('items.csv', showProgress = TRUE)
stores <- fread('stores.csv', showProgress = TRUE)
transactions <- fread('transactions.csv', showProgress = TRUE) 
dtrain <- fread('train.csv', showProgress = TRUE) # this is a 3.6GB file. So it is beneficial to show progress along the way.
holidays = fread('holidays_events.csv')
test <- fread('test.csv')













#####
# Create a duplicate of original training data for temporary adjustments and to make sure the initial is not damaged / lost
train <- dtrain













##### Initial data inspections
summary(train)















#### Options:
# Option 1:
sum(is.na(dtrain$onpromotion))
na_omit_train <- na.omit(train)
# This will outright remove all the NAs, which will reduce 21,657,651 records from the data set. We can all agree that this is not a great idea...

# Possible reasons why it is bad? That is probably relevant data. It will skew the results. 













# Option 2: 
table(train$onpromotion)

# FALSE     TRUE 
# 96028767  7810622 




sum(is.na(train$onpromotion))
# [1] 21657651



# What if we just made all the NAs to 0
train$onpromotion[is.na(train$onpromotion)] <- 0


# We thereby state that if we have no information about the data set promotion, then we assume it is false.
sum(is.na(train$onpromotion))
# [1] 0











# Option 3: 
#Impute


train <- dtrain


require(mice)
require(dplyr)

sample_train <- sample_n(train, 10000, replace = FALSE)
#
sum(is.na(sample_train))
md.pattern(sample_train)







impute_train <- mice(sample_train,
                     m=5, # The number of imputed data sets, 5 is default
                     maxit=50, 
                     meth='pmm', # The Method of imputing (pmm = predicted mean matching)
                     seed=500)


# Error
# iter imp variable
# 1   1  onpromotionError in solve.default(xtx + diag(pen)) : 
#  system is computationally singular: reciprocal condition number = 1.28881e-17



impute_train <- mice(sample_train,
                     m=5, # The number of imputed data sets, 5 is default
                     maxit=50, 
                     meth='cart', # The Method of imputing (cart = classification and regression trees)
                     seed=500)


summary(impute_train)


#### Variable Importance

#Dirty XGBoost and Feature Importance for Store Number 44 and Item Number 329362
#
#We do a XGBoost on the following features
# year            
# month           
# day of the month          
# day of the week         
# oil price        
# National Holiday or not          

#We do a `Five Fold` Cross Validation on the data set and also find the feature importance of the variables.                                 
#We also do `predictions` for the test set using the XGBoost Model. Thanks to `Alex Young` for the revised predictions code

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
  
  HolidaysNational = holidays %>%
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
    geom_bar(stat='identity',colour="white", fill = "blue") +
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

predictions_old = round(predict(XGBModel,HighestSoldItemTest,na.action= na.pass))
predictions_old

#### Improved
xgbGrid <- expand.grid(nrounds = 500,
                       max_depth = 5,
                       eta = .3, # learning rate
                       gamma = 5, # controls regularization, prevents overfitting, higher the value, higher the penalty for values that don't improve
                       colsample_bytree = .5,
                       min_child_weight = 1,
                       subsample = .6)

set.seed(13)

XGBModel = train(formula, data = HighestSoldItem,
                 method = "xgbTree",trControl = fitControl,
                 tuneGrid = xgbGrid,na.action = na.pass,metric="RMSE")


importance = varImp(XGBModel)

PlotImportance(importance)

predictions = round(predict(XGBModel,HighestSoldItemTest,na.action= na.pass))
predictions
predictions_old


##### Arima

HighestSoldItem = train %>%
  filter(store_nbr == 44) %>%
  filter(item_nbr == 329362) %>%
  arrange(desc(date)) %>%
  select(unit_sales) %>% head(60)

tsHighestSoldItem = ts(HighestSoldItem)

fit <- auto.arima(tsHighestSoldItem)

preds = forecast(fit, h = 16)

preds %>% autoplot(include=60) +theme_bw()


#### ETS
fit <- ets(tsHighestSoldItem)

preds = forecast(fit, h = 16)

preds %>% autoplot(include=60) +theme_bw()
predictions = round(as.numeric(preds$mean))

cat("The predictions are ","\n")
predictions

#Prophet

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
prophet_plot_components(m, forecast)



##### Improved


m <- prophet(HighestSoldItem, growth = 'linear', mcmc.samples = 1200, weekly.seasonality = TRUE, interval.width = 0.95)
future <- make_future_dataframe(m, periods = 16, freq = 'day') 
# make future predictions for a determined amount of weeks
forecast <- predict(m, future) # use future time frame to predict m
plot(m, forecast)  + add_changepoints_to_plot(m) # initial plot with change points identified
# note approximately 9 per year
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) # for validation, delete later. Note vertical lines for inflection points
prophet_plot_components(m, forecast) # component analysis
dyplot.prophet(m, forecast) # interactive plot


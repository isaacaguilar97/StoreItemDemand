library(doParallel)

num_cores <- parallel::detectCores() #How many cores do I have?
cl <- makePSOCKcluster(num_cores)
registerDoParallel(cl)

# Load Packages -----------------------------------------------------------


library(tidyverse)
library(vroom)
library(tidymodels)
library(naivebayes)
library(discrim)
library(DataExplorer)
library(bonsai)
library(lightgbm)
library(embed)
library(timetk)
library(patchwork)
library(modeltime) #Extensions of tidymodels to time series
library(timetk) #Some nice time series functions



# Load the data -----------------------------------------------------------
# setwd('~/College/Stat348/StoreItemDemand')

# Load Data ---------------------------------------------------------------
trainSet <- vroom('./train.csv')
testSet <- vroom('./test.csv')

# EDA ---------------------------------------------------------------------

dplyr::glimpse(trainSet)


# trainSet %>%
#   plot_time_series(date, sales, .interactive=FALSE)

# Create 4 combinations of StoreItem
storeitem1 <- trainSet %>% filter(store == 1, item == 10)
storeitem2 <- trainSet %>% filter(store == 2, item == 9)
storeitem3 <- trainSet %>% filter(store == 3, item == 8)
storeitem4 <- trainSet %>% filter(store == 4, item == 7)

storeitem1t <- testSet %>% filter(store == 1, item == 10)
storeitem2t <- testSet %>% filter(store == 2, item == 9)

# ACF Function plots
g1 <- storeitem1 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365) +
  ggtitle("Store 1 - Item 10, Autocorrelation of Sales Over 2 years")
g2 <- storeitem2 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365) +
  ggtitle("Store 2 - Item 9, Autocorrelation of Sales Over 2 years")
g3 <- storeitem3 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365) +
  ggtitle("Store 3 - Item 8, Autocorrelation of Sales Over 2 years")
g4 <- storeitem4 %>%
  pull(sales) %>%
  forecast::ggAcf(., lag.max=2*365) +
  ggtitle("Store 4 - Item 7, Autocorrelation of Sales Over 2 years")

# Combination
(g1 + g2) / (g3 + g4)

dplyr::glimpse(storeitem4)


# My recipe ---------------------------------------------------------------

my_recipe <- recipe(sales~., data=storeitem4) %>%
  step_date(date, features = c("dow", "month", "year", "week", "doy", "decimal")) %>%
  step_holiday(date) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>%
  # step_lag(date, lag = 365) %>%
  # step_lag(date, lag = 30) %>%
  # step_lag(date, lag = 7) %>%
  # step_naomit(columnslag_365_date, lag_30_date, lag_7_date) %>%
  step_rm('store', 'item')

# 
# # Bake
# prep <- prep(my_recipe)
# train_clean <- bake(prep, new_data = storeitem4)
# view(train_clean)


# # Modeling RF----------------------------------------------------------------
# 
# # Model
# rf_mod <- rand_forest(mtry = tune(),
#                       min_n=tune(),
#                       trees=200) %>%
#   set_engine("ranger") %>%
#   set_mode("regression")
# 
# ## Workflow
# rf_workflow <- workflow() %>%
#   add_recipe(my_recipe) %>%
#   add_model(rf_mod) %>%
#   fit(data = storeitem4)
# 
# ## Set up grid of tuning values
# tuning_grid <- grid_regular(mtry(range = c(1,12)), # How many Variables to choose from
#                             # researches have found log of total variables is enough
#                             min_n(),
#                             levels = 5)
# 
# # Set up K-fold CV
# folds <- vfold_cv(storeitem4, v = 5, repeats=1)
# 
# # Cross Validation
# CV_results <- rf_workflow %>%
#   tune_grid(resamples=folds,
#             grid=tuning_grid,
#             metrics=metric_set(smape))
# 
# # Find best tuning parameters
# bestTune <- CV_results %>%
#   select_best("smape")
# 
# collect_metrics(CV_results) %>%
#   filter(mtry == bestTune$mtry, min_n == bestTune$min_n, .config == bestTune$.config) %>%
#   pull(mean)
# 
# # Finalize workflow
# final_wf <- rf_workflow %>%
#   finalize_workflow(bestTune) %>%
#   fit(data=amazon_train)
# 
# # Predict
# predictions <- final_wf %>%
#   predict(new_data = testSet)
# 
# 
# # Format table
# 
# 
# # get csv file
# vroom_write(results, 'AmazonPredsrf.csv', delim = ",")


# Time Series -------------------------------------------------------------

cv_split1 <- time_series_split(storeitem1, assess="3 months", cumulative = TRUE)
cv_split2 <- time_series_split(storeitem2, assess="3 months", cumulative = TRUE)
# 
# # Plot
# cv_split1 %>%
#   tk_time_series_cv_plan() %>% #Put into a data frame
#   plot_time_series_cv_plan(date, sales, .interactive=FALSE)
# 
# 
# es_model1 <- exp_smoothing() %>%
#   set_engine("ets") %>%
#   fit(sales~date, data=training(cv_split1))
# 
# es_model2 <- exp_smoothing() %>%
#   set_engine("ets") %>%
#   fit(sales~date, data=training(cv_split2))
# 
# ## Cross-validate to tune model
# cv_results1 <- modeltime_calibrate(es_model1,
#                                    new_data = testing(cv_split1))
# cv_results2 <- modeltime_calibrate(es_model2,
#                                    new_data = testing(cv_split2))
# 
# ## Visualize CV results
# p1 <- cv_results1 %>%
#   modeltime_forecast(
#     new_data = testing(cv_split1),
#     actual_data = storeitem1
#     ) %>%
#   plot_modeltime_forecast(.interactive=TRUE)
# 
# p2 <- cv_results2 %>%
#   modeltime_forecast(
#     new_data = testing(cv_split2),
#     actual_data = storeitem2
#   ) %>%
#   plot_modeltime_forecast(.interactive=TRUE)
# 
# ## Evaluate the accuracy
# cv_results1 %>%
#   modeltime_accuracy() %>%
#   table_modeltime_accuracy(
#     .interactive = FALSE
#     )
# 
# cv_results2 %>%
#   modeltime_accuracy() %>%
#   table_modeltime_accuracy(
#     .interactive = FALSE
#   )
#   
# ## Refit to all data then forecast1
# es_fullfit1 <- cv_results1 %>%
#   modeltime_refit(data = storeitem1)
# 
# es_fullfit2 <- cv_results2 %>%
#   modeltime_refit(data = storeitem2)

## prepareing to submit
# es_preds1 <- es_fullfit1 %>%
#   modeltime_forecast(h = "3 months") %>%
#   rename(date=.index, sales=.value) %>%
#   select(date, sales) %>%
#   full_join(., y=test, by="date") %>%
#   select(id, sales)
# 
# es_preds2 <- es_fullfit2 %>%
#   modeltime_forecast(h = "3 months") %>%
#   rename(date=.index, sales=.value) %>%
#   select(date, sales) %>%
#   full_join(., y=test, by="date") %>%
#   select(id, sales)

# p3 <- es_fullfit1 %>%
#   modeltime_forecast(h = "3 months", actual_data = storeitem1) %>%
#   plot_modeltime_forecast(.interactive=FALSE)
# 
# p4 <- es_fullfit2 %>%
#   modeltime_forecast(h = "3 months", actual_data = storeitem2) %>%
#   plot_modeltime_forecast(.interactive=FALSE)
# 
# plotly::subplot(p1,p3,p2,p4, nrows=2)


# SARIMA ------------------------------------------------------------------

cv_split1 <- time_series_split(storeitem1, assess="3 months", cumulative = TRUE)
cv_split2 <- time_series_split(storeitem2, assess="3 months", cumulative = TRUE)

storeitem1t <- testSet %>% filter(store == 1, item == 10)
storeitem2t <- testSet %>% filter(store == 2, item == 9)

arima_recipe <- recipe(sales~., data=storeitem1) %>%
  step_date(date, features = c("dow", "month", "year", "week", "doy", "decimal")) %>%
  step_holiday(date) %>%
  step_range(date_doy, min = 0, max = pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))
  # step_lag(date, lag = 365) %>%
  # step_lag(date, lag = 30) %>%
  # step_lag(date, lag = 7) %>%
  # step_naomit(columnslag_365_date, lag_30_date, lag_7_date) %>%
  # step_rm('store', 'item') # For the linear model part
S <- 365
arima_model <- arima_reg(seasonal_period=S,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2) %>% #default max D to tune
  set_engine("auto_arima")

arima_wf1 <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split1))

arima_wf2 <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split2))


## Calibrate (i.e. tune) workflow

cv_results1 <- modeltime_calibrate(arima_wf1,
                                   new_data = testing(cv_split1))
cv_results2 <- modeltime_calibrate(arima_wf2,
                                   new_data = testing(cv_split2))


# ## Visualize CV results
p1 <- cv_results1 %>%
  modeltime_forecast(
    new_data = testing(cv_split1),
    actual_data = storeitem1
    ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

p2 <- cv_results2 %>%
  modeltime_forecast(
    new_data = testing(cv_split2), # We need to change this
    actual_data = storeitem2
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

# ## Refit to all data then forecast1
es_fullfit1 <- cv_results1 %>%
  modeltime_refit(data = storeitem1)

es_fullfit2 <- cv_results2 %>%
  modeltime_refit(data = storeitem2)

p3 <- es_fullfit1 %>%
  modeltime_forecast(new_data = storeitem1t, actual_data = storeitem1) %>% # new_data = item
  plot_modeltime_forecast(.interactive=FALSE)

p4 <- es_fullfit2 %>%
  modeltime_forecast(new_data = storeitem2t,actual_data = storeitem2) %>%
  plot_modeltime_forecast(.interactive=FALSE)

plotly::subplot(p1,p3,p2,p4, nrows=2)



# Facebook PROPHET's Model ------------------------------------------------

prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(y ~ date, data = training(cv_split))

## Calibrate (i.e. tune) workflow

## Visualize & Evaluate CV accuracy

## Refit best model to entire data and predict

# Modeling ----------------------------------------------------------------

nStores <- max(train$store)
nItems <- max(train$item)
for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train %>%
    filter(store==s, item==i)
    storeItemTest <- test %>%
    filter(store==s, item==i)
    
    ## Fit storeItem models here
    
    ## Predict storeItem sales
    
    ## Save storeItem predictions
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
  }
}

stopCluster(cl)

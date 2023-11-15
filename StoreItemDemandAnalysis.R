
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

# My recipe
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
  

# Bake
prep <- prep(my_recipe)
train_clean <- bake(prep, new_data = storeitem4)
view(train_clean)


# Modeling RF----------------------------------------------------------------

# Model
rf_mod <- rand_forest(mtry = tune(),
                      min_n=tune(),
                      trees=200) %>%
  set_engine("ranger") %>%
  set_mode("regression")

## Workflow
rf_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_mod) %>%
  fit(data = storeitem4)

## Set up grid of tuning values
tuning_grid <- grid_regular(mtry(range = c(1,12)), # How many Variables to choose from
                            # researches have found log of total variables is enough
                            min_n(),
                            levels = 5)

# Set up K-fold CV
folds <- vfold_cv(storeitem4, v = 5, repeats=1)

# Cross Validation
CV_results <- rf_workflow %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(smape))

# Find best tuning parameters
bestTune <- CV_results %>%
  select_best("smape")

collect_metrics(CV_results) %>%
  filter(mtry == bestTune$mtry, min_n == bestTune$min_n, .config == bestTune$.config) %>%
  pull(mean)

# Finalize workflow
final_wf <- rf_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data=amazon_train)

# Predict
predictions <- final_wf %>%
  predict(new_data = testSet)


# Format table


# get csv file
vroom_write(results, 'AmazonPredsrf.csv', delim = ",")


  
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

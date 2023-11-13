
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


# Load the data -----------------------------------------------------------
# setwd('~/College/Stat348/StoreItemDemand')

# Load Data ---------------------------------------------------------------
trainSet <- vroom('./train.csv')
testSet <- vroom('./test.csv')

# EDA ---------------------------------------------------------------------

dplyr::glimpse(trainSet)


# trainSet %>%
#   plot_time_series(date, sales, .interactive=FALSE)

# ACF Function plots
storeItem %>%
  pull(sales)
forecast::ggAcf(., lag.max=2*365)

max(testSet$store)
max(testSet$item)


# Combination




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

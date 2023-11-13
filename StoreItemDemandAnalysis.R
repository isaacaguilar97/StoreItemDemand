
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

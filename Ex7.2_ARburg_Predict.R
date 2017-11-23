#Author: Thomas Hollis
#Subject: Bachelor Thesis

#1. Data Import
data <- read.csv("R/BTC.csv")

#2. Data Split
data_train <- data[1:100,2]
data_test <- data[101:377,2]

#3. Model Train & apply Model
performance <- 0
for (j in 1:20)
{
  data_train <- data[1:100,2]
  c_count <- 0
  for (i in 1:277)
  {
    btc_ar <- ar.burg(data_train, order.max = j, aic = FALSE)
    btc_pred <- predict(object = btc_ar, n.ahead = 1)
    
    if(i > 1)
    {
      if(((btc_pred$pred[1] > data_test[i-1]) && (data_test[i] > data_test[i-1])) || ((btc_pred$pred[1] < data_test[i-1]) && (data_test[i] < data_test[i-1])))
      {
        c_count = c_count+1
      }
    }
    
    data_train <- c(data_train, data_test[i]) 
  }
  performance[j] <- c_count/276.00  
}
round(performance, digits = 2)
round(mean(performance), digits = 2)
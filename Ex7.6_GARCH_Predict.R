#Author: Thomas Hollis
#Subject: Bachelor Thesis

#0. Package Import
library(fGarch)

#1. Data Import
data <- read.csv("R/BTC.csv")

#2. Data Split
data_train <- data[1:100,2]
data_test <- data[101:377,2]

#3. Model Train & apply Model
performance <- matrix(0,2,20)

for (j in 1:20)
{
  data_train <- data[1:100,2]
  c_count <- 0
  
  p <- round(runif(1, 1, 6))
  q <- round(runif(1, 1, 6))
  
  for (i in 1:277)
  {
    btc_arch <- garchFit(~1+garch(p,q),data = data_train,trace=F)
    btc_pred <- predict(btc_arch, 1)
    
    if(i >= 2)
    {
      if(((btc_pred$pred[1] > data_test[i-1]) && (data_test[i] > data_test[i-1])) || ((btc_pred$pred[1] < data_test[i-1]) && (data_test[i] < data_test[i-1])))
      {
        c_count = c_count+1
      }
    }
    
    data_train <- c(data_train, data_test[i]) 
  }
  performance[1,j] <- p*100+d*10+q
  performance[2,j] <- c_count/276.00 
}
round(performance, digits = 2)
round(mean(performance[2,]), digits = 2)
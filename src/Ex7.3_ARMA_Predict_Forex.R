#Author: Thomas Hollis
#Subject: Bachelor Thesis

#1. Data Import
data <- read.csv("R/EUR-GBP400 (40h 44m)h.csv")

#2. Data Split
data_train <- data[1:100,2]
data_test <- data[101:335,2]

#3. Model Train & apply Model
performance <- matrix(0,2,20)

for (j in 1:20)
{
  data_train <- data[1:100,2]
  c_count <- 0
  
  p <- round(runif(1, 1, 6))
  d <- 0
  q <- round(runif(1, 1, 6))
  
  for (i in 1:235)
  {
    btc_arima <- arima(data_train, order = c(p,d,q), method = "CSS")
    btc_pred <- predict(btc_arima, 1)
    
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
  performance[2,j] <- c_count/234.00 
}
round(performance, digits = 2)
round(mean(performance[2,]), digits = 2)

#Author: Thomas Hollis
#Subject: Bachelor Thesis

#1. Package & Data Import
library(tsDyn)
data <- read.csv("R/EUR-GBP400h.csv")

#2. Data Split

data_train <- data[1:235,2]
data_test <- data[235:335,2]

predictions <- matrix(0,1,101)
count = integer(100)

for (i in 1:101)
{
  narxBTC <- nnetTs(data_train, m=5, size = 10, steps = 1)
  predictions[i] <- predict(narxBTC)
  data_train <- c(data_train, data_test[i]) 
  
  if(i >= 2)
  {
    if((predictions[i] > data_test[i-1] && data_test[i] > data_test[i-1]) || (predictions[i] < data_test[i-1] && data_test[i] < data_test[i-1]))
    {
      count[i-1] = 1  
    }
  }
}

values <- seq(235,335,1)
plot(data_train[1:235], ylim=range(data_train), xlim=range(1,335), xlab = "Hours Elapsed", ylab = "EUR Price (GBP)", type = "l", col = "blue", main = "NARMAX")
par(new = TRUE)
plot(x = values, y = data_test, ylim=range(data_train), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", col = "black", type = "l")
par(new = TRUE)
plot(x = values, y = predictions, ylim=range(data_train), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "red")
par(new = TRUE)

sum(count)/100

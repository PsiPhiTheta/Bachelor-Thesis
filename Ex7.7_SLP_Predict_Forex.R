#Author: Thomas Hollis
#Subject: Bachelor Thesis

#1. Data Import & Global Variables
data <- read.csv("R/EUR-GBP400h.csv") #import data

#2. Data post-processing
data_train <- data[1:134,2] #split data to training set
data_test <- data[135:335,2] #split data to testing set
acc = 0

#3. Model application
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_train <- normalize(data_train)
data_test <- normalize(data_test)

w <- runif(10, -0.2, 0.2)
b = 0
alpha = 0.1
y = integer(8)
right_value = 0

for(i in 1:134)
{
  trainingData <- data_train[i:(i+9)]
  
  if(data_train[i+10] > data_train[i+9])
  {
    right_value = 1
  }else
  {
    right_value = 0
  }
  
  if((trainingData%*%w + b) > 0)
  {
    y[i] = 1
  }else
  {
    y[i] = 0
  }
  
  w = w + alpha*(right_value - y[i])*right_value
  b = b + alpha*(right_value - y[i])
}


#prediction
real_result = integer(200)
result = integer(200)

for(i in 2:200)
{
  if(data_test[i] > data_test[i-1])
  {
    real_result[i] = 1
  }else
  {
    real_result[i] = 0
  }
}

for(i in 2:191)
{
  testData <- data_test[i:(i+9)]
  
  if((testData%*%w + b) > 0)
  {
    result[i] = 1
  }else
  {
    result[i] = 0
  }
  
  if(result[i] == real_result[i])
  {
    acc = acc+1
  }
}

plot(result, col = "green")
par(new = TRUE)
plot(real_result, col = "red")

result
real_result
acc/200
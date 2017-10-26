#Author: Thomas Hollis

#1. Data Import & Global Variables
data <- read.csv("R/DigitData.csv") #import data
alpha = 0.1 #initialises model parameter (learning rate)
bias = 0 #initialises a bias of 0
weights = integer(25) #initialises up 25 weights 
y = integer(8) #initialises model output
result = integer(4) #initialises array of results 

#2. Data post-processing
data_train <- data[1:8, ] #split data to training set
data_test <-data[9:12, ] #split data to testing set

#3. Forward propagate the NN model
for(i in 1:8)
{
  if((sum(t(data_train[i,2:26])*weights) + bias) > 0) #perceptron binary classification equation
  {
    y[i] = 1 #guess digit 1
  }
  else
  {
    y[i] = -1 #guess digit 0
  }
  weights = weights + alpha*(data_train$Number[i] - y[i])*data_train[i,2:26] #update weights, adapted from H. Yin instructions
  bias = bias + alpha*(data_train$Number[i] - y[i]) #update bias, from H. Yin instructions
}

#4. Use the trained model to predict
for(i in 1:4) 
{
  if((sum(t(data_test[i,2:26])*weights) + bias) > 0) #use the final weights and bias to make prediction
  {
    result[i] = 1
  }
  else
  {
    result[i] = 0
  }
}
result #prints the result

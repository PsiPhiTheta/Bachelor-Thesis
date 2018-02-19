#Author: Thomas Hollis
#Subject: Bachelor Thesis


#0. Load packages________________________________
library(stochvol)


#1. Functions________________________________
normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x))-0.5)
}


#2. Data Import________________________________
DeepData <- read.csv("R/CryptoData.csv")

#3. Data Process________________________________
DeepDataSV <- svsample(DeepData$CloseBTC) #Markov Chain Monte Carlo Sampler
volplot(DeepDataSV)

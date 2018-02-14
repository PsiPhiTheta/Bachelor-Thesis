#Author: Thomas Hollis
#Subject: Bachelor Thesis


#0. Load packages________________________________
library(neuralnet)


#1. Functions________________________________
normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x))-0.5)
}


#2. Data Import________________________________
DeepData <- read.csv("R/ALL_minute_OCLH_S.csv")


#3. Data Processing________________________________
DeepDataNorm <- DeepData
for (i in 2:ncol(DeepData))
{
  DeepDataNorm[,i] <- normalize(DeepData[,i])
}

DeepDataZ <- DeepData
for (i in 2:ncol(DeepData))
{
  DeepDataZ[,i] <- scale(DeepData[,i], center = TRUE, scale = TRUE)
}

DeepDataDiff <- DeepData[1:20894,]
DeepDataBin <- DeepDataDiff
for (i in 2:ncol(DeepData))
{
  DeepDataDiff[,i] <- diff(DeepData[,i])
  
  for(k in 1:nrow(DeepDataBin))
  {
    if(DeepDataDiff[k,i] > 0)
    {
      DeepDataBin[k,i] = 1
    }
    else
    {
      DeepDataBin[k,i] = 0
    }
  }
}


#4. Data Split________________________________
DeepDataTrain <- DeepData[1:13500,]
DeepDataDev <- DeepData[13501:17000]
DeepDataTest <- DeepData[17001:20895,]

DeepDataNormTrain <- DeepDataNorm[1:13500,]
DeepDataNormDev <- DeepData[13501:17000]
DeepDataNormTest <- DeepDataNorm[17001:20895,]

DeepDataZTrain <- DeepDataZ[1:13500,]
DeepDataZDev <- DeepData[13501:17000]
DeepDataZTest <- DeepDataZ[17001:20895,]

DeepDataBinTrain <- DeepDataBin[1:13500,]
DeepDataBinDev <- DeepData[13501:17000]
DeepDataBinTest <- DeepDataBin[17001:20895,]


#5. Model training________________________________
#NNModel <- neuralnet(DeepDataBinTrain$OpenBTC ~ DeepDataBinTrain$HighBTC + DeepDataBinTrain$LowBTC + DeepDataBinTrain$CloseBTC + DeepDataBinTrain$OpenBCH + DeepDataBinTrain$HighBCH + DeepDataTrain$LowBCH + DeepDataBinTrain$CloseBCH + DeepDataBinTrain$OpenDASH + DeepDataBinTrain$HighDASH + DeepDataBinTrain$LowDASH + DeepDataBinTrain$CloseDASH + DeepDataBinTrain$OpenETH + DeepDataBinTrain$HighETH + DeepDataBinTrain$LowETH + DeepDataBinTrain$CloseETH + DeepDataBinTrain$OpenLTC + DeepDataBinTrain$HighLTC + DeepDataBinTrain$LowLTC + DeepDataBinTrain$CloseLTC, data = DeepDataBinTrain[ ,3:21], hidden = 1, linear.output = TRUE)
#n <- names(CryptoData[,3:21])
#f <- as.formula(paste('CryptoDataBinTrain$OpenBTC ~', paste(n[!n %in% ''], collapse = ' + ')))
#NNModel <- neuralnet(f, data = CryptoDataBinTrain[,3:21], hidden = 1, linear.output = TRUE)
NNModel <- neuralnet(CloseBTC ~ OpenBTC + HighBTC + LowBTC + OpenBCH + HighBCH + LowBCH + CloseBCH + OpenDASH + HighDASH + LowDASH + CloseDASH + OpenETH + HighETH + LowETH + CloseETH + OpenLTC + HighLTC + LowLTC + CloseLTC, data = DeepDataBinTrain, hidden = 1, linear.output = FALSE)

NNResult <- compute(NNModel, CryptoDataBinDev[ ,3:21])

NNPredicted <- NNResult$net.result

NNAccuracy = 0 

for(p in 1:(nrow(NNPredicted)-1))
{
  if(NNPredicted[p] > 0.5)
  {
    NNPredicted[p] = 1
  }
  else
  {
    NNPredicted[p] = 0
  }
  
  if(NNPredicted[p] == CryptoDataBinTest[p,2])
  {
    NNAccuracy = NNAccuracy + 1
  }
}

NNAccuracy/nrow(NNPredicted)*100

plot(NNModel)

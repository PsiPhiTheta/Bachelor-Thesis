#Author: Thomas Hollis
#Subject: Bachelor Thesis

#0.1 Required Packages
library(lmtest)
library(vars)

#0.2 Normalize function
normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x))-0.5)
}

#0.3 Data processing (custom data webscraped off CoinMarketCap)
GrangerData1 <- read.csv("R/DB_CMC_BTC_365.csv")
GrangerData2 <- read.csv("R/DB_CMC_LTC_365.csv")
GrangerData3 <- read.csv("R/DB_CMC_XRP_365.csv")
GrangerData4 <- read.csv("R/DB_CMC_ETH_365.csv")
GrangerData5 <- read.csv("R/DB_CMC_DASH_365.csv")
GrangerData6 <- read.csv("R/DB_CMC_BCH_365.csv")
GrangerData7 <- read.csv("R/DB_CMC_IOTA_365.csv")

GrangerData1Price_N <- normalize(GrangerData1$Close)
GrangerData2Price_N <- normalize(GrangerData2$Close)
GrangerData3Price_N <- normalize(GrangerData3$Close)
GrangerData4Price_N <- normalize(GrangerData4$Close)
GrangerData5Price_N <- normalize(GrangerData5$Close)
GrangerData6Price_N <- normalize(GrangerData6$Close)
GrangerData7Price_N <- normalize(GrangerData7$Close)

GrangerData1Price_NR <- GrangerData1Price_N[length(GrangerData1Price_N):1]
GrangerData2Price_NR <- GrangerData2Price_N[length(GrangerData2Price_N):1]
GrangerData3Price_NR <- GrangerData3Price_N[length(GrangerData3Price_N):1]
GrangerData4Price_NR <- GrangerData4Price_N[length(GrangerData4Price_N):1]
GrangerData5Price_NR <- GrangerData5Price_N[length(GrangerData5Price_N):1]
GrangerData6Price_NR <- GrangerData6Price_N[length(GrangerData6Price_N):1]
GrangerData7Price_NR <- GrangerData7Price_N[length(GrangerData7Price_N):1]

plot(GrangerData1Price_NR, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "black")
par(new = TRUE)
plot(GrangerData2Price_NR, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "blue")
par(new = TRUE)
plot(GrangerData3Price_NR, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "green")
par(new = TRUE)
plot(GrangerData4Price_NR, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "red")
par(new = TRUE)
plot(GrangerData5Price_NR, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "purple")
par(new = TRUE)
#plot(GrangerData6Price_NR, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "yellow")
#par(new = TRUE)
#plot(GrangerData7Price_NR, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "orange")

legend("topleft", legend=c("BTC", "LTC", "XRP", "ETH", "DASH"), col=c("black", "blue", "green", "red", "purple"), bty = "n", cex = 0.9, pch = 16, text.col = "black")

#Test 1: Granger with custom lag order
grangertest(GrangerData1Price_NR~GrangerData2Price_NR, order=100) #granger test with lag of 2, normalised data

#Test 2: Granger with optimal lag order via AIC
GrangerData1Price_NRD <- diff(GrangerData1Price_NR)
GrangerData2Price_NRD <- diff(GrangerData2Price_NR)
GrangerData3Price_NRD <- diff(GrangerData3Price_NR)
GrangerData4Price_NRD <- diff(GrangerData4Price_NR)
GrangerData5Price_NRD <- diff(GrangerData5Price_NR)
#GrangerData6Price_NRD <- diff(GrangerData6Price_NR)
#GrangerData7Price_NRD <- diff(GrangerData7Price_NR)

GrangerData1Price_NRD_pair1 <- cbind(GrangerData1Price_NRD, GrangerData2Price_NRD)
GrangerData_VAR=VAR(GrangerData1Price_NRD_pair1, type="const", lag.max=10, ic="AIC")

causality(GrangerData_VAR, cause = "GrangerData1Price_NRD")$Granger

#Test 3: Granger with binary data?
GrangerData1Price_NRD <- diff(GrangerData1Price_NR)
GrangerData2Price_NRD <- diff(GrangerData2Price_NR)
GrangerData3Price_NRD <- diff(GrangerData3Price_NR)
GrangerData4Price_NRD <- diff(GrangerData4Price_NR)
GrangerData5Price_NRD <- diff(GrangerData5Price_NR)
#GrangerData6Price_NRD <- diff(GrangerData6Price_NR)
#GrangerData7Price_NRD <- diff(GrangerData7Price_NR)

GrangerData1Price_NRDbin = integer(30)
GrangerData2Price_NRDbin = integer(30)
GrangerData3Price_NRDbin = integer(30)
GrangerData4Price_NRDbin = integer(30)
GrangerData5Price_NRDbin = integer(30)
#GrangerData6Price_NRDbin = integer(30)
#GrangerData7Price_NRDbin = integer(30)

for (i in 310:340)
{
  if (GrangerData1Price_NRD[i] > 0)
  {
    GrangerData1Price_NRDbin[i-311] <- 1
  }
  else
  {
    GrangerData1Price_NRDbin[i-311] <- 0
  }
}

for (i in 310:340)
{
  if (GrangerData2Price_NRD[i] > 0)
  {
    GrangerData2Price_NRDbin[i-311] <- 1
  }
  else
  {
    GrangerData2Price_NRDbin[i-311] <- 0
  }
}

for (i in 310:340)
{
  if (GrangerData3Price_NRD[i] > 0)
  {
    GrangerData3Price_NRDbin[i-311] <- 1
  }
  else
  {
    GrangerData3Price_NRDbin[i-311] <- 0
  }
}

for (i in 310:340)
{
  if (GrangerData4Price_NRD[i] > 0)
  {
    GrangerData4Price_NRDbin[i-311] <- 1
  }
  else
  {
    GrangerData4Price_NRDbin[i-311] <- 0
  }
}

for (i in 310:340)
{
  if (GrangerData5Price_NRD[i] > 0)
  {
    GrangerData5Price_NRDbin[i-311] <- 1
  }
  else
  {
    GrangerData5Price_NRDbin[i-311] <- 0
  }
}

GrangerData1Price_NRDbin
GrangerData2Price_NRDbin
GrangerData3Price_NRDbin
GrangerData4Price_NRDbin
GrangerData5Price_NRDbin

plot(GrangerData1Price_NRDbin, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "black")
plot(GrangerData2Price_NRDbin, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "blue")
plot(GrangerData3Price_NRDbin, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "green")
plot(GrangerData4Price_NRDbin, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "red")
plot(GrangerData5Price_NRDbin, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "purple")

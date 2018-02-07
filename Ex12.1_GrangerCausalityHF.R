library(lmtest)
library(vars)

normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x))-0.5)
}

GrangerData1 <- read.csv("R/Poloniex_BTCUSD_m.csv")
GrangerData2 <- read.csv("R/Poloniex_BCHUSD_m.csv")
GrangerData3 <- read.csv("R/Poloniex_DASHUSD_m.csv")
GrangerData4 <- read.csv("R/Poloniex_ETCUSD_m.csv")
GrangerData5 <- read.csv("R/Poloniex_ETHUSD_m.csv")
GrangerData6 <- read.csv("R/Poloniex_XMRUSD_m.csv")

GrangerData1Price_N <- GrangerData1$Close
GrangerData2Price_N <- GrangerData2$Close
GrangerData3Price_N <- GrangerData3$Close
GrangerData4Price_N <- GrangerData4$Close
GrangerData5Price_N <- GrangerData5$Close
GrangerData6Price_N <- GrangerData6$Close

GrangerData1Price_NR <- GrangerData1Price_N[50000:1]
GrangerData2Price_NR <- GrangerData2Price_N[50000:1]
GrangerData3Price_NR <- GrangerData3Price_N[50000:1]
GrangerData4Price_NR <- GrangerData4Price_N[50000:1]
GrangerData5Price_NR <- GrangerData5Price_N[50000:1]
GrangerData6Price_NR <- GrangerData6Price_N[50000:1]

GrangerData1Price_NR <- normalize(GrangerData1Price_NR)
GrangerData2Price_NR <- normalize(GrangerData2Price_NR)
GrangerData3Price_NR <- normalize(GrangerData3Price_NR)
GrangerData4Price_NR <- normalize(GrangerData4Price_NR)
GrangerData5Price_NR <- normalize(GrangerData5Price_NR)
GrangerData6Price_NR <- normalize(GrangerData6Price_NR)



plot(GrangerData1Price_NR, xlab = "Days elapsed", ylab = "Price (USD)", type = "l", col = "black")
par(new = TRUE)
plot(GrangerData2Price_NR, xlab = "", ylab = "", type = "l", col = "blue", axes = FALSE)
par(new = TRUE)
plot(GrangerData3Price_NR, xlab = "", ylab = "", type = "l", col = "green", axes = FALSE)
par(new = TRUE)
plot(GrangerData4Price_NR, xlab = "", ylab = "", type = "l", col = "red", axes = FALSE)
par(new = TRUE)
plot(GrangerData5Price_NR, xlab = "", ylab = "", type = "l", col = "purple", axes = FALSE)
par(new = TRUE)
plot(GrangerData6Price_NR, xlab = "", ylab = "", type = "l", col = "yellow", axes = FALSE)

legend("topleft", legend=c("BTC", "BCH", "DASH", "ETC", "ETH", "XMR"), col=c("black", "blue", "green", "red", "purple", "yellow"), bty = "n", cex = 0.9, pch = 16, text.col = "black")

#Test 1: Granger with custom lag order
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=100) #granger test with lag of 2, normalised data
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=200)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=300)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=400)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=500)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=600)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=700)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=800)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=900)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=1000)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=1100)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=1200)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=1300)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=1400)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=1500)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=1600)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=1700)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=1800)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=1900)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=2000)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=2100)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=2200)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=2300)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=2400)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=2500)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=2600)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=2700)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=2800)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=2900)
grangertest(GrangerData2Price_NR~GrangerData3Price_NR, order=3000)

#Test 1.5: Granger using different package

GrangerDataPair_BCH_DASH <- cbind(GrangerData2Price_NR, GrangerData3Price_NR)
granger.test(GrangerDataPair_BCH_DASH, 3000)


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
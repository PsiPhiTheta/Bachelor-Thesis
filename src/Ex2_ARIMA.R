#Author: Thomas Hollis
#Subject: Bachelor Thesis

#1. Data Import
data <- read.csv("R/BTC.csv")

#2. Data Split
data_train <- data[1:282,2]
data_test <- data[283:376,2]

#3. Apply Model
btc_ar <- arima(data_train, order = c(1, 2, 1), method = "CSS")
btc_pred <- predict(object = btc_ar, n.ahead = 94)

#4. Display & Refine Model
xvals <- seq(283,376,1)
plot(data_train, ylim=range(data[,2]), xlim=range(1,376), xlab="Hours Elapsed", ylab="BTC Price (USD)", type = "l", col = "black")
lines(x = xvals, data_test, ylim=range(data[,2]), xlim=range(1,376), xlab = "", ylab = "", type = "l", col = "black")
lines(1:length(data_train),fitted(btc_ar),type="l",col="red")

btc_ar <- arima(data_train, order = c(1,3,1), method = "CSS")
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = xvals, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), xlab = "", ylab = "", type = "l", col = "green")

btc_ar <- arima(data_train, order = c(3,4,5), method = "CSS")
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = xvals, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), xlab = "", ylab = "", type = "l", col = "blue")

btc_ar <- arima(data_train, order = c(5,5,5), method = "CSS")
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = xvals, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), xlab = "", ylab = "", type = "l", col = "orange")


legend("topleft", legend=c("BTC Data", "ARMA Train", "ARIMA(1, 3, 1) Predict", "ARIMA(3, 4, 5) Predict", "ARIMA(5, 5, 5) Predict"), col=c("black", "red", "green", "blue", "orange"), bty = "n", cex = 0.9, pch = 16, text.col = "black")

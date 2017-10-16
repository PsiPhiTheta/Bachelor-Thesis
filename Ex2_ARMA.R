data <- read.csv("R/BTC400h.csv")

data_train <- data[1:282,2]
data_test <- data[283:376,2]

btc_ar <- arima(data_train, order = c(3, 1, 1), method = "CSS")
btc_pred <- predict(object = btc_ar, n.ahead = 94)

x1 <- seq(283,376,1)
plot(data_train, ylim=range(data[,2]), xlim=range(1,376), xlab="Hours Elapsed", ylab="BTC Price (USD)", type = "l", col = "black")
lines(x = x1, data_test, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "black")
lines(x = x1, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "red")

btc_ar <- arima(data_train, order = c(4,1,3), method = "CSS")
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = x1, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "green")

btc_ar <- arima(data_train, order = c(1,1,3), method = "CSS")
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = x1, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "blue")

btc_ar <- arima(data_train, order = c(4,1,5), method = "CSS")
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = x1, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "brown")

btc_ar <- arima(data_train, order = c(2,1,3), method = "CSS")
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = x1, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "pink")

legend("topleft", legend=c("Coefficients: 3, 1, 1", "Coefficients: 4, 1, 3", "Coefficients: 1, 1, 3", "Coefficients: 4, 1, 5", "Coefficients: 2, 3, 3"), col=c("red", "green", "blue", "brown", "pink"), bty = "n", cex = 0.9, pch = 16, text.col = "black")
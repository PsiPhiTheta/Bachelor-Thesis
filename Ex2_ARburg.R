data <- read.csv("R/BTC.csv")

data_train <- data[1:282,2]
data_test <- data[283:376,2]

btc_ar <- ar.burg(data_train, order.max = 1, aic = FALSE)
btc_pred <- predict(object = btc_ar, n.ahead = 94)

x1 <- seq(283,376,1)
plot(data_train, ylim=range(data[,2]), xlim=range(1,376), xlab="Hours Elapsed", ylab="BTC Price (USD)", type = "l", col = "black")
lines(x = x1, data_test, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "black")

lines(x = x1, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "red")

btc_ar <- ar.burg(data_train, order.max = 2, aic = FALSE)
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = x1, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "green")

btc_ar <- ar.burg(data_train, order.max = 5, aic = FALSE)
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = x1, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "blue")

btc_ar <- ar.burg(data_train, order.max = 50, aic = FALSE)
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = x1, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "brown")

btc_ar <- ar.burg(data_train, order.max = 250, aic = FALSE)
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = x1, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "pink")

legend("topleft", legend=c("1st Order", "2nd Order", "5th Order", "50th Order", "250th Order"), col=c("red", "green", "blue", "brown", "pink"), bty = "n", cex = 0.9, pch = 16, text.col = "black")
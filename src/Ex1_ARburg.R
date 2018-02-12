#Author: Thomas Hollis
#Subject: Bachelor Thesis
 
#1. Data Import
data <- read.csv("R/BTC.csv")

#2. Data Split
data_train <- data[1:282,2]
data_test <- data[283:376,2]

#3. Apply Model
btc_ar <- ar.burg(data_train, order.max = 1, aic = FALSE)
btc_pred <- predict(object = btc_ar, n.ahead = 94)

#4. Display
xvals <- seq(283,376,1)
plot(data_train, ylim=range(data[,2]), xlim=range(1,376), xlab="Hours Elapsed", ylab="BTC Price (USD)", type = "l", col = "black")
lines(x = xvals, data_test, ylim=range(data[,2]), xlim=range(1,376), xlab = "", ylab = "", type = "l", col = "black")
points(1:length(data_train),fitted(btc_ar),type="l",col="red") 
lines(x = xvals, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), xlab = "", ylab = "", type = "l", col = "red")

btc_ar <- ar.burg(data_train, order.max = 2, aic = FALSE)
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = xvals, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), xlab = "", ylab = "", type = "l", col = "green")

btc_ar <- ar.burg(data_train, order.max = 5, aic = FALSE)
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = xvals, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), xlab = "", ylab = "", type = "l", col = "green")

btc_ar <- ar.burg(data_train, order.max = 50, aic = FALSE)
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = xvals, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), xlab = "", ylab = "", type = "l", col = "blue")

btc_ar <- ar.burg(data_train, order.max = 250, aic = FALSE)
btc_pred <- predict(object = btc_ar, n.ahead = 94)
lines(x = xvals, btc_pred$pred, ylim=range(data[,2]), xlim=range(1,376), xlab = "", ylab = "", type = "l", col = "orange")

legend("topleft", legend=c("BTC Data", "AR Train", "AR Predict (1st Order)", "AR Predict (2nd Order)", "AR Predict (5th Order)", "AR Predict (50th Order)", "AR Predict (250th Order)"), col=c("black", "red", "pink", "brown", "green", "blue", "orange"), bty = "n", cex = 0.9, pch = 16, text.col = "black")


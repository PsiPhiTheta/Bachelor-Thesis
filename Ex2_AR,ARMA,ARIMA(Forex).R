data <- read.csv("R/EUR-GBP400h.csv")

data_train <- data[1:251,2]
data_test <- data[251:335,2]

curr_ar <- arima(data_train, order = c(1, 5, 1), method = "CSS")
curr_pred <- predict(object = curr_ar, n.ahead = 84)

x1 <- seq(251,335,1)
plot(data_train, ylim=range(0.88,0.903), xlim=range(1,335), xlab="Hours Elapsed", ylab="EUR Price (GBP)", type = "l", col = "black")
par(new = TRUE)
plot(x = x1, y = data_test, ylim=range(0.88,0.903), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "black")
par(new = TRUE)
plot(curr_pred$pred, ylim=range(0.88,0.903), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "red")
par(new = TRUE)

curr_ar <- arima(data_train, order = c(3,3,3), method = "CSS")
curr_pred <- predict(object = curr_ar, n.ahead = 84)
plot(curr_pred$pred, ylim=range(data[,2]), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "green")
par(new = TRUE)

curr_ar <- arima(data_train, order = c(5,5,5), method = "CSS")
curr_pred <- predict(object = curr_ar, n.ahead = 84)
plot(curr_pred$pred, ylim=range(data[,2]), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "blue")
par(new = TRUE)

curr_ar <- arima(data_train, order = c(2,4,2), method = "CSS")
curr_pred <- predict(object = curr_ar, n.ahead = 84)
plot(curr_pred$pred, ylim=range(data[,2]), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "brown")
par(new = TRUE)

curr_ar <- arima(data_train, order = c(2,5,3), method = "CSS")
curr_pred <- predict(object = curr_ar, n.ahead = 84)
plot(curr_pred$pred, ylim=range(data[,2]), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "pink")
par(new = TRUE)

legend("topleft", legend=c("Coefficients: 1, 5, 1", "Coefficients: 3, 3, 3", "Coefficients: 5, 5, 5", "Coefficients: 2, 4, 2", "Coefficients: 2, 5, 3"), col=c("red", "green", "blue", "brown", "pink"), bty = "n", cex = 0.9, pch = 16, text.col = "black")
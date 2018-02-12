#Author: Thomas Hollis
#Subject: Bachelor Thesis

library(fGarch)

data <- read.csv("R/EUR-GBP400 (40h 44m)h.csv")

data_train <- data[1:251,2]
data_test <- data[252:335,2]

curr_ar <- garchFit(~1+garch(1,0),data = data_train,trace=F)
curr_pred <- predict(curr_ar, n.ahead = 84)

x1 <- seq(252,335,1)
plot(data_train, ylim=range(data[,2]), xlim=range(1,335), xlab="Hours Elapsed", ylab="EUR Price (GBP)", type = "l", col = "black")
par(new = TRUE)
plot(x = x1, data_test, ylim=range(data[,2]), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "black")
par(new = TRUE)
plot(x = x1, curr_pred$meanForecast, ylim=range(data[,2]), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "red")
par(new = TRUE)

curr_ar <- garchFit(~1+garch(2,0),data = data_train,trace=F)
curr_pred <- predict(curr_ar, n.ahead = 84)
plot(x = x1, curr_pred$meanForecast, ylim=range(data[,2]), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "green")
par(new = TRUE)

curr_ar <- garchFit(~1+garch(5,0),data = data_train,trace=F)
curr_pred <- predict(curr_ar, n.ahead = 84)
plot(x = x1, curr_pred$meanForecast, ylim=range(data[,2]), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "blue")
par(new = TRUE)

curr_ar <- garchFit(~1+garch(10,0),data = data_train,trace=F)
curr_pred <- predict(curr_ar, n.ahead = 84)
plot(x = x1, curr_pred$meanForecast, ylim=range(data[,2]), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "brown")
par(new = TRUE)

curr_ar <- garchFit(~1+garch(20,0),data = data_train,trace=F)
curr_pred <- predict(curr_ar, n.ahead = 84)
plot(x = x1, curr_pred$meanForecast, ylim=range(data[,2]), xlim=range(1,335), axes = FALSE, xlab = "", ylab = "", type = "l", col = "orange")
par(new = TRUE)

legend("topleft", legend=c("ARCH(1)", "ARCH(2)", "ARCH(5)", "ARCH(10)", "ARCH(20)"), col=c("red", "green", "blue", "brown", "orange"), bty = "n", cex = 0.9, pch = 16, text.col = "black")

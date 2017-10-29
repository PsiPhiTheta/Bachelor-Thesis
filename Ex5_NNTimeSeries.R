#Author: Thomas Hollis
#Subject: Bachelor Thesis

library(caret)
library(lattice)
library(forecast)

data <- read.csv("R/BTC.csv")

data_train <- data[1:282,2]
data_test <- data[283:376,2]

model <- nnetar(data_train)

xvalues <- seq(283,376,1)
plot(forecast(model,h=93), xlab = "Hours Elapsed", ylab = "BTC Price (USD)")
lines(x = xvalues, data_test, ylim=range(data[,2]), xlim=range(1,376), axes = FALSE, xlab = "", ylab = "", type = "l", col = "red")
points(1:length(data_train),fitted(model),type="l",col="blue")

legend("topleft", legend=c("BTC Data (Train)", "BTC Data (Test)", "NNetAR(3,2) Forecast"), col=c("black", "red", "blue"), bty = "n", cex = 0.9, pch = 16, text.col = "black")

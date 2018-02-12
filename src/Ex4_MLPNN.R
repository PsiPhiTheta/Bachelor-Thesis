#Author: Thomas Hollis
#Subject: Bachelor Thesis

MNistdata <- read.csv("MNIST_train.csv")

MNistTrain <- MNistdata[1:2000, ]
MNistTest <- MNistdata[2001:2500, ]

MNistTrain <- cbind(MNistTrain[, 2:785], class.ind(MNistTrain$label))
colLabel <- c("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

colnames(MNistTrain)[785:794] <- colLabel

n <- names(MNistTrain[,1:784])
f <- as.formula(paste('zero + one + two + three + four + five + six + seven + eight + nine ~', paste(n[!n %in% ''], collapse = ' + ')))

MNistModel <- neuralnet(f, data = MNistTrain,
                        hidden = 30,
                        linear.output = FALSE)

model_result <- compute(MNistModel, MNistTest[ ,1:784])

predicted <- model_result$net.result


MNistTest <- cbind(MNistTest[, 2:785], class.ind(MNistTest$label))
colnames(MNistTest)[785:794] <- colLabel

picks<-(0:9)[apply(predicted,1,which.max)]

prop.table(table(MNistTest[, 785:794] == picks))

#Needs further development

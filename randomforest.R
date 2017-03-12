library(randomForest)
library(xtable)
library(ggplot2)
data.total <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/organized data/pm1107(cv).csv")

time.year <- 10:14
time.month <- 1:12

#model fitting (including time lag 1; ntree = 1000)
rmse <- data.frame(rep(NA, 12), NA, NA, NA, NA)
for (i in time.year){
  for ( j in time.month){
    data <- subset(data.total, month== j & year== i & pmissed==0 & wmissed==0 & pm2.5lag != -666666 & pm2.5!= -666666)
    data <- data[,c("pm2.5", "hour", "pm2.5lag","TEMPimpute", "DEWPimpute", "PRESimpute", "HUMIimpute", "wd", "Iws")]
    result <- randomForest(data[,-1], data[,1], prox=TRUE, importance=TRUE,ntree=1000)
    rmse[j, i-9] <- sqrt(mean((predict(result, data[,-1]) - data[,1])^2))
  }
}
print(rmse)
xtable(rmse)

#model fitting (excluding time lag 1; ntree = 1000 )
rmse <- data.frame(time.month, NA, NA, NA, NA, NA)
for (i in time.year){
  for ( j in time.month){
    data <- subset(data.total, month== j & year== i & pmissed==0 & wmissed==0 & pm2.5lag != -666666 & pm2.5!= -666666)
    data <- data[,c("pm2.5","hour", "TEMPimpute", "DEWPimpute", "PRESimpute", "HUMIimpute", "wd", "Iws")]
    result <- randomForest(data[,-1], data[,1], prox=TRUE, importance=TRUE,ntree=1000)
    rmse[j, i-8] <- sqrt(mean((predict(result, data[,-1]) - data[,1])^2))
  }
}
print(rmse)
xtable(rmse)

#draw some plots
par(mfrow=c(2,2))
data <- subset(data.total, month== 10 & year== 14 & pmissed==0 & wmissed==0 & pm2.5lag != -666666 & pm2.5!= -666666)
data <- data[,c("pm2.5", "pm2.5lag","TEMPimpute", "DEWPimpute", "PRESimpute", "HUMIimpute", "wd", "Iws")]
result <- randomForest(data[,-1], data[,1], prox=TRUE, importance=TRUE,ntree=1000)
prediction1 <- predict(result, data[,-1])
data <- subset(data.total, month== 10 & year== 14 & pmissed==0 & wmissed==0 & pm2.5lag != -666666 & pm2.5!= -666666)
data <- data[,c("pm2.5", "TEMPimpute", "DEWPimpute", "PRESimpute", "HUMIimpute", "wd", "Iws")]
result <- randomForest(data[,-1], data[,1], prox=TRUE, importance=TRUE,ntree=1000)
prediction2 <- predict(result, data[,-1])

plot(1:length(data[,1]), data[,1], type="l", add=T, xlab="days in Oct 2014", ylab="pm25")
points(1:length(data[,1]), prediction1, type="l", col="red", add=T)
points(1:length(data[,1]), prediction2, type="l", col="blue", add=T)


com.result <- data.frame(days=rep(1:length(data[,1]),3), 
                         pm25=c(data[,1], prediction1, prediction2),
                         type=c(rep("True value", length(data[,1])),
                                rep("with time lag", length(data[,1])),
                                rep("withou time lag", length(data[,1]))))

qplot(days, pm25, data=com.result, color=type, geom="line")+theme_classic()

#one-step prediction of randomforest 
rmse <- data.frame(rep(NA, 10))
for ( j in 1:10){
    data <- subset(data.total, month== j & year!=14 & pmissed==0 & wmissed==0 & pm2.5lag != -666666 & pm2.5!= -666666)
    data <- data[,c("pm2.5", "pm2.5lag","TEMPimpute", "DEWPimpute", "PRESimpute", "HUMIimpute", "wd", "Iws")]
    result <- randomForest(data[,-1], data[,1], prox=TRUE, importance=TRUE, ntree=1000)
    data.14 <- subset(data.total, month== j & year==14 & pmissed==0 & wmissed==0 & pm2.5lag != -666666 & pm2.5!= -666666)
    data.14 <- data.14[,c("pm2.5", "pm2.5lag","TEMPimpute", "DEWPimpute", "PRESimpute", "HUMIimpute", "wd", "Iws")]
    rmse[j, 1] <- sqrt(mean((predict(result, data.14[,-1]) - data.14[,1])^2))
    print(rmse[j,1])
}
print(rmse)
xtable(rmse)

# draw plots of prediction
par(mfrow=c(2,1))
j=10
data <- subset(data.total, month== j & year!=14 & pmissed==0 & wmissed==0 & pm2.5lag != -666666 & pm2.5!= -666666)
data <- data[,c("pm2.5", "pm2.5lag","TEMPimpute", "DEWPimpute", "PRESimpute", "HUMIimpute", "wd", "Iws")]
result <- randomForest(data[,-1], data[,1], prox=TRUE, importance=TRUE, ntree=1000)
data.14 <- subset(data.total, month== j & year==14 & pmissed==0 & wmissed==0 & pm2.5lag != -666666 & pm2.5!= -666666)
data.14 <- data.14[,c("pm2.5", "pm2.5lag","TEMPimpute", "DEWPimpute", "PRESimpute", "HUMIimpute", "wd", "Iws")]

plot(1:length(data.14[,1]), data.14[,1], type="l", add=T, xlab="days in Oct 2014", ylab="pm25")
points(1:length(data.14[,1]), predict(result, data.14[,-1]), type="l", col="red", add=T)

#comparison of random forest and nonparametric regression
data.new <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/comparison.csv", head=T)
colnames(data.new) <- c("pm2.5", "pm2.5lag","TEMPimpute", "DEWPimpute", "PRESimpute", "HUMIimpute", "wd", "Iws")
data.new[,c("TEMPimpute")] <- as.numeric(data.new[,c("TEMPimpute")])
data.new[,c("PRESimpute")] <- as.numeric(data.new[,c("PRESimpute")])
data.new[,c("HUMIimpute")] <- as.numeric(data.new[,c("HUMIimpute")])

data <- subset(data.total, month ==11&pmissed==0 & wmissed==0 & pm2.5lag != -666666 & pm2.5!= -666666)
levels(data.new$wd) <- levels(data$wd)
data <- data[,c("pm2.5", "pm2.5lag","TEMPimpute", "DEWPimpute", "PRESimpute", "HUMIimpute", "wd", "Iws")]
result <- randomForest(data[,-1], data[,1], prox=TRUE, importance=TRUE, ntree=1000)
b <- predict(result, data.new[,-1])

a <-matrix(NA, 19, 6)
a[1,] <- c(46.05918, 38.98995, 39.20686, 40.91716, 41.33459, 40.11214)
a[2,] <- c(55.5, 37.74196, 41.41011, 51.17137, 51.46361, 45.44676)
a[3,] <- c(69.05714, 66.01084, 65.1165, 64.65766, 65.17906, 65.24101)
a[4,] <- c(78.74082, 77.3447, 77.37365, 73.99151, 75.8963, 76.15154)
a[5,] <- c(76.80408, 89.52627, 89.17628, 87.16172, 88.83801, 88.67557)
a[6,] <- c(65.18367, 76.21304, 75.08419, 77.83419, 78.59335, 76.93119)
a[7,] <- c(69.05714, 76.91321, 61.88213, 77.68875, 78.27504,73.68978)
a[8,] <- c(74.86735, 71.78563, 71.35849, 73.48442, 73.40599, 72.50863)
a[9,] <- c(78.74082, 74.61487, 86.23462, 72.06773, 75.6329, 77.13753)
a[10,] <- c(76.80408, 62.63451, 26.03094, 85.62571, 88.74945, 65.76015)
a[11,] <- c(86.48776, 75.24811, 74.07649, 84.43138, 83.04449, 79.20012)
a[12,] <- c(80.67755, 95.04228, 92.49785, 94.95166, 95.69049, 94.54557)
a[13,] <- c(72.93061, 81.71642, 76.39196, 86.75335, 86.12184, 82.74589)
a[14,] <- c(76.80408, 64.34341, 64.322, 68.29515, 63.88022, 65.21019)
a[15,] <- c(82.61429, 46.91453, 45.38625, 73.96168, 77.76426, 61.00668)
a[16,] <- c(86.48776, 85.64846, 85.48534, 83.55493, 85.78633, 85.11876)
a[17,] <- c(90.36122, 105.2628, 106.3408, 88.63036, 88.33892, 97.14323)
a[18,] <- c(76.80408, 94.15985, 94.32141, 95.52966, 94.2229, 94.55845)
a[19,] <- c(72.93061, 71.86608, 71.94751, 64.49069, 63.27486, 67.89478)
a <- cbind(a, b)

sqrt(mean((a[,1] - a[,2])^2))
sqrt(mean((a[,1] - a[,3])^2))
sqrt(mean((a[,1] - a[,4])^2))
sqrt(mean((a[,1] - a[,5])^2))
sqrt(mean((a[,1] - a[,6])^2))
sqrt(mean((a[,1] - a[,7])^2))

plot(0:18, a[,1], type="l", lty=1, ylim=c(0,130))
lines(0:18, a[,7], lty=2, )
xtable(a)
#importance
data <- subset(data.total, month== 10 & year== 14 & pmissed==0 & wmissed==0 & pm2.5lag != -666666 & pm2.5!= -666666)
data <- data[,c("pm2.5","TEMPimpute", "DEWPimpute", "PRESimpute", "HUMIimpute", "wd", "Iws")]
result <- randomForest(data[,-1], data[,1], prox=TRUE, importance=TRUE,ntree=1000)
xtable(importance(result))
plot(result, main="MSE and the numbers of trees")
varImpPlot(result, main="visualization of variables' importance")
partialPlot(result, data, x.var=pm2.5lag)
partialPlot(result, data, x.var=PRESimpute)
partialPlot(result, data, x.var=Iws)
partialPlot(result, data, x.var=TEMPimpute)
partialPlot(result, data, x.var=DEWPimpute)
partialPlot(result, data, x.var=wd)


data <- subset(data.total, month== 10 & year== 14)
data <- data[,c("pm2.5","TEMPimpute", "DEWPimpute", "PRESimpute", "wd", "Iws")]
result <- randomForest(data[,-1], data[,1], prox=TRUE, importance=TRUE,ntree=1000)
xtable(importance(result))


plot(result)
varImpPlot(result)

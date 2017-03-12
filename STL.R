library(plyr)
library(ggplot2)
library(TTR)
data.total <- read.csv("F:/Dropbox/projects/pm2.5/data/organized data/pm1107(cv).csv")
data <- subset(data.total[,c(3,4,5, 10)], pm2.5!=-666666)
data <- ddply(data, .(year, month), summarize,
              daily.mean = log(mean(pm2.5)))
data.14 <- subset(data, year==14)

qplot(1:length(daily.mean), daily.mean, data=data, geom="line")
plot.ts(SMA(data.14$daily.mean,n=2))


decompose(SMA(data.14$daily.mean,n=20))

plot(stl(SMA(data.14$daily.mean,n=20), "per"))
plot(stl(data$daily.mean, s.window = 12))

plot(stllc <- stl(log(co2), s.window = 21))
summary(stllc)

acf(data$daily.mean)

library(lmom)

data.total <- read.csv("F:/Dropbox/projects/pm2.5/data/organized data/pm1107(cv).csv")
time.year <- 10:14
time.month <- 1:12
mon.max <- data.frame(rep(NA, 12*5), NA, NA)
k=0
for (i in time.year){
  for ( j in time.month){
    k = k+1
    data <- subset(data.total, month==j & year==i & pm2.5!= -666666)
    mon.max[k,] <- c(i, j, max(data$pm2.5))
  }
}
print(mon.max)
mon.max <- mon.max[-c(59,60),]

par.pe3 <- pelpe3(samlmu(mon.max[,3], nmom=3))
plot(quantile(mon.max[,3], probs=seq(0, 1, 0.01)), seq(0, 1, 0.01), 
                       xlab="maximum hourly precipitation", ylab="cdf", 
                       main="Fitting by Person Type III distribution", pch=16, cex=0.7)
lines(seq(200, 1000, 10), cdfpe3(seq(200, 1000, 10), par=par.pe3), col="red", cex=3)
                  
par.gev <- pelgev(samlmu(mon.max[,3], nmom=3))
plot(quantile(mon.max[,3], probs=seq(0, 1, 0.01)), seq(0, 1, 0.01), 
                       xlab="Maximum PM2.5 Values", ylab="cdf", 
                       main="Maximum PM2.5 Values of Beijing Fitted by GEV", pch=16, cex=0.5)
lines(seq(200, 1000, 10), cdfgev(seq(200, 1000, 10), par=par.gev), col="red", cex=3, size=1)
                  
library(lubridate)
library(tseries)
library(np)
library(MSBVAR)
library(xtable)

#pm data input
data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/pm0331(cvs).csv", header=T)
data <- subset(data, year==14&month==9)
data$pm2.5 [data$pm2.5 <= 0] <- NA
data$pm2.5lag [data$pm2.5lag <= 0] <- NA

#other pollutants
no.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/beijing_nongzhanguan.csv", header=T)
no.data$NO2 <- as.numeric(as.character(no.data$NO2))
no.data$CO <- as.numeric(as.character(no.data$CO))
no.data$SO2 <- as.numeric(as.character(no.data$SO2))
no.data$PM10 <- as.numeric(as.character(no.data$PM10))
no.data$O3 <- as.numeric(as.character(no.data$O3))
no.data$Time <- ymd_hms(no.data$Time)
no.data$year <- year(no.data$Time)
no.data$month <- month(no.data$Time)
no.data$day <- day(no.data$Time)
no.data$hour <- hour(no.data$Time)
no.data$weekday <- wday(no.data$Time)

no.data$NO2 [no.data$NO2<=0] <- NA
no.data$CO [no.data$CO<=0] <- NA
no.data$SO2 [no.data$SO2<=0] <- NA
no.data$PM10 [no.data$PM10<=0] <- NA
no.data$O3 [no.data$O3<=0] <- NA

no.data$NO2lag <- c(NA, no.data$NO2[1:(nrow(no.data)-1)])
no.data$COlag <- c(NA, no.data$CO[1:(nrow(no.data)-1)])
no.data$SO2lag <- c(NA, no.data$SO2[1:(nrow(no.data)-1)])
no.data$PM10lag <- c(NA, no.data$PM10[1:(nrow(no.data)-1)])
no.data$O3lag <- c(NA, no.data$O3[1:(nrow(no.data)-1)])

no.data <- subset(no.data, year==2014&month==9)
data$NO2 <- no.data$NO2
data$CO <- no.data$CO
data$SO2 <- no.data$SO2
data$PM10 <- no.data$PM10
data$O3 <- no.data$O3
data$NO2lag <- no.data$NO2lag
data$COlag <- no.data$COlag
data$SO2lag <- no.data$SO2lag
data$PM10lag <- no.data$PM10lag
data$O3lag <- no.data$O3lag

#relationship between pollutants 
par(mfrow=c(3,2))
plot(data$PM10, data$pm2.5,xlab="PM10", ylab="PM2.5")
plot(data$NO2, data$pm2.5,xlab="NO2", ylab="PM2.5")
plot(data$SO2, data$pm2.5,xlab="SO2", ylab="PM2.5")
plot(data$CO, data$pm2.5,xlab="CO", ylab="PM2.5")
plot(data$O3, data$pm2.5,xlab="O3", ylab="PM2.5")
par(mfrow=c(3,2))
plot(data$pm2.5,type="l", main="PM2.5",ylab="")
plot(data$PM10, type="l", main="PM10",ylab="")
plot(data$NO2, type="l", main="NO2",ylab="")
plot(data$SO2, type="l", main="SO2",ylab="")
plot(data$CO, type="l", main="CO",ylab="")
plot(data$O3, type="l", main="O3",ylab="")

tmp <- matrix(NA,30,6)
rownames(tmp)<- rownames(granger.test(data[,c(11,31,32,33,34,35)],p=1))
for(i in 1:6){
  tmp[,i] <- granger.test(data[,c(11,31,32,33,34,35)],p=i)[,2]
}
xtable(round(tmp,2))
adf.test(data$error)
Box.test(reg.no$residuals, type="Ljung-Box")
Box.test(reg.no$mean, type="Ljung-Box")


#Partially linear nonparametric model
bw.pm25 <- npplregbw(ydat=data$pm2.5,
                     zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                     xdat=data.frame(data$pm2.5lag,data$PM10lag,data$NO2lag,data$SO2lag,data$COlag,data$O3lag))
reg.pl.pm25 <- npplreg(bw.pm25,residuals=TRUE)
data$reg.pl.pm25.resid [!is.na(data$pm2.5+data$pm2.5lag+
                                 data$PM10lag+data$NO2lag+data$SO2lag+data$COlag+data$O3lag)] <- reg.pl.pm25$resid
round(sqrt(mean(data$reg.pl.pm25.resid^2,na.rm=TRUE)),2)
abs(reg.pl.pm25$xcoef/reg.pl.pm25$xcoeferr) < qnorm(0.975)
round(reg.pl.pm25$xcoef,2)
round(reg.pl.pm25$xcoeferr,2)


bw.pm10 <- npplregbw(ydat=data$PM10,
                     zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                     xdat=data.frame(data$pm2.5lag,data$NO2lag,data$SO2lag,data$COlag,data$O3lag))
reg.pl.pm10 <- npplreg(bw.pm10,residuals=TRUE)
data$reg.pl.pm10.resid [!is.na(data$PM10+data$pm2.5lag+
                                 data$NO2lag+data$SO2lag+data$COlag+data$O3lag)] <- reg.pl.pm10$resid
round(sqrt(mean(data$reg.pl.pm10.resid^2,na.rm=TRUE)),2)
abs(reg.pl.pm10$xcoef/reg.pl.pm10$xcoeferr) < qnorm(0.975)
round(reg.pl.pm10$xcoef,2)
round(reg.pl.pm10$xcoeferr,2)

bw.no2 <- npplregbw(ydat=data$NO2,
                     zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                     xdat=data.frame(data$pm2.5lag,data$PM10lag,data$SO2lag,data$COlag,data$O3lag))
reg.pl.no2 <- npplreg(bw.no2,residuals=TRUE)
data$reg.pl.no2.resid [!is.na(data$NO2+data$pm2.5lag+
                                 data$PM10lag+data$SO2lag+data$COlag+data$O3lag)] <- reg.pl.no2$resid
round(sqrt(mean(data$reg.pl.no2.resid^2,na.rm=TRUE)),2)
abs(reg.pl.no2$xcoef/reg.pl.no2$xcoeferr) < qnorm(0.975)
round(reg.pl.no2$xcoef,2)
round(reg.pl.no2$xcoeferr,2)

bw.so2 <- npplregbw(ydat=data$SO2,
                    zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                    xdat=data.frame(data$pm2.5lag,data$PM10lag,data$NO2lag,data$COlag,data$O3lag))
reg.pl.so2 <- npplreg(bw.so2,residuals=TRUE)
data$reg.pl.so2.resid [!is.na(data$SO2+data$pm2.5lag+
                                data$PM10lag+data$NO2lag+data$COlag+data$O3lag)] <- reg.pl.so2$resid
round(sqrt(mean(data$reg.pl.so2.resid^2,na.rm=TRUE)),2)
abs(reg.pl.so2$xcoef/reg.pl.so2$xcoeferr) < qnorm(0.975)
round(reg.pl.so2$xcoef,2)
round(reg.pl.so2$xcoeferr,2)

bw.co <- npplregbw(ydat=data$CO,
                    zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                    xdat=data.frame(data$pm2.5lag,data$PM10lag,data$NO2lag,data$SO2lag,data$O3lag))
reg.pl.co <- npplreg(bw.co,residuals=TRUE)
data$reg.pl.co.resid [!is.na(data$CO+data$pm2.5lag+
                                data$PM10lag+data$NO2lag+data$SO2lag+data$O3lag)] <- reg.pl.co$resid
round(sqrt(mean(data$reg.pl.co.resid^2,na.rm=TRUE)),2)
abs(reg.pl.co$xcoef/reg.pl.co$xcoeferr) < qnorm(0.975)
round(reg.pl.co$xcoef,3)
round(reg.pl.co$xcoeferr,3)

bw.o3 <- npplregbw(ydat=data$O3,
                   zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                   xdat=data.frame(data$pm2.5lag,data$PM10lag,data$NO2lag,data$SO2lag,data$COlag))
reg.pl.o3 <- npplreg(bw.o3,residuals=TRUE)
data$reg.pl.o3.resid [!is.na(data$O3+data$pm2.5lag+
                                data$PM10lag+data$NO2lag+data$SO2lag+data$COlag)] <- reg.pl.o3$resid
round(sqrt(mean(data$reg.pl.o3.resid^2,na.rm=TRUE)),2)
abs(reg.pl.o3$xcoef/reg.pl.o3$xcoeferr) < qnorm(0.975)
round(reg.pl.o3$xcoef,2)
round(reg.pl.o3$xcoeferr,2)

par(mfrow=c(3,2))
plot(data$reg.pl.pm10.resid, data$reg.pl.pm25.resid,xlab="PM10", ylab="PM2.5")
plot(data$reg.pl.no2.resid, data$reg.pl.pm25.resid,xlab="NO2", ylab="PM2.5")
plot(data$reg.pl.so2.resid, data$reg.pl.pm25.resid,xlab="SO2", ylab="PM2.5")
plot(data$reg.pl.co.resid, data$reg.pl.pm25.resid,xlab="CO", ylab="PM2.5")
plot(data$reg.pl.o3.resid, data$reg.pl.pm25.resid,xlab="O3", ylab="PM2.5")

tmp<-na.omit(data[,41:46])
cor.test(tmp[,1],tmp[,4])
xtable(cor(na.omit(data[,41:46])))
xtable(cor(na.omit(data[,c(11,34,31,33,32,35)])))
cor.test(data[,43],data[,44],na.action=na.omit)

#Partially linear nonparametric model
bw <- npplregbw(ydat=data$pm2.5,
                zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                xdat=data.frame(data$pm2.5lag,data$NO2lag,data$COlag,data$O3lag))

reg.pl <- npplreg(bw)
reg.pl$xcoeferr 

bw1 <- npplregbw(ydat=data$pm2.5,
              zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
              xdat=data.frame(data$pm2.5lag,data$NO2lag,data$SO2lag))
reg.pl1 <- npplreg(bw1)
reg.pl1$xcoefvcov

bw2 <- npplregbw(ydat=data$pm2.5,
                 zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                 xdat=data.frame(data$pm2.5lag,data$NO2lag,data$SO2lag,data$COlag,data$O3lag))
reg.pl2 <- npplreg(bw2)
reg.pl2$xcoefvcov

bw3 <- npplregbw(ydat=data$pm2.5,
                 zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                 xdat=data.frame(data$pm2.5lag,data$PM10lag,data$NO2lag,data$SO2lag,data$COlag,data$O3lag))
reg.pl3 <- npplreg(bw3)
reg.pl3$xcoefvcov


bw4 <- npplregbw(ydat=data$pm2.5,
                 zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                 xdat=data.frame(data$pm2.5lag,data$PM10lag,data$NO2lag,data$SO2lag,data$O3lag))
reg.pl4 <- npplreg(bw4)
reg.pl4$xcoefvcov

bw5 <- npplregbw(ydat=data$pm2.5,
                 zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                 xdat=data.frame(data$pm2.5lag))
reg.pl5 <- npplreg(bw5)
reg.pl5$xcoefvcov

bw6 <- npplregbw(ydat=data$pm2.5,
                 zdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws),
                 xdat=data.frame(data$pm2.5lag,data$NO2lag))
reg.pl6 <- npplreg(bw6)
reg.pl6$xcoefvcov

missing <- is.na(data$pm2.5)+is.na(data$TEMPimpute)+is.na(data$DEWPimpute)+
  is.na(data$PRESimpute)+is.na(data$wd)+is.na(data$Iws)+is.na(data$pm2.5lag)+
  is.na(data$NO2lag)+is.na(data$COlag)+is.na(data$O3lag)+is.na(data$PM10lag)
data$PM25.res <- NA
data$PM25.res[missing==0] <- data$pm2.5 [missing==0]-reg.pl6$mean
sqrt(mean(data$PM25.res^2,na.rm=T))
#sqrt(mean(data$error^2,na.rm=T))
par(mfrow=c(3,1))
plot(data$error,ylab="residual",main="NP model",type="l",ylim=c(-130,130))
plot(data$PM25.res,ylab="residual",main="PL model (lag1: PM2.5)",type="l",ylim=c(-130,130))
#plot(data$PM25.res,ylab="residual",main="PL model (lag1: PM2.5,NO2,SO2,CO,O3)",type="l",ylim=c(-130,130))

#pollutants'a lag conditional on weather variables
lagbw1 <- npregbw(pm2.5lag ~ TEMPimpute + DEWPimpute + PRESimpute + wd + Iws, data=data)
reg1 <- npreg(lagbw1,txdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws), 
              tydat=data$pm2.5lag)
data$pm25.res <- NA
data$pm25.res[-reg1$rows.omit] <- data$pm2.5lag[-reg1$rows.omit]-reg1$mean

lagbw2 <- npregbw(PM10lag ~ TEMPimpute + DEWPimpute + PRESimpute + wd + Iws, data=data)
reg2 <- npreg(lagbw2,txdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws), 
              tydat=data$PM10lag)
data$PM10.res <- NA
data$PM10.res[-reg2$rows.omit] <- data$PM10lag[-reg2$rows.omit]-reg2$mean

lagbw3 <- npregbw(NO2lag ~ TEMPimpute + DEWPimpute + PRESimpute + wd + Iws, data=data)
reg3 <- npreg(lagbw3,txdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws), 
              tydat=data$NO2lag)
data$NO2.res <- NA
data$NO2.res[-reg3$rows.omit] <- data$NO2lag[-reg3$rows.omit]-reg3$mean

lagbw4 <- npregbw(SO2lag ~ TEMPimpute + DEWPimpute + PRESimpute + wd + Iws, data=data)
reg4 <- npreg(lagbw4,txdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws), 
              tydat=data$SO2lag)
data$SO2.res <- NA
data$SO2.res[-reg4$rows.omit] <- data$SO2lag[-reg4$rows.omit]-reg4$mean

lagbw5 <- npregbw(COlag ~ TEMPimpute + DEWPimpute + PRESimpute + wd + Iws, data=data)
reg5 <- npreg(lagbw5,txdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws), 
              tydat=data$COlag)
data$CO.res <- NA
data$CO.res[-reg5$rows.omit] <- data$COlag[-reg5$rows.omit]-reg5$mean

lagbw6 <- npregbw(O3lag ~ TEMPimpute + DEWPimpute + PRESimpute + wd + Iws, data=data)
reg6 <- npreg(lagbw6,txdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws), 
              tydat=data$O3lag)
data$O3.res <- NA
data$O3.res[-reg6$rows.omit] <- data$O3lag[-reg6$rows.omit]-reg6$mean



#eliminate the information of weather
bw <- npregbw(PM10 ~ TEMPimpute + DEWPimpute + PRESimpute + wd + Iws, data=data)
reg.no <- npreg(bw,txdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws), tydat=data$PM10)
data$PM10.res <- NA
data$PM10.res[-reg.no$rows.omit] <- data$PM10[-reg.no$rows.omit]-reg.no$mean
plot(data$error,type="l", ylim=c(-150,150))
lreg <- lm(data$error~data$PM10.res)
lines(lreg$coefficient[2]*data$PM10.res, col="red")

bw <- npregbw(NO2 ~ TEMPimpute + DEWPimpute + PRESimpute + wd + Iws, data=data)
reg.no <- npreg(bw,txdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws), tydat=data$NO2)
data$NO.res <- NA
data$NO.res[-reg.no$rows.omit] <- data$NO2[-reg.no$rows.omit]-reg.no$mean
plot(data$error,type="l", ylim=c(-150,150))
lreg <- lm(data$error~data$NO.res)
lines(lreg$coefficient[2]*data$NO.res, col="red")

bw <- npregbw(CO ~ TEMPimpute + DEWPimpute + PRESimpute + wd + Iws, data=data)
reg.no <- npreg(bw,txdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws), tydat=data$CO)
data$CO.res <- NA
data$CO.res[-reg.no$rows.omit] <- data$CO[-reg.no$rows.omit]-reg.no$mean

plot(data$error,type="l", ylim=c(-150,150))
lreg <- lm(data$error~data$CO.res)
lines(lreg$coefficient[2]*data$CO.res, col="red")

bw <- npregbw(SO2 ~ TEMPimpute + DEWPimpute + PRESimpute + wd + Iws, data=data)
reg.no <- npreg(bw,txdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws), tydat=data$SO2)
data$SO2.res <- NA
data$SO2.res[-reg.no$rows.omit] <- data$SO2[-reg.no$rows.omit]-reg.no$mean
plot(data$error,type="l", ylim=c(-150,150))
lreg <- lm(data$error~data$SO2.res)
lines(lreg$coefficient[2]*data$SO2.res, col="red")

bw <- npregbw(O3 ~ TEMPimpute + DEWPimpute + PRESimpute + wd + Iws, data=data)
reg.no <- npreg(bw,txdat=data.frame(data$TEMPimpute,data$DEWPimpute,data$PRESimpute,data$wd,data$Iws), tydat=data$O3)
data$O3.res <- NA
data$O3.res[-reg.no$rows.omit] <- data$O3[-reg.no$rows.omit]-reg.no$mean

plot(data$error,type="l", ylim=c(-150,150))
lreg <- lm(data$error~data$O3.res)
lines(lreg$coefficient[2]*data$O3.res, col="red")

plot(data$PM10.res, data$error,xlab="PM10",ylab="PM2.5")
plot(data$NO.res, data$error,xlab="NO2",ylab="PM2.5")
plot(data$SO2.res, data$error,xlab="SO2",ylab="PM2.5")
plot(data$CO.res, data$error,xlab="CO",ylab="PM2.5")
plot(data$O3.res, data$error,xlab="O3",ylab="PM2.5")

data$PM25.res <- data$error
xtable(round(cor(na.omit(data[,c(41,36,37,38,39,40)])),digits=2))
granger.test(data[,c(41,36,37,38,39,40)],p=1)
tmp <- matrix(NA,30,6)
rownames(tmp)<- rownames(granger.test(data[,c(41,36,37,38,39,40)],p=1))
for(i in 1:6){
  tmp[,i] <- round(granger.test(data[,c(41,36,37,38,39,40)],p=i)[,2],digits=2)
}
xtable(tmp)

bw <- npregbw(PM25.res ~ PM10.res + NO.res + SO2.res + CO.res +O3.res , data=data)
reg.no <- npreg(bw,txdat=data.frame(data$PM10.res, data$NO.res,data$SO2.res,data$CO.res,data$O3.res), tydat=data$PM25.res)
reg.no
sqrt(mean((data$PM25.res[-reg.no$rows.omit] - reg.no$mean)^2))
plot(data$PM25.res[-reg.no$rows.omit],type="l", ylab="residuals")
lines(reg.no$mean,col="red")
legend("topleft",lty=c(1,1),col=c("black","red"),text=c("PM2.5 residual","fitted residual"))
plot(data$PM25.res[-reg.no$rows.omit] - reg.no$mean,type="l", ylab="new residuals")

tmp.data <-data[,c(46,41,42,44,43,45)]
correlation <- matrix(NA, 6,6)
for (i in 1:6){
  for(j in 1:6){
    tmp <- cbind(tmp.data[,i],tmp.data[,j])
    tmp <- na.omit(tmp)
    correlation[i,j] <-cor(tmp)[1,2]
  }
}
xtable(correlation)

#residuals and pm2.5
datatmp <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/pm0331(cvs).csv", header=T)
correlation <- matrix(NA, 12,5)
rmse <- matrix(NA,12,5)
newrmse <- matrix(NA,12,5)
for(i in 10:14){
  for(j in 1:12){
    tmp <- subset(datatmp,year==i&month==j)
    tmp$pm2.5 [tmp$pm2.5 <= 0] <- NA
    correlation[j,i-9] <- cor(na.omit(cbind(tmp$pm2.5,abs(tmp$error))))[1,2]
    tt <- na.omit(cbind(tmp$pm2.5,tmp$error))
    rmse[j,i-9] <- sqrt(mean(tmp$error^2,na.rm=T))
    newrmse[j,i-9] <- sqrt(mean((tt[,2]/tt[,1])^2))
  }
}
xtable(newrmse)
xtable(rmse)
xtable(correlation)

# mean is close to 0 but variance is very large; heavy tailed, likely to be t-distribution
mean(data$error)
var(data$error)
plot(density(data$error/data$pm2.5))
plot(density(data$error))
plot(ecdf(data$error))
plot(no.data$weekday,no.data$NO2)
plot(abs(data$error),type="l")
plot(data$pm2.5,type="l")
plot(abs(data$error/data$pm2.5),type="l")
cor.test(abs(data$error),data$pm2.5)
par(mfrow=c(1,1))
pacf(data$error)
acf(data$error)

data$diff <- abs(data$pm2.5-data$pm2.5lag)
data$deriv <- c(NA,data$diff[-1])+data$diff+c(NA,NA,data$diff[-c(1,2)])+
  c(data$diff[-nrow(data)],NA)+c(data$diff[-c(nrow(data),nrow(data)-1)],NA,NA)
cor.test(abs(data$error[is.na(data$deriv)==FALSE]),data$deriv[is.na(data$deriv)==FALSE])
plot(data$deriv,type="l")
plot(abs(data$error),type="l")
plot(data$pm2.5,type="l")
plot(data$pm2.5-data$pm2.5lag,type="l")

cor.test(data$error[-c(1,2)],data$error[-c(nrow(data),nrow(data)-1)])

adf.test(data$error)
Box.test(abs(data$error), type="Ljung-Box")

reg.error <- lm(abs(data$error)~0+data$pm2.5)
summary(reg.error)
plot(reg.error$residuals,type="l")
adf.test(reg.error$residuals)
Box.test(reg.error$residuals, type="Ljung-Box")


#-------------------------------------------------------
data1 <- subset(data, month==2&pm2.5>0)
data2 <- subset(data, pm2.5>0)
mean(abs(data2$error/data2$pm2.5),na.rm=T)
mean(abs(data1$error/data1$pm2.5),na.rm=T)
mean(abs(data2$pm2.5),na.rm=T)
mean(abs(data1$pm2.5),na.rm=T)

rmse <- matrix(NA, 12,5)
for (i in 2010:2014){
  for (j in 1:12){
    tmp <- subset(data, month==j&year==i&abs(error)<200)
    rmse[j,i-2009] <- sqrt(mean(tmp$error^2,na.rm=T))
  } 
}
rse <- matrix(NA, 12,5)
for (i in 2010:2014){
  for (j in 1:12){
    tmp <- subset(data, month==j&year==i&pm2.5>0)
    rse[j,i-2009] <- mean(abs(tmp$error/tmp$pm2.5)*100,na.rm=T)
  } 
}
rse.var <- matrix(NA, 12,5)
for (i in 2010:2014){
  for (j in 1:12){
    tmp <- subset(data, month==j&year==i&pm2.5>0)
    rse.var[j,i-2009] <- var(abs(tmp$error/tmp$pm2.5)*100,na.rm=T)
  } 
}

data$pm2.5[data$pm2.5==-666666] <-NA

plot(data$pm2.5,abs(data$error), pch=19, cex=0.1)
plot(data$pm2.5,abs(data$error/data$pm2.5), pch=19, cex=0.1)
summary(lm(data$error~data$pm2.5))
plot(density(data$error/data$pm2.5, na.rm=T))
table(data$wd[which(abs(data$error)>30)-2])

data[which(abs(data$error)>200),]

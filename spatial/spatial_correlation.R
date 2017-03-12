library(xtable)
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)
library(reshape2)

geocode("baoding")
geocode("cangzhou")

directory <- "C:/Users/haozhe/Dropbox/projects/pm2.5/data/pm_data/beijing and hebei/"
y <- 14
m <- c(6,7,8,9,10)

#draw maps of Hebei provinces

location <- read.csv(paste(directory, "location.csv", sep=""), head = T)
print(xtable(location[,1:4]))
map("china", col = "black",  panel.first = grid())
points(location$longitude, location$latitude, pch = 1, col = "red")

qplot(longitude, latitude, data=location, geom="point",xlim=c(114.2,118.4), colour="red", size=1)+
  geom_text(aes(longitude, latitude,label=name,parse=FALSE), color="black")+
  theme(legend.position='none')

#input pm data
location.name <- c("us.embassy", "baoding", "cangzhou", "changpingzhen", "chaoyangaotizhongxin", "chaoyangnongzhanguan",
                   "chengde", "daxinghuangcunzhen", "dongsihuanbeilu", "fengtaihuayuan", "guandangxiao", 
                   "haidianwanliu", "jixian", "langfang", "miyun", "nansanhuanxilu", "pinggu", "qianmendongdajie",
                   "sanhe", "shijiazhuang", "shijingshangucheng", "shunyixincheng", "tangshan", "tianjin",
                   "tongzhouxincheng", "xianghe", "xinglong", "yanqing", "zhangjiakou", "zhuozhou")

us.embassy <- read.csv(paste(directory, "pm1107(cv).csv", sep=""))
us.embassy$pm2.5 [us.embassy$pm2.5==-666666] <- NA
us.embassy<- subset(us.embassy, year==14&month %in% m)

pm.data <- matrix(NA, nrow(us.embassy), length(location.name))
pm.data <- data.frame(pm.data)
colnames(pm.data) <- location.name
pm.data$us.embassy <- us.embassy$pm2.5


for ( i in 2:length(location.name)){
  temp <- read.csv(paste(c(directory,location.name[i], "PM25.csv"), collapse=""), head = T)
  temp$hour <- as.character(temp$hour)
  temp <- subset(temp, hour != ".")
  temp <- temp$concentrate [temp$month %in% m]
  pm.data[,i] <- temp
}
pm.data <- na.omit(pm.data)


#calculate correlation and covariance matrix
correlation <-cor(pm.data)
correlation <- data.frame(correlation)
covariance <-cov(pm.data)
covariance <- data.frame(covariance)
print(xtable(correlation))
print(xtable(covariance))
write.csv(correlation, file=paste(directory, "correlation.csv", sep=""))
write.csv(covariance, file=paste(directory, "covariance.csv", sep=""))

#calculate distance matrix
distance <- matrix(NA, length(location.name), length(location.name))
distance <- data.frame(distance)
colnames(distance) <- location.name
rownames(distance) <- location.name
location <- read.csv(paste(directory, "location.csv", sep=""))
location$city <- as.character(location$city)

for ( i in 1:ncol(distance)){
  for ( j in 1:ncol(distance)){

    a1 <- location$latitude [location$city == location.name[i]]/180*pi#latitude
    b1 <- location$longitude [location$city == location.name[i]]/180*pi#longitude
    a2 <- location$latitude [location$city == location.name[j]]/180*pi#latitude
    b2 <- location$longitude [location$city == location.name[j]]/180*pi#longitude
    
    distance[i,j] <- 12732.4*asin(1/2*sqrt((cos(a1)*cos(b1)-cos(a2)*cos(b2))^2+
                                            (cos(a1)*sin(b1)-cos(a2)*sin(b2))^2+(sin(a1)-sin(a2))^2))
  }
}

#combine distance and correlation
dist.cor <- data.frame(matrix(NA, length(correlation)^2, 2))
colnames(dist.cor) <- c("distance", "correlation")
for (i in 1:length(correlation)){
  for (j in 1:length(correlation)){
    dist.cor[(i-1)*length(correlation)+j,] <-c(distance[i,j], correlation[i,j])
  }
}
dist.cor <- dist.cor[!duplicated(dist.cor),]
qplot(distance, correlation, data=dist.cor)+
  geom_smooth(method="lm")+
  #ggtitle("Distance and correlation between different cities")+
  xlab("distance (km)")

#combine distance and covariance
dist.cov <- data.frame(matrix(NA, length(covariance)^2, 2))
colnames(dist.cov) <- c("distance", "covariance")
for (i in 1:length(covariance)){
  for (j in 1:length(covariance)){
    dist.cov[(i-1)*length(covariance)+j,] <-c(distance[i,j], covariance[i,j])
  }
}
qplot(distance, covariance, data=dist.cov)+
  geom_smooth()+
  #geom_text(aes(c(0,0),c(6300, 10480),label=c("langfang", "baoding")), col="red")+
  #ggtitle("Distance and covariance between different cities")+
  xlab("distance (km)")

#beijing
dist.cor.beijing <- data.frame(distance[,1], correlation[,1])
qplot(distance...1., correlation...1., data=dist.cor.beijing)+
  geom_smooth(method="lm")+
  xlab("distance (km)")+ylab("correlation")

dist.cov.beijing <- data.frame(distance[,1], covariance[,1])
qplot(distance...1., covariance...1., data=dist.cov.beijing)+
  geom_smooth(method="lm")+
  xlab("distance (km)")+ylab("covariance")


#summary statistics; boxplot of pm.data
pm.melt <- melt(pm.data, measure.vars=1:30)
colnames(pm.melt) <- c("location", "pm")
qplot(location, pm, data=pm.melt, geom="boxplot")+coord_flip()
pm.stat <- ddply(pm.melt, .(location), summarise,
      mean=mean(pm,na.rm=T),
      variance=var(pm, na.rm=T))

ggplot(pm.stat, aes(location, mean)) +
  geom_bar(stat="identity",position = "dodge",fill="#F8766D")+
  coord_flip()+
  ylab("mean of pm data")

ggplot(pm.stat, aes(location, variance)) +
  geom_bar(stat="identity",position = "dodge",fill="#F8766D")+
  coord_flip()+
  ylab("variance of pm data")

#beijing -baoding : different wind direction
baoding <- read.csv(paste(c(directory,"baoding", "PM25.csv"), collapse=""), head = T)
baoding$hour <- as.character(baoding$hour)
baoding <- subset(baoding, hour != ".")
baoding <- baoding$concentrate [baoding$month %in% m]
bj.bd.data <- na.omit(data.frame(us.embassy$pm2.5, us.embassy$wd, baoding))

stat.bj.bd <- ddply(bj.bd.data, .(us.embassy.wd), summarise,
                    cor = cor(us.embassy.pm2.5, baoding))
stat.bj.bd$us.embassy.wd <- as.character(stat.bj.bd$us.embassy.wd)
stat.bj.bd <- rbind(stat.bj.bd, c("all directions", cor(bj.bd.data[,1],bj.bd.data[,3])))
stat.bj.bd$cor <- as.numeric(stat.bj.bd$cor)
stat.bj.bd$us.embassy.wd <- as.factor(stat.bj.bd$us.embassy.wd)

ggplot(stat.bj.bd, aes(us.embassy.wd, cor)) +
  geom_bar(stat="identity",position = "dodge",fill="black")+
  xlab("wind direction")+ylab("correlation")

#beijing - baoding : without time lag
lag <- -72:72

baoding <- read.csv(paste(c(directory,"baoding", "PM25.csv"), collapse=""), head = T)
baoding$hour <- as.character(baoding$hour)
baoding <- subset(baoding, hour != ".")
baoding <- baoding$concentrate [baoding$month %in% m]
n <- length(baoding)

cor.lag <- data.frame(time.lag=lag, correlation=NA)
for ( i in 1:length(lag)){
  if(lag[i] >0){
    bj.bd.data <- na.omit(data.frame(us.embassy$pm2.5[(1+lag[i]):n], baoding[1:(n-lag[i])]))
    cor.lag[i,2] <- cor(bj.bd.data[,1],bj.bd.data[,2])
  }
  if(lag[i] <= 0){
    bj.bd.data <- na.omit(data.frame(us.embassy$pm2.5[1:(n+lag[i])], baoding[(1-lag[i]):n]))
    cor.lag[i,2] <- cor(bj.bd.data[,1],bj.bd.data[,2])
  }
}
qplot(time.lag, correlation, data=cor.lag)+xlab("time lag (hour)")
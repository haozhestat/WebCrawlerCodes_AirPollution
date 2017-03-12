library(mapproj)
library(xtable)
library(maps)
library(mapdata)
library(ggplot2)
library(ggmap)
library(reshape2)
library(geoR)
library(fields)
library(lubridate)
library(maptools)

directory <- "C:/Users/haozhe/Dropbox/projects/pm2.5/data/pm_data/beijing and hebei/"
y <- 14
m <- c(6,7,8,9,10)

#input data
location <- read.csv(paste(directory, "location.csv", sep=""), head = T)
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
for( i in 1:nrow(pm.data)){
  for(j in 1:ncol(pm.data)){
    if(pm.data[i,j] == 0) pm.data[i,j] <- NA
  }
}
pm.data <- na.omit(pm.data)
pm.data <- log(pm.data)
time <- ymd_hms("2014-06-01 00:00:00") + hours(1:nrow(pm.data))
data <- data.frame(cbind(time, pm.data))
data <- na.omit(data)

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

#calculate empirical variogram
vario <- matrix(NA, length(location.name), length(location.name))
for (i in 1:length(location.name)){
  for (j in 1:length(location.name)){
    vario[i,j] <- mean((pm.data[,i]-pm.data[,j])^2)/2
  }
}

dist.vario <- data.frame(matrix(NA, length(vario), 2))
colnames(dist.vario) <- c("distance", "variogram")
for (i in 1:nrow(vario)){
  for (j in 1:nrow(vario)){
    dist.vario[(i-1)*nrow(vario)+j,] <-c(distance[i,j], vario[i,j])
  }
}
qplot(distance, variogram, data=dist.vario)+geom_smooth()+xlab("distance (km)")

nug.dist.vario <- subset(dist.vario, distance!=0)
lin.result <- lm(nug.dist.vario$variogram~nug.dist.vario$distance)
summary(lin.result)
lin.result$coefficients


#prediction
Gamma <- matrix(NA, nrow(vario)+1, nrow(vario)+1)
Gamma[1:nrow(vario), 1:nrow(vario)] <- vario
Gamma[, nrow(vario)+1] <- 1
Gamma[nrow(vario)+1, ] <- 1
Gamma[nrow(vario)+1, nrow(vario)+1] <- 0
inverse.Gamma <- solve(Gamma)

dist.krig <- function(a,b){
    dist.x0 <- matrix(NA, ncol(distance), 1)
    for ( j in 1:ncol(distance)){
      a2 <- location$latitude[j]
      b2 <- location$longitude[j]
      if(a==a2&b==b2) return(as.matrix(vario[,j]))
      a1 <- a/180*pi#latitude
      b1 <- b/180*pi#longitude      
      a2 <- location$latitude[j]/180*pi#latitude
      b2 <- location$longitude[j]/180*pi#longitude 
      dist.x0[j, 1] <- 12732.4*asin(1/2*sqrt((cos(a1)*cos(b1)-cos(a2)*cos(b2))^2+
                                               (cos(a1)*sin(b1)-cos(a2)*sin(b2))^2+(sin(a1)-sin(a2))^2))
      if(dist.x0[j, 1]>285.9556){
        dist.x0[j, 1] <- 0.6
      }
      else{
        dist.x0[j, 1] <- lin.result$coefficients[1] + lin.result$coefficients[2]*dist.x0[j, 1]
      }      
    }
    return(dist.x0)
}

coef <- inverse.Gamma%*%rbind(dist.krig(location[1,3], location[1,4]), 1)
as.matrix(pm.data[1,])%*%as.matrix(coef[1:(length(coef)-1),])
prediction <- function(x, y){
  gamma <- rbind(dist.krig(x, y), 1)
  coef <- inverse.Gamma%*%gamma
  #return(coef)
  return(as.matrix(coef[1:(length(coef)-1),]))
}

as.matrix(pm.data[1,])%*%prediction(39.95432,116.46632)

#draw the plots
grid.num <- 50
t <- 104
s=location[,3:4]
sp1<-seq(min(s[,2])-0.1,max(s[,2])+0.1,length=grid.num)
sp2<-seq(min(s[,1])-0.1,max(s[,1])+0.1,length=grid.num)
sp<-expand.grid(sp1,sp2)
pred <- matrix(NA, grid.num, grid.num)
final.pred <- matrix(NA,grid.num^2, 3)
for (i in 1:grid.num){
  for (j in 1:grid.num){
    pred[i,j] <- as.matrix(pm.data[t,])%*%prediction(sp2[j], sp1[i])
    final.pred[grid.num*(i-1)+j, 1] <- sp1[i]
    final.pred[grid.num*(i-1)+j, 2] <- sp2[j]
    final.pred[grid.num*(i-1)+j, 3] <- pred[i,j]
  }
}

pred <- exp(pred)

inChina<-map.where("world",x=sp[,1],y=sp[,2])
inChina[is.na(inChina)]<-"NA"
inChina<-inChina=="China"
pred[!inChina]<-NA
image.plot(sp1,sp2,pred,xlab="longitude",ylab="latitude")
map("china",add=T)
map.cities(country = "China", capitals = 1, cex=0.8)
map.cities(country = "China", capitals = 2, cex=0.8)
points(location$longitude, location$latitude, pch=16, cex=0.6)

install.packages("animation")
library(animation)

saveGIF({
  for (t in 1020:1026){
    s=location[,3:4]
    sp1<-seq(min(s[,2])-0.1,max(s[,2])+0.1,length=grid.num)
    sp2<-seq(min(s[,1])-0.1,max(s[,1])+0.1,length=grid.num)
    sp<-expand.grid(sp1,sp2)
    pred <- matrix(NA, grid.num, grid.num)
    final.pred <- matrix(NA,grid.num^2, 3)
    for (i in 1:grid.num){
      for (j in 1:grid.num){
        pred[i,j] <- as.matrix(pm.data[t,])%*%prediction(sp2[j], sp1[i])
        final.pred[grid.num*(i-1)+j, 1] <- sp1[i]
        final.pred[grid.num*(i-1)+j, 2] <- sp2[j]
        final.pred[grid.num*(i-1)+j, 3] <- pred[i,j]
      }
    }
    
    #pred <- exp(pred)
    
    inChina<-map.where("world",x=sp[,1],y=sp[,2])
    inChina[is.na(inChina)]<-"NA"
    inChina<-inChina=="China"
    pred[!inChina]<-NA
    image.plot(sp1,sp2,pred,xlab="longitude",ylab="latitude")
    map("china",add=T)
    map.cities(country = "China", capitals = 1, cex=0.8)
    map.cities(country = "China", capitals = 2, cex=0.8)
    points(location$longitude, location$latitude, pch=16, cex=0.6)
  }
},loop=TRUE,interval=0.4)

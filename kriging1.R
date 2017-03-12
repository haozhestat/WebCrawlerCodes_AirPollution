
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


#-----------------------------------
coord.proj <- project(cbind(location$longitude,location$latitude), "+proj=utm +zone=50 ellps=WGS84")
distance <- dist(coord.proj, method = "euclidean", diag = TRUE, upper = TRUE, p = 2)

#--------------------------------
grid.num <- 200
sp1 <- seq(min(coord.proj[,1])-50,max(coord.proj[,1])+50,length=grid.num)
sp2 <- seq(min(coord.proj[,2])-50,max(coord.proj[,2])+50,length=grid.num)
sp <- expand.grid(sp1,sp2)


#-----------------------------------------------
saveGIF({
  for (t in 1210:1218){
    #t=1201
    pm.t <- data.frame(coord.proj[,1], coord.proj[,2], t(pm.data[t,]), data[t,1])
    pm.t <- as.geodata(pm.t)
    variogram.t <- variog(pm.t,estimator.type="modulus")
    #plot(variogram.t)
    fit.t <- variofit(variogram.t, ini.cov.pars=c(0.13, 120000), cov.model="matern",weights="cressie")    
    kriging <- krige.control(type.krige="ok",cov.model="matern",
                             cov.pars=fit.t$cov.pars, nugget=fit.t$nugget)
    pred <- krige.conv(pm.t, locations=sp, krige=kriging)
    image.plot(sp1,sp2,matrix(pred$predict,grid.num,grid.num))
    #points(coord.proj, pch=16, cex=0.6)
    text(coord.proj[1,1],coord.proj[1,2],"Beijing")
    text(coord.proj[30,1],coord.proj[30,2],"Tianjin")
    text(coord.proj[17,1],coord.proj[17,2],"baoding")
    text(coord.proj[27,1],coord.proj[27,2],"shijiazhuang")
    text(coord.proj[22,1],coord.proj[22,2],"zhangjiakou")
    text(coord.proj[19,1],coord.proj[19,2],"chengde")
    text(coord.proj[21,1],coord.proj[21,2],"tangshan")
  }
},loop=TRUE,interval=0.4)


setwd("C:/Users/haozhe/Dropbox/projects/pm2.5/spatial analysis/Data_2_2/")

#beijing
list <- c("01","02","03","04","05","06","07","08","09","10","11","12")
pm.miss <- matrix(NA, 24*30, length(list))
for(k in 1:length(list)){
  tmp.miss <- data.frame(month=rep(NA,24*30),day=rep(NA,24*30),hour=rep(NA,24*30))
  tmp <- read.csv(paste("pm_data/beijing/beijing_beijing_10",list[k],"A.csv",sep=""), head=T)
  tmp <- subset(tmp,monthtemp==1&daytemp>1)
  print(dim(tmp))
  track=0
  month=1
    for (day in 2:31)
      for(hour in 0:23){
        track=track+1;
        tmp.miss$month[track] <- month
        tmp.miss$day[track] <- day
        tmp.miss$hour[track] <- hour
        if(sum(tmp$month==month&tmp$day==day&tmp$hour==hour)==1)
          pm.miss[track,k] <- tmp$pm2_5[tmp$month==month&tmp$day==day&tmp$hour==hour]
      }
}
beijing.pm <- data.frame(tmp.miss, pm=NA)
for (i in 1:nrow(beijing.pm)){
  if(sum(is.na(pm.miss[i,]))==ncol(pm.miss))
    beijing.pm$pm[i] <- NA
  else
    beijing.pm$pm[i] <- mean(pm.miss[i,], na.rm=T)
}
plot(beijing.pm$pm, type='l', ylim=c(0,700))

#baoding
list <- c("51","52","53","54","55","56")
pm.miss <- matrix(NA, 24*30, length(list))
for(k in 1:length(list)){
  tmp.miss <- data.frame(month=rep(NA,24*30),day=rep(NA,24*30),hour=rep(NA,24*30))
  tmp <- read.csv(paste("pm_data/hebei/hebei_baoding_10",list[k],"A.csv",sep=""), head=T)
  tmp <- subset(tmp,monthtemp==1&daytemp>1)
  print(dim(tmp))
  track=0
  month=1
  for (day in 2:31)
    for(hour in 0:23){
      track=track+1;
      tmp.miss$month[track] <- month
      tmp.miss$day[track] <- day
      tmp.miss$hour[track] <- hour
      if(sum(tmp$month==month&tmp$day==day&tmp$hour==hour)==1)
        pm.miss[track,k] <- tmp$pm2_5[tmp$month==month&tmp$day==day&tmp$hour==hour]
    }
}
baoding.pm <- data.frame(tmp.miss, pm=NA)
for (i in 1:nrow(beijing.pm)){
  if(sum(is.na(pm.miss[i,]))==ncol(pm.miss))
    baoding.pm$pm[i] <- NA
  else
    baoding.pm$pm[i] <- mean(pm.miss[i,], na.rm=T)
}
lines(baoding.pm$pm, type='l', col="red")

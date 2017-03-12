# This code is used to organize the pm data 
library(lubridate)


# For the data in Beijing
# setwd("F:/我的文件/pm2.5/data_1_7/beijing")
station_list<-read.csv("/home/binguo/pm2.5/data_1_18/api_data/api_station_name.csv")


for(i in 1:dim(station_list)[1])
{  
  readdir<-paste0("/home/binguo/pm2.5/data_1_18/api_data/",station_list$province_name[i])
  writedir<-paste0("/home/binguo/pm2.5/data_1_18/api_data_new/",station_list$province_name[i]) 
  tempname<-paste0(readdir,"/",station_list$province_name[i],"_",station_list$city_name[i],"_",station_list$station_code[i],".csv")
  tempdata<-read.csv(tempname)
  
  #paste(strsplit(as.character(tempdata$time_point[1]), "[A-Z]")[[1]], collapse=" ")
  
  timetemp0=NULL
  yeartemp=NULL
  monthtemp=NULL
  daytemp=NULL
  hourtemp=NULL
  
  for(j in 1:dim(tempdata)[1])
  {
    timetemp=paste(strsplit(as.character(tempdata$time_point[j]), "[A-Z]")[[1]], collapse=" ")
    timetemp0=c(timetemp0,paste(strsplit(as.character(tempdata$time_point[j]), "[A-Z]")[[1]], collapse=" "))
    yeartemp=c(yeartemp,year(timetemp))
    monthtemp=c(monthtemp,month(timetemp))
    daytemp=c(daytemp,day(timetemp))
    hourtemp=c(hourtemp,hour(timetemp))
  }
  #tempdata=as.data.frame(tempdata)
  tempdata$timetemp=timetemp0
  tempdata$yeartemp=yeartemp
  tempdata$monthtemp=monthtemp
  tempdata$daytemp=daytemp
  tempdata$hourtemp=hourtemp
  #write.csv(tempdata,file="temp.csv")
  
  #tempdata=tempdata[tempdata$aqi!=0,]
  temp=tempdata[1,]
  for(k in 2:dim(tempdata)[1])
  {
    if(tempdata$timetemp[k]!=tempdata$timetemp[k-1])
    {
      temp=rbind(temp,tempdata[k,])
    }
  }
  writefile=paste0(writedir,"/",station_list$province_name[i],"_",station_list$city_name[i],"_",station_list$station_code[i],".csv")
  write.csv(temp,file=writefile)
  write.csv(data.frame(i,station_list$station_code[i]),file="/home/binguo/pm2.5/data_1_18/times.csv" )
}

 
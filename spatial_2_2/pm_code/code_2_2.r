# Data organization for Feb.2 
# This code is used to organize the pm data 
library(lubridate)


# For the data in Beijing
# setwd("F:/?ҵ??ļ?/pm2.5/data_1_7/beijing")
station_list<-read.csv("/home/guobin/Desktop/Raw data/Data_2_2/api_station_name.csv")


for(i in 1013:dim(station_list)[1])
{  
  readdir<-paste0("/home/guobin/Desktop/Raw data/Data_2_2/schen-l2_api_data_2015010702_20150201/",station_list$province_name[i])
  readdir_1<-paste0("/home/guobin/Desktop/Raw data/Data_2_2/schen-l3_api_data_20150101/",station_list$province_name[i])
  
  writedir<-paste0("/home/guobin/Desktop/Raw data/Data_2_2/Data_2_2/",station_list$province_name[i]) 
  
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
  
  temp1name<-paste0(readdir_1,"/",station_list$province_name[i],"_",station_list$city_name[i],"_",station_list$station_code[i],".csv")
  temp1data<-read.csv(temp1name)
  
  #paste(strsplit(as.character(temp1data$time_point[1]), "[A-Z]")[[1]], collapse=" ")
  
  timetemp0=NULL
  yeartemp=NULL
  monthtemp=NULL
  daytemp=NULL
  hourtemp=NULL
  
  for(j in 1:dim(temp1data)[1])
  {
    timetemp=paste(strsplit(as.character(temp1data$time_point[j]), "[A-Z]")[[1]], collapse=" ")
    timetemp0=c(timetemp0,paste(strsplit(as.character(temp1data$time_point[j]), "[A-Z]")[[1]], collapse=" "))
    yeartemp=c(yeartemp,year(timetemp))
    monthtemp=c(monthtemp,month(timetemp))
    daytemp=c(daytemp,day(timetemp))
    hourtemp=c(hourtemp,hour(timetemp))
  }
  #temp1data=as.data.frame(temp1data)
  temp1data$timetemp=timetemp0
  temp1data$yeartemp=yeartemp
  temp1data$monthtemp=monthtemp
  temp1data$daytemp=daytemp
  temp1data$hourtemp=hourtemp
  #write.csv(temp1data,file="temp1.csv")
  
  #temp1data=temp1data[temp1data$aqi!=0,]
  temp1=temp1data[1,]
  for(k in 2:dim(temp1data)[1])
  {
    if(temp1data$timetemp[k]!=temp1data$timetemp[k-1])
    {
      temp1=rbind(temp1,temp1data[k,])
    }
  }
  if(dim(temp)[1]>dim(temp1)[1])
  {
    temp_temp<-temp
    temp<-temp1
    temp1<-temp_temp
  }
  
  temp2<-temp[!(temp$timetemp%in%temp1$timetemp),]
  temp3<-NULL
  for(kk in 2:dim(temp1)[1])
  {
    temp3<-rbind(temp3,temp1[kk-1,])
    for(jj in 1:dim(temp2)[1])
    {
      if(temp2$timetemp[jj]>temp1$timetemp[kk-1]&&temp2$timetemp[jj]<temp1$timetemp[kk])
      {
        temp3<-rbind(temp3,temp2[jj,])
      }
    }
  }
  if(temp2$timetemp[dim(temp2)[1]]>temp1$timetemp[dim(temp1)[1]])
  {
    temp3<-rbind(temp3,temp2[dim(temp2)[1],])
  }
  if(temp2$timetemp[dim(temp2)[1]]<=temp1$timetemp[dim(temp1)[1]])
  {
    temp3<-rbind(temp3,temp1[dim(temp1)[1],])
  }
  
  writefile=paste0(writedir,"/",station_list$province_name[i],"_",station_list$city_name[i],"_",station_list$station_code[i],".csv")
  write.csv(temp3,file=writefile)
  write.csv(data.frame(i,station_list$station_code[i]),file="/home/guobin/Desktop/Raw data/Data_2_2/Data_2_2/times.csv" )

}



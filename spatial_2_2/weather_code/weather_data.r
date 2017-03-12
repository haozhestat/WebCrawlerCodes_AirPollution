weather_data<-function(weather)
{
library(lubridate)

names(weather)<-c("Time",  "Temperature" , "V3" , "Pressure"  ,"Precipitation",  "Humidity" , "Wind_dir",    "V9" ,"Wind_speed ", "V10", "time")
# Adjust Humidity such that it is numeric
for(i in 1:dim(weather)[1])
{
  if(weather$Precipitation[i]=="无数据相对湿度")
  {
    weather$Precipitation[i]=NA
  }
  if(weather$Humidity[i]=="无数据")
  {
    weather$Humidity[i]=NA
  }
}

Humidity=strsplit(as.character(weather$Humidity[1:dim(weather)[1]]), "%")
Humidity=as.numeric(Humidity)*0.01
weather$Humidity=Humidity
# Adjust Wind_speed such that it is numeric
Wind_speed=strsplit(as.character(weather$Wind_speed[1:dim(weather)[1]]), "M/S")
Wind_speed=as.numeric(Wind_speed)
weather$Wind_speed=Wind_speed
# Adjust the wind direction such that it contains the "CV"
NW_dir=c("N","W","NW","NNW","WNW")
NE_dir=c("NE","NNE","ENE")
SE_dir=c("S","E","SE","SSE","ESE")
SW_dir=c("SW","SSW","WSW")
cv_dir=c("No persistent wind direction",NA)
weather_dir=rep(NA,dim(weather)[1])
for(i in 1:dim(weather)[1])
{
   
  if(as.character(weather$Wind_dir[i])%in%NW_dir)
  {
    weather_dir[i]="NW"
  }
  if(as.character(weather$Wind_dir[i])%in%NE_dir)
  {
    weather_dir[i]="NE"
  }
  if(as.character(weather$Wind_dir[i])%in%SE_dir)
  {
    weather_dir[i]="SE"
  }
  if(as.character(weather$Wind_dir[i])%in%SW_dir)
  {
    weather_dir[i]="SW"
  }
  if(as.character(weather$Wind_dir[i])%in%cv_dir)
  {
    weather_dir[i]="cv"
    weather$Wind_speed[i]=0.445
  }
}
weather$Wind_dir=weather_dir

# Find the corresponding time (year month day hour)

timetemp0=NULL
yeartemp=NULL
monthtemp=NULL
daytemp=NULL
hourtemp=NULL

for(j in 1:dim(weather)[1])
{
  timetemp=paste(strsplit(as.character(weather$Time[j]), "[A-Z]")[[1]], collapse=" ")
  timetemp0=c(timetemp0,paste(strsplit(as.character(weather$Time[j]), "[A-Z]")[[1]], collapse=" "))
  yeartemp=c(yeartemp,year(timetemp))
  monthtemp=c(monthtemp,month(timetemp))
  daytemp=c(daytemp,day(timetemp))
  hourtemp=c(hourtemp,hour(timetemp))
}
#tempdata=as.data.frame(tempdata)
weather$timetemp=timetemp0
weather$yeartemp=yeartemp
weather$monthtemp=monthtemp
weather$daytemp=daytemp
weather$hourtemp=hourtemp
#write.csv(tempdata,file="temp.csv")

tempdata=data.frame(weather$timetemp,weather$yeartemp,weather$monthtemp,weather$daytemp,weather$hourtemp,weather$Temperature,weather$Pressure,weather$Precipitation,weather$Humidity,weather$Wind_dir,weather$Wind_speed)
names(tempdata)=c("time","year","month","day","hour","Temperature","Pressure","Precipitation","Humidity","Wind_dir","Wind_speed")
temp=tempdata[1,]
for(k in 2:dim(tempdata)[1])
{
  if(tempdata$hour[k]!=tempdata$hour[k-1])
  {
    temp=rbind(temp,tempdata[k,])
  }
}
#write.csv(temp,file="F:/?ҵ??ļ?/pm2.5/data_1_7/weather_BJ_HB_TJ_20150108/temp.csv")
#Next step is used to calculate the Iws
temp<-temp[!(is.na(temp$Wind_dir)),]
Iws=rep(NA,dim(temp)[1])
Iws[1]=temp$Wind_speed[1]
for(i in 2:dim(temp)[1])
{
  if(as.character(temp$Wind_dir[i-1])==as.character(temp$Wind_dir[i]))
  {
    Iws[i]=Iws[i-1]+temp$Wind_speed[i]
  }
  else
  {
    Iws[i]=temp$Wind_speed[i]
  }
}
temp$Iws=Iws
return(temp)
}

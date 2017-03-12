source("/home/guobin/Desktop/Raw data/Data_2_2/weather_code/weather_data.r") 

location<-read.csv("/home/guobin/Desktop/Raw data/Data_2_2/weather_code/weather_website.csv")
dir<-"/home/guobin/Desktop/Raw data/Data_2_2/schen-l2_weather_20150201/"
writedir<-"/home/guobin/Desktop/Raw data/Data_2_2/weather_data/"
#for(i in 156:dim(location)[1])
for(i in  1:dim(location)[1])
{
  temp<-try(read.csv(paste(c(dir,as.character(location$province_code[i]), "_",as.character(location$city_name[i]),".csv"), collapse=""), head =F),silent=TRUE)
  
  if(inherits(temp, "try-error"))
  {
    next
  } 
  if((dim(temp)[2]==11)&&(!all(is.na(temp[,7]))))
  {
    datatemp<-weather_data(temp)
    write.csv(datatemp,file=paste(c(writedir,as.character(location$province_code[i]), "_",as.character(location$city_name[i]),".csv"), collapse=""))
    write.csv(i,file="/home/guobin/Desktop/Raw data/Data_2_2/times.csv")
  }
}

 


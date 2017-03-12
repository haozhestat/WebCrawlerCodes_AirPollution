source("/home/binguo/pm2.5/dataorganize/weather_data.r") 

location<-read.csv("/home/binguo/pm2.5/data_1_18/weather_data/weather_website.csv")
dir<-"/home/binguo/pm2.5/data_1_18/weather_data/data/"
writedir<-"/home/binguo/pm2.5/data_1_18/weather_data/data_organize/"
for(i in 156:dim(location)[1])
{
  temp<-read.csv(paste(c(dir,as.character(location$province_code[i]), "_",as.character(location$city_name[i]),".csv"), collapse=""), head = T)
  if((dim(temp)[2]==11)&&(!all(is.na(temp[,7]))))
  {
  datatemp<-weather_data(temp)
  write.csv(datatemp,file=paste(c(writedir,as.character(location$province_code[i]), "_",as.character(location$city_name[i]),".csv"), collapse=""))
  write.csv(i,file="/home/binguo/pm2.5/data_1_18/weather_data/times.csv")
  }
}
station_list<-read.csv("/home/guobin/Desktop/Raw data/Data_2_2/api_station_name.csv")


for(i in 1:dim(station_list)[1])
{  
  readdir<-paste0("/home/guobin/Desktop/Raw data/Data_2_2/api_data_new_organize/",station_list$province_name[i])
  readdir_1<-paste0("/home/guobin/Desktop/Raw data/Data_2_2/Data_2_2/",station_list$province_name[i])
  
  writedir<-paste0("/home/guobin/Desktop/Raw data/Data_2_2/pm_data/",station_list$province_name[i]) 
  
  tempname<-paste0(readdir,"/",station_list$province_name[i],"_",station_list$city_name[i],"_",station_list$station_code[i],".csv")
  tempdata<-read.csv(tempname)
  
  tempname1<-paste0(readdir_1,"/",station_list$province_name[i],"_",station_list$city_name[i],"_",station_list$station_code[i],".csv")
  tempdata_1<-read.csv(tempname1)
  tempdata_1<-tempdata_1[-1,]
  
  tempdata_2<-rbind(tempdata,tempdata_1)
  
  writefile=paste0(writedir,"/",station_list$province_name[i],"_",station_list$city_name[i],"_",station_list$station_code[i],".csv")
  write.csv(tempdata_2,file=writefile)
  write.csv(data.frame(i,station_list$station_code[i]),file="/home/guobin/Desktop/Raw data/Data_2_2/pm_data/times.csv" )
  
}  
# Orgnize the data by taking average for each city and match the corresponding weather variable

# You just only need to modify the following four lines 
readdir_0<-"C:/Users/haozhe/Dropbox/projects/pm2.5/data/data_0225/pm_organized/" # location where pm data saved
weather_read<-"C:/Users/haozhe/Dropbox/projects/pm2.5/data/data_0225/weather_organized/"    # location where weather data saved
writedir<-paste0("C:/Users/haozhe/Dropbox/projects/pm2.5/data/data_0225/final_data/")   # location where to save the final data
location<-read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/api_station_name.csv")  # read the "api_station_name.csv"


# Three provinces
province<-c("beijing","hebei","tianjin")
weather<-c("ABJ","AHE","ATJ")
# Find the corresponding cites in the three provinces
location_new<-location[location$province_name.English%in%province,]
cites<-as.character(unique(location_new$city_name.English))

Data_orgnize<-NULL

for(i in 1:length(province))
{
  location_province<-location_new[location$province_name.English==province[i],]
  for(j in 1:length(cites))
  {
    station_cites<-location_province[location_province$city_name.English==cites[j],]  
    if(dim(station_cites)[1]!=0)
    {
      Data_temp<-NULL
      Dim_data<-NULL
      Names<-NULL
      
      for(k in 1:dim(station_cites)[1])
      {
        if(as.character(station_cites$station_code[k])!="1043A")  # The pm data of 1043A is problemic, we omit it 
        {  
        # The following code is used to read the corresponding pm2.5 data in pm stations in every city  
        Names<-c(Names,as.character(station_cites$station_code[k]))  
        readdir<-paste0(readdir_0,station_cites$province_name[i])  
        tempname<-paste0(readdir,"/",station_cites$province_name[i],"_",station_cites$city_name[k],"_",station_cites$station_code[k],".csv")
        tempdata<-read.csv(tempname)
        Data_temp<-cbind(Data_temp,tempdata$pm2_5)
        }
      }  
      # Add the year month day hour  
      Data_temp<-cbind(Data_temp, tempdata$yeartemp)  
      Data_temp<-cbind(Data_temp, tempdata$monthtemp)  
      Data_temp<-cbind(Data_temp, tempdata$daytemp)
      Data_temp<-cbind(Data_temp, tempdata$hourtemp) 
      # Calculate the average pm2.5 for each city
      Data_temp<-cbind(Data_temp,rowMeans(Data_temp[,1:(dim(Data_temp)[2]-4)]))
      # Add more columns to match weather variable
      Data_temp<-cbind(Data_temp,matrix(rep(NA,(dim(Data_temp)[1])*7),ncol=7))
      Names<-c(Names, "year","month","day","hour","Average_pm","Temperature","Pressure","Precipitation","Humidity",	"Wind_dir",	"Wind_speed",	"Iws")  
      colnames(Data_temp)<-Names
      Data_temp<-data.frame(Data_temp)
    
      
     # The next step is used to match the weather data
     weather_name<-paste0(weather_read,weather[i],"_",cites[j],".csv")
     weather_data<-read.csv(weather_name)
     for(s1 in 1:dim(Data_temp)[1])
     {
       for(s2 in 1:dim(weather_data)[1])
       {
         if(Data_temp$year[s1]==weather_data$year[s2])
         {
           if(Data_temp$month[s1]==weather_data$month[s2])
           {
             if(Data_temp$day[s1]==weather_data$day[s2])
             {
               if(Data_temp$hour[s1]==weather_data$hour[s2])
               {
                 Data_temp$Temperature[s1]=weather_data$Temperature[s2]
                 Data_temp$Pressure[s1]=weather_data$Pressure[s2]
                 Data_temp$Precipitation[s1]=weather_data$Precipitation[s2]
                 Data_temp$Humidity[s1]=weather_data$Humidity[s2]
                 Data_temp$Wind_dir[s1]=as.character(weather_data$Wind_dir[s2])
                 Data_temp$Wind_speed[s1]=weather_data$Wind_speed[s2]
                 Data_temp$Iws[s1]=weather_data$Iws[s2]
               }
             }
           }
         }
       }
     }
     writefile=paste0(writedir,"/",station_cites$city_name[k],".csv")
     write.csv(Data_temp,file=writefile)
    }
  }  
}


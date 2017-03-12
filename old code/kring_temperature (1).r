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
library(rgdal)
library(animation)

setwd("F:/我的文件/pm2.5/data_1_18/Data_1_18/weather")
# pm2.5 station
api_station<-read.csv("api_station_name_weather_code.csv")
# weather station
weather_station<-read.csv("weather_website.csv")

# select weather station in Beijing, Tianjin, Hebei and Shangdong
names_province<-c("ABJ","ATJ","ASD","AHE")
weather_station<-weather_station[weather_station$province_code%in%names_province,]

# select PM2.5 station in Beijing, Tianjin, Hebei 
beijing_list<-api_station[api_station$province_name.English=="beijing",]
tianjin_list<-api_station[api_station$province_name.English=="tianjin",]
hebei_list<-api_station[api_station$province_name.English=="hebei",]
shandong_list<-api_station[api_station$province_name.English=="shandong",]

# Only select some stations in Shandong, they are latitude larger than 36 and longitude less than 119  
# You may see the map latter
shandong_list<-shandong_list[shandong_list$latitude>36,]
shandong_list<-shandong_list[shandong_list$longitude<119,]
location<-rbind(beijing_list,tianjin_list,hebei_list,shandong_list)


# First step: pick out the data from Jan 16. 12pm - Jan Jan 18. 12 pm
readdir<-"F:/我的文件/pm2.5/data_1_18/Data_1_18/weather/data_organize/"  # The weather data location
writedir<-"C:/Users/bin/Desktop/kriging_2015/"    # The location to save the image plot
# We only pick the following date from Jan 16, 12pm to Jan 18, 12pm as an example 
year=2015
month=1
day_0=16
hour_0=12

day_1=18
hour_1=12

# weather variable
# weather_index=7:  Temperature
# weather_index=8:  Pressure
# weather_index=9:  Precipitation (do not need to do kriging for this variable, I set all of them as 0)
# weather_index=10: Humidity
# weather_index=11:  Wind direction
# weather_index=12:  Wind speed
# weather_index=13: Iws
weather_index=7

pm.data <- matrix(NA, 49, dim(location)[1])  # The time length is 49
pm.data <- data.frame(pm.data)
colnames(pm.data) <- location$station_code
for(i in 1:dim(pm.data)[2])
{
  for(j in 1:dim(weather_station)[1])
  {
    # From the weather_num to find the corresponding weather station for each Pm2.5 station
    if(location$weather_num[i]==weather_station$weather_num[j]) 
    { 
      
      # Read the corresponding weather data  
      data_temp_name<-paste0(readdir,weather_station$province_code[j],"_",weather_station$city_name[j],".csv")
      data_temp<-read.csv(data_temp_name)
      # Find the weather data at the fixed time, if can not find, then shows NA 
      # Find the weather data in the time gap
      date_0<-c(1:dim(data_temp)[1])[(data_temp$year==year)&(data_temp$month==month)&(data_temp$day==day_0)&((data_temp$hour==hour_0))]
      date_1<-c(1:dim(data_temp)[1])[(data_temp$year==year)&(data_temp$month==month)&(data_temp$day==day_1)&((data_temp$hour==hour_1))]
      
      data_temp<-data_temp[date_0:date_1,]
      # Find the corresponding weather variable
      pm.data[,i]<-data_temp[,weather_index] 
    }
    
  }
}

# The following are used to plot the time series and the ACF
names_1<-c("1001A","1013A","1067A","1036A","1051A","1057A")
names_2<-c("beijing","tianjin","langfang","tangshan","baoding","zhangjiakou")
names_3<-c("BJ","TJ","LF","TS","BD","ZJK")

par(mfrow=c(3,2))
for(i in 1:6)
{
  plot(pm.data[,names(pm.data)==names_1[i]],ylab="Temperaure",main=names_2[i],type="l")
  points(pm.data[,names(pm.data)==names_1[i]],col="blue")
}
par(mfrow=c(1,1))
pm.datanew<-pm.data[,names(pm.data)%in%names_1]
names(pm.datanew)<-names_3
acf(pm.datanew)

# the following is used to carry out spatial analysis
coord.proj <- project(cbind(location$longitude,location$latitude), "+proj=utm +zone=50 ellps=WGS84")/10000
distance <- dist(coord.proj, method = "euclidean", diag = TRUE, upper = TRUE, p = 2)
location_name<-as.character(location$city_name.English)  

#--------------------------------

grid.num <- 50
sp1 <- seq(min(coord.proj[,1])-1,max(coord.proj[,1])+1,length=grid.num)
sp2 <- seq(min(coord.proj[,2])-1,max(coord.proj[,2])+1,length=grid.num)
sp <- expand.grid(sp1,sp2)

# Here we need to add a very small noise to avoid the error in kriging,
# That is to avoid some of the sigular estimated covariance 

pm.data<-pm.data+10^(-5)*matrix(rnorm((dim(pm.data)[1])*(dim(pm.data)[2])),nrow=(dim(pm.data)[1]))

for(i in 1:dim(pm.data)[1]) 
{
  
  names1=paste0(writedir,"image_",i,".ps")
  ps.options(horizontal=F, width=15, height=15, reset=T, points=20.3)
  postscript(file=names1, onefile=F, print=F)
  
  coord.proj_1<-cbind(coord.proj,as.numeric(pm.data[i,]))
  
  pm.t <- data.frame(coord.proj_1)
  pm.t <- as.geodata(pm.t)
  
  # Estimate the empirical variograms using the robust estimtor
  
  variogram.t <- variog(pm.t,max.dist=30,estimator.type="modulus")
  
  
  # Fit the variogram using the exponential models 
  
  fit.t <- variofit(variogram.t, ini.cov.pars=c(5, 10),cov.model="exponential",weights="cressie")    
  #plot(variogram.t) 
  #lines(fit.t)  
  
  
  kriging <- krige.control(type.krige="ok",cov.model="exponential",
                           cov.pars=fit.t$cov.pars, nugget=fit.t$nugget)
  
  
  # The following code means if there is some errors happen, then go to next loop 
  res <- try(krige.conv(pm.t, locations=sp, krige=kriging),silent=TRUE)
  if(inherits(res, "try-error"))
  {
    next
  }
  
  pred <- krige.conv(pm.t, locations=sp, krige=kriging)
  
  # q1 <- kriging(coord.proj[,1],coord.proj[,2],as.numeric(pm.data[i,]))
  
  # The image plot
  # in image plot, you may set "zlim=" such that the corlour bar are the same
  image.plot(sp1,sp2, (matrix((pred$predict),grid.num,grid.num)), horizontal =TRUE,zlim=c(min(pm.data),max(pm.data)),main=paste0("Ordinary kriging,"," ","T=",i))
  # The 3D plot
  
  # To show the points in the image plot
  points(coord.proj[location_name=="beijing",1],coord.proj[location_name=="beijing",2])
  coord<-coord.proj[location_name=="beijing",]
  text(coord[1,1],coord[1,2],"BJ")
  points(coord.proj[location_name=="tianjin",1],coord.proj[location_name=="tianjin",2])
  coord<-coord.proj[location_name=="tianjin",]
  text(coord[1,1],coord[1,2],"TJ")
  points(coord.proj[location_name=="zhangjiakou",1],coord.proj[location_name=="zhangjiakou",2])
  coord<-coord.proj[location_name=="zhangjiakou",]
  text(coord[1,1],coord[1,2],"ZJK")
  points(coord.proj[location_name=="shijiazhuang",1],coord.proj[location_name=="shijiazhuang",2])
  coord<-coord.proj[location_name=="shijiazhuang",]
  text(coord[1,1],coord[1,2],"SJZ")
  points(coord.proj[location_name=="tangshan",1],coord.proj[location_name=="tangshan",2])
  coord<-coord.proj[location_name=="tangshan",]
  text(coord[1,1],coord[1,2],"TS")
  points(coord.proj[location_name=="baoding",1],coord.proj[location_name=="baoding",2])
  coord<-coord.proj[location_name=="baoding",]
  text(coord[1,1],coord[1,2],"BD")
  points(coord.proj[location_name=="xingtai",1],coord.proj[location_name=="xingtai",2])
  coord<-coord.proj[location_name=="xingtai",]
  text(coord[1,1],coord[1,2],"XT")
  points(coord.proj[location_name=="qinhuangdao",1],coord.proj[location_name=="qinhuangdao",2])
  coord<-coord.proj[location_name=="qinhuangdao",]
  text(coord[1,1],coord[1,2],"QHD")
  points(coord.proj[location_name=="cangzhou",1],coord.proj[location_name=="cangzhou",2])
  coord<-coord.proj[location_name=="cangzhou",]
  text(coord[1,1],coord[1,2],"CZ")
  points(coord.proj[location_name=="hengshui",1],coord.proj[location_name=="hengshui",2])
  coord<-coord.proj[location_name=="hengshui",]
  text(coord[1,1],coord[1,2],"HS")
  dev.off()
  
}

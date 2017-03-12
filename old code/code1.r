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

# Plot the map of the locations in this aera
plot(x=location$longitude, y=location$latitude,xlab="longitude",ylab="latitude",main="Pm2.5 station")
map("china",add=T)
map.cities(country = "China", capitals = 1, cex=0.8)
map.cities(country = "China", capitals = 2, cex=0.8)




 # The locations where the organized weather data are saved
 readdir<-"F:/我的文件/pm2.5/data_1_18/Data_1_18/weather/data_organize/"
 
 # We only pick the following date as an example 
 year=2014
 month=12
 day=12
 hour=11
 # weather variable
 # weather_index=7:  Temperature
 # weather_index=8:  Pressure
 # weather_index=9:  Precipitation (do not need to do kriging for this variable, I set all of them as 0)
 # weather_index=10: Humidity
 # weather_index=11:  Wind direction
 # weather_index=12:  Wind speed
 # weather_index=13: Iws
 
 weather_index=7
 
 # You need to run code1.r, then code2.r
 coord.proj <- project(cbind(location$longitude,location$latitude), "+proj=utm +zone=50 ellps=WGS84")/10000
 distance <- dist(coord.proj, method = "euclidean", diag = TRUE, upper = TRUE, p = 2)
 
 #--------------------------------
 
 grid.num <- 10
 sp1 <- seq(min(coord.proj[,1])-1,max(coord.proj[,1])+1,length=grid.num)
 sp2 <- seq(min(coord.proj[,2])-1,max(coord.proj[,2])+1,length=grid.num)
 sp <- expand.grid(sp1,sp2)
 
 # Pick up the corresponding weather data for different locations at a fixed time. 
 pm.data <- matrix(NA, 1, dim(location)[1]) 
 pm.data <- data.frame(pm.data)
 colnames(pm.data)<-location$station_code
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
       data_temp<-data_temp[(data_temp$year==year)&(data_temp$month==month)&(data_temp$day==day)&((data_temp$hour==hour)),]
 # Find the corresponding weather variable
       pm.data[i]<-data_temp[,weather_index] 
     }
     
   }
 }
 # There are total 116 PM2.5 locations
 length(pm.data)
 # Match the lattitude and longitude for the weather variable
 coord.proj<-cbind(coord.proj,as.numeric(pm.data))

 # Find the corresponding places for the weather variable  
 location_name<-as.character(location$city_name.English[!is.na(coord.proj[,3])])  
 # Remove the location where the weather variable is NA
 coord.proj<-coord.proj[!is.na(coord.proj[,3]),]

 # Shows how mang locations that has the observed weather variable  
 dim(coord.proj)[1]
 
# The next step is to carry out the ordinary kriging
 
 pm.t <- data.frame(coord.proj)
 pm.t <- as.geodata(pm.t)

# Estimate the empirical variograms using the robust estimtor
 
 variogram.t <- variog(pm.t,max.dist=30,estimator.type="modulus")
 plot(variogram.t) 
 lines(fit.t)  

# Fit the variogram using the exponential models 
 
 fit.t <- variofit(variogram.t, ini.cov.pars=c(2,4), cov.model="exponential",weights="cressie")    
 
 kriging <- krige.control(type.krige="ok",cov.model="exponential",
                          cov.pars=fit.t$cov.pars, nugget=fit.t$nugget)
 pred <- krige.conv(pm.t, locations=sp, krige=kriging)
 
 # The image plot
 # in image plot, you may set "zlim=" such that the corlour bar are the same
 image.plot(sp1,sp2, (matrix((pred$predict),grid.num,grid.num)), horizontal =TRUE)
 # The 3D plot
 persp(sp1,sp2, (matrix((pred$predict),grid.num,grid.num)), xlab="x coordinate",
       ylab="y coordinate", zlab="Predicted values of z",
       main="Perspective plot of the predicted values")
 
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
 
 
 
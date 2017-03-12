

##################################################################################################################
###                                                                                                            ###
###			The following code is written to illustrate Variogram and its model fitting as well				   ###
###					Spatial Predictions using Ordinary, Simple and Universal Kriging.                          ###                           
###                                                                                                            ###
###                                       Author: JAMES B. ODEI 	                                           ###			
###                                                                                                            ###
###                                        Date: April 10, 2009                                                ###  
###                                                                                                            ###
##################################################################################################################


################################## EXAMPLE #1 (VARIOGRAM) #############################################

data_url<-"http://www.math.usu.edu/~symanzik/teaching/2009_stat6560/RDataAndScripts/odei_james_project2_ozone.txt"
ozonedata<-read.table(url(data_url), header=FALSE)
names(ozonedata)=c("Longitude", "Latitude", "Ozone.Conc")

#ozonedata=read.table("odei_james_project2_ozone.txt")
#names(ozonedata)=c("Longitude", "Latitude", "Ozone.Conc")


########## Spatial Plot for Ozone Conc.  #####################

pdf(file = "odei_james_project2_fig2.pdf",width=8, height=8, pointsize = 12, bg = "white")
plot(ozonedata[,1],ozonedata[,2],type="n",xlab="Longitude",ylab="Latitude",xlim=c(-88.35, -87), ylim=c(41.4, 42.5), 
		main="Spatial Process Field of Ozone Conc. in Chicago (Summer 1987)")

symbols(ozonedata[,1],ozonedata[,2],circles=(ozonedata[,3]-min(ozonedata[,3]))/600,bg=2,inches=FALSE,add=TRUE)

dev.off()


#############  Variogram (as well as Cloud) ########## 
library(gstat)

coordinates(ozonedata)=c("Longitude", "Latitude")

plot(variogram(log(Ozone.Conc) ~ 1, ozonedata), col=2, main="Variogram of Ozone Conc. in Chicago (Summer 1987)")

plot(variogram(log(Ozone.Conc) ~ 1,ozonedata, cloud=TRUE), col=4,  main="Variogram Cloud of Ozone Conc. in Chicago (Summer 1987)")


#################### Variogram Modeling ######################## 

show.vgms()    # An Overview of the basic variogram models available in gstat package

show.vgms(model="Mat", kappa.range=c(0.1,0.2,0.5,1,2,5,10), max=10)  # An overview of various models in the Matern Class


####   Variogram models are buit as follows: #######

vgm()    ### Gives list of model types

vgm(1, "Sph", 300)

vgm(1, "Sph", 300, 0.5)

v1=vgm(1, "Sph", 300, 0.5)
v2=vgm(0.8, "Sph", 300, add.to=v1)

v2

vgm(0.5, "Nug", 0)  ## and so on

##  NOTE:   Not all of these models are equally useful, in practice. Most practical studies have so used Exponential, Spherical, Gaussian, Matern,
##   	    or Power models with or without a nugget or a combination of those.


vm=variogram(log(Ozone.Conc) ~ 1, ozonedata)
plot(vm, col=2, main="Variogram of Ozone Concentration in Chicago (Summer 1987)")

#par(mfrow=c(2,2))

vm.fit1= fit.variogram(vm, vgm(0.08, "Sph", 0.3, 0))
plot(vm, vm.fit1, col=4, main="Variogram of Ozone Conc. in Chicago (Summer 1987)
				and Shperical Fitted Model" )

vm.fit2= fit.variogram(vm, vgm(0.08, "Exp", 0.2, 0))
plot(vm, vm.fit2, col=4, main="Variogram of Ozone Conc. in Chicago (Summer 1987) 
			and Exponential Fitted Model")

vm.fit3= fit.variogram(vm, vgm(0.15, "Gau", 0.5, 0))
plot(vm, vm.fit3, col=4, main="Variogram of Ozone Conc. in Chicago (Summer 1987)
 			and Gaussian Fitted Model" )

vm.fit4= fit.variogram(vm, vgm(1, "Mat", 1, kappa=5))
plot(vm, vm.fit4, col=4, main="Variogram of Ozone Conc. in Chicago (Summer 1987)
			and Matern Fitted Model" )


################################## EXAMPLE #2 (SPATIAL PREDICTIONS) #######################################

data_url<-"http://www.math.usu.edu/~symanzik/teaching/2009_stat6560/RDataAndScripts/odei_james_project2_rabbit.txt"
rabbitdata<-read.table(url(data_url), header=TRUE)

data_url<-"http://www.math.usu.edu/~symanzik/teaching/2009_stat6560/RDataAndScripts/odei_james_project2_rabbit.grid.txt"
rabbit.grid<-read.table(url(data_url), header=TRUE)


library(gstat)


#rabbitdata=read.table("odei_james_project2_rabbit.txt", header=TRUE)
coordinates(rabbitdata)=c("UTMX", "UTMY")

#rabbit.grid=read.table("odei_james_project2_rabbit.grid.txt", header=TRUE)
gridded(rabbit.grid) = c("UTMX", "UTMY")


rt1=variogram(log(Sign/Obs+0.001) ~ 1, rabbitdata)
plot(rt1, vgm(3.5, "Exp",2000,0))

rabbit.lm1=krige(log(Sign/Obs+0.001) ~ 1, rabbitdata, rabbit.grid)


rt2=variogram(log(Sign/Obs+0.001) ~ sqrt(Slope_mean), rabbitdata)
plot(rt2, vgm(3.5, "Exp",2000,0))


rabbit.lm2=krige(log(Sign/Obs+0.001)~ sqrt(Slope_mean), rabbitdata, rabbit.grid)


m=vgm(3.5, "Exp",2000,0)


######## Ordinary Kriging: ########
x <- krige(log(Sign/Obs+0.001) ~ 1, rabbitdata, rabbit.grid, model = m)
spplot(x["var1.pred"], main = "Ordinary kriging predictions for Rabbit Burrow Occupancy")

spplot(x["var1.var"],  main = "Ordinary kriging variance for Rabbit Burrow Occupancy")


####### Simple Kriging: ########
y <- krige(log(Sign/Obs+0.001) ~ 1, rabbitdata, rabbit.grid, model = m, beta = 2)
spplot(y["var1.pred"], main = "Simple kriging predictions for Rabbit Burrow Occupancy")

spplot(y["var1.var"],  main = "Simple kriging variance for Rabbit Burrow Occupancy")


####### Universal Block Kriging: ######
z <- krige(log(Sign/Obs+0.001) ~ sqrt(Slope_mean), rabbitdata, rabbit.grid, model = m, block = c(0,0))
spplot(z["var1.pred"], main = "Universal kriging predictions for Rabbit Burrow Occupancy")

































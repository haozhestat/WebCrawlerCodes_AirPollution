# contour plot for PRESimpute and P, H, D

library(np)
library(plot3D)
library(scatterplot3d)
setwd("F:/我的文件/pm2.5/partial linear model")
pm2.5_2014=read.csv("data2014_9_10.csv")
a=pm2.5_2014[,-1]
pm2.5_2014=a[a$pm2.5!=-666666,]

# contour plot

finalse=pm2.5_2014[pm2.5_2014$wd=="SE",]
#finalse=data.frame(datase[,1:10],predse,datase[,11:length(datase[1,])])

finalnw=pm2.5_2014[pm2.5_2014$wd=="NW",]
#finalnw=data.frame(datanw[,1:10],prednw,datanw[,11:length(datanw[1,])])

finalne=pm2.5_2014[pm2.5_2014$wd=="NE",]
#finalne=data.frame(datane[,1:10],predne,datane[,11:length(datane[1,])])

finalsw=pm2.5_2014[pm2.5_2014$wd=="SW",]
#finalsw=data.frame(datasw[,1:10],predsw,datasw[,11:length(datasw[1,])])

finalcalm=pm2.5_2014[pm2.5_2014$wd=="calm",]
#finalcalm=data.frame(datacalm[,1:10],predcalm,datacalm[,11:length(datacalm[1,])])

finalvariable=pm2.5_2014[pm2.5_2014$wd=="variable",]


xse_2014=finalse$TEMPimpute
yse_2014=finalse$PRESimpute
zse_2014=finalse$pm2.5

xnw_2014=finalnw$TEMPimpute
ynw_2014=finalnw$PRESimpute
znw_2014=finalnw$pm2.5

xsw_2014=finalsw$TEMPimpute
ysw_2014=finalsw$PRESimpute
zsw_2014=finalsw$pm2.5

xne_2014=finalne$TEMPimpute
yne_2014=finalne$PRESimpute
zne_2014=finalne$pm2.5


xcalm_2014=finalcalm$TEMPimpute
ycalm_2014=finalcalm$PRESimpute
zcalm_2014=finalcalm$pm2.5

xvariable_2014=finalvariable$TEMPimpute
yvariable_2014=finalvariable$PRESimpute
zvariable_2014=finalvariable$pm2.5

exdatxse_2014=seq(from=min(xse_2014),to=max(xse_2014),length.out=100)
exdatxnewse_2014=rep(exdatxse_2014,100)

exdatyse_2014=seq(from=min(yse_2014),to=max(yse_2014),length.out=100)
exdatynewse_2014=NULL
for(i in 1:length(exdatyse_2014))
{
  exdatynewse_2014=c(exdatynewse_2014,rep(exdatyse_2014[i],100))
}

bw <- npreg(tydat=zse_2014,txdat=data.frame(xse_2014,yse_2014),exdat=data.frame(exdatxnewse_2014,exdatynewse_2014),regtype="ll")

#####################################################################
bwse<-bw$bw
#ghat<-npreg(bws=2*bw$bw,xdat=finalnw$TEMPimpute,ydat=finalnw$pm2.5)



zsefinal_2014=matrix(bw$mean,nrow=100,byrow=T)

#write.csv(data.frame(exdatxse_2014,exdatyse_2014),file="/home/binguo/pm2.5/contour/n_2014/xyse_2014.csv")
#write.csv(data.frame(zsefinal_2014),file="/home/binguo/pm2.5/contour/n_2014/zse_2014.csv")

exdatxsw_2014=seq(from=min(xsw_2014),to=max(xsw_2014),length.out=100)
exdatysw_2014=seq(from=min(ysw_2014),to=max(ysw_2014),length.out=100)
exdatxnewsw_2014=rep(exdatxsw_2014,100)
exdatynewsw_2014=NULL
for(i in 1:length(exdatysw_2014))
{
  exdatynewsw_2014=c(exdatynewsw_2014,rep(exdatysw_2014[i],100))
}

bw <- npreg(tydat=zsw_2014,txdat=data.frame(xsw_2014,ysw_2014),exdat=data.frame(exdatxnewsw_2014,exdatynewsw_2014),regtype="ll")
#ghat<-npreg(bws=2*bw$bw,xdat=finalnw$TEMPimpute,ydat=finalnw$pm2.5)
bwsw<-bw$bw
zswfinal_2014=matrix(bw$mean,nrow=100,byrow=T)

#write.csv(data.frame(exdatxsw_2014,exdatysw_2014),file="/home/binguo/pm2.5/contour/n_2014/xysw_2014.csv")
#write.csv(data.frame(zswfinal_2014),file="/home/binguo/pm2.5/contour/n_2014/zsw_2014.csv")

exdatxnw_2014=seq(from=min(xnw_2014),to=max(xnw_2014),length.out=100)
exdatynw_2014=seq(from=min(ynw_2014),to=max(ynw_2014),length.out=100)
exdatxnewnw_2014=rep(exdatxnw_2014,100)
exdatynewnw_2014=NULL
for(i in 1:length(exdatynw_2014))
{
  exdatynewnw_2014=c(exdatynewnw_2014,rep(exdatynw_2014[i],100))
}

bw <- npreg(tydat=znw_2014,txdat=data.frame(xnw_2014,ynw_2014),exdat=data.frame(exdatxnewnw_2014,exdatynewnw_2014),regtype="ll")
#ghat<-npreg(bws=2*bw$bw,xdat=finalnw$TEMPimpute,ydat=finalnw$pm2.5)
bwnw<-bw$bw
znwfinal_2014=matrix(bw$mean,nrow=100,byrow=T)

#write.csv(data.frame(exdatxnw_2014,exdatynw_2014),file="/home/binguo/pm2.5/contour/n_2014/xynw_2014.csv")
#write.csv(data.frame(znwfinal_2014),file="/home/binguo/pm2.5/contour/n_2014/znw_2014.csv")

exdatxne_2014=seq(from=min(xne_2014),to=max(xne_2014),length.out=100)
exdatyne_2014=seq(from=min(yne_2014),to=max(yne_2014),length.out=100)
exdatxnewne_2014=rep(exdatxne_2014,100)
exdatynewne_2014=NULL
for(i in 1:length(exdatyne_2014))
{
  exdatynewne_2014=c(exdatynewne_2014,rep(exdatyne_2014[i],100))
}

bw <- npreg(tydat=zne_2014,txdat=data.frame(xne_2014,yne_2014),exdat=data.frame(exdatxnewne_2014,exdatynewne_2014),regtype="ll")
#ghat<-npreg(bws=2*bw$bw,xdat=finalne$TEMPimpute,ydat=finalne$pm2.5)
bwne<-bw$bw
znefinal_2014=matrix(bw$mean,nrow=100,byrow=T)

#write.csv(data.frame(exdatxne_2014,exdatyne_2014),file="/home/binguo/pm2.5/contour/n_2014/xyne_2014.csv")
#write.csv(data.frame(znefinal_2014),file="/home/binguo/pm2.5/contour/n_2014/zne_2014.csv")

exdatxcalm_2014=seq(from=min(xcalm_2014),to=max(xcalm_2014),length.out=100)
exdatycalm_2014=seq(from=min(ycalm_2014),to=max(ycalm_2014),length.out=100)
exdatxnewcalm_2014=rep(exdatxcalm_2014,100)
exdatynewcalm_2014=NULL
for(i in 1:length(exdatycalm_2014))
{
  exdatynewcalm_2014=c(exdatynewcalm_2014,rep(exdatycalm_2014[i],100))
}

bw <- npreg(tydat=zcalm_2014,txdat=data.frame(xcalm_2014,ycalm_2014),exdat=data.frame(exdatxnewcalm_2014,exdatynewcalm_2014),regtype="ll")
#ghat<-npreg(bws=2*bw$bw,xdat=finalcalm$TEMPimpute,ydat=finalcalm$pm2.5)
bwcalm<-bw$bw
zcalmfinal_2014=matrix(bw$mean,nrow=100,byrow=T)

#write.csv(data.frame(exdatxcalm_2014,exdatycalm_2014),file="/home/binguo/pm2.5/contour/n_2014/xycalm_2014.csv")
#write.csv(data.frame(zcalmfinal_2014),file="/home/binguo/pm2.5/contour/n_2014/zcalm_2014.csv")

exdatxvariable_2014=seq(from=min(xvariable_2014),to=max(xvariable_2014),length.out=100)
exdatyvariable_2014=seq(from=min(yvariable_2014),to=max(yvariable_2014),length.out=100)
exdatxnewvariable_2014=rep(exdatxvariable_2014,100)
exdatynewvariable_2014=NULL
for(i in 1:length(exdatyvariable_2014))
{
  exdatynewvariable_2014=c(exdatynewvariable_2014,rep(exdatyvariable_2014[i],100))
}

bw <- npreg(tydat=zvariable_2014,txdat=data.frame(xvariable_2014,yvariable_2014),exdat=data.frame(exdatxnewvariable_2014,exdatynewvariable_2014),regtype="ll")
#ghat<-npreg(bws=2*bw$bw,xdat=finalvariable$TEMPimpute,ydat=finalvariable$pm2.5)
bwvariable<-bw$bw
zvariablefinal_2014=matrix(bw$mean,nrow=100,byrow=T)

#write.csv(data.frame(exdatxvariable_2014,exdatyvariable_2014),file="/home/binguo/pm2.5/contour/n_2014/xyvariable_2014.csv")
#write.csv(data.frame(zvariablefinal_2014),file="/home/binguo/pm2.5/contour/n_2014/zvariable_2014.csv")

x=exdatxse_2014
y=exdatyse_2014
z=zsefinal_2014
setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_SE_scatter_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

s3d<-scatterplot3d(xse_2014,yse_2014,zse_2014, type="h",main="3D Scatterplot, SE 2014",xlab="DEW",ylab="PRESimpute",zlab="pm2.5")
fit <- lm(zse_2014 ~ xse_2014+yse_2014) 
s3d$plane3d(fit,col="red")
dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_SE_3D_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

#persp(x,y,as.matrix(z),col="red")

name1=paste("Smoothing plot, SE 2014")
name2=paste("TEMPimpute_bw:", round(bwse[1],3),"  PRESimpute_bw:", round(bwse[2],3))
persp(x,y,as.matrix(z),main=name1,sub=name2,cex.sub = 1.2,xlab="TEMPimpute",ylab="PRESimpute",zlab="pm2.5", theta = 30, phi = 30, expand = 0.5, col = "lightblue")->res
points(trans3d(xse_2014,yse_2014,zse_2014,pmat=res),col = 2, pch = 16)
dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_SE_contour_TEMPimpute_PRESimpute.eps", onefile=T, print=F)
contour(x,y,as.matrix(z),main="contour plot, SE 2014")
dev.off()

#################################################################

x=exdatxsw_2014
y=exdatysw_2014
z=zswfinal_2014

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_SW_scatter_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

s3d<-scatterplot3d(xsw_2014,ysw_2014,zsw_2014, type="h",main="3D Scatterplot, SW 2014",xlab="TEMPimpute",ylab="PRESimpute",zlab="pm2.5")
fit <- lm(zsw_2014 ~ xsw_2014+ysw_2014) 
s3d$plane3d(fit,col="red")
dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_SW_3D_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

name1=paste("Smoothing plot, SW 2014")
name2=paste("TEMPimpute_bw:", round(bwsw[1],3),"  PRESimpute_bw:", round(bwsw[2],3))
persp(x,y,as.matrix(z),main=name1,sub=name2,cex.sub = 1.2,xlab="TEMPimpute",ylab="PRESimpute",zlab="pm2.5", theta = 30, phi = 30, expand = 0.5, col = "lightblue")->res
points(trans3d(xsw_2014,ysw_2014,zsw_2014,pmat=res),col = 2, pch = 16)
dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_SW_contour_TEMPimpute_PRESimpute.eps", onefile=T, print=F)
contour(x,y,as.matrix(z),main="contour plot, SW 2014")
dev.off()


###########################################################################

x=exdatxne_2014
y=exdatyne_2014
z=znefinal_2014

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_NE_scatter_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

s3d<-scatterplot3d(xne_2014,yne_2014,zne_2014, type="h",main="3D Scatterplot, NE 2014",xlab="TEMPimpute",ylab="PRESimpute",zlab="pm2.5")
fit <- lm(zne_2014 ~ xne_2014+yne_2014) 
s3d$plane3d(fit,col="red")
dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_NE_3D_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

name1=paste("Smoothing plot, NE 2014")
name2=paste("TEMPimpute_bw:", round(bwne[1],3),"  PRESimpute_bw:", round(bwne[2],3))
persp(x,y,as.matrix(z),main=name1,sub=name2,cex.sub = 1.2,xlab="TEMPimpute",ylab="PRESimpute",zlab="pm2.5", theta = 30, phi = 30, expand = 0.5, col = "lightblue")->res
points(trans3d(xne_2014,yne_2014,zne_2014,pmat=res),col = 2, pch = 16)
dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_NE_contour_TEMPimpute_PRESimpute.eps", onefile=T, print=F)
contour(x,y,as.matrix(z),main="contour plot, NE 2014")
dev.off()
############################################################################

x=exdatxnw_2014
y=exdatynw_2014
z=znwfinal_2014


setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_NW_scatter_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

s3d<-scatterplot3d(xnw_2014,ynw_2014,znw_2014, type="h",main="3D Scatterplot, NW 2014",xlab="TEMPimpute",ylab="PRESimpute",zlab="pm2.5")
fit <- lm(znw_2014 ~ xnw_2014+ynw_2014) 
s3d$plane3d(fit,col="red")
dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_NW_3D_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

name1=paste("Smoothing plot, NW 2014")
name2=paste("TEMPimpute_bw:", round(bwnw[1],3),"  PRESimpute_bw:", round(bwnw[2],3))
persp(x,y,as.matrix(z),main=name1,sub=name2,cex.sub = 1.2,xlab="TEMPimpute",ylab="PRESimpute",zlab="pm2.5", theta = 10, phi = 30, expand = 0.5, col = "lightblue")->res
points(trans3d(xnw_2014,ynw_2014,znw_2014,pmat=res),col = 2, pch = 16)
dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_NW_contour_TEMPimpute_PRESimpute.eps", onefile=T, print=F)
contour(x,y,as.matrix(z),main="contour plot, NW 2014")
dev.off()

###########################################################################
x=exdatxcalm_2014
y=exdatycalm_2014
z=zcalmfinal_2014

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_calm_scatter_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

s3d<-scatterplot3d(xcalm_2014,ycalm_2014,zcalm_2014, type="h",main="3D Scatterplot, calm 2014",xlab="TEMPimpute",ylab="PRESimpute",zlab="pm2.5")
fit <- lm(zcalm_2014 ~ xcalm_2014+ycalm_2014) 
s3d$plane3d(fit,col="red")
dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_calm_3D_TEMPimpute_PRESimpute.eps", onefile=T, print=F)


name1=paste("Smoothing plot, calm 2014")
name2=paste("TEMPimpute_bw:", round(bwcalm[1],3),"  PRESimpute_bw:", round(bwcalm[2],3))
persp(x,y,as.matrix(z),main=name1,sub=name2,cex.sub = 1.2,xlab="TEMPimpute",ylab="PRESimpute",zlab="pm2.5", theta = 30, phi =30, expand = 0.5, col = "lightblue")->res
points(trans3d(xcalm_2014,ycalm_2014,zcalm_2014,pmat=res),col = 2, pch = 16)
dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)

postscript(file="C:/Users/bin/Desktop/contour/2014_calm_contour_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

contour(x,y,as.matrix(z),main="contour plot, calm 2014")
dev.off()

##############################################################################
x=exdatxvariable_2014
y=exdatyvariable_2014
z=zvariablefinal_2014

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_variable_scatter_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

s3d<-scatterplot3d(xvariable_2014,yvariable_2014,zvariable_2014, type="h",main="3D Scatterplot, variable 2014",xlab="TEMPimpute",ylab="PRESimpute",zlab="pm2.5")
fit <- lm(zvariable_2014 ~ xvariable_2014+yvariable_2014) 
s3d$plane3d(fit,col="red")
dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_variable_3D_TEMPimpute_PRESimpute.eps", onefile=T, print=F)

name1=paste("Smoothing plot, variable 2014")
name2=paste("TEMPimpute_bw:", round(bwvariable[1],3),"  PRESimpute_bw:", round(bwvariable[2],3))
persp(x,y,as.matrix(z),main=name1,sub=name2,cex.sub = 1.2,xlab="TEMPimpute",ylab="PRESimpute",zlab="pm2.5", theta = 30, phi = 30, expand = 0.5, col = "lightblue")->res
points(trans3d(xvariable_2014,yvariable_2014,zvariable_2014,pmat=res),col = 2, pch = 16)

dev.off()

setEPS(horizontal=F, width=10.7, height=10.7, reset=T, points=15.3)
postscript(file="C:/Users/bin/Desktop/contour/2014_variable_contour_TEMPimpute_PRESimpute.eps", onefile=T, print=F)
contour(x,y,as.matrix(z),main="contour plot, variable 2014")
dev.off()

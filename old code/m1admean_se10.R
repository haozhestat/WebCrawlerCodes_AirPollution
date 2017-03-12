
library(np)
library(boot)

a=read.csv(file="~/pm2.5/pm1107(cv).csv",head=TRUE)
a=a[a$pm2.5!=-666666,]

y=10

getbw=function(pm)
{
calm=pm[pm$Iws==0.45,]
rain=pm[pm$Ir!=0,]
snow=pm[pm$Is!=0,]
n=dim(pm)[1]
nr=dim(rain)[1]
ns=dim(snow)[1]
nIws=dim(calm)[1]
if(n==0)
{
c=1
}else if(n==nIws)
{
if(nr*ns!=0)
{
c=c(15,16,17,18,25,26)
}
else if(ns!=0)
{
c=c(15,16,17,18,26)
}
else if(nr!=0)
{
c=c(15,16,17,18,25)
}else
{
c=c(15,16,17,18)
}
}else if(nr*ns!=0)
{
c=c(15,16,17,18,25,26,28)
}else if(ns!=0)
{
c=c(15,16,17,18,26,28)
}else if(nr!=0)
{
c=c(15,16,17,18,25,28)
}else
{
c=c(15,16,17,18,28)
}

if(c[1]!=1)
{
bw=npregbw(ydat=pm[,10],xdat=pm[,c])
}else
{
bw=list(bw="null")
}
return(list(bw=bw,c=c))
}

adjustmean=function(bw1,bw2,bw3,bw4,bw5,allpm)
{
if(bw1$c[1]!=1)
{
evex=allpm[allpm$wd=="cv",]
np=npreg(bw1$bw,exdat=evex[,bw1$c])
mean1=np$mean
n1=dim(evex)[1]
}else
{
mean1=n1=0
}
if(bw2$c[1]!=1)
{
evex=allpm[allpm$wd=="NE",]
np=npreg(bw2$bw,exdat=evex[,bw2$c])
mean2=np$mean
n2=dim(evex)[1]
}else
{
mean2=n2=0
}

if(bw3$c[1]!=1)
{
evex=allpm[allpm$wd=="NW",]
np=npreg(bw3$bw,exdat=evex[,bw3$c])
mean3=np$mean
n3=dim(evex)[1]
}else
{
mean3=n3=0
}

if(bw4$c[1]!=1)
{
evex=allpm[allpm$wd=="SE",]
np=npreg(bw4$bw,exdat=evex[,bw4$c])
mean4=np$mean
n4=dim(evex)[1]
}else
{
mean4=n4=0
}

if(bw5$c[1]!=1)
{
evex=allpm[allpm$wd=="SW",]
np=npreg(bw5$bw,exdat=evex[,bw5$c])
mean5=np$mean
n5=dim(evex)[1]
}else
{
mean5=n5=0
}

mean=sum(sum(mean1)+sum(mean2)+sum(mean3)+sum(mean4)+sum(mean5))/(n1+n2+n3+n4+n5)
return(mean)
}


admean=matrix(0,3,12)
tauboot=matrix(0,101,12)
for(k in 1:12)
{

pm=a[a$year==y&a$month==k,]
pm1=pm[pm$wd=="cv",]
pm2=pm[pm$wd=="NE",]
pm3=pm[pm$wd=="NW",]
pm4=pm[pm$wd=="SE",]
pm5=pm[pm$wd=="SW",]


bw1=getbw(pm1)
bw2=getbw(pm2)
bw3=getbw(pm3)
bw4=getbw(pm4)
bw5=getbw(pm5)

allpm=a[a$month==k,]
admean[1,k]=adjustmean(bw1,bw2,bw3,bw4,bw5,allpm)
bw=matrix(0,5,28)
bw[1,bw1$c]=bw1$bw$bw
bw[2,bw2$c]=bw2$bw$bw
bw[3,bw3$c]=bw3$bw$bw
bw[4,bw4$c]=bw4$bw$bw
bw[5,bw5$c]=bw5$bw$bw

colnames(bw)=colnames(a)
rownames(bw)=c("cv","NE","NW","SE","SW")

infile=paste("~/pm2.5/m1admeanresults_se/bw-",y,"-",k,".csv",sep="")
write.csv(bw,infile)


if(k<=10)
{
d10=a[a$year==10&a$month==k,]
d11=a[a$year==11&a$month==k,]
d12=a[a$year==12&a$month==k,]
d13=a[a$year==13&a$month==k,]
d14=a[a$year==14&a$month==k,]
}else
{
d10=a[a$year==10&a$month==k,]
d11=a[a$year==11&a$month==k,]
d12=a[a$year==12&a$month==k,]
d13=a[a$year==13&a$month==k,]
}
if(k<=10)
{
boot10=tsboot(as.matrix(d10),function(x)x,R=100,l=6,sim="fixed")
boot11=tsboot(as.matrix(d11),function(x)x,R=100,l=6,sim="fixed")
boot12=tsboot(as.matrix(d12),function(x)x,R=100,l=6,sim="fixed")
boot13=tsboot(as.matrix(d13),function(x)x,R=100,l=6,sim="fixed")
boot14=tsboot(as.matrix(d14),function(x)x,R=100,l=6,sim="fixed")

bootd10=boot10$t
bootd11=boot11$t
bootd12=boot12$t
bootd13=boot13$t
bootd14=boot14$t
}else
{
boot10=tsboot(as.matrix(d10),function(x)x,R=100,l=6,sim="fixed")
boot11=tsboot(as.matrix(d11),function(x)x,R=100,l=6,sim="fixed")
boot12=tsboot(as.matrix(d12),function(x)x,R=100,l=6,sim="fixed")
boot13=tsboot(as.matrix(d13),function(x)x,R=100,l=6,sim="fixed")
bootd10=boot10$t
bootd11=boot11$t
bootd12=boot12$t
bootd13=boot13$t
}


for(r in 1:100)
{
if(k<=10)
{
mboot10=data.frame(matrix(bootd10[r,],ncol=dim(a)[2]))
mboot11=data.frame(matrix(bootd11[r,],ncol=dim(a)[2]))
mboot12=data.frame(matrix(bootd12[r,],ncol=dim(a)[2]))
mboot13=data.frame(matrix(bootd13[r,],ncol=dim(a)[2]))
mboot14=data.frame(matrix(bootd14[r,],ncol=dim(a)[2]))
mboot=rbind(mboot10,mboot11,mboot12,mboot13,mboot14)
}else
{
mboot10=data.frame(matrix(bootd10[r,],ncol=dim(a)[2]))
mboot11=data.frame(matrix(bootd11[r,],ncol=dim(a)[2]))
mboot12=data.frame(matrix(bootd12[r,],ncol=dim(a)[2]))
mboot13=data.frame(matrix(bootd13[r,],ncol=dim(a)[2]))
mboot=rbind(mboot10,mboot11,mboot12,mboot13)
}
mboot[,15]=as.numeric(as.character(mboot[,15]))
mboot[,16]=as.numeric(as.character(mboot[,16]))
mboot[,17]=as.numeric(as.character(mboot[,17]))
mboot[,18]=as.numeric(as.character(mboot[,18]))
mboot[,25]=as.numeric(as.character(mboot[,25]))
mboot[,26]=as.numeric(as.character(mboot[,26]))
mboot[,28]=as.numeric(as.character(mboot[,28]))

mboot[,27]=factor(mboot[,27],levels=levels(a[,27]))
colnames(mboot)=colnames(a)
boot=adjustmean(bw1,bw2,bw3,bw4,bw5,mboot)
tauboot[r,k]=boot
}
tauboot[101,k]=admean[1,k]
admean[2,k]=sqrt(sum((tauboot[1:100,k]-tauboot[101,k])^2)/99)
admean[3,k]=sd(tauboot[1:100,k])

print(k)
}
infile=paste("~/pm2.5/m1admeanresults_se/tauboot-",y,".csv",sep="")
write.csv(tauboot,infile)

infile=paste("~/pm2.5/m1admeanresults_se/admean-",y,".csv",sep="")
write.csv(admean,infile)

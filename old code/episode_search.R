library(fda)

data2013_11_12=read.csv("data2013_11_12.csv")
names(data2013_11_12)
data2013_11_12=data2013_11_12[data2013_11_12$pm2.5!=-666666,]
logpm=log(data2013_11_12$pm2.5)
#plot(data2013_11_12$pm2.5,type='l')

n=length(data2013_11_12$pm2.5)
#hist(data2013_11_12$pm2.5,40)
#sum(data2013_11_12$pm2.5<=12)

#define time range
timerng=c(1,n)
#observation time points
obstime=1:n

# create smoothing spline basis, order=6 since we penalize 4th derivative
bbasis=create.bspline.basis(timerng,norder=6,breaks=obstime)

# log-lambda range for grid search, where lambda is the smoothing parameter
loglam=seq(-3,1,0.5)
nlam=length(loglam)
# save space for GCV score
gcvsave=rep(NA,nlam)
names(gcvsave) = loglam
#harmLfd = vec2Lfd( c(1,(2*pi/n),0),c(1,n) )
for (ilam in 1:nlam) {
  cat(paste('log10 lambda =',loglam[ilam],'\n'))
  lambda        = 10^loglam[ilam]
  fdParobj      = fdPar(bbasis, 4, lambda)
  #smoothlist    = smooth.basis(obstime, data2013_11_12$pm2.5,fdParobj)
  smoothlist    = smooth.basis(obstime, logpm,fdParobj)
  gcvsave[ilam] = smoothlist$gcv
}

i=which.min(gcvsave)
lambda=10^loglam[i]
#lambda=10^3
fdParobj=fdPar(bbasis, 4, lambda)
#tempfd=smooth.basis(obstime,data2013_11_12$pm2.5,fdParobj)
#plotfit.fd(data2013_11_12$pm2.5,obstime,tempfd$fd)
#abline(h=12)
tempfd=smooth.basis(obstime,logpm,fdParobj)
plotfit.fd(logpm,obstime,tempfd$fd)

# evaluate the fitted function, the 1st derivative and the 2nd derivative
fdline=eval.fd(obstime,tempfd$fd)
veline=eval.fd(obstime,tempfd$fd,1)
acline=eval.fd(obstime,tempfd$fd,2)
plot(fdline,type='l')
abline(h=log(12),col='red')




timecheck=120:150
par(mfrow=c(3,1))
plot(fdline[timecheck],type='l')
abline(h=log(12))
plot(veline[timecheck],type='l')
plot(acline[timecheck],type='l')


# plot phase-plane graphics
plot(veline[timecheck],acline[timecheck],type='l')


      

 



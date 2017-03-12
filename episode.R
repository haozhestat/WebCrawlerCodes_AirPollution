library(ggplot2)
library(lmom)
ep.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/episode.csv")
pm.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/organized data/pm1107(cv).csv")
ep.data <- na.omit(ep.data)

par.gev <- pelgev(samlmu(ep.data$maxpm, nmom=3))
plot(quantile(ep.data$maxpm, probs=seq(0, 1, 0.01)), seq(0, 1, 0.01), 
     xlab="Maximum PM2.5 Values in Episodes", ylab="cdf", 
     #main="Fitting by General Extreme Value Distribution", 
     pch=16, cex=0.7)
lines(seq(30, 900, 10), cdfgev(seq(30, 900, 10), par=par.gev), col="red", cex=3)
#---------------------------------------------------------------------------------
ep.data$up.length <- ep.data$maxindex - ep.data$begin + 1
ep.data$down.length <- ep.data$end - ep.data$maxindex + 1
par(mfrow=c(2,2))
plot(ep.data$up.length, ep.data$maxpm, pch=16, xlab="length of rise phases", ylab="maximum pm2.5")
lines(smooth.spline(ep.data$up.length, ep.data$maxpm, spar=1.2), col="red")
plot(ep.data$down.length, ep.data$maxpm, pch=16, xlab="length of decline phases", ylab="maximum pm2.5")
lines(smooth.spline(ep.data$down.length, ep.data$maxpm, spar=1.3), col="red")
plot(ep.data$length, ep.data$maxpm, pch=16, xlab="length of episodes", ylab="maximum pm2.5")
lines(smooth.spline(ep.data$length, ep.data$maxpm, spar=1.3), col="red")

#-------------------------------------------------------------------------
ep.data$up.wd <- NA
ep.data$down.wd <- NA
ep.data$total.wd <- NA
for (i in 1:nrow(ep.data)){
  tmp.up <- subset(pm.data, no>=ep.data$begin[i] &no<=ep.data$maxindex[i])
  tmp.down <- subset(pm.data, no>ep.data$maxindex[i] & no<=ep.data$end[i])
  tmp.total <- subset(pm.data, no>=ep.data$begin[i] & no<=ep.data$end[i])
  ep.data$up.wd[i] <- sum(tmp.up$wd %in% c("NE", "NW"))/length(tmp.up$wd)
  ep.data$down.wd[i] <- sum(tmp.down$wd %in% c("NE", "NW"))/length(tmp.down$wd)
  ep.data$total.wd[i] <- sum(tmp.total$wd %in% c("NE", "NW"))/length(tmp.total$wd)           
}
par(mfrow=c(2,2))
plot(ep.data$up.wd, ep.data$up.length, pch=16, cex=0.8, 
     xlab="percentages of northerly wind in rise phases", ylab="length of episodes")
abline(lm(ep.data$up.length~ep.data$up.wd), col="red")
plot(ep.data$down.wd, ep.data$down.length, pch=16,
     xlab="percentages of northerly wind in decline phases", ylab="length of episodes")
abline(lm(ep.data$down.length~ep.data$down.wd), col="red")

#--------------------------------------------------
ep.data$up.wd <- NA
ep.data$down.wd <- NA
ep.data$total.wd <- NA
for (i in 1:nrow(ep.data)){
  tmp.up <- subset(pm.data, no>=ep.data$begin[i] &no<=ep.data$maxindex[i])
  tmp.down <- subset(pm.data, no>ep.data$maxindex[i] & no<=ep.data$end[i])
  tmp.total <- subset(pm.data, no>=ep.data$begin[i] & no<=ep.data$end[i])
  ep.data$up.wd[i] <- sum(tmp.up$wd %in% c("SE", "SW"))/length(tmp.up$wd)
  ep.data$down.wd[i] <- sum(tmp.down$wd %in% c("SE", "SW"))/length(tmp.down$wd)
  ep.data$total.wd[i] <- sum(tmp.total$wd %in% c("SE", "SW"))/length(tmp.total$wd)                
}
plot(ep.data$up.wd, ep.data$up.length, pch=16,
     xlab="percentages of southerly wind in rise phases", ylab="length of episodes")
abline(lm(ep.data$up.length~ep.data$up.wd), col="red")
plot(ep.data$down.wd, ep.data$down.length, pch=16,
     xlab="percentages of northerly wind in decline phases", ylab="length of episodes")
abline(lm(ep.data$down.length~ep.data$down.wd), col="red")


#---------------------------------------------------------------------
ep.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/episode.csv")
pm.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/organized data/pm1107(cv).csv")
ep.data <- na.omit(ep.data)
ep.data$up.humi <- NA
ep.data$down.humi <- NA
ep.data$total.humi <- NA
ep.data$max.humi <- NA
for (i in 1:nrow(ep.data)){
  temp.up <- subset(pm.data, no>=ep.data$begin[i] &no<=ep.data$maxindex[i])
  temp.down <- subset(pm.data, no>ep.data$maxindex[i] & no<=ep.data$end[i])
  temp.total <- subset(pm.data, no>=ep.data$begin[i] & no<=ep.data$end[i])
  ep.data$up.humi[i] <- mean(temp.up$HUMIimpute)
  ep.data$down.humi[i] <- mean(temp.down$HUMIimpute)
  ep.data$total.humi[i] <- mean(temp.total$HUMIimpute)
  ep.data$max.humi[i] <- max(temp.total$HUMIimpute)                 
}
par(mfrow=c(2,2))
plot(ep.data$up.humi, ep.data$maxpm, pch=16, xlab="mean of Humidity in rise phase", ylab="maximum PM2.5")
lines(smooth.spline(ep.data$total.humi, ep.data$maxpm, spar=1.5), col="red")
plot(ep.data$down.humi, ep.data$maxpm, pch=16, xlab="mean of Humidity in decline phase", ylab="maximum PM2.5")
lines(smooth.spline(ep.data$total.humi, ep.data$maxpm, spar=1.5), col="red")
plot(ep.data$total.humi, ep.data$maxpm, pch=16, xlab="mean of Humidity in one episode", ylab="maximum PM2.5")
lines(smooth.spline(ep.data$total.humi, ep.data$maxpm, spar=1.5), col="red")
plot(ep.data$max.humi, ep.data$maxpm, pch=16, xlab="Maximum Humidity in rise phase", ylab="maximum PM2.5")
lines(smooth.spline(ep.data$max.humi, ep.data$maxpm, spar=1), col="red", cex=2)
#-----------------------------------------------------------------------
ep.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/episode.csv")
pm.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/organized data/pm1107(cv).csv")
ep.data <- na.omit(ep.data)
ep.data$up.temp <- NA
ep.data$down.temp <- NA
ep.data$total.temp <- NA
ep.data$max.temp <- NA
for (i in 1:nrow(ep.data)){
  temp.up <- subset(pm.data, no>=ep.data$begin[i] &no<=ep.data$maxindex[i])
  temp.down <- subset(pm.data, no>ep.data$maxindex[i] & no<=ep.data$end[i])
  temp.total <- subset(pm.data, no>=ep.data$begin[i] & no<=ep.data$end[i])
  ep.data$up.temp[i] <- mean(temp.up$TEMPimpute)
  ep.data$down.temp[i] <- mean(temp.down$TEMPimpute)
  ep.data$total.temp[i] <- mean(temp.total$TEMPimpute)
  ep.data$max.temp[i] <- max(temp.total$TEMPimpute)                 
}
par(mfrow=c(2,2))
plot(ep.data$up.temp, ep.data$maxpm, pch=16, xlab="mean of temperature in rise phase", ylab="maximum PM2.5 in episodes")
plot(ep.data$down.temp, ep.data$maxpm, pch=16, xlab="mean of temperature in decline phase", ylab="maximum PM2.5 in episodes")
plot(ep.data$total.temp, ep.data$maxpm, pch=16, xlab="mean of temperature in one episode", ylab="maximum PM2.5 in episodes")
#-----------------------------------------------------------------------
ep.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/episode.csv")
pm.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/organized data/pm1107(cv).csv")
ep.data <- na.omit(ep.data)
ep.data$up.dewp <- NA
ep.data$down.dewp <- NA
ep.data$total.dewp <- NA
ep.data$max.dewp <- NA
for (i in 1:nrow(ep.data)){
  dewp.up <- subset(pm.data, no>=ep.data$begin[i] &no<=ep.data$maxindex[i])
  dewp.down <- subset(pm.data, no>ep.data$maxindex[i] & no<=ep.data$end[i])
  dewp.total <- subset(pm.data, no>=ep.data$begin[i] & no<=ep.data$end[i])
  ep.data$up.dewp[i] <- mean(dewp.up$DEWPimpute)
  ep.data$down.dewp[i] <- mean(dewp.down$DEWPimpute)
  ep.data$total.dewp[i] <- mean(dewp.total$DEWPimpute)
  ep.data$max.dewp[i] <- max(dewp.total$DEWPimpute)                 
}
par(mfrow=c(2,2))
plot(ep.data$up.dewp, ep.data$maxpm, pch=16, xlab="mean of dewpoint in rise phase", ylab="maximum PM2.5 in episodes")
plot(ep.data$down.dewp, ep.data$maxpm, pch=16, xlab="mean of dewpoint in decline phase", ylab="maximum PM2.5 in episodes")
plot(ep.data$total.dewp, ep.data$maxpm, pch=16, xlab="mean of dewpoint in one episode", ylab="maximum PM2.5 in episodes")
plot(ep.data$max.dewp, ep.data$maxpm, pch=16, xlab="Maximum dewpoint in rise phase", ylab="maximum PM2.5 in episodes")
#-----------------------------------------------------------------------
ep.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/episode.csv")
pm.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/organized data/pm1107(cv).csv")
ep.data <- na.omit(ep.data)
ep.data$uppres <- NA
ep.data$downpres <- NA
ep.data$totalpres <- NA
ep.data$maxpres <- NA
for (i in 1:nrow(ep.data)){
  pres.up <- subset(pm.data, no>=ep.data$begin[i] &no<=ep.data$maxindex[i])
  pres.down <- subset(pm.data, no>ep.data$maxindex[i] & no<=ep.data$end[i])
  pres.total <- subset(pm.data, no>=ep.data$begin[i] & no<=ep.data$end[i])
  ep.data$up.pres[i] <- mean(pres.up$PRESimpute)
  ep.data$down.pres[i] <- mean(pres.down$PRESimpute)
  ep.data$total.pres[i] <- mean(pres.total$PRESimpute)
  ep.data$max.pres[i] <- max(pres.total$PRESimpute)                 
}
par(mfrow=c(2,2))
plot(ep.data$up.pres, ep.data$maxpm, pch=16, xlab="mean of pressure in rise phase", ylab="maximum PM2.5 in episodes")
plot(ep.data$down.pres, ep.data$maxpm, pch=16, xlab="mean of pressure in decline phase", ylab="maximum PM2.5 in episodes")
plot(ep.data$total.pres, ep.data$maxpm, pch=16, xlab="mean of pressure in one episode", ylab="maximum PM2.5 in episodes")
plot(ep.data$max.pres, ep.data$maxpm, pch=16, xlab="Maximum pressure in rise phase", ylab="maximum PM2.5 in episodes")
#---------------------------------------
ep.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/episode.csv")
pm.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/organized data/pm1107(cv).csv")
ep.data <- na.omit(ep.data)
ep.data$up.Iws <- NA
ep.data$down.Iws <- NA
ep.data$total.Iws <- NA
ep.data$max.Iws <- NA
for (i in 1:nrow(ep.data)){
  Iws.up <- subset(pm.data, no>=ep.data$begin[i] &no<=ep.data$maxindex[i])
  Iws.down <- subset(pm.data, no>ep.data$maxindex[i] & no<=ep.data$end[i])
  Iws.total <- subset(pm.data, no>=ep.data$begin[i] & no<=ep.data$end[i])
  ep.data$up.Iws[i] <- mean(Iws.up$Iws)
  ep.data$down.Iws[i] <- mean(Iws.down$Iws)
  ep.data$total.Iws[i] <- mean(Iws.total$Iws)
  ep.data$max.Iws[i] <- max(Iws.total$Iws)                 
}
par(mfrow=c(2,2))
plot(ep.data$up.Iws, ep.data$maxpm, pch=16, xlab="mean of Iws in rise phase", ylab="maximum PM2.5 in episodes")
plot(ep.data$down.Iws, ep.data$maxpm, pch=16, xlab="mean of Iws in decline phase", ylab="maximum PM2.5 in episodes")
plot(ep.data$total.Iws, ep.data$maxpm, pch=16, xlab="mean of Iws in one episode", ylab="maximum PM2.5 in episodes")
plot(ep.data$max.Iws, ep.data$maxpm, pch=16, xlab="Maximum Iws in rise phase", ylab="maximum PM2.5 in episodes")
#-----------------------------------------------
ep.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/episode.csv")
pm.data <- read.csv("C:/Users/haozhe/Dropbox/projects/pm2.5/data/organized data/pm1107(cv).csv")
ep.data <- na.omit(ep.data)

ep.data$up.wd <- NA
ep.data$down.wd <- NA
ep.data$total.wd <- NA
for (i in 1:nrow(ep.data)){
  tmp.up <- subset(pm.data, no>=ep.data$begin[i] &no<=ep.data$maxindex[i])
  tmp.down <- subset(pm.data, no>ep.data$maxindex[i] & no<=ep.data$end[i])
  tmp.total <- subset(pm.data, no>=ep.data$begin[i] & no<=ep.data$end[i])
  ep.data$up.wd[i] <- sum(tmp.up$wd %in% c("NE", "NW"))/length(tmp.up$wd)
  ep.data$down.wd[i] <- sum(tmp.down$wd %in% c("NE", "NW"))/length(tmp.down$wd)
  ep.data$total.wd[i] <- sum(tmp.total$wd %in% c("NE", "NW"))/length(tmp.total$wd)           
}
par(mfrow=c(2,2))
plot(density(ep.data$up.wd), xlim=c(0,1), main="Percentages of Northly wind in rise phases")
plot(density(ep.data$down.wd), xlim=c(0,1), main="Percentages of Northly wind in decline phases")

ep.data$up.wd <- NA
ep.data$down.wd <- NA
ep.data$total.wd <- NA
for (i in 1:nrow(ep.data)){
  tmp.up <- subset(pm.data, no>=ep.data$begin[i] &no<=ep.data$maxindex[i])
  tmp.down <- subset(pm.data, no>ep.data$maxindex[i] & no<=ep.data$end[i])
  tmp.total <- subset(pm.data, no>=ep.data$begin[i] & no<=ep.data$end[i])
  ep.data$up.wd[i] <- sum(tmp.up$wd %in% c("SE", "SW"))/length(tmp.up$wd)
  ep.data$down.wd[i] <- sum(tmp.down$wd %in% c("SE", "SW"))/length(tmp.down$wd)
  ep.data$total.wd[i] <- sum(tmp.total$wd %in% c("SE", "SW"))/length(tmp.total$wd)           
}
plot(density(ep.data$up.wd), xlim=c(0,1), main="Percentages of Southerly wind in rise phases")
plot(density(ep.data$down.wd), xlim=c(0,1), main="Percentages of Southerly wind in decline phases")

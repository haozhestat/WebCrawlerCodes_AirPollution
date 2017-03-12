pm.data <- read.csv("F:/Dropbox/projects/pm2.5/data/organized data/pm1107(cv).csv", header=T)
pm.data$pm2.5 [pm.data$pm2.5==-666666] <- NA
tpi.data <- read.csv("F:/Dropbox/projects/pm2.5/data/TPI2012.csv", header=T)
colnames(tpi.data) <- c("date", "morning", "afternoon", "allday")
tpi.data$allday [tpi.data$allday == "-"] <- NA
tpi.data$morning [tpi.data$morning == "-"] <- NA
tpi.data$afternoon [tpi.data$afternoon == "-"] <- NA
tpi.data$allday <- as.numeric(as.character(tpi.data$allday))
tpi.data$morning <- as.numeric(as.character(tpi.data$morning))
tpi.data$afternoon <- as.numeric(as.character(tpi.data$afternoon))

pm.data.12 <- ddply(subset(pm.data, year==12), .(year, month, dates), summarize,
              daily.mean = mean(pm2.5, na.rm=T))

data <- data.frame(tpi.data, daily.mean =pm.data.12[,4])

plot(data$allday, data$daily.mean, pch=16, xlab="daily traffic performance index", ylab="daily pm2.5",
     main="Relationship between daily TPI and daily pm2.5 in 2012")

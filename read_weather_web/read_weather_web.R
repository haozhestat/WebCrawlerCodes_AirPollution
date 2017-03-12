t0=Sys.time()
website <- readLines("C:/Users/haozhe/Dropbox/pm2.5/data_code/weather_web.txt")
website <- strsplit(website, "0000")
str(website)
website <- website[[1]][seq(2,length(website[[1]]),2)]
n <- nrow(data.frame(website))
name <- rep(NA,n)
for (i in 1:n){
  temp1 <- substr(website[i], 40, 42)
  temp2 <- substr(website[i], 44, nchar(website[i])-12)
  temp <- paste(paste(paste(temp1,'_', sep=''), temp2, sep=''), '.csv', sep='')
  name[i] <- temp
}
website <- data.frame(website)
name <- data.frame(name)
write.table(website, file="C:/Users/haozhe/Dropbox/pm2.5/data_code/weather_web.csv",col.names = FALSE,row.names=F,quote=F)
write.table(name, file="C:/Users/haozhe/Dropbox/pm2.5/data_code/name.csv",col.names = FALSE,row.names=F,quote=F)
t1=Sys.time()
t1-t0
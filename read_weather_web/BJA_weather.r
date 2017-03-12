require("XML")
require("RCurl")
varname=c('TimeUTC','WDIR','WAGL', 'WSPM', 'WSPG', 'TEMP', 'DEWP', 'WICH', 'SKY', 'Weather','PRES','HUMI','PREC')
cat(varname,"\n",file='C:/Users/haozhe/Dropbox/projects/pm2.5/data/raw data/weatherbeijing_12_16.csv',sep=",")
ii=seq(from=0,to=40*100,by=40)
for (i in ii){
z=paste("http://weather.nocrew.org/show.html?obid=14&offset=",i,"&sort=1&weather=",sep="")
url = getURL(z,.encoding="UTF-8", timeout = 50)
tmp = htmlTreeParse(url,useInternalNodes=T)
for (k in 1:40){
#  k=3
a2 = xpathSApply(tmp,
                 "//td",
                 xmlValue)
zz1=gsub("\\n","",a2[(15+13*(k-1)):(15+12+13*(k-1))])
cur_wea=matrix(zz1,nrow=1)
write.table(cur_wea,file='C:/Users/haozhe/Dropbox/projects/pm2.5/data/raw data/weatherbeijing_01052015.csv',col.names = FALSE,row.names=F,quote=F,sep=",",append=TRUE)
}
}








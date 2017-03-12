setwd("/data/zoutao/crontab")
require("XML")
require("RCurl")




require("tmcn")


#varname=c('RecordTime','Temp','BodyTemp', 'Pressure', 'Rainfall',       'RelHumid', 'WindDirec', 'WindDegree','WindSpeed', 'Comfort','UpdateTime')

#cat(varname,"\n",file='/data/zoutao/crontab/rainfall.csv',sep=",")

#for (i in 1:1){
#  tryCatch({


z="http://www.nmc.gov.cn/publish/forecast/AHE/shijiazhuang_iframe.html"

#a=read.table(z)

t0 <- Sys.time()
url = getURL(z,.encoding="UTF-8", timeout=2)
print(Sys.time() - t0 )

tmp = htmlTreeParse(url,useInternalNodes=T)

Time = Sys.time()
pb.date <- as.POSIXct(Time, tz="America/Chicago")
Time=format(pb.date, tz="Asia/Shanghai")



a1 = xpathSApply(tmp,
                   "//div[contains(@class,'temp_pic')]",
                   xmlValue)
a1=toUTF8(a1)

a1=strsplit(a1,'：')

a11=strsplit(a1[[2]][2],'℃')[[1]][1]
a12=strsplit(a1[[2]][3],'℃')[[1]][1]
a13=strsplit(a1[[2]][4],'hPa')[[1]][1]
a14=strsplit(a1[[2]][5],'mm')[[1]][1]
a15=strsplit(a1[[2]][6],'风')[[1]][1]
a16=strsplit(a1[[2]][7],'\\s')[[1]][1]
a17=strsplit(a1[[2]][7],'\\s')[[1]][2]
a18=strsplit(strsplit(a1[[2]][7],'\\s')[[1]][3],'舒')[[1]][1]
a19=a1[[2]][8]

a2= xpathSApply(tmp,
                   "//script[contains(@language,'javascript')]",
                   xmlValue)

a2=strsplit(a2,'setRealWeatherDate')
a2=strsplit(a2[[1]][2],'\\;')[[1]][1]


cur_wea=Time


cur_wea=as.data.frame(cur_wea)

chinese=c("西北风","西南风","东北风","东南风","北风","南风","东风","西风","级","微风舒适度","微风","舒适，最可接受","凉爽，较舒适","无持续风向","温暖，较舒适","暖，不舒适","凉，不舒适","无数据\\w*")
english=c("NW","SW","NE","SE","N","S","E","W"," Degree","breeze","breeze","Comfortable and most acceptable","Cool and comfortable","No persistent wind direction","Warm and comfortable","Warm and not comfortable","Cool and not comfortable","")

for (i in 1:length(chinese)){
a16=gsub(chinese[i],english[i],a16)
a17=gsub(chinese[i],english[i],a17)
a19=gsub(chinese[i],english[i],a19)
}

a2=gsub("\\(\\'","",a2)
a2=gsub("\\'\\)","",a2)
cur_wea$Temp=a11
cur_wea$BodyTemp=a12
cur_wea$Pressure=a13 
cur_wea$Rainfall=a14       
cur_wea$RelHumid=a15 
cur_wea$WindDirec=a16
cur_wea$WindDegree=a17
cur_wea$WindSpeed=a18
cur_wea$Comfort=a19
cur_wea$UpdateTime=a2








write.table(cur_wea,file='/data/zoutao/crontab/hengshui.csv',col.names = FALSE,row.names=F,quote=F,sep=",",append=TRUE)

#  }, error=function(e){})

#}

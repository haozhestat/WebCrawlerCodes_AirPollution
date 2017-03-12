setwd("/data/zoutao/crontab")
require("XML")
require("RCurl")
require("tmcn")



#for (i in 1:1){
  
#  tryCatch({


result=matrix(0,nrow=1,ncol=12+11*9)
result=as.data.frame(result)



z="http://aqicn.org/city/beijing/cn/m/"

#a=read.table(z)

url = getURL(z,.encoding="UTF-8")

url = toUTF8(url)
chinese = c("更新时间",paste("星期",c("一","二","三","四","五","六","日","天"),sep=""))
weekday = c("Update_time","Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday","Sunday")

for (i in 1:length(chinese))
{
  url = gsub(chinese[i],weekday[i],url)
}

url=gsub('font-size:12px;color:#888;','font-size:12px;color:#888888;',url)

tmp = htmlTreeParse(url,useInternalNodes=T)

up_time = xpathSApply(tmp,
                      "//div[contains(@style,'font-size:12px;color:#888888;')]",
                      xmlValue)
up_time = unlist(strsplit(up_time,split=" "))[2]

index = paste("tr",c("pm25","pm10","o3","no2","so2","t","d","p","h","w"),sep="_")
path = paste("//tr[contains(@id,'",index,"')]/td",sep="")
stat = sapply(path,function(x){
  res = xpathSApply(tmp,x,xmlValue)
  rr = res[(length(res)-3):length(res)][1]
  return(rr)
})
stat[sapply(stat, is.null)] <- NA 
#stat$`//tr[contains(@id,'tr_o3')]/td`="Null"

result[1,2]=up_time
result[1,3:12]=stat


z="http://aqicn.org/city/hebei/langfangshi/guandangxiao/cn/m/"

#a=read.table(z)

url = getURL(z,.encoding="UTF-8")

url = toUTF8(url)
chinese = c("更新时间",paste("星期",c("一","二","三","四","五","六","日","天"),sep=""))
weekday = c("Update_time","Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday","Sunday")

for (i in 1:length(chinese))
{
  url = gsub(chinese[i],weekday[i],url)
}

url=gsub('font-size:12px;color:#888;','font-size:12px;color:#888888;',url)

tmp = htmlTreeParse(url,useInternalNodes=T)

up_time = xpathSApply(tmp,
                      "//div[contains(@style,'font-size:12px;color:#888888;')]",
                      xmlValue)
up_time = unlist(strsplit(up_time,split=" "))[2]

index = paste("tr",c("pm25","pm10","o3","no2","so2","t","d","p","h","w"),sep="_")
path = paste("//tr[contains(@id,'",index,"')]/td",sep="")
stat = sapply(path,function(x){
  res = xpathSApply(tmp,x,xmlValue)
  rr = res[(length(res)-3):length(res)][1]
  return(rr)
})

stat[sapply(stat, is.null)] <- NA 

result[1,(2+11*1)]=up_time
result[1,(3+11*1):(12+11*1)]=stat

z="http://aqicn.org/city/hebei/langfangshi/xianghehuanbaoju/cn/m/"

#a=read.table(z)

url = getURL(z,.encoding="UTF-8")

url = toUTF8(url)
chinese = c("更新时间",paste("星期",c("一","二","三","四","五","六","日","天"),sep=""))
weekday = c("Update_time","Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday","Sunday")

for (i in 1:length(chinese))
{
  url = gsub(chinese[i],weekday[i],url)
}

url=gsub('font-size:12px;color:#888;','font-size:12px;color:#888888;',url)

tmp = htmlTreeParse(url,useInternalNodes=T)

up_time = xpathSApply(tmp,
                      "//div[contains(@style,'font-size:12px;color:#888888;')]",
                      xmlValue)
up_time = unlist(strsplit(up_time,split=" "))[2]

index = paste("tr",c("pm25","pm10","o3","no2","so2","t","d","p","h","w"),sep="_")
path = paste("//tr[contains(@id,'",index,"')]/td",sep="")
stat = sapply(path,function(x){
  res = xpathSApply(tmp,x,xmlValue)
  rr = res[(length(res)-3):length(res)][1]
  return(rr)
})

stat[sapply(stat, is.null)] <- NA 

result[1,(2+11*2)]=up_time
result[1,(3+11*2):(12+11*2)]=stat

z="http://aqicn.org/city/hebei/langfangshi/sanhejiaotongju/cn/m/"

#a=read.table(z)

url = getURL(z,.encoding="UTF-8")

url = toUTF8(url)
chinese = c("更新时间",paste("星期",c("一","二","三","四","五","六","日","天"),sep=""))
weekday = c("Update_time","Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday","Sunday")

for (i in 1:length(chinese))
{
  url = gsub(chinese[i],weekday[i],url)
}

url=gsub('font-size:12px;color:#888;','font-size:12px;color:#888888;',url)

tmp = htmlTreeParse(url,useInternalNodes=T)

up_time = xpathSApply(tmp,
                      "//div[contains(@style,'font-size:12px;color:#888888;')]",
                      xmlValue)
up_time = unlist(strsplit(up_time,split=" "))[2]

index = paste("tr",c("pm25","pm10","o3","no2","so2","t","d","p","h","w"),sep="_")
path = paste("//tr[contains(@id,'",index,"')]/td",sep="")
stat = sapply(path,function(x){
  res = xpathSApply(tmp,x,xmlValue)
  rr = res[(length(res)-3):length(res)][1]
  return(rr)
})

stat[sapply(stat, is.null)] <- NA 

result[1,(2+11*3)]=up_time
result[1,(3+11*3):(12+11*3)]=stat

z="http://aqicn.org/city/tianjin/donghuanlu/cn/m/"

#a=read.table(z)

url = getURL(z,.encoding="UTF-8")

url = toUTF8(url)
chinese = c("更新时间",paste("星期",c("一","二","三","四","五","六","日","天"),sep=""))
weekday = c("Update_time","Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday","Sunday")

for (i in 1:length(chinese))
{
  url = gsub(chinese[i],weekday[i],url)
}

url=gsub('font-size:12px;color:#888;','font-size:12px;color:#888888;',url)

tmp = htmlTreeParse(url,useInternalNodes=T)

up_time = xpathSApply(tmp,
                      "//div[contains(@style,'font-size:12px;color:#888888;')]",
                      xmlValue)
up_time = unlist(strsplit(up_time,split=" "))[2]

index = paste("tr",c("pm25","pm10","o3","no2","so2","t","d","p","h","w"),sep="_")
path = paste("//tr[contains(@id,'",index,"')]/td",sep="")
stat = sapply(path,function(x){
  res = xpathSApply(tmp,x,xmlValue)
  rr = res[(length(res)-3):length(res)][1]
  return(rr)
})

stat[sapply(stat, is.null)] <- NA 

result[1,(2+11*4)]=up_time
result[1,(3+11*4):(12+11*4)]=stat



z="http://aqicn.org/city/beijing/pingguzhen/cn/m/"

#a=read.table(z)

url = getURL(z,.encoding="UTF-8")

url = toUTF8(url)
chinese = c("更新时间",paste("星期",c("一","二","三","四","五","六","日","天"),sep=""))
weekday = c("Update_time","Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday","Sunday")

for (i in 1:length(chinese))
{
  url = gsub(chinese[i],weekday[i],url)
}

url=gsub('font-size:12px;color:#888;','font-size:12px;color:#888888;',url)

tmp = htmlTreeParse(url,useInternalNodes=T)

up_time = xpathSApply(tmp,
                      "//div[contains(@style,'font-size:12px;color:#888888;')]",
                      xmlValue)
up_time = unlist(strsplit(up_time,split=" "))[2]

index = paste("tr",c("pm25","pm10","o3","no2","so2","t","d","p","h","w"),sep="_")
path = paste("//tr[contains(@id,'",index,"')]/td",sep="")
stat = sapply(path,function(x){
  res = xpathSApply(tmp,x,xmlValue)
  rr = res[(length(res)-3):length(res)][1]
  return(rr)
})
stat[sapply(stat, is.null)] <- NA 


result[1,(2+11*5)]=up_time
result[1,(3+11*5):(12+11*5)]=stat


z="http://aqicn.org/city/beijing/miyunzhen/cn/m/"

#a=read.table(z)

url = getURL(z,.encoding="UTF-8")

url = toUTF8(url)
chinese = c("更新时间",paste("星期",c("一","二","三","四","五","六","日","天"),sep=""))
weekday = c("Update_time","Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday","Sunday")

for (i in 1:length(chinese))
{
  url = gsub(chinese[i],weekday[i],url)
}

url=gsub('font-size:12px;color:#888;','font-size:12px;color:#888888;',url)

tmp = htmlTreeParse(url,useInternalNodes=T)

up_time = xpathSApply(tmp,
                      "//div[contains(@style,'font-size:12px;color:#888888;')]",
                      xmlValue)
up_time = unlist(strsplit(up_time,split=" "))[2]

index = paste("tr",c("pm25","pm10","o3","no2","so2","t","d","p","h","w"),sep="_")
path = paste("//tr[contains(@id,'",index,"')]/td",sep="")
stat = sapply(path,function(x){
  res = xpathSApply(tmp,x,xmlValue)
  rr = res[(length(res)-3):length(res)][1]
  return(rr)
})
stat[sapply(stat, is.null)] <- NA 


result[1,(2+11*6)]=up_time
result[1,(3+11*6):(12+11*6)]=stat

z="http://aqicn.org/city/beijing/yanqingzhen/cn/m/"

#a=read.table(z)

url = getURL(z,.encoding="UTF-8")


url = toUTF8(url)
chinese = c("更新时间",paste("星期",c("一","二","三","四","五","六","日","天"),sep=""))
weekday = c("Update_time","Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday","Sunday")

for (i in 1:length(chinese))
{
  url = gsub(chinese[i],weekday[i],url)
}

url=gsub('font-size:12px;color:#888;','font-size:12px;color:#888888;',url)

tmp = htmlTreeParse(url,useInternalNodes=T)

up_time = xpathSApply(tmp,
                      "//div[contains(@style,'font-size:12px;color:#888888;')]",
                      xmlValue)
up_time = unlist(strsplit(up_time,split=" "))[2]

index = paste("tr",c("pm25","pm10","o3","no2","so2","t","d","p","h","w"),sep="_")
path = paste("//tr[contains(@id,'",index,"')]/td",sep="")
stat = sapply(path,function(x){
  res = xpathSApply(tmp,x,xmlValue)
  rr = res[(length(res)-3):length(res)][1]
  return(rr)
})


stat[sapply(stat, is.null)] <- NA 
result[1,(2+11*7)]=up_time
result[1,(3+11*7):(12+11*7)]=stat

z="http://aqicn.org/city/zhangjiakou/cn/m/"

#a=read.table(z)

url = getURL(z,.encoding="UTF-8")

url = toUTF8(url)
chinese = c("更新时间",paste("星期",c("一","二","三","四","五","六","日","天"),sep=""))
weekday = c("Update_time","Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday","Sunday")

for (i in 1:length(chinese))
{
  url = gsub(chinese[i],weekday[i],url)
}

url=gsub('font-size:12px;color:#888;','font-size:12px;color:#888888;',url)

tmp = htmlTreeParse(url,useInternalNodes=T)

up_time = xpathSApply(tmp,
                      "//div[contains(@style,'font-size:12px;color:#888888;')]",
                      xmlValue)
up_time = unlist(strsplit(up_time,split=" "))[2]

index = paste("tr",c("pm25","pm10","o3","no2","so2","t","d","p","h","w"),sep="_")
path = paste("//tr[contains(@id,'",index,"')]/td",sep="")
stat = sapply(path,function(x){
  res = xpathSApply(tmp,x,xmlValue)
  rr = res[(length(res)-3):length(res)][1]
  return(rr)
})

stat[sapply(stat, is.null)] <- NA 
#stat$`//tr[contains(@id,'tr_o3')]/td`="Null"
result[1,(2+11*8)]=up_time
result[1,(3+11*8):(12+11*8)]=stat


z="http://aqicn.org/city/shijiazhuang/cn/m/"
#a=read.table(z)

url = getURL(z,.encoding="UTF-8")


url = toUTF8(url)
chinese = c("更新时间",paste("星期",c("一","二","三","四","五","六","日","天"),sep=""))
weekday = c("Update_time","Monday", "Tuesday","Wednesday", "Thursday", "Friday", "Saturday", "Sunday","Sunday")

for (i in 1:length(chinese))
{
  url = gsub(chinese[i],weekday[i],url)
}

url=gsub('font-size:12px;color:#888;','font-size:12px;color:#888888;',url)

tmp = htmlTreeParse(url,useInternalNodes=T)

up_time = xpathSApply(tmp,
                      "//div[contains(@style,'font-size:12px;color:#888888;')]",
                      xmlValue)
up_time = unlist(strsplit(up_time,split=" "))[2]

index = paste("tr",c("pm25","pm10","o3","no2","so2","t","d","p","h","w"),sep="_")
path = paste("//tr[contains(@id,'",index,"')]/td",sep="")
stat = sapply(path,function(x){
  res = xpathSApply(tmp,x,xmlValue)
  rr = res[(length(res)-3):length(res)][1]
  return(rr)
})

#stat$`//tr[contains(@id,'tr_o3')]/td`="Null"
stat[sapply(stat, is.null)] <- NA 
result[1,(2+11*9)]=up_time
result[1,(3+11*9):(12+11*9)]=stat


Time = Sys.time()
pb.date <- as.POSIXct(Time, tz="America/Chicago")
Time=format(pb.date, tz="Asia/Shanghai", "%D %H:%M")
result[1,1]=Time 

#write.table(stat,file="/data/zoutao/pm2.5/pm2.5beijing.csv",col.names = FALSE,row.names=F,quote=F,sep=",",append=T)

write.table(result,file='/data/zoutao/crontab/pm2.52.csv',col.names = FALSE,row.names=F,quote=F,sep=",",append=TRUE)







#  }, error=function(e){})
#}



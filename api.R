#-------------------------------------------------
#Below is the code to download pm2.5 data by API
library(RCurl)
library(rjson)

url <- "http://www.pm25.in/api/querys/pm2_5.json?city=beijing&token=xAUnEKEqLeszc7szaKGT"
data <- fromJSON(getURL(url,.encoding="UTF-8"))
back <- data
for (i in 1:length(data)){
  for (j in 1:length(data[[1]])){
    if(as.character(data[[i]][j]) == "NULL"){
      print(c(i,j))
      data[[i]][j] <- '-666666'
    }
  }
}
for (i in 1:length(data)){
  tmp <- data.frame(t(sapply(unlist(data[[i]]),c)))
  tmp <- tmp[, -16]
  write.table(tmp, file="~/api/pm_api.csv",col.names = FALSE,row.names=F,quote=F,sep=",",append=TRUE)
}




#-------------------------------------------------------------------
library(rjson)
#library(RJSONIO)
library(RCurl)
install.packages("tmcn", repos = "http://R-Forge.R-project.org", type = "source")
library(tmcn)
url.total <- "http://www.pm25.in/api/querys/all_cities.json?token=xAUnEKEqLeszc7szaKGT"
data.total <- fromJSON(getURL(url.total,.encoding="UTF-8"))
data.total.9 <- fromJSON(getURL(url.total,.encoding="UTF-8"))
data.total.10 <- fromJSON(getURL(url.total,.encoding="UTF-8"))
result <- data.frame(matrix("", ncol=length(data.total[[1]]), nrow=length(data.total)))
colnames(result) <- toupper(names(data.total[[1]]))
for (i in 1:length(data.total)){
  tmp <- data.total[[i]]
  for (j in 1:length(data.total[[1]])){
      if(as.character(tmp[j]) == "NULL"){
      result[i,j] <- NA
    }
    else{
      result[i,j] <- tmp[j]
    }
  }
}
write.csv(data.total, file="D:/")

result <- matrix("", ncol=length(data.total[[1]]), nrow=length(data.total))
colnames(result) <- toupper(names(data.total[[1]]))
for (i in 1:length(data.total)){
  tmp <- data.total[[i]]
  for (j in 1:length(tmp)){
    if(is.null(tmp[j]) == TRUE){
      tmp[j] <- NA
    }
  }
  result[i,1] <- tmp$aqi
  result[i,2] <- tmp$area
  result[i,3] <- tmp$co
  result[i,4] <- tmp$co_24h
  result[i,5] <- tmp$no2
  result[i,6] <- tmp$no2_24h
  result[i,7] <- tmp$o3
  result[i,8] <- tmp$o3_24h
  result[i,9] <- tmp$o3_8h
  result[i,10] <- tmp$o3_8h_24h
  result[i,11] <- tmp$pm10
  result[i,12] <- tmp$pm10_24h
  result[i,13] <- tmp$pm2_5
  result[i,14] <- tmp$pm2_5_24h
  result[i,15] <- tmp$position_name
  result[i,16] <- tmp$primary_pollutant
  result[i,17] <- tmp$quality
  result[i,18] <- tmp$so2
  result[i,19] <- tmp$so2_24h
  result[i,20] <- tmp$station_code
  result[i,21] <- tmp$time_point
}
Encoding(as.vector(data.total[[1]])) <- "UTF-8"
write.csv(unlist(data.total), file="D:/data_total.txt")
a <- unlist(data.total)
str(a)

library(RCurl)
library(rjson)

url <- "http://www.pm25.in/api/querys/all_cities.json?token=xAUnEKEqLeszc7szaKGT"
data <- fromJSON(getURL(url,.encoding="UTF-8"))
back <- data
for (i in 1:length(data)){
  for (j in 1:length(data[[1]])){
    if(as.character(data[[i]][j]) == "NULL"){
      data[[i]][j] <- ' '
    }
  }
}
for (i in 1:length(data)){
  tmp <- data.frame(t(sapply(unlist(data[[i]]),c)))
  write.table(tmp, file="~/test/test2.csv",col.names = FALSE,row.names=F,quote=F,sep=",",append=TRUE)
}

library(RCurl)
library(rjson)

url <- "http://www.pm25.in/api/querys/all_cities.json?token=xAUnEKEqLeszc7szaKGT"
data <- fromJSON(getURL(url,.encoding="UTF-8"))
for (i in 1:length(data)){
  tmp <- data.frame(t(sapply(unlist(data[[i]]),c)))
  write.table(tmp, file="~/test/test.csv",col.names = FALSE,row.names=F,quote=F,sep=",",append=TRUE)
}





data.frame(t(sapply(data,c)))


result <- data.frame(matrix("", ncol=length(data[[1]]), nrow=length(data)))
colnames(result) <- toupper(names(data[[1]]))
for (i in 1:length(data)){
  tmp <- data[[i]]
  for (j in 1:length(data[[1]])){
    result[i,j] <- tmp[j]
  }
}

t<-data.frame(Reduce(rbind, data))

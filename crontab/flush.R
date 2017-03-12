setwd("/data/zoutao/crontab")
z=c(
"http://aqicn.org/city/hongkong/central/cn/m/",
"http://aqicn.org/city/beijing/qianmendongdajie/cn/m/",
"http://aqicn.org/city/beijing/chaoyangaotizhongxin/cn/m/",
"http://aqicn.org/city/beijing/haidianwanliu/cn/m/",
"http://aqicn.org/city/beijing/fengtaihuayuan/cn/m/",
"http://aqicn.org/city/beijing/nansanhuanxilu/cn/m/",
"http://aqicn.org/city/beijing/shijingshangucheng/cn/m/",
"http://aqicn.org/city/beijing/daxinghuangcunzhen/cn/m/",
"http://aqicn.org/city/beijing/tongzhouxincheng/cn/m/",
"http://aqicn.org/city/beijing/shunyixincheng/cn/m/",
"http://aqicn.org/city/beijing/changpingzhen/cn/m/",
"http://aqicn.org/city/beijing/cn/m/",
"http://aqicn.org/city/langfang/cn/m/",
"http://aqicn.org/city/tangshan/cn/m/",
"http://aqicn.org/city/baoding/cn/m/",
"http://aqicn.org/city/cangzhou/cn/m/",
"http://aqicn.org/city/tianjin/cn/m/",
"http://aqicn.org/city/chengde/cn/m/",
"http://aqicn.org/city/hebei/chengdeshi/xinglongxianzhengfu/cn/m/",
"http://aqicn.org/city/beijing/chaoyangnongzhanguan/cn/m/",
"http://aqicn.org/city/hebei/baodingshi/zhuozhoujiancezhan/cn/m/",
"http://aqicn.org/city/beijing/dongsihuanbeilu/cn/m/",
"http://aqicn.org/city/hebei/langfangshi/guandangxiao/cn/m/",
"http://aqicn.org/city/hebei/langfangshi/xianghehuanbaoju/cn/m/",
"http://aqicn.org/city/hebei/langfangshi/sanhejiaotongju/cn/m/",
"http://aqicn.org/city/tianjin/donghuanlu/cn/m/",
"http://aqicn.org/city/beijing/pingguzhen/cn/m/",
"http://aqicn.org/city/beijing/miyunzhen/cn/m/",
"http://aqicn.org/city/beijing/yanqingzhen/cn/m/",
"http://aqicn.org/city/zhangjiakou/cn/m/",
"http://aqicn.org/city/shijiazhuang/cn/m/"
)





for (i in 1:length(z)){
  
  tryCatch({


a=read.table(z[i])

  }, error=function(e){})
}

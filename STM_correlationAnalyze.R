# 本脚本用于Sentiment项目敏感性分析中变量相关性提取

data.stm.raw<-read.xlsx("STM_correlation.xlsx",sheetIndex = 2)%>%as.data.table()

coef(data.stm.raw[,c("SNR","meanDelay", "sdDelay","continousLossRate","lossHappenedRate","overall.Packetloss")],complete = TRUE)
cor(data.stm.raw[,c("SNR","meanDelay", "sdDelay","continousLossRate","lossHappenedRate","overall.Packetloss")],method = "spearman")

#                           SNR  meanDelay    sdDelay continousLossRate lossHappenedRate overall.Packetloss
# SNR                 1.0000000 -0.7131370 -0.6344953        -0.3462247       -0.7732038         -0.7701980
# meanDelay          -0.7131370  1.0000000  0.9571429         0.5080235        0.6877251          0.7053842
# sdDelay            -0.6344953  0.9571429  1.0000000         0.5846099        0.7404821          0.7487634
# continousLossRate  -0.3462247  0.5080235  0.5846099         1.0000000        0.7165102          0.7334014
# lossHappenedRate   -0.7732038  0.6877251  0.7404821         0.7165102        1.0000000          0.9870652
# overall.Packetloss -0.7701980  0.7053842  0.7487634         0.7334014        0.9870652          1.0000000


# 根据导入网络参数 生成network schedule
data.stm.network.para<-read.xlsx("STM_NetworkParameter.xlsx",sheetName = "network")%>%
  as.data.table()%>%cbind(.,id=c(1:nrow(.)))


# 生成马尔可夫链的长度
TimeCount=7200

#data.stm.network.para<-data.stm.network.para[1:2]#测试用

apply(data.stm.network.para[,c("continousPacketLoss","lossHappenedRate","meanDelay","stdDelay","id")],MARGIN = 1,FUN = function(x){
  #当前0; 当前1
  info.prob.packetLoss<-data.table(next0=c(x[1],x[2]),
                                   next1=c(1-x[1],1-x[2])) #每行行号为当前的状态
  s<-1 #第一个包认为是成功发送
  cat(x[5]," to chain generator \n")
  for(t in 2:TimeCount){
    s<-c(s,generatorFunction(s[t-1]))
  }
  cat(x[5]," to schedule\n")
  data.network.schedule<-data.table(time=c(1:TimeCount),
                                    isRcv=s,
                                    delay=abs(rnorm(TimeCount,mean = x[3],sd = x[4])))
  cat(x[5]," to output\n")
  write.xlsx(data.network.schedule,file = paste("networkSchedule_case5_id_",x[5],".xlsx",sep = ""),row.names = FALSE)
  cat(x[5]," output complete\n")
})
#会有一个NULL的输出，目前未发现是什么问题，需注意


####随机抽样函数####
generatorFunction<-function(currentState){ 
  sample(0:1,1,prob = info.prob.packetLoss[currentState+1,]) 
}

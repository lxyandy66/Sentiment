# 本脚本为处理模拟结果的脚本
# 包括 震荡幅度

data.stm.simResult.raw<-read.xlsx("Result_id_2.xlsx",sheetIndex = 1)%>%as.data.table()


# 阀门行程计算
data.stm.simResult.raw$V_p1<-c(0,data.stm.simResult.raw[1:(nrow(data.stm.simResult.raw)-1)]$V)
data.stm.simResult.raw[,':='(deltaValve=(V-V_p1))]
data.stm.simResult.raw$V_p1<-NULL
sumValve<-sum(abs(data.stm.simResult.raw$deltaValve))

data.stm.simResult.raw<-data.stm.simResult.raw[1:250]

nn<-directionCount(data.stm.simResult.raw$deltaValve)
# 结果可视化
ggplot(data=data.stm.simResult.raw[,c("time","Tsup.set","Tsup","Tsup.snd","Tsup.rcv","V","Vset")]%>%
         melt(.,id.var=c("time")),aes(x=time,y=value,color=variable))+geom_line()



directionCount<-function(delta){#delta为需要判断对象的时间序列，data.table形式
  stateValve<-NULL#时间序列的方向
  directionLoc<-c()#发生变化的位置
  # browser()
  for(i in c(1:length(deltaValve))){
    if(is.null(stateValve)){
      stateValve<-append(stateValve,sign(deltaValve[i]))
      next
    }
    if(sign(deltaValve[i])!=stateValve[length(stateValve)]&&sign(deltaValve[i])!=0){
      stateValve<-append(stateValve,sign(deltaValve[i]))
      directionLoc<-append(directionLoc,i)
    }
  }
  # 返回序列方向集合、发生变化位置的list
  return(list(stateValve,directionLoc))
}
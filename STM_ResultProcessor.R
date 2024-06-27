#### 本脚本为处理模拟结果的脚本 ####
# stat.stm.simResult.simLab为最终输出至simlab中的文件内容
stat.stm.simResult.simLab<-data.table(testId=numeric(0),
                                      overshoot=numeric(0),
                                      sseFinal50=numeric(0),
                                      sseFinal45=numeric(0),
                                      sseFinal43=numeric(0),
                                      std50=numeric(0),
                                      std45=numeric(0),
                                      std43=numeric(0),
                                      valveMove=numeric(0),
                                      changeDirCount=numeric(0),
                                      sumDeltaTsup=numeric(0),
                                      meanOscRange45=numeric(0),
                                      meanOscRange43=numeric(0),
                                      meanOscInv45=numeric(0),
                                      meanOscInv43=numeric(0))
# 供simlab可用的输出
# testId
# Tsup.set
# peakValue
# peakTime
# meanFinal
# stdFinal
# isStable
# sumDeltaTsup
# overshootPer
# overshoot
# valveMove
# changeDirCount
# sideOscCount
# meanOscRange
# meanOscInv




for(i in 1:3){
  # 包括 震荡幅度
  TestId<-i
  data.stm.simResult.raw<-read.xlsx(paste("AutoEvaluation_Test/Result_id_",i,".xlsx",sep = ""),sheetIndex = 1)%>%as.data.table()
  # data.stm.simResult.raw<-data.stm.simResult.raw[1:250]
  
  # "testId"         "valveMove"      "changeDirCount" "sumDeltaTsup" "testId"       
  # "overshoot"    "sseFinal50"   "sseFinal45"   "sseFinal43"   "std50"        "std45"        "std43" 
  # "testId"         "meanOscRange45" "meanOscRange43" "meanOscInv45"   "meanOscInv43"  
  
  ####辅助点计算####
  ## 辅助参数计算
  #送风温度相关
  data.stm.simResult.raw[,Tsup.smooth:=getMovingAverageValue(Tsup,n = 3,onlyPast = FALSE)]
  data.stm.simResult.raw$Tsup.smooth_p1<-c(NA,data.stm.simResult.raw[1:(nrow(data.stm.simResult.raw)-1)]$Tsup.smooth)
  data.stm.simResult.raw[,':='(deltaTsup=(Tsup.smooth-Tsup.smooth_p1))]
  
  data.stm.simResult.raw$Tsup.smooth_p1<-NULL
  #阀门相关
  data.stm.simResult.raw$V_p1<-c(NA,data.stm.simResult.raw[1:(nrow(data.stm.simResult.raw)-1)]$V)
  data.stm.simResult.raw[,':='(deltaValve=(V-V_p1))]
  data.stm.simResult.raw$V_p1<-NULL
  
  # 标出极值点
  tsupDirect<-directionCount(data.stm.simResult.raw$deltaTsup)
  data.stm.simResult.raw[,":="(isTsupCritical=NA,previousDir=as.numeric(NA))]
  data.stm.simResult.raw[tsupDirect$directionLoc]$isTsupCritical<-TRUE
  data.stm.simResult.raw[tsupDirect$directionLoc]$previousDir<-tsupDirect$previousDir
  
  #### 阀门行为计算 ####
  #阀门行为统计
  stat.stm.simResult.valve<-data.stm.simResult.raw[,.(valveMove=sum(abs(deltaValve),na.rm = TRUE),# 阀门总行程
                                                      changeDirCount=length(directionCount(deltaValve)$state)# 阀门换向次数
                                                      ),by=Tsup.set]
  stat.stm.simResult.simLab.append<-data.table(testId=TestId,data.stm.simResult.raw[,.(valveMove=sum(abs(deltaValve),na.rm = TRUE),# 阀门总行程
                                                             changeDirCount=length(directionCount(deltaValve)$state)# 阀门换向次数
                                                             )])#对于simlab，记录总的阀门移动和换向次数
  
  
  
  #### 送风温度相关计算 ####
  stat.stm.simResult.ctrlPerf<-data.stm.simResult.raw[,.(peakValue=max(Tsup),
                            peakTime=time[Tsup==max(Tsup)][1],
                            meanFinal=mean(Tsup[c((length(Tsup)-60):length(Tsup))],na.rm=TRUE),#最终60s的平均值 
                            stdFinal=sd(Tsup[c((length(Tsup)-60):length(Tsup))],na.rm=TRUE),#最终60s的标准差
                            #判断是否有超出稳定范围的，思路：最后60s的Tsup是否在stableRange范围内，若有FALSE（即不在），则不稳定
                            isStable=all(Tsup[c((length(Tsup)-60):length(Tsup))]>=getStableRange(Tsup.set[1])[1])&&
                                     all(Tsup[c((length(Tsup)-60):length(Tsup))]<=getStableRange(Tsup.set[1])[2]),
                            #注意这里逻辑判断的向量形式，例如c(2,3)>=2&&c(2,5)<=4 会返回TRUE
                            sumDeltaTsup=sum(abs(Tsup-Tsup.set),na.rm = TRUE)#温度总偏移量
                            ),by=Tsup.set]
  stat.stm.simResult.ctrlPerf[,":="(overshootPer=abs(peakValue-Tsup.set)/Tsup.set,overshoot=abs(peakValue-Tsup.set)/Tsup.set)]
  
  
  stat.stm.simResult.simLab.ctrlPerf<-data.stm.simResult.raw[,.(sumDeltaTsup=sum(abs(Tsup-Tsup.set),na.rm = TRUE))]
  stat.stm.simResult.simLab.ctrlPerf[,":="(testId=TestId,
                                  overshoot=stat.stm.simResult.ctrlPerf[Tsup.set==50]$peakValue-50,
                                  sseFinal50=stat.stm.simResult.ctrlPerf[Tsup.set==50]$meanFinal-50,
                                  sseFinal45=stat.stm.simResult.ctrlPerf[Tsup.set==45]$meanFinal-45,
                                  sseFinal43=stat.stm.simResult.ctrlPerf[Tsup.set==43]$meanFinal-43,
                                  std50=stat.stm.simResult.ctrlPerf[Tsup.set==50]$stdFinal,
                                  std45=stat.stm.simResult.ctrlPerf[Tsup.set==45]$stdFinal,
                                  std43=stat.stm.simResult.ctrlPerf[Tsup.set==43]$stdFinal)]
  stat.stm.simResult.simLab.append<-merge(x=stat.stm.simResult.simLab.append,y=stat.stm.simResult.simLab.ctrlPerf,by = "testId")
  
  
  
  
  ####震荡幅度相关计算####
  data.stm.simResult.oscillation<-data.stm.simResult.raw[isTsupCritical==TRUE & time %in% c(250:500,750:1000,1250:1500)]
  setorderv(data.stm.simResult.oscillation,"time",-1)
  #选出单侧计算振幅的点数
  data.stm.simResult.oscillation<-data.stm.simResult.oscillation[,.(time=time,
                                                                    Tsup=Tsup,
                                                                    Tsup_preOsc=c(NA,Tsup[c(1:(length(Tsup)-1))]),
                                                                    time_preOsc=c(NA,time[c(1:(length(Tsup)-1))])
                                                                  ),by=Tsup.set]
  data.stm.simResult.oscillation[,":="(deltaTsupOsc=Tsup-Tsup_preOsc,deltaTimeOsc=time-time_preOsc)]#计算
  stat.stm.simResult.oscillation<-data.stm.simResult.oscillation[,.(sideOscCount=length(deltaTsupOsc[!is.na(deltaTsupOsc)]),#记录了几个单侧震荡
                                                                    # 注意，此处振幅和间隔，可能需要结合是否震荡来考虑
                                                                    meanOscRange=mean(abs(deltaTsupOsc),na.rm=TRUE),#平均振幅
                                                                    meanOscInv=2*mean(abs(deltaTimeOsc),na.rm=TRUE)#单侧振幅间隔
                                                                    ),by=Tsup.set]
  #simLab输出
  stat.stm.simResult.simLab.oscillation<-stat.stm.simResult.oscillation%>%{
                                            data.table(testId=TestId,
                                                       meanOscRange45=.[Tsup.set==45]$meanOscRange,
                                                       meanOscRange43=.[Tsup.set==43]$meanOscRange,
                                                       meanOscInv45=.[Tsup.set==45]$meanOscInv,
                                                       meanOscInv43=.[Tsup.set==43]$meanOscInv)}
  stat.stm.simResult.simLab.append<-merge(x=stat.stm.simResult.simLab.append,y=stat.stm.simResult.simLab.oscillation,by="testId")
  
  #### 结果输出及可视化 ####
  # 计算结果合并
  # 带simlab的为用于分析的数据
  # stat.stm.simResult.ctrlPerf #送风温度相关，直接控制性能
  # stat.stm.simResult.oscillation #送风温度震荡幅度
  # stat.stm.simResult.valve #阀门行为相关
  # stat.stm.simResult.final #为当前test下所有评估数据，单独输出文件，并不全部至simlab分析文件中
  # stat.stm.simResult.simLab.append #为单次循环中导入simlab分析的文件
  # stat.stm.simResult.simLab #为最终汇总版simlab文件
  
  stat.stm.simResult.final<-merge(stat.stm.simResult.ctrlPerf,stat.stm.simResult.valve,by="Tsup.set")
  stat.stm.simResult.final<-data.table(testId=TestId,merge(stat.stm.simResult.final,stat.stm.simResult.oscillation,by="Tsup.set"))
  setorderv(stat.stm.simResult.final,cols = "Tsup.set",order = -1)
  write.xlsx(x=stat.stm.simResult.final,file=paste("AutoEvaluation_Test/Id_",TestId,"_Evaluation.xlsx",sep = ""))#评估结果输出
  
  stat.stm.simResult.simLab<-rbind(stat.stm.simResult.simLab,stat.stm.simResult.simLab.append)
  
  
  #控制绘图输出
  ggsave(file=paste("AutoEvaluation_Test/Id_",TestId,"_Image.png",sep=""),
         plot=ggplot(data=data.stm.simResult.raw[,c("time","isTsupCritical","Tsup.set","Tsup","Tsup.snd","Tsup.rcv","V","Vset","Tsup.smooth")]%>%
                               melt(.,id.var=c("time","isTsupCritical")),aes(x=time,y=value,color=variable))+geom_line()+geom_point(aes(shape=isTsupCritical)),
         width=12,height = 5,dpi = 100)
  
  # 删除中间变量
  rm(stat.stm.simResult.ctrlPerf,stat.stm.simResult.oscillation,stat.stm.simResult.valve,
     stat.stm.simResult.final,stat.stm.simResult.simLab.append,
     stat.stm.simResult.simLab.oscillation,stat.stm.simResult.simLab.ctrlPerf,tsupDirect)
}
write.table()



#### 一些工具类函数 ####

getStableRange<-function(Tsup.set){
  if(Tsup.set==50)
    return(c(47.5,52.5))
  else if(Tsup.set==45)
    return(c(42.75,47.25))
  else if(Tsup.set==43)
    return(c(40.85,45.15))
  else
    return(NA)
}

directionCount<-function(delta){#delta为需要判断对象的时间序列，data.table形式
  state<-NULL#时间序列的方向
  directionLoc<-c()#发生变化的位置
  previousDir<-c()
  # browser()
  for(i in c(1:length(delta))){
    # NA不计入判断
    if(is.na(delta[i])){
      cat(i," is NA, skipped\n")
      next
    }
    # 初始状态序列为空时，将第一个非空状态加入
    if(is.null(state)){
      if(sign(delta[i])==0) #若非空
        next
      else
        state<-append(state,sign(delta[i]))
        next
    }
    if(sign(delta[i])!=state[length(state)]&&sign(delta[i])!=0){
      previousDir<-append(previousDir,state[length(state)])
      state<-append(state,sign(delta[i]))
      directionLoc<-append(directionLoc,i)
    }
  }
  # 返回序列方向集合、发生变化位置的list
  return(list("state"=state,"directionLoc"=directionLoc,"previousDir"=previousDir))
}

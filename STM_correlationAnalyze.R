# 本脚本用于Sentiment项目敏感性分析中变量相关性提取

data.stm.raw<-read.xlsx("STM_correlation.xlsx",sheetIndex = 1)

coef(data.stm.raw,complete = TRUE)
cor(data.stm.raw[,c(2:5)],method = "spearman")

#             RSSI        SNR      delay      packetLoss
# RSSI        1.0000000 -0.8519384  0.6715495  0.7143540
# SNR        -0.8519384  1.0000000 -0.6777551 -0.5866442
# delay       0.6715495 -0.6777551  1.0000000  0.4184690
# packetLoss  0.7143540 -0.5866442  0.4184690  1.0000000
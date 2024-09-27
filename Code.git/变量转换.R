rm(list = ls())


# 安装和加载需要使用的包
install.packages('survival')
install.packages('survminer')

library(survival)
library(survminer)


# 读取数据
data_exercise <- read.csv('data_exercise.csv')

#核查数据
names(data_exercise)
nrow(data_exercise)

head(data_exercise)
summary(data_exercise)

#基于完整数据分析 
data.complete <- na.omit(data_exercise)

# 查看完整个例个数
nrow(data.complete)

## 连续变量转换成二分类变量

# 设置阈值
c <- 37

data.complete$D <- ifelse(data.complete$X>=c,1,0)

table(data.complete$D)











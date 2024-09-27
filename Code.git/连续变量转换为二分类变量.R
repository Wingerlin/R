rm(list = ls())


# 安装和加载包
install.packages("pROC")
install.packages("maxstat")
install.packages("survminer")
install.packages("survival")

library(pROC)
library(maxstat)
library(survminer)
library(survival)

# 读取数据
data_exercise <- read.csv('data_exercise.csv')

data <- data_exercise


# 查看变量
summary(data$X)

hist(data$X)

# 二分类函数
dichotomize <- function (x, cutoff) {
  
  x_new <- ifelse(x>cutoff,1,0)
  x_new
} 

#使用cut-off值将变量转化为二分类变量
roc_X <- roc(data$Status_death,data$X)

plot(roc_X)

cutoff_binary <- coords(roc_X, "best",best.method = "youden",transpose = FALSE)

data$X_dich_optimal_binary <- dichotomize(data$X,cutoff=cutoff_binary$threshold[1]) # add [1] in case of multiple optimal cut-offs







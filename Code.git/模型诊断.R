rm(list = ls())

## 安装包
install.packages('glmnet')
install.packages('MASS')
install.packages('survival')
install.packages('rms')
install.packages('car')
install.packages('ggplot2')
install.packages('survminer')

library(car)
library(ggplot2)
library(glmnet)
library(MASS)
library(survival)
library(rms)
library(survminer)


# 读取数据
data_exercise <- read.csv('data_exercise.csv')

data <- data_exercise



# 重新拟合模型

# 定义结局变量和预测因子
Outcome <- "Status_death"
FinalVariables <- c("B3","C1", "C3", "C4", "C5_log_minus" )
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(FinalVariables, collapse=" + ")))

# 使用所有变量拟合模型
model.final <- glm(Formula, data=data,family=binomial)
summary(model.final)


# 线性关系


# recall
dd <- datadist(data)
dd$limits$C1[2] <- 0
dd$limits$C3[2] <- 0
dd$limits$C4[2] <- 0
dd$limits$C5_log_minus[2] <- 0
options(datadist="dd")




# C1
fit.C1.logistic <- lrm(Status_death ~ rcs(C1,3) + B3 + C3 + C4 + C5_log_minus,data=data,x=TRUE,y=TRUE)
plot(rms::Predict(fit.C1.logistic,C1,ref.zero=TRUE))

# C3
fit.C3.logistic <- lrm(Status_death ~ rcs(C3,3) + B3 + C1 + C4 + C5_log_minus,data=data,x=TRUE,y=TRUE)
plot(rms::Predict(fit.C3.logistic,C3,ref.zero=TRUE))



#影响点

plot(model.final,4)


#多重共线性

vif(model.final)





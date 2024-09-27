rm(list = ls())

## 安装加载包
install.packages('glmnet')
install.packages('MASS')
install.packages('survival')
install.packages('rms')


library(glmnet)
library(MASS)
library(survival)
library(rms)


# 读取数据
data_exercise <- read.csv('data_exercise.csv')

data <- data_exercise




## 利用前一步筛选出的变量拟合模型

# 逐步回归法
summary(model.step)
FinalVariables <- c("B3","C1", "C3", "C4", "C5_log_minus" )
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(FinalVariables, collapse=" + ")))

#用筛选出的所有变量拟合模型
model.final <- glm(Formula, data=data,family=binomial)
summary(model.final)

# Lasso selection

coef(model.lasso , s=model.lasso$lambda[4]) 

FinalVariables <- c("C1","C2", "C3", "C4", "C5_log_minus" )
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(FinalVariables, collapse=" + ")))

model.final <- glm(Formula, data=data,family=binomial)

summary(model.final)


# 选择最佳lambda
coef(cv.model, s=cv.model$lambda.min) 

# 仅用5个变量
coef(model.lasso , s=model.lasso$lambda[4]) 



# 制作最终预测模型

# 计算线性预测值
lp <-  predict(model.final, data=data, type="link")
hist(lp)

# 计算预测概率
prob <- predict(model.final, data=data, type="response")

prob1 <- exp(lp)/(1+exp(lp))

prob-prob1



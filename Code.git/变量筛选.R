rm(list = ls())

## 安装包
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

# 查看完整个案
data <- na.omit(data)



#变量筛选
# 1.1 逐步回归筛选

model.step <- stepAIC(model.full, direction="both")

summary(model.step)


# 1.2 Lasso

# 将数据帧转化为矩阵
tmp.y <- data$Status_death
tmp.x <- model.matrix(~.,data[CandidateVariables])


# 拟合模型
model.lasso <-  glmnet(tmp.x, tmp.y, family="binomial", nlambda=50, alpha=1, standardize=TRUE)

plot(model.lasso,xvar="lambda",label=TRUE)

# 发现最佳模型
cv.model <- cv.glmnet(tmp.x, tmp.y, family="binomial", nlambda=50, alpha=1, standardize=TRUE)
plot(cv.model)
cv.model$lambda.min
coef(cv.model, s=cv.model$lambda.min) 

# 增加lambda进一步减少变量数
cv.model$lambda.1se
coef(cv.model, s=cv.model$lambda.1se) 


# 假如最终模型中只想有5个变量
coef(model.lasso , s=model.lasso$lambda[4]) 

coef(model.lasso , s=0.149000) 







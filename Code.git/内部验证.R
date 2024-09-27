rm(list = ls())

## 安装包
install.packages('glmnet')
install.packages('MASS')
install.packages('survival')
install.packages('rms')
install.packages('car')
install.packages('ggplot2')
install.packages('survminer')
install.packages('ggridges')
install.packages('pROC')
install.packages("plotROC")
install.packages("riskRegression")

library(car)
library(ggplot2)
library(glmnet)
library(MASS)
library(survival)
library(rms)
library(survminer)
library(ggridges)
library(pROC)
library(plotROC)
library(riskRegression)



# 读取数据
data_exercise <- read.csv('data_exercise.csv')

data <- data_exercise


# 内部验证

#假定病人来自N个不同群体
N_Center <- 4     

data$group <- sample(1:N_Center,nrow(data),replace = TRUE)

table(data$group)



#逻辑回归

#重新拟合模型
#创建一个公式
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(CandidateVariables, collapse=" + ")))

# 拟合模型
model.full <- glm(Formula, data=data,family=binomial)

model.final <- stepAIC(model.full, direction="both")

summary(model.final)


# 计算模型表现

data$p_prediction <- predict(model.final, data, type="response")

roc.apparent <- roc(data$Status_death, data$p_prediction)

c_apparent <- roc.apparent$auc

brier_apparent <- mean((data$p_prediction-data$Status_death)^2)


# 内部验证
N_folds <- max(data$group)

c_test <- 0
brier_test <- 0



for (i in 1:N_folds){
  
  data.train <- subset(data,data$group != i)
  
  # any data driven variable transformation or variable selection need to be added in each loop
  
  # define outcome and predictors
  Outcome <- "Status_death"
  CandidateVariables <- c("B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", "C5_log_minus" ,"C6_log")
  
  # create a formula
  Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                           paste(CandidateVariables, collapse=" + ")))
  
  # fit a model with all candidate varaibles
  
  
  model.full <- glm(Formula, data= data.train,family=binomial)
  
  # stepwise selection (as an example, other selection methods can be used as well)
  
  model.train <- stepAIC(model.full, direction="both")
  
  data.test <- subset(data,data$group == i)
  
  data.test$p_prediction <- predict(model.train, data.test, type="response")
  
  roc.test <- roc(data.test$Status_death, data.test$p_prediction)
  
  c_test[i] <- roc.test$auc
  
  brier_test[i] <- mean((data.test$p_prediction-data.test$Status_death)^2)
  
}

c_test
brier_test

summary(c_test)
summary(brier_test)







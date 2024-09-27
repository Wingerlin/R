rm(list = ls())


# 安装和加载包
install.packages('VIM')
install.packages('naniar')
install.packages('ggplot2')
install.packages('mice')

library(VIM)
library(naniar)
library(ggplot2)
library(mice)

# 读取数据
data_exercise <- read.csv('data_exercise.csv')

data <- data_exercise


summary(data)


# 查看缺失机制

# with naniar
gg_miss_upset(data)


# with package VIM

aggr(data, numbers = TRUE,  prop = c(TRUE, FALSE), combined = F,  sortVars = F,  labels = names(data),   cex.axis=.7, gap=3, ylab = c("Histogram of missing data", "Pattern"))


# scatter plot with missing values

ggplot(data, aes(x = C1, y = C3)) +
  geom_point()

ggplot(data, aes(x = C1, y = C3)) +
  geom_miss_point()

ggplot(data, aes(x = C1, y = C2)) +
  geom_miss_point()

# 使用 MICE进行插补

# 变量转换为正态

data$C6_trans <- log10(data$C6)


#插补模型中的变量
varlist <- c("ID", "B1", "B2", "B3", "B4", 
             "C1", "C2", "C3", "C4", "C5", "C6_trans", 
             "Time_death", "Status_death", "haz_os")


#创建一个新的数据集用作插补
data.impu = data[varlist]


##分类变量转化为因子变量
data.impu$B1 <- factor(data.impu$B1)
data.impu$B2 <- factor(data.impu$B2)
data.impu$B3 <- factor(data.impu$B3)
data.impu$B4 <- factor(data.impu$B4)

data.impu$Status_death <- factor(data.impu$Status_death)


#查看预测结构
pred <- quickpred(data.impu, exclude = c("ID","Time_death"))
pred

# 插补方法
meth <- impu_default$meth
meth   


# imputation


# 多重插补
K <- 20 # 20 times 

data_imputated <- vector(K,mode="list") 


imputation_20 <- mice(data.impu, maxit = 25, m = K, seed = 1234, pred = pred, meth = meth, print = TRUE)

for (i in 1:K) {
  data_imputated[[i]] <- mice::complete(imputation_20, i)
}







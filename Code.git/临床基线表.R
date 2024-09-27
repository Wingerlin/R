install.packages("tableone")
library(tableone)

#将数值变量转换成分类变量
data$A <- as.factor(data$A)
data$B <- as.factor(data$B)

#将变量打包
names(aa)
#所有变量
myVars <- c("A","B","C","D","E", "F", "G")
#分类变量
catVars <- c("A","B","C","D","E", "F", "G")


table <- CreateTableOne(vars=myVars,
                        factorVars = catVars,
                        strata = "Outcome",
                        data = aa,
                        test = TRUE,
                        addOverall = TRUE);table

table1 <- print(table,
                catDigits = 2, contDigits = 3, pDigits = 3,
                showAllLevels = TRUE,
                quote = FALSE,
                noSpaces = TRUE,
                printToggle = TRUE)

write.csv(table1,"F:/prediction/case/01-Data/二分类/临床基线表.csv")

rm(list = ls())

## install packages


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
install.packages("ipred")

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
library(ipred)




# read data
data_exercise <- read.csv('data_exercise.csv')


# read external validation data
data_external <- test


data <- data_exercise

# exclude all missing values (complete cases analaysis)
data <- na.omit(data)


# variable trasformation (from 3.4)


data_external$WBC <- as.factor(data_external$WBC)
data_external$LY <- as.factor(data_external$LY)
data_external$HGB <- as.factor(data_external$HGB)
data_external$MCV <- as.factor(data_external$MCV)
data_external$APTT <- as.factor(data_external$APTT)
data_external$DDI <- as.factor(data_external$DDI)
data_external$Urea <- as.factor(data_external$Urea)
data_external$Cr <- as.factor(data_external$Cr)
data_external$UA <- as.factor(data_external$UA)




# 5.1 Logistic regression


# develop a model in data (3.5 and 3.6)


# define outcome and predictors
Outcome <- "Status_death"
CandidateVariables <- c("B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", "C5_log_minus" ,"C6_log")

# create a formula
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(CandidateVariables, collapse=" + ")))

# fit a model with all candidate varaibles
model.full <- glm(Formula, data=data,family=binomial)

# stepwise selection (as an example, other selection methods can be used as well)

model.final <- stepAIC(model.full, direction="both")

summary(model.final)


# External validation


### External validation of a published model


# 1. compare outcome (0 or 1)

table(data$Outcome)    # 0 is 159, 1 is 79, this information can be found in published paper


table(data_external$Outcome)

table_outcome <-  matrix(c(123,121,108,1), nrow = 2, ncol = 2) 

prop.table(table_outcome,2) # percentage

chisq.test(table_outcome, correct=FALSE)


# 2. make prediction with external validation data


## 2.1 variable transformation same as in model development


summary(model.final)  # check which variables are in the final model

data_external$C5_log_minus <- log(max(data_external$C5)+1-(data_external$C5))  # only possible if reported in original paper



## 2.2 calculate linear predictor and predicted probability in validation data

model.final$coefficients # assume all coefficients were provided in original paper


coefficients_development <- c(-4.5642415, 1.1318677,0.9115242, 0.8280161, 1.1196556, 0.8218200,-1.1090684,1.0539621,1.9877494, -0.9604039)

# use lrm and coefficients from original paper to reproduce the model
model.development <- lrm(Outcome~WBC+LY+HGB+MCV+APTT+DDI+Urea+Cr+UA,init= coefficients_development,  maxit=0,data=data_external)


# with the replicated model, we can calculate linear predictor and predicted probability
data_external$lp <- predict(model.development, newdata=data_external, type="lp")   # linear predictor

data_external$p_prediction <- predict(model.development, newdata=data_external, type="fitted")   # predicted probability


log_odds <- predict(model.final)
odds <- exp(log_odds)
data_external$p_prediction <- plogis(log_odds)
# 3. Calculate performance measures

## 3.1 C statistics


data_external$Status_death <- data_external$Status

roc_external <- roc(data_external$Status_death, data_external$p_prediction)

auc_external <- roc_external$auc    # AUC

ci.auc(roc_external)  # AUC 95% CI

plot(roc_external)


## 3.2 Brier score


brier_external <- mean((data_external$p_prediction-data_external$Status_death)^2)



## 3.3 Calibration intercept


model.calibration <- glm(Status_death~lp, data=data_external,family=binomial)

model.calibration$coefficients[1]   # calibration intercept


## 3.4 Calibration slope and calibration curve

model.calibration$coefficients[2]   # calibration slope


val.prob(p = data_external$p_prediction, y = data_external$Status_death,logistic.cal=F)



## 3.5 Discrimination slope

fit <- lm(p_prediction~Status_death, data=data_external)

fit$coefficients[2]    # discrimination slope

ggboxplot(data_external, "Status_death", "p_prediction",
          color = "Status_death", palette =c("red", "blue"),
          add = "jitter")







# 5.2 Cox regression


# full model

# define outcome and predictors
Outcome <- "Surv(Time_death,Status_death)"
CandidateVariables <- c("B1", "B2", "B3", "B4", "C1", "C2", "C3", "C4", "C5_log_minus" ,"C6_log")

# create a formula
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(CandidateVariables, collapse=" + ")))

# fit a model with all candidate varaibles, in training set
model.full <- coxph(Formula, data=data,x=TRUE)

# stepwise selection

model.step <- stepAIC(model.full, direction="both")

summary(model.step)




# External validation


### External validation of a published model


# 1. compare outcome (survival outcome)

## survival probability in development data
fit_development <- survfit(Surv(Time_death,Status_death) ~ 1, data=data)
survival_development <- 0
survival_development$time <- fit_development$time
survival_development$surv <- fit_development$surv

S_12 <- min(survival_development$surv[survival_development$time <= 12])  # S12 should be reported in original paper
S_24 <- min(survival_development$surv[survival_development$time <= 24])
S_60 <- min(survival_development$surv[survival_development$time <= 60])  

fit_validation <- survfit(Surv(Time,Status) ~ 1, data=data_external)
survival_validation <- 0
survival_validation$time <- fit_validation$time
survival_validation$surv <- fit_validation$surv

S_12_validation <- min(survival_validation$surv[survival_validation$time <= 12])
S_24_validation <- min(survival_validation$surv[survival_validation$time <= 24])
S_60_validation <- min(survival_validation$surv[survival_validation$time <= 60])



ggsurvplot(survfit(Surv(Time, Status) ~ 1, data = data_external),
           conf.int = TRUE,          # Add confidence interval
           risk.table = TRUE,        # Add number at risk
           risk.table.col = "strata",# Risk table color by groups
           legend.labs = c( "Validation"),    # Change legend labels
           risk.table.height = 0.25, 
           ggtheme = theme_bw() )






# 2. make prediction with external validation data


## 2.1 variable transformation same as in model development


summary(model.step)  # check which variables are in the final model

data_external$C5_log_minus <- log(max(data_external$C5)+1-(data_external$C5))


data_external$Status_death <- data_external$Status
data_external$Time_death <- data_external$Time




## 2.2 calculate linear predictor in validation data


model.step$coefficients    


coefficients_development <- c(0.7242658, 1.1942633, 1.1107172, 0.9123046, 0.5383042, -2.1639685)

# use lrm and coefficients from original paper to reproduce the model

model.development <- cph(Surv(Time_death,Status_death)~B1+B3+C1+C3+C4+C5_log_minus, data=data_external,init=coefficients_development, iter.max=0,x=TRUE,y=TRUE)


data_external$lp <- predict(model.development, newdata=data_external, type="lp")   # linear predictor



## 2.3 calculate predicted survival probability

Hazard <- basehaz(model.step, centered=F)
S0_12 <- min(exp(-Hazard[Hazard[,2] <= 12,])[,1])

S0_12    # this value should be provided in original paper

data_external$S_12 <- S0_12^exp(data_external$lp)

# 3. Calculate performance measures

## 3.1 C statistics

time <- 12   # set time of evaluation


c_harrell_external <- (cph(Surv(Time_death,Status_death)~lp, data=data_external,x=TRUE,y=TRUE)$stats["Dxy"]+1)/2

c_time_external <- Score(list("model.development"=model.development),Surv(Time_death,Status_death)~1,data=data_external,times=time, plots="cal",metrics=c("AUC"))$AUC$score$AUC

plotROC(Score(list("model.development"=model.development),Surv(Time_death,Status_death)~1,data=data_external,times=time, plots="roc",metrics=c("AUC")))

## 3.2 Brier score (at time=t)

# t=12



brier_external <- sbrier(Surv(data_external$Time_death,data_external$Status_death), data_external$S_12, btime=time)



## 3.3 Calibration intercept (compare baseline survival)

Hazard_development <- basehaz(model.step, centered=F)
Hazard_development <- data.frame(Hazard_development)
Hazard_development$Dataset <- "Development"

model.validation <- cph(Surv(Time_death,Status_death)~lp, data=data_external,init=1, iter.max=0,x=TRUE,y=TRUE)

Hazard_validation <- basehaz(model.validation, centered=F)
Hazard_validation <- data.frame(Hazard_validation)
Hazard_validation$Dataset <- "Validation"


Hazard_combine <- rbind(Hazard_development,Hazard_validation)
Hazard_combine$S0 <- exp(-Hazard_combine$hazard)

ggplot(data=Hazard_combine, aes(x=time, y=S0, group=Dataset, color=Dataset)) +
  geom_line() + 
  theme_minimal()

## 3.4 Calibration slope and calibration curve

model.calibration <- cph(Surv(Time_death,Status_death)~lp, data=data_external,x=TRUE,y=TRUE)

model.calibration$coefficients    # calibration slope


data_external$group10<-cut(data_external$lp, quantile(data_external$lp, seq(0,1,0.1)), right=FALSE, labels=c(1:10))
data_external$group10[data_external$lp==max(data_external$lp)] <- 10

table(data_external$group10)

survival_predicted <- aggregate(data_external$S_12, list(data_external$group10), mean)


survival_observed <-0

for (i in 1:10) {
  
data_temp <- subset(data_external,data_external$group10==i)
  
fit_calibration <- survfit(Surv(Time,Status) ~ 1, data=data_temp)

survival_observed[i] <- min(fit_calibration$surv[fit_calibration$time <= 12])

}






survival_comparison <- data.frame(survival_predicted,survival_observed)


ggplot(data=survival_comparison, aes(x=x, y=survival_observed)) +
  geom_line()+
  geom_point()+
  xlim(0,1)+
  ylim(0,1)+
  geom_abline(intercept = 0, slope = 1,lty=2)+
  labs(title="Calibration Curve at T=12",x="Predicted Survival Probability", y = "Observed Survival Probability")

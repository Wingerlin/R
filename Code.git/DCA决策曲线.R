install.packages("ggprism")
install.packages("rmda")



library(ggDCA)
library(ggplot2)
library(rms)
library(survival)
library(ggprism)
library(rmda)
library(caret)

set.seed(123)

d<- decision_curve(Outcome~C1+C2+C3+C4+C5+C6,x=T,y=T,
       data=data,
       thresholds = seq(0,0.4,by=0.005),bootstraps=10)

ggplot(d,linetype=F,lwd=2)+
  theme_classic()+
  theme_prism(base_size = 17)+
  theme(legend.position = c(0.25,0.3))+
  scale_x_continuous(
    limits=c(0.2,1),
    guide = "prism_minor"
  )+
  scale_y_continuous(
    limits = c(-0.05,0.8),
    guide="prism_minor"
  )+
  scale_colour_prism(
    palette="candy_bright",
            name = "Cylinders",
            label=("prediction model"))
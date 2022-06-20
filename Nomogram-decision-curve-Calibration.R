library(Hmisc)
library(grid) 
library(lattice)
library(Formula)
library(ggplot2)
library(rms)
library(foreign)

library(rmda)
gbc <- read.spss("C:/Users/dyy/Desktop/dyy.sav",use.value.labels=T,to.data.frame=T)


dd=datadist(gbc)
options(datadist="dd")

f1 <- lrm(label~variable1+variable2+variable3+variable4,data = gbc,x=TRUE,y=TRUE)
f1

nom <- nomogram(f1, fun= function(x)1/(1+exp(-x)), fun.at=c(0.01, 0.1, 0.4, seq(0.1,0.9,by=0.3),0.95),# or fun=plogis
lp=F, funlabel="Risk")
plot(nom)

cal1 <-calibrate(f1,method = "boot",B=1000)
plot(cal1,xlim=c(0,1.0),ylim=c(0,1.0),
xlab = "Nomogram Predicted Survival", ylab = "Actual Survival")


library(rmda)
Data<- read.spss("C:/Users/dyy/Desktop/final.sav",use.value.labels=T,to.data.frame=T)

model1<-decision_curve(label~variable1,
                        data = Data,family = binomial(link ='logit'),
                        thresholds = seq(0,1, by = 0.01),
                        confidence.intervals= 0.95,
                        study.design = 'case-control',
                        population.prevalence= 0.3)

model2<-decision_curve(S~variable2
                        data = Data,family = binomial(link ='logit'),
                        thresholds = seq(0,1, by = 0.01),
                        confidence.intervals= 0.95,
                        study.design = 'case-control',
                        population.prevalence= 0.3)

model3<-decision_curve(S~variable3,
                        data = Data,family = binomial(link ='logit'),
                        thresholds = seq(0,1, by = 0.01),
                        confidence.intervals= 0.95,
                        study.design = 'case-control',
                        population.prevalence= 0.3)

model4<-decision_curve(S~variable4,
                        data = Data,family = binomial(link ='logit'),
                        thresholds = seq(0,1, by = 0.01),
                        confidence.intervals= 0.95,
                        study.design = 'case-control',
                        population.prevalence= 0.3)

List<- list(model1,model2,model3,model4)

plot_decision_curve(List,
                    curve.names=c('model1','model2','model3','model4'),
                    cost.benefit.axis =FALSE,col= c('red','blue','black','green'),
                    confidence.intervals=FALSE,
                    standardize = FALSE)

cal1 <-calibrate(f1,method = "boot",B=1000)
plot(cal1,xlim=c(0,1.0),ylim=c(0,1.0),
xlab = "Nomogram Predicted Survival", ylab = "Actual Survival")

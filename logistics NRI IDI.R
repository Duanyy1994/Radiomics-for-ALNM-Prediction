## -------------------- IDI NRI ----------------------
dev.off()
cat("\014"); rm(list = ls());   options(warn = -1); options(digits=3)
library(glmnet); library(PredictABEL)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mydata <- read.csv("SVM_train_predict.csv")
test_data <- read.csv("SVM_test_predict.csv")

## ----------- Training NRI IDI ------------------------ ##
mold = glm(label==1 ~ Age+Pathology, data = mydata, family=binomial(link="logit"), x = T, y = T)
mnew = glm(label==1 ~ Age+Pathology+radscore, data = mydata, family=binomial(link="logit"), x = T, y = T)

Label = which(colnames(mydata)=="label")
reclassification(data=mydata, cOutcome=Label,cutoff = c(0.0, 0.333, 0.666, 1.0),
                 predrisk1 = mold$fitted.values, predrisk2 = mnew$fitted.values)

## ----------- Testing NRI IDI ------------------------ ##
mold_test = predict(mold, newdata = test_data, type = 'response')
mnew_test = predict(mnew, newdata = test_data, type = 'response')

Label = which(colnames(test_data)=="label")
reclassification(data=test_data, cOutcome=Label,cutoff = c(0.0, 0.3, 0.6, 1.0),
                 predrisk1 = mold_test, predrisk2 = mnew_test)


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

fita <- lrm(label ~ dt,data = gbc,x=TRUE,y=TRUE) #Linearmodel1

fitb <- lrm(label ~ gb,data = gbc,x=TRUE,y=TRUE)  #Linearmodel2

fitc <- lrm(label ~ knn,data = gbc,x=TRUE,y=TRUE)  #Linearmodel3

fitd <- lrm(label ~ lsvc,data = gbc,x=TRUE,y=TRUE)  #Linearmodel4



library(pROC)

gfita <-roc(label~predict(fita),data = gbc)    #buildroccurve1

gfitb <-roc(label~predict(fitb),data = gbc)    #buildroccurve2

gfitc <-roc(label~predict(fitc),data = gbc)    #buildroccurve3

gfitd <-roc(label~predict(fitd),data = gbc)    #buildroccurve4


plot(gfita,
     print.auc=FALSE, print.auc.x=0.4, print.auc.y=0.5,# export AUC values on image, coordinate(x,y)
     #auc.polygon=TRUE, auc.polygon.col="#fff7f7", #setting colors
     #max.auc.polygon=FALSE,  
     grid=c(0.5, 0.2),  #
     
     smooth=F, # plot unsmooth curve
     main="Models development based on LASSO", # Titile
     col="red",  # coloer
     legacy.axes=TRUE)   

plot.roc(gfitb,
         add=T,  # add curves
         col="blue", # setting as blue color
      
         print.auc=FALSE, print.auc.x=0.4,print.auc.y=0.4,# export AUC values on image, coordinate(x,y)
         smooth = F)  # plot unsmooth curve

plot.roc(gfitc,
         add=T,  # add curves
         col="black", # setting as black
         
         print.auc=FALSE, print.auc.x=0.4,print.auc.y=0.4,# export AUC values on image, coordinate(x,y)
         smooth = F)  # plot unsmooth curve


plot.roc(gfitd,
         add=T,  # add curves
         col="green", # setting as green
         
         print.auc=FALSE, print.auc.x=0.4,print.auc.y=0.4,# export AUC values on image, coordinate(x,y)
         smooth = F)  # plot unsmooth curve


legend(0.35,0.45,  # legend position
       bty = "n",  # 
       title="",   # 
       legend=c("DT","GB","KNN","Linear SVC"),  # name it your corresponding model
       col=c("red","blue","black","green",),  # in line with previous colors
       lwd=2)  #



savePlot(filename = "Rplot",

         type ="pdf",

         device = dev.cur(),

         restoreConsole = TRUE)
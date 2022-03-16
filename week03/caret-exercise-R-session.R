library(caret)
library(AppliedPredictiveModeling)
library(e1071)
########################################################
###############pairs plot#########################
########################################################
transparentTheme(trans = .4)
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))
########################################################
###############get training/test set#########################
########################################################
set.seed(215)
train.indx=createDataPartition(iris$Species,p=0.5,list=FALSE,times = 1)
train=iris[train.indx,-5]; train.label=iris[train.indx,5]
test=iris[-train.indx,-5]; test.label=iris[-train.indx,5]
########################################################
###############knn in caret#########################
########################################################
#### set up train control
fitControl = trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated five times
  repeats = 5)
#### training process
set.seed(596)
knnFit1=train(train,train.label, method = "knn",
                trControl = fitControl,
                metric = "Accuracy",
                tuneLength=10)
knnFit1
plot(knnFit1)
#### test process
pred1=predict(knnFit1,test)
acc1=mean(pred1==test.label)
acc1
########################################################
#### specify a tuning grid###################
kNNGrid=expand.grid(k=c(1,3,5,7,9,11))
#### training process
set.seed(596)
knnFit2=train(train,train.label, method = "knn",
              trControl = fitControl,
              metric = "Accuracy",
              tuneGrid=kNNGrid)
knnFit2
plot(knnFit2)
#### test process
pred2=predict(knnFit2,test)
acc2=mean(pred2==test.label)
acc2
########################################################
#### preprocessing: standardise data###################
set.seed(596)
knnFit3=train(train,train.label, method = "knn",
              trControl = fitControl,
              metric = "Accuracy",
              preProcess = c("center","scale"),
              tuneGrid=kNNGrid)
knnFit3
plot(knnFit3)
#### test process
pred3=predict(knnFit3,test)
acc3=mean(pred3==test.label)
acc3

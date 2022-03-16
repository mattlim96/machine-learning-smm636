####################################################
##### Use LDA as a dimension reduction method#########
####################################################
##### 
library(caret)
#load german credit data from caret package
data(GermanCredit)
# classify two status: good or bad
## Show the first 10 columns
str(GermanCredit[, 1:10])
## Delete two variables where all values are the same for both classes
GermanCredit[,c("Purpose.Vacation","Personal.Female.Single")] <- list(NULL)
####################################################
##### use knn directly
# create training and test sets
set.seed(12)
trainIndex = createDataPartition(GermanCredit$Class, p = 0.7, list = FALSE, times = 1)
train.feature=GermanCredit[trainIndex,-10] # training features
train.label=GermanCredit$Class[trainIndex] # training labels
test.feature=GermanCredit[-trainIndex,-10] # test features
test.label=GermanCredit$Class[-trainIndex] # test labels
# set up train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated five times
  repeats = 5)
# training process
set.seed(5)
knnFit=train(train.feature,train.label, method = "knn",
             trControl = fitControl,
             metric = "Accuracy",
             preProcess = c("center","scale"),
             tuneLength=10)
knnFit
plot(knnFit)
knnFit$finalModel
# test process
pred=predict(knnFit,test.feature)
acc=mean(pred==test.label)
acc
####################################################
##### use lda to reduce dimension before using knn
# reduce dimension by lda
library(MASS)
fit=lda(train.feature,train.label)
train.feature.proj=predict(fit,train.feature)$x
test.feature.proj=predict(fit,test.feature)$x
# set up train control
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated five times
  repeats = 5)
# training process of knn
set.seed(5)
knnFit2=train(train.feature.proj,train.label, method = "knn",
             trControl = fitControl,
             metric = "Accuracy",
             preProcess = c("center","scale"),
             tuneLength=10)
knnFit2
plot(knnFit2)
knnFit2$finalModel
# test process
pred2=predict(knnFit2,test.feature.proj)
acc2=mean(pred2==test.label)
acc2

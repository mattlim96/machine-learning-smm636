library(ISLR)
library(class)
library(caret)
library(pROC)

data("GermanCredit")
## Delete two variables where all values are the same for both classes
GermanCredit[,c("Purpose.Vacation","Personal.Female.Single")] = list(NULL)
# create training and test sets
set.seed(12)
trainIndex = createDataPartition(GermanCredit$Class, p = 0.7, list = FALSE, times = 1)
train.feature=GermanCredit[trainIndex,-10] # training features
train.label=GermanCredit$Class[trainIndex] # training labels
test.feature=GermanCredit[-trainIndex,-10] # test features
test.label=GermanCredit$Class[-trainIndex] # test labels
####################################
#### knn
#### set up train control
fitControl = trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated five times
  repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE)
#### training process
set.seed(5)
knnFit=train(train.feature,train.label, method = "knn",
             trControl = fitControl,
             metric = "ROC",
             preProcess = c("center","scale"),
             tuneLength=10)
knnFit
####################################
#### ROC curve for knn
knn.pred = predict(knnFit,test.feature)
confusionMatrix(knn.pred,test.label)
knn.probs = predict(knnFit,test.feature,type="prob")
knn.ROC = roc(predictor=knn.probs$Bad,
               response=test.label)
knn.ROC$auc
plot(knn.ROC,main="ROC curve")
####################################
####################################
#### Use SMOTE to make the class distribution balanced
table(GermanCredit$Class)
####################################
#### knn with smote
#### set up train control
fitControls = trainControl(## 5-fold CV
  method = "repeatedcv",
  number = 5,
  ## repeated five times
  repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  sampling="smote")
#### training process
set.seed(5)
knnFits=train(train.feature,train.label, method = "knn",
             trControl = fitControls,
             metric = "ROC",
             preProcess = c("center","scale"),
             tuneLength=10)
knnFits
####################################
#### ROC curve for knn with SMOTE
knn.preds = predict(knnFits,test.feature)
confusionMatrix(knn.preds,test.label)
knn.probss = predict(knnFits,test.feature,type="prob")
head(knn.probss)
knn.ROCs = roc(predictor=knn.probss$Bad,
               response=test.label)
knn.ROCs$auc
plot(knn.ROCs,main="ROC curve")


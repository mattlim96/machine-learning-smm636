###########################################
##### Use the lda function in caret#########
###########################################
##### 
library(caret)
##### 
# Prepare training and test set:
n=25 # the number of training data in each class
NN=dim(iris)[1]/3# the total number of observations in each class
set.seed(983)
index_s=sample(which(iris$Species=="setosa"),n)
index_c=sample(which(iris$Species=="versicolor"),n)
index_v=sample(which(iris$Species=="virginica"),n)
# get training and test set
train_rand = rbind(iris[index_s,], iris[index_c,], iris[index_v,])
test_rand = rbind(iris[-c(index_s,index_c,index_v),])
##### 
# fit the lda model
ldaFit=train(train_rand[,-5],train_rand[,5],method="lda",
             trControl=trainControl(method = "none"))
ldaFit$finalModel
# prediction and calculate accuracy
pred=predict(ldaFit,test_rand[,-5])
acc=mean(pred==test_rand[,5])
acc
###########################################
##### Tune the dimension in lda#########
###########################################
##### 
# Tune the parameter by repeated CV
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 5)
set.seed(836)
lda2Fit=train(train_rand[,-5],train_rand[,5],method="lda2",
              trControl=fitControl)
plot(lda2Fit)
#####
# prediction and calculate accuracy
pred2=predict(lda2Fit,test_rand[,-5])
acc2=mean(pred2==test_rand[,5])
acc2

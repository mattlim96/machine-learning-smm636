###########################################
##### Use the lda function in MASS#########
###########################################
##### 
library(MASS) # the lda function is in the MASS library
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
# get class factor for training data
train_label= factor(c(rep("s",n), rep("c",n), rep("v",n)))
# get class factor for test data
test_label_true=factor(c(rep("s",NN-n), rep("c",NN-n), rep("v",NN-n)))
##### 
# fit the lda model
fit1=lda(Species~.,data=train_rand)
# or use
# fit2=lda(train_rand[,-5],train_rand[,5])
fit1
##### prediction for test set
pred=predict(fit1,test_rand[,-5])
pred
##### get the classification accuracy
acc = mean(test_rand[,5]==pred$class)
acc

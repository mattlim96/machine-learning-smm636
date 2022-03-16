library(tree)
library(randomForest)
library(gbm)
library(caret)
heart=read.csv("Heart.csv",header = TRUE)
sum(is.na(heart)) #check missing values
heart=heart[complete.cases(heart),]
heart$AHD=factor(heart$AHD)
heart$ChestPain=factor(heart$ChestPain)
heart$Thal=factor(heart$Thal)
#################################################################
#########Decision tree: full#####################################
#################################################################
#################################################################
#########random split to training and test set##################
set.seed(345)
train.index=createDataPartition(heart$AHD,p=0.7,list=FALSE)
train=heart[train.index,]
test=heart[-train.index,]
#################################################################
#########Train a decision tree####################################
heart.tree=tree(AHD ~ . , train)
summary(heart.tree)
# have a look at the details of the tree
heart.tree
# plot the tree
plot(heart.tree)
text(heart.tree,pretty=2)
#################################################################
#########Test error######################################################
pred=predict(heart.tree,test[,-ncol(test)],type="class")
mean(pred==test[,ncol(test)])
#################################################################
#########Decision tree: prune#####################################
#################################################################
#################################################################
set.seed(102)
heart.cv=cv.tree(heart.tree,FUN=prune.misclass)
heart.cv
#plot CV results
par(mfrow=c(1,2))
plot(heart.cv$size,heart.cv$dev,type="b",
     xlab="number of leaves of the tree",ylab="CV error rate%",
     cex.lab=1.5,cex.axis=1.5)
plot(heart.cv$k,heart.cv$dev,type="b",
     xlab=expression(alpha),ylab="CV error rate%",
     cex.lab=1.5,cex.axis=1.5)
#######prune the tree
heart.prune=prune.misclass(heart.tree,best=9) # >>> 9 leaves picked from lowest heart.cv$dev
par(mfrow=c(1,1))
plot(heart.prune)
text(heart.prune,pretty=1)
#######predict the test instances
pred.prune=predict(heart.prune,test[,-ncol(test)],type="class")
mean(pred.prune==test[,ncol(test)])

#################################################################
#########caret for trees#####################################
#################################################################
#################################################################
fitcontrol=trainControl(method = "repeatedcv", number = 10,
                        repeats = 3)
set.seed(1) 
heart.rpart=train(train[,-ncol(heart)],train[,ncol(heart)], 
                  method = "rpart", tuneLength=5,
                  trControl = fitcontrol) # >>> rpart = alpha
heart.rpart
#To look at the details of this tree 
print(heart.rpart$finalModel)
# plot the tree
plot(heart.rpart$finalModel) 
text(heart.rpart$finalModel,pretty=1)
#### get fancy trees by rattle
library(rattle)
fancyRpartPlot(heart.rpart$finalModel)
#################################################################
#########Bagging#####################################
#################################################################
#################################################################
set.seed(103)
heart.bag=randomForest(AHD~.,data=train,mtry=13,importance=TRUE,ntree=500)
                      # >>> mtry is # of features to randomly select
heart.bag
pred.bag=predict(heart.bag,newdata=test[,-ncol(test)])
mean(pred.bag==test[,ncol(test)])
#################################################################
#########Random forest#####################################
#################################################################
#################################################################
set.seed(921)
heart.rf=randomForest(AHD~.,data=train,mtry=2,importance=TRUE,ntree=500)
heart.rf
pred.rf=predict(heart.rf,newdata=test[,-ncol(test)])
mean(pred.rf==test[,ncol(test)])
#######importance of variables
detach(package:rattle)
importance(heart.rf) # >>> variable importance measure
varImpPlot(heart.rf)
#################################################################
#########caret for random forest#####################################
#################################################################
#################################################################
fitControl=trainControl( method = "repeatedcv", number = 5,
                         repeats = 3)
set.seed(921)
rfFit=train(AHD~.,data=train,method="rf",metric="Accuracy",
                        trControl=fitControl,tuneLength=5) # Have a look at the model
                            # >>> tuneLength here is associated to the mtry output
                            # >>> # of features to randomly select
rfFit
pred.rfFit=predict(rfFit,newdata=test[,-ncol(test)])
mean(pred.rfFit==test[,ncol(test)])
plot(rfFit)
rfFit$finalModel
varImp(rfFit,scale=FALSE)
#################################################################
#########Boosting#####################################
#################################################################
#################################################################
set.seed(18)
train[,ncol(train)]=ifelse(train$AHD=="No",0,1)
test[,ncol(test)]=ifelse(test$AHD=="No",0,1)
heart.boost=gbm(AHD~.,data=train,distribution="bernoulli",n.trees=5000,
                interaction.depth=1,shrinkage=0.01)
summary(heart.boost)
pred.boost=predict(heart.boost,newdata=test[,-ncol(test)],n.trees=5000,type="response")
pred.boost=ifelse(pred.boost<0.5,0,1)
mean(pred.boost==test[,ncol(test)])

library(ISLR)
library(class)
library(caret)
library(ROCR)
library(MASS)
str(Default)
####get training set index
pr=0.7
n1=floor(sum(Default$default=="No")*pr)
n2=floor(sum(Default$default=="Yes")*pr)
set.seed(20)
index1=sample(which(Default$default=="No"),n1)
index2=sample(which(Default$default=="Yes"),n2)
####get training test set
Default[,3]=scale(Default[,3])
Default[,4]=scale(Default[,4])
train.feature=Default[c(index1,index2),c(3,4)]
train.label=Default[c(index1,index2),1]
test.feature=Default[-c(index1,index2),c(3,4)]
test.label=Default[-c(index1,index2),1]
#### 9NN
set.seed(47)
kk=9
knn3.pred=knn3Train(train.feature, test.feature, train.label, k = kk, prob=TRUE)
att=attributes(knn3.pred)$prob
#### LDA
fit=lda(x=train.feature,grouping=train.label)
lda.pred=predict(fit,test.feature)
#### ROC curves
predkNN.ROCR = prediction(att[,2], test.label)
rockNN.curve = performance(predkNN.ROCR,"tpr","fpr")
plot(rockNN.curve,lwd=2,cex.lab=1.5,cex.axis=1.5,  font.lab=2,xlab="False positive rate (1-specificity)",
     ylab="True positive rate (sensitivity)",
     xlim=c(0,1),ylim=c(0,1))
predlda.ROCR = prediction(lda.pred$posterior[,2], test.label)
roclda.curve = performance(predlda.ROCR,"tpr","fpr")
plot(roclda.curve,lwd=2,col="blue", lty=3,add=TRUE)
#### add a line with auc=0.5
x=seq(0,1,by=0.01); y=x
lines(x,y,lwd =2, col =" red",lty=2)
legend("bottomright",legend=c("9NN","LDA"),
       col=c("black","blue"),lty=c(1,3),cex=1,text.font=2)
#### get auc
auc.kNN = performance(predkNN.ROCR, measure = "auc")
auc.kNN = auc.kNN@y.values[[1]]
auc.kNN
auc.lda = performance(predlda.ROCR, measure = "auc")
auc.lda = auc.lda@y.values[[1]]
auc.lda



set.seed(10)
library(MASS)
class1=mvrnorm(n = 30, mu=c(1,1), Sigma=matrix(c(1,0,0,1),2,2))
class2=mvrnorm(n = 30, mu=c(2,2), Sigma=matrix(c(1,0,0,1),2,2))
train.feature=rbind(class1,class2)
train.label=c(rep(1,30),rep(2,30))
##### scatter plot of the training data
plot(train.feature,col=ifelse(train.label==1, "blue", "red"),
     pch=ifelse(train.label==1, 16, 17),xlab="x1",ylab="x2",cex=1.5)
points(2.1,1.5,pch=15,col="green",cex=1.5) ## test point
##### find k nearest neighbours
library("FNN")
test.feature=t(c(2.1,1.5))
k=1
knearest=get.knnx(train.feature,test.feature, k)
neighbour=train.feature[knearest$nn.index,]
if(k==1){
  points(t(neighbour),pch=0,col="black",cex=1.8)}else{
    points((neighbour),pch=0,col="black",cex=1.8)
  }
library(class)
##### load training data
simulate=read.table("simulate.txt",header=TRUE)
train.feature=simulate[,-3]
train.label=simulate[,3]
##### scatter plot of the training data
plot(train.feature,col=ifelse(train.label==1, "blue", "red"),
       pch=ifelse(train.label==1, 16, 17),main="15 nearest neighbours")
##### generate test data
new.x1=seq(-3,4.5,by=0.1)
new.x2=seq(-2,4,by=0.1)
new=expand.grid(new.x1,new.x2)
##### get predictions from 15-NN
pred = knn(train.feature, new, train.label, k=15, prob=TRUE)
##### get P(Y=1|new)
prob = attr(pred, "prob")
prob = ifelse(pred=="1", prob, 1-prob)
##### get contour P(Y=1|X)=0.5
contour(new.x1, new.x2, matrix(prob, nrow = length(new.x1),ncol = length(new.x2)), 
        levels=0.5,label="", axes=FALSE, add=TRUE)

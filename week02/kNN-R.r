library(class)
########################################################
############### Example in help#########################
########################################################
# get training and test set
train = rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test = rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
# get class factor
cl = factor(c(rep("s",25), rep("c",25), rep("v",25)))
# classification using knn, with k=3
set.seed(47)
knn_pred=knn(train, test, cl, k = 3, prob=TRUE)
# see the attributes of the knn model
att=attributes(knn_pred)
# get the probabilities associated with the final prediction
prob_pred=att$prob
######### 1NN ###############
set.seed(47)
knn1_pred=knn1(train, test, cl)
######### see all probabilities###############
library(caret)
set.seed(47)
knn3_pred=knn3Train(train, test, cl, k = 3, prob=TRUE)
attributes(knn3_pred)
########################################################
############### Randomly sample from data###############
########################################################
# get indexes for training data
n=25 # the number of training data in each class
NN=dim(iris3)[1]# the total number of observations in each class
set.seed(983)
index_s=sample(1:NN,n)
index_c=sample(1:NN,n)
index_v=sample(1:NN,n)
# get training and test set
train_rand = rbind(iris3[index_s,,1], iris3[index_c,,2], iris3[index_v,,3])
test_rand = rbind(iris3[-index_s,,1], iris3[-index_c,,2], iris3[-index_v,,3])
# get class factor for training data
train_label= factor(c(rep("s",n), rep("c",n), rep("v",n)))
# get class factor for test data
test_label_true=factor(c(rep("s",NN-n), rep("c",NN-n), rep("v",NN-n)))
# classification using knn, with k=5
kk=5 # number of nearest neighbours
set.seed(275)
knn_pred=knn(train=train_rand,test=test_rand,cl=train_label, k=kk, prob=FALSE)


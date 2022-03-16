library(e1071)
data(iris)
attach(iris)
## classification mode
# default with factor response:
model = svm(Species ~ ., data = iris)
# alternatively the traditional interface:
model = svm(iris[,-5], iris[,5])
print(model)
summary(model)

# test with train data
pred = predict(model, iris[,-5])
# (same as:)
pred = fitted(model)
# Check accuracy:
table(pred, iris[,5])
# compute decision values and probabilities:
pred = predict(model, iris[,-5], decision.values = TRUE)
attr(pred, "decision.values")[1:4,]
pred[1:4]
# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])
# get probabilities
model = svm(iris[,-5], iris[,5], probability = TRUE)
pred = predict(model, iris[,-5], decision.values = TRUE, probability = TRUE)
attr(pred, "decision.values")[1:4,]
attr(pred, "probabilities")[1:4,]
##############tune parameters for radial kernel
set.seed(392)
tune.out= tune(svm, iris[,-5], iris[,5],
            ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
            tunecontrol = tune.control(sampling = "cross"),cross=10)
summary(tune.out)
plot(tune.out)
# use the best model to predict the training instances
tune.out$best.model
pred=predict(tune.out$best.model,iris[,-5])
table(pred, iris[,5])
########################################################
################use svm in caret############################
########################################################
library(caret)
#load german credit data from caret package
data(GermanCredit)
# classify two status: good or bad
## Show the first 10 columns
str(GermanCredit[, 1:10])
## Delete two variables where all values are the same for both classes
GermanCredit[,c("Purpose.Vacation","Personal.Female.Single")] <- list(NULL)
#Get training and test sets
set.seed(12)
trainIndex = createDataPartition(GermanCredit$Class, p = 0.7, list = FALSE, times = 1)
train=GermanCredit[trainIndex,] # training set
test=GermanCredit[-trainIndex,-10] # test set
#train the model
fitControl=trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3)
set.seed(2333)
svm.Radial=train(Class ~., data = train, method = "svmRadial",
                 trControl=fitControl,
                 preProcess = c("center", "scale"),
                 tuneLength = 5)
svm.Radial
plot(svm.Radial)
# tune both parameters
grid_radial=expand.grid(sigma = c(0.01, 0.1, 1,10),
                        C = c(0.01, 0.1, 1, 10))
fitControl=trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3)
set.seed(2333)
svm.Radialg=train(Class ~., data = train, method = "svmRadial",
                  trControl=fitControl,
                  preProcess = c("center", "scale"),
                  tuneGrid = grid_radial)
svm.Radialg
plot(svm.Radialg)

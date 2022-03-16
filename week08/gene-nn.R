#############################################################
####### installation #######################################
### install tensorflow
#install.packages("tensorflow")
#library(tensorflow)
#install_tensorflow() #this step takes a while
#library(tensorflow)
#tf$constant("Hellow Tensorflow")
### install keras
#install.packages("keras")
#library(keras)
#############################################################
library(keras)
library(ISLR)
######get gene data
nci.labs=NCI60$labs 
nci.data=NCI60$data
####################################
######PCA
pr.out=prcomp(nci.data, scale=TRUE)
######PC plots
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,
       xlab="Z1",ylab="Z2")
############################################################
######Neural network with one hidden layer
model = keras_model_sequential() 
model %>% 
  layer_dense(units = 2, activation = 'linear', input_shape = ncol(nci.data),name = "subspace") %>%
  layer_dense(units = ncol(nci.data), activation = 'linear')
summary(model)
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = "adam"
)
history <- model %>% fit(
  as.matrix(nci.data), as.matrix(nci.data), 
  epochs = 1000
)
subspace <- keras_model(inputs = model$input, outputs = get_layer(model, "subspace")$output)
projection1 <- predict(subspace, nci.data)
plot(projection1, col=Cols(nci.labs), pch=19,
     xlab="W1",ylab="W2")
######################################################
######Neural network with three hidden layers
model2 <- keras_model_sequential() 
model2 %>% 
  layer_dense(units = 10, activation = 'tanh', input_shape = ncol(nci.data)) %>%
  layer_dense(units = 2, activation = 'linear',name = "subspace") %>%
  layer_dense(units = 10, activation = 'tanh') %>%
  layer_dense(units = ncol(nci.data), activation = 'tanh')
summary(model2)
model2 %>% compile(
  loss = 'mean_squared_error',
  optimizer = "adam"
)
history <- model2 %>% fit(
  as.matrix(nci.data), as.matrix(nci.data), 
  epochs = 150
)
subspace <- keras_model(inputs = model2$input, outputs = get_layer(model2, "subspace")$output)
projection2 <- predict(subspace, nci.data)
plot(projection2, col=Cols(nci.labs), pch=19,
     xlab="W1",ylab="W2")

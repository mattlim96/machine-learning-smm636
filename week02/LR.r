##################################
###### Logistic regression ######
##################################
library(ISLR)
# have a look at the dataset
names(Smarket)
dim(Smarket)
summary(Smarket)
# get the correlation between predictors
# cor(Smarket)
cor(Smarket[, -9])
# fit a logistic regression model
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data = Smarket, family = binomial)

summary(glm.fits)
# predict the probability that the market will go up
glm.probs = predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Smarket$Direction)
# classification based on the predicted probabilities
glm.pred = ifelse(glm.probs > .5, "Up", "Down")
####################################################
###### Train the model based on training data ######
###### Test the model for test data ######
####################################################
# get training data indexes
train = (Smarket$Year < 2005)
# get test data
Smarket.2005 = Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 = Smarket$Direction[!train]
# train a logistic regression model based on training data
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data = Smarket, family = binomial, subset = train)
# predict probabilities that the market will go up for the test data
glm.probs = predict(glm.fits, Smarket.2005, type = "response")
# classification of the test data
glm.pred = ifelse(glm.probs > .5, "Up", "Down")



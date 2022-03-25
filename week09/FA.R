#############################################
### load data
library(lavaan)
dat = HolzingerSwineford1939[HolzingerSwineford1939$school=="Grant-White",]
dat = dat[, -c(1:6)]
names(dat) = c("visual", "cubes", "lozenge", "paragraph", "sentence", "wordm", 
               "add", "counting", "straight")
dat[1:3, ]
###########################################
### determine the number of factors of FA
fa.cor = cor(dat)
fa.eigen = eigen(fa.cor)
fa.eigen$values
sum(fa.eigen$values)
cumsum(fa.eigen$values)/9
# use the scree plot
plot(fa.eigen$values, type = "b", ylab = "Eigenvalues", xlab = "Factor") # we choose 4
##############################################
### factor analysis
fa.res = factanal(x = dat, factors = 4, rotation = "none")
fa.res
##############################################
### factor rotation
fa.res = factanal(x = dat, factors = 4, rotation = "promax")
print(fa.res, cut = 0.2)
##############################################
### factor scores
fa.res = factanal(x = dat, factors = 4, rotation = "promax", scores = "Bartlett")
head(fa.res$scores)
summary(lm(Factor2 ~ Factor1, data = as.data.frame(fa.res$scores)))

##########################################
########## PCA on the USArrests data
# have a look at the USArrests data
states = row.names(USArrests)
states
names(USArrests)
# briefly examine the data
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
# apply PCA
pr.out = prcomp(USArrests, scale = TRUE)
names(pr.out)
# have a look at the output
pr.out$center
pr.out$scale
pr.out$rotation
# get the PC vector
dim(pr.out$x)
# plot the PCs
biplot(pr.out, scale = 0,cex=0.5)
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale =0,cex=0.5)
# check the variance explained by each PC
pr.out$sdev
pr.var = pr.out$sdev ^2
pr.var
# proportion of variance explained
pve = pr.var/sum(pr.var)
pve
plot(pve, xlab = " Principal Component", ylab = "Proportion of
Variance Explained", ylim = c(0,1), type = "b")

plot(cumsum(pve), xlab = "Principal Component", ylab ="
Cumulative Proportion of Variance Explained", ylim = c(0,1),
     type = "b")
############################################
######## PCA for mixed data
library(PCAmixdata)
data("gironde")
head(gironde$housing)
# apply PCA on the housing data
split = splitmix(gironde$housing)
X1 = split$X.quanti
X2 = split$X.quali
res.pcamix = PCAmix(X.quanti = X1, X.quali = X2, rename.level = TRUE, graph = FALSE)
res.pcamix$eig
# have a look at the results
par(mfrow=c(2,2))
plot(res.pcamix, choice = "ind", coloring.ind = X2$houses, label = FALSE,
     posleg = "bottomright", main ="(a) Observations")
plot(res.pcamix, choice = "levels", xlim = c(-1.5,2.5), main = "(b) Levels")
plot(res.pcamix, choice = "cor", main = "(c) Numerical variables")
plot(res.pcamix, choice = "sqload", coloring.var = T, leg = TRUE,
     posleg = "topright", main = "(d) All variables")
sort(res.pcamix$ind$coord[,2])[1:10]

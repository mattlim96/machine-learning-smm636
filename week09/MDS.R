#######################################
### load data
library(haven)
cerealnut = read_dta("cerealnut.dta")
apply(cerealnut[,-1], 2, var)
########################################
### apply classical MDS directly
cereal.dist = dist(cerealnut[,-1])
cereal.mds = cmdscale(cereal.dist)
cereal.mds
# plot the configuration
rownames(cereal.mds) = cerealnut$brand
plot(cereal.mds, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", 
     xlim = c(-200, 250), ylim = c(-200, 250))
text(cereal.mds, rownames(cereal.mds), cex=0.5)
########################################
### apply classical MDS after standardisation
cerealnut.sc =scale(cerealnut[,-1])
cereal.sc.dist = dist(cerealnut.sc)
cereal.sc.mds = cmdscale(cereal.sc.dist)
rownames(cereal.sc.mds) = cerealnut$brand
plot(cereal.sc.mds, type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
text(cereal.sc.mds, rownames(cereal.sc.mds), cex=0.5)
#########################################
### ordinal MDS
brand.ratings = read.csv("http://goo.gl/IQl8nc")
head(brand.ratings)
tail(brand.ratings)
# manipulate data before proceeding
brand.mean = aggregate(. ~ brand, data=brand.ratings, mean)
brand.mean
rownames(brand.mean) = brand.mean[, 1] # use brand for the row names
brand.mean = brand.mean[, -1] # remove brand name column
brand.rank = data.frame(lapply(brand.mean, function(x) ordered(rank(x))))
brand.rank
# get distance matrix
library(cluster)
brand.dist.r = daisy(brand.rank, metric="gower")
# ordinal MDS
library(MASS)
brand.mds.r = isoMDS(brand.dist.r)
plot(brand.mds.r$points, type="n")
text(brand.mds.r$points, rownames(brand.mean), cex=1)

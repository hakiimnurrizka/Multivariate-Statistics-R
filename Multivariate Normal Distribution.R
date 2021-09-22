###multivariate normal distribution###
library(mvtnorm)
library(MVN)

##simulating data from MN(mu,sigma)
set.seed(100)
mu = c(1,221,98)
sigma = matrix(c(200,1,-2,1,1325,4,-2,4,1432), nrow = 3, ncol = 3, byrow = T)

x = rmvnorm(100,mu , sigma,method=c("eigen", "svd", "chol"), pre0.9_9994 = FALSE)
head(x)
list_x = list(x[,1],x[,2], x[,3])

makeProfilePlot(list_x, names= c("v1","v2", "v3"))

##Multiv normality test
#mardia
mvn(iris, mvnTest = "mardia")
#HZ
mvn(iris, mvnTest = "hz")
#Royston
mvn(iris, mvnTest = "royston")
#DH
mvn(iris, mvnTest = "dh")
#energy
mvn(iris, mvnTest = "energy")

#perspective andn contour plot
mvn(iris[,2:3], mvnTest = "hz", multivariatePlot = "persp")
mvn(iris[,3:4], mvnTest = "mardia", multivariatePlot = "contour")
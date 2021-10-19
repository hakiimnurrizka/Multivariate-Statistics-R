###Multivariate Test on Two Mean Vectors###
#Source : https://rpubs.com/SaraGarcesCespedes/587169 #
if (!require('devtools')) install.packages('devtools')
devtools::install_github('fhernanb/stests', force=TRUE)
#Materials : Methods_of_Multivariate_Analysis-_3rd_Edition Rencher & Christensen #

library(MASS)
library(stests)
library(ggplot2)
library(rgl)
library(car)
library(RColorBrewer)
library(stats)
library(gridExtra)
library(tidyverse)
library(DescTools)

##On testing mean vectors
#at this point, most of us should have already been familiar on testing mean from 2 variables which is a univariate case.
#in the case of multivariate test on means, "testing mean" means that we want to use the correlation between variables, which
#in univariate case mostly always being avoided. This is one of the advantage on using multivariate test rather than
#testing individually each of the variable in univariate realm. Other adavantages on considering multivariate test are
#power of the test and probability of type 1 error. Important assumption used are : normality and independence

##Testing when the covariance matrix is known
#similar to univariate case, lets start with testing means when the covariance is known.
#simulate the samples
meanvect = c(13,5,1250)
cov_m = matrix(c(124,123,47,
                 123,203,162,
                 47,162,3257), ncol = 3)
#define a function to sample from a MN dist
set.seed(123)
gen.data = function(mu, sigma, n) {
  require(MASS)
  dt = mvrnorm(n, mu=mu, Sigma=sigma) #samples
  meandt = mean(dt)
  return(dt)
}
gen.samples = gen.data(meanvect, cov_m, 300)
colnames(gen.samples) = c("v1", "v2", "v3")
plot3d(gen.samples)

#Testing for one sample
#Lets test the generated sample gen.samples with the null hypothesis mu* = mu = c(10,5,1200)
#Known covariance matrix (we use the covariance of the previous normal distribution)
n = dim(gen.samples)[1]
mu.sam = apply(gen.samples, 2, mean)
mu = c(10, 5, 1200)

z2 = n*t(mu.sam-mu)%*%solve(cov_m)%*%(mu.sam-mu)
p.z2 = dchisq(z2, 3) #Z2 is distributed as chisq(p) if null is true
p.z2
#We conclude from the pvalue above that there is not enough evidence for the null hypothesis 
#mu* = mu = c(10,5,1200) to be accepted

#Using hotellingT2 function from desctools package for unknown covariance matrix
HotellingsT2Test(gen.samples, mu = mu)

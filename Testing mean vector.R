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
library(MVN)
library(RVAideMemoire)
library(BSDA)

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
view(gen.samples)
plot3d(gen.samples)

#Testing for one sample
#Lets test the generated sample gen.samples with the null hypothesis mu* = mu = c(10,5,1200)
#Known covariance matrix (we use the covariance of the previous normal distribution)
n = dim(gen.samples)[1]
mu.sam = apply(gen.samples, 2, mean)
mu = c(10, 5, 1200)

z.2 = n*t(mu.sam-mu)%*%solve(cov_m)%*%(mu.sam-mu)
p.z2 = pchisq(z.2, 3, lower.tail = F) #Z2 is distributed as chisq(p), p : degree of freedom if null is true
p.z2
#We conclude from the pvalue above that there is not enough evidence for the null hypothesis 
#mu* = mu = c(10,5,1200) to be accepted
HotellingsT2Test(gen.samples, mu = mu, cov_m, test = "chi")#Using function from desctools package

#Testing mu for unknown covariance matrix
t2 = n*t(mu.sam-mu)%*%solve(cov(gen.samples))%*%(mu.sam-mu)#T2 statistics, 
f.eq = (297/(3*299))*t2
HotellingsT2Test(gen.samples, mu = mu, cov(gen.samples), test = "f")

##Side Note: For function HotellingsT2Test above, the statistics are actually the approximation distribution 
##stated in the argument "test = ". That is for the test = "chi" the T.2 in the output is actually chi square
##approximation, a similar logic follows for test = "f". Another note is that the scaled value of the statistics
##is only applicable for 2 sample test, so for above example we can only use the p value.

#assumption test
mvn(gen.samples) #the result is expected since we draw the sample from a normal distribution
#But there is an interesting finding, where the univariate normality of the 3rd variable is rejected
mshapiro.test(gen.samples)#From library RVAideMemoire
det(cov(gen.samples))
chol(cov(gen.samples))

##Comparison the result with univariate case
#H0 : (1) mu1 = 10; (2) mu2 = 5; (3) mu3 = 1200

#Known Variance using the covariance matrix from multivariate normal distribution above
cov_m
#1st variable
z1 = (mean(gen.samples[,1])-10)/(124/sqrt(length(gen.samples[,1])))#Z statistics
z1
2*pnorm(z1, 0, 1, lower.tail = F)#P value for testing 2 way hypothesis with alternative h1 : mu1 =/= 10
z.test(gen.samples[,1], sigma.x = 124, mu = 10)
#2nd variable
z2 = (mean(gen.samples[,2])-5)/(203/sqrt(length(gen.samples[,2])))
z2
2*pnorm(z2, 0, 1, lower.tail = F)#P value for testing 2 way hypothesis with alternative h1 : mu1 =/= 10
#Rounding error, leads to pvalue > 1
z.test(gen.samples[,2], sigma.x = 203, mu = 5)
#3rd variable
z3 = (mean(gen.samples[,3])-1200)/(3257/sqrt(length(gen.samples[,3])))
z3
2*pnorm(z3, 0, 1, lower.tail = F)
z.test(gen.samples[,3], sigma.x = 3257, mu = 1200)[2]
#All individual univariate tests have the same conclusion, that is acceptance of null hypothesis
ztest.ind = data.frame(var1 = c(NA, NA), var2 = c(NA,NA), var3 = c(NA,NA))
rownames(ztest.ind) = c("z stat", "2 way-p value")
for(j in 1:3){
    ztest.ind[1,j] = as.numeric(z.test(gen.samples[,j], sigma.x = cov_m[j,j], mu = mu[j])[1])
    ztest.ind[2,j] = as.numeric(z.test(gen.samples[,j], sigma.x = cov_m[j,j], mu = mu[j])[2])
}


#Unknown variance
p.val_test = data.frame(pv.test1 = NA, pv.test2 = NA, pv.test3 = NA)
for(i in 1:3){
  p.val_test[i] = as.numeric(t.test(gen.samples[,i], mu = mu[i])[3])
}
p.val_test
#Using T test for individual testing of null hypothesis mu_sam[i] = mu[i], it is found that only 2nd variable
#test that has its null hypothesis accepted.
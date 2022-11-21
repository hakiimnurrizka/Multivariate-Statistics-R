##Tugas 1 Praktikum##

#1
simulate.mvn = function(n,h){
  library(MASS)
  z2.stats = rep(NA, h)
  reject.null = rep(NA, h)
  for(i in 1:h){
    size = n*3
    mu1 = c(0, 12, 7)
    sigma1 = matrix(c(0.65, 0.59, 0.22, 0.59, 2.71, 0.56, 0.22, 0.56, 0.24), byrow = T, ncol = 3)
    simulated = matrix(rep(NA, size), ncol = 3)
    for(j in 1:n){
      simulated[j,] = mvrnorm(1, mu = mu1, Sigma = sigma1)
    }
    mean.sim = apply(simulated, 2, mean)
    cov.sim = cov(simulated)
    mu.null = c(0.02, 11.95, 7.04)
    z.2 = as.numeric(n*t(mean.sim-mu.null)%*%solve(sigma1)%*%(mean.sim-mu.null))
    z2.stats[i] = z.2
    p.z2 = pchisq(z.2, 3, lower.tail = F)
    reject.null[i] = ifelse(p.z2<0.05, 1, 0)
  }
  return(list(output.statistics = data.frame(statistics.z2 = z2.stats, reject.null), 
              null.rejection.proportion = sum(reject.null)/h))
}
#2
set.seed(1706047391)
simulate.mvn(100, 500)#21.2% of the total resampling are found to be rejecting the null hypothesis

#3
simulate.mvn2 = function(n,h){
  library(MASS)
  f.stats = rep(NA, h)
  reject.null = rep(NA, h)
  for(i in 1:h){
    size = n*3
    mu1 = c(0, 12, 7)
    sigma1 = matrix(c(0.65, 0.59, 0.22, 0.59, 2.71, 0.56, 0.22, 0.56, 0.24), byrow = T, ncol = 3)
    simulated = matrix(rep(NA, size), ncol = 3)
    for(j in 1:n){
      simulated[j,] = mvrnorm(1, mu = mu1, Sigma = sigma1)
    }
    mean.sim = apply(simulated, 2, mean)
    cov.sim = cov(simulated)
    mu.null = c(0.02, 11.95, 7.04)
    t.2 = as.numeric(n*t(mean.sim-mu.null)%*%solve(cov.sim)%*%(mean.sim-mu.null))
    f.eq = t.2*(n-3)/((n-1)*3)
    f.stats[i] = f.eq
    p.f = pf(f.eq, 3, (n-3), lower.tail = F)
    reject.null[i] = ifelse(p.f<0.05, 1, 0)
  }
  return(list(output.statistics = data.frame(statistics.f = f.stats, reject.null), 
              null.rejection.proportion = sum(reject.null)/h))
}
set.seed(1706047391)
simulate.mvn2(300, 1000)#51.3% of the total resampling are found to be rejecting the null hypothesis

#4
#The previous simulation give an illustration to "power" of each test. The first test is Zsquared with
#known variance while the 2nd is Zsquared with unknown variance. The null rejection for the known variance
#has lower proportion indicated that the Zsquared with known variance has higher tendency to accept
#the null compared to Zsquared with unknown variance. This also implies that the zsquared statistics
#is more likely to reject null when using information solely from the data.


#5
satis = read.csv("satisfaction.csv")
v.mean = apply(satis, 2, mean)
cov.sat = cov(satis)

#6
n = dim(satis)[1]
mu.sam = apply(satis[,c(1,4)], 2, mean)
mu = c(70, 2.3)
cov_m = cov(satis[,c(1,4)])
z.2 = n*t(mu.sam-mu)%*%solve(cov_m)%*%(mu.sam-mu)
p.z2 = pchisq(z.2, 3, lower.tail = F) #Z2 is distributed as chisq(p), p : degree of freedom if null is true
p.z2
#Using zsquared test of vector mean with unknown variance, it is found that the null hypothesis of
#mean vector equal to (70, 2.3) is rejected. Therefore, the claim about the average satisfaction and
#stress level is not true.

#7
library(corrplot)
corrplot(cor(satis), type = "upper")
cor(satis)
#The correlation matrix and plot above shows that each pair of variable in the data has significant
#correlation. Thus the claim about each variable in the data having strong association is evidently true.

#8
n = dim(satis)[1]
mu.sam = apply(satis, 2, mean)
mu = c(70, 42, 43, 2.3)
cov_m = cov(satis)
z.2 = n*t(mu.sam-mu)%*%solve(cov_m)%*%(mu.sam-mu)
p.z2 = pchisq(z.2, 3, lower.tail = F) #Z2 is distributed as chisq(p), p : degree of freedom if null is true
p.z2
#Using zsquared test of vector mean with unknown variance, it is found that the null hypothesis of
#mean vector equal to (70, 42, 43, 2.3) is rejected. Therefore, the claim about the sample came from
#an ideal conditioned population is evidently false.
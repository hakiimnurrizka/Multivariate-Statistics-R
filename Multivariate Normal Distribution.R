###multivariate normal distribution###
library(mvtnorm)
library(MVN)
library(JWileymisc)
library(MASS)
library(dplyr)
library(ellipse)
library(rgl)

##To give an introduction, lets do a quick review on normal univariate distrivution
#Suppose we have a normally distributed random variable X with some mean mu and standard deviance sigma
mu = 0 #Set mu = 0
sigma = 1 #Set sigma = 1
rnorm(1, mean = mu, sigma) #randomize a number x1 (realization of variable X)
rnorm(1, mean = mu, sigma) #If we do another run on the same code, the value of x1 will change
#Thus in R, to make sure we get the same output on a randomization process, we are setting a "pseudo" random number
#using function set.seed()
set.seed(11)
rnorm(1, mean = mu, sigma)
#Suppose we want to get more than 1 realization number, we can either set the number of sampling in the function
#of randomization or define an independent sampling method.
#Lets say, we want to get a sample with size of 1000
set.seed(11)
x_samp1 = rnorm(1000, mean = mu, sigma)#By setting the number of extracted realization numbers to n = 1000

x_samp2 = rep(NA, 1000)#We set a "void" vector to be used as the independent sampling vector
n = 1000 #Set number of observations
set.seed(11)
for(i in 1:n){
  x_samp2[i] = rnorm(1, mean = mu, sigma)#Independently sampling each realization value of X_i
}
x_samp1 == x_samp2#compare both sampling methods
hist(x_samp1)
hist(x_samp2)
##Testing univariate normality
#With the null hypothesis of the data comes from normal distribution, each of this test can be used to the hypothesis
shapiro.test(x_samp1)#Shapiro-Wilk, decide to not reject null
library(nortest)
ad.test(x_samp1)#Anderson-Darling, decide to not reject null
lillie.test(x_samp1)#Lilliefors/Kolmogorov-Smirnov, decide not to reject null
cvm.test(x_samp1)#Cramer von Mises, decide not to reject null


##Simulating mutivariate normal (MN) distribution based on correlation/covariance matrix, means, and standard deviation
set.seed(100) #setting seed for pseudo-random initiation
#in a case where we have the covariance matrix, it is then become a straightforward method to simulate
#MN distribution. Meanwhile in case we have correlation matrix, we use the previous equation to transform
#correlation matrix into covariance matrix.
#library jwileymisc provide the function to simplify this transformation
v = matrix(c(1,.152,.096,.043,.109,
             .152,1,.400,-.016,.297,
             .096,.400,1,.092,.382,
             .043,-.016,.092,1,.103,
             .109,.297,.382,.103,1),5,5) #correlation matrix
sigma = c(.4421,1.0880,8.5073,.4700,1.1249) #vector of standard deviation
cov.matrix = cor2cov(v, sigma) #convert correlation into covariance matrix
#after getting the covariance matrix, we can use mvrnorm to simulate the MN distribution based on
#previous covariance matrix
#in addition we also have to provide the means vector
mu = c(.7337,2.7300,46.9970,2.6002,1.7491)
sim.mn.data = data.frame(mvrnorm(n = 5000, Sigma = cov.matrix, mu = mu)) #simulated data with total 5000 observations
str(sim.mn.data)
sim.mn.data = rename(sim.mn.data, low = X1, high = X2, throw = X3, dodge = X4, make = X5)

##visualizing the multivariate normal data
#after generating MN data, we can visualize the data
#Visualizing the simulated data can be ilustratedusing the following 1 dimension subset of previously generated matrix data.
hist(sim.mn.data$low)#var 1
hist(sim.mn.data$high)#var 2
hist(sim.mn.data$throw)#var 3
hist(sim.mn.data$dodge)#var 4
hist(sim.mn.data$make)#var 5

#since we are limited in 3dimensional representation, we are limited to only use until first 3 variables 
#of the simulated data from before.
#it is guaranteed that marginal distributions from MN is always normal(see Methods_of_Multivariate_Analysis-_3rd_Edition Rencher & Christensen )
#2d contour
sim.mn.kernel = kde2d(sim.mn.data[,1], sim.mn.data[,3], n = 150) #calculate kernel density estimate
image(sim.mn.kernel) #heatmap based on the kernel estimate
contour(sim.mn.kernel, add = T) #contour plot added above the heatmap
#next is the "clean" version of bivariate normal distribution which using ellipse function
#in addition, confidence intervals is also added
rho = cor(sim.mn.data[,1:2])
y_on_x = lm(high ~ low, sim.mn.data)    # Regression Y ~ X
x_on_y = lm(low ~ high, sim.mn.data)    # Regression X ~ Y
plot_legend = c("99% CI green", "95% CI red","90% CI blue",
                 "Y on X black", "X on Y brown")
plot(sim.mn.data[,1:2], xlab = "low", ylab = "high",
     col = "dark blue",
     main = "Bivariate Normal with Confidence Intervals")
lines(ellipse(rho), col="red")       # ellipse() from ellipse package
lines(ellipse(rho, level = .99), col="green")
lines(ellipse(rho, level = .90), col="blue")
abline(y_on_x)
abline(x_on_y, col="brown")
legend(3,1,legend=plot_legend,cex = .5, bty = "n")
#3d representation
persp(sim.mn.kernel, phi = 45, theta = 30, shade = .1, border = NA) # from base graphics package
#using interactive plot from RGL library
color2 = heat.colors(length(sim.mn.kernel$z))[rank(sim.mn.kernel$z)]
persp3d(x=sim.mn.kernel, col = color2)


##Multivariate normality test
#there are several methods in which each of them has unique characteristics and may be better off used
#on certain type of data.
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

#we can also create perspective and contour plot using the mvn function 
mvn(iris[,2:3], mvnTest = "hz", multivariatePlot = "persp")
mvn(iris[,3:4], mvnTest = "mardia", multivariatePlot = "contour")
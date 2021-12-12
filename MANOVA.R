library(RVAideMemoire)
install.packages("biotools")
library(biotools)

data("iris")
attach(iris)

###manova###
###one way
##test statistics
#test with manova, when the null hypothesis is accepted, proceed with anova
man.iris = manova(cbind(Sepal.Width, Sepal.Length, Petal.Length) ~ Species)
summary(man.iris)

lm1.iris = lm(Sepal.Width ~ Species)
anova1 = anova(lm1.iris)
anova1

lm2.iris = lm(Sepal.Length ~ Species)
anova2 = anova(lm2.iris)
anova2

lm3.iris = lm(Petal.Length ~ Species)
anova3 = anova(lm3.iris)
anova3

##assumption
# H0: normality
library(MVN)
mshapiro.test(man.iris$residuals)
mvn(man.iris$residuals)

# H0 : homogeneity of variance
la <- data.frame(man.iris$residuals)
boxM(man.iris$residuals, iris[,5]) 
boxM(man.iris$residuals, Species)
Species

#for independence, need information about the sampling method
summary(man.iris) # default Pillai
summary(man.iris, test = 'Wilks')
summary(man.iris, test = 'Hotelling-Lawley')
summary(man.iris, test = 'Roy')
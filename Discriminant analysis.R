### Discriminant analysis ###

#Installing a package outside cran
options(repos = c(
  fawda123 = 'https://fawda123.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Install ggord
install.packages('ggord')

library(klaR)
library(psych)
library(MASS)
library(ggord)
library(devtools)
library(dplyr)
library(ggplot2)

# For the sake of simplicity as to get the points easier to be understood, 
#we'll start to illustrate the methods from the linear discriminant.
# We use iris data
data("iris")
iris = iris[1:100,-c(2,4)]
iris$Species = as.factor(iris$Species)
iris$spec = c(rep(1, 50), rep(2,50))
iris %>% ggplot(aes(Sepal.Length, Petal.Length), color = Species) + geom_point(alpha = 1.8, size = 2.5, col = iris$spec) + 
  scale_color_manual(values = c('black', 'red', 'green')) + labs(x='Feature_1', y="Feature_2")

#Try projecting the data into each feature
iris %>% ggplot(aes(Sepal.Length, 0), color = Species) + geom_point(alpha = 1.8, size = 2.5, col = iris$spec) + 
  scale_color_manual(values = c('black', 'red', 'green')) + labs(x='Feature_1', y="Feature_2")
iris %>% ggplot(aes(0, Petal.Length), color = Species) + geom_point(alpha = 1.8, size = 2.5, col = iris$spec) + 
  scale_color_manual(values = c('black', 'red', 'green')) + labs(x='Feature_1', y="Feature_2")
# Based on above plot, if we only use the previous 2 variables on iris, we can have complete separation between 2 species
#by only considering petal.length.
#Of course in general we may not want to just disregard one or more feature/variable
#Thats where discriminant analysis comes into play.
#Discriminant analysis use all information on the data, and try to find a transformation such as the
#resulting transformation will yield the best separation of the data.
#To find the optimal solution, we want to build discriminant function which has greatest difference of
#mean between categories and lowest total variation within each category

# Linear DA
lda_iris = lda(Species ~., data = iris[,-4] )
lda_iris$scaling #Parameter in the discriminant function
iris$pred = predict(lda_iris)$x #extract the transformed data

# LDA plot for the 2 categories
iris %>% ggplot(aes(x = pred, y = 0)) + geom_point(alpha = 1.8, size = 2.5, col = iris$spec) + 
  scale_color_manual(values = c('black', 'red', 'green')) + labs(x='Linear_Disc1', y=" ")
plot(lda_iris)
partimat(Species ~. , data = iris[,1:3], method = "lda")

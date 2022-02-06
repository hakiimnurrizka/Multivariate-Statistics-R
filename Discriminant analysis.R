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

## Linear DA
lda_iris = lda(Species ~., data = iris[,-4] )
lda_iris$scaling #Parameter in the discriminant function
iris$pred = predict(lda_iris)$x #extract the transformed data

# LDA plot for the 2 categories
iris %>% ggplot(aes(x = pred, y = 0)) + geom_point(alpha = 1.8, size = 2.5, col = iris$spec) + 
  scale_color_manual(values = c('black', 'red', 'green')) + labs(x='Linear_Disc1', y=" ")
plot(lda_iris)
partimat(Species ~. , data = iris[,1:3], method = "lda")

#Lets try classify 3 species on the iris data
data("iris")
iris = iris[,-c(2,4)]
iris$spec = c(rep(1, 50), rep(2,50), rep(3,50))
iris %>% ggplot(aes(Sepal.Length, Petal.Length), color = Species) + geom_point(alpha = 1.8, size = 2.5, col = iris$spec) + 
  scale_color_manual(values = c('black', 'red', 'green')) + labs(x='Feature_1', y="Feature_2")

lda_iris2 = lda(Species ~., data = iris[,-4] )
lda_iris2$scaling 
iris = as.data.frame(cbind(iris, predict(lda_iris2)$x)) 
plot(lda_iris2)
pred = predict(lda_iris2, iris[,1:2])
mean(pred$class == iris$Species)
ldahist(data = pred$x[,1], g = iris$spec)
iris %>% ggplot(aes(LD1, LD2), color = Species) + geom_point(alpha = 1.8, size = 2.5, col = iris$spec) + 
  scale_color_manual(values = c('black', 'red', 'green')) + labs(x='Linear_Disc1', y="Linear_Disc2")
partimat(Species ~. , data = iris[,1:3], method = "lda")
ggord(lda_iris2, iris$Species)

# LDA assumptions
library(MVN)
library(biotools)
#Normality
mvn(iris[,1:2]) #non-multivariate and univariate normal
#Homogeneity
boxM(iris[,1:2], iris$Species) #non homogeneous variance
#Ideally we want to avoid these kind of violation by simply using other models (e.g : non linear discriminant)
#In practice, unless we want to make sure our result is valid and to avoid instability in prediction, 
#we can simply ignore such violations.
#Next is several examples on non linear DA, solutions for the previous and some other problems.

## Non linear DA for 3 Categories
# Quadratic DA
#More flexible in variance assumption. Tends to work better than LDA when dataset is bigger (more observation)
#QDA is recommended if the training set is very large, so that the variance of the classifier is not a major issue, 
#or if the assumption of a common covariance matrix for the K classes is clearly untenable (James et al. 2014).
qda_iris = qda(Species~., data = iris[,-c(4:6)])
qda_iris$scaling
# Accuracy and predictions
pred = predict(qda_iris, iris[,1:2])
mean(pred$class == iris$Species)

library(mda)
# Mixture DA
#Each class is assumed to be a Gaussian mixture of sub-classes, where each data point 
#has a probability of belonging to each class
mda_iris = mda(Species~., data = iris[,-c(4:6)])
# Accuracy and predictions
mda_iris$confusion
pred = predict(mda_iris, iris[,1:2])
mean(pred == iris$Species)

# Flexible DA
#Builds the discriminant function by combining non linear predictors such as splines.
#Good to model non-normality or non-linearity relationships among variables within each category
fda_iris = fda(Species~., data = iris[,-c(4:6)])
# Accurarcy and predictions
pred = predict(fda_iris, iris[,1:2])
mean(pred == iris$Species)

library(klaR)
# Regularized DA
#Classification rule by regularizing the group covariance matrices (Friedman 1989).
#Tackling multicollinearity problem.
#Can be seen as a trade-off between LDA and QDA, as in shrinking the separate covariance in QDA to incline more
#towards common variance as in LDA.
rda_iris = rda(Species~., data = iris[,-c(4:6)])
# Accuracy and predictions
pred = predict(rda_iris, iris[,1:2])
mean(pred$class == iris$Species)

## Application to client data
# Predicting whether a client will skip at least one monthly payment within 6 months post-observation
# Train and test data
client_train = read_excel("~/Github/Machine-Learning/client-data.xlsx", 
                          sheet = "client_train")
client_train1 = client_train[,-c(1,2:12)]   #we are using predictors in this sub data
client_test = read_excel("~/Github/Machine-Learning/client-data.xlsx", 
                         sheet = "client_train")

#Separate train data for accuracy test
library(caret)
set.seed(12345)
examples = client_train1$default.payment.next.month %>% 
  createDataPartition(p = 0.25, list = FALSE)
client_train1.1  = client_train1[examples,]
client_train1.2 = client_train1[-examples,]

# Fit model and accuracy
#LDA
lda_client = lda(default.payment.next.month~., data = client_train1.1)
pred_client = predict(lda_client, client_train1.2[,1:12])
lda_acc = mean(pred_client$class == client_train1.2$default.payment.next.month)
#QDA
qda_client = qda(default.payment.next.month~., data = client_train1.1)
pred2_client = predict(qda_client, client_train1.2[,1:12])
qda_acc = mean(pred2_client$class == client_train1.2$default.payment.next.month)
#MDA
mda_client = mda(default.payment.next.month~., data = client_train1.1)
pred3_client = predict(mda_client, client_train1.2)
mda_acc = mean(pred3_client == client_train1.2$default.payment.next.month)
#FDA
fda_client = fda(default.payment.next.month~., data = client_train1.1, method = mars, keep.fitted = TRUE)
pred4_client = predict(fda_client, client_train1.2)
fda_acc = mean(pred4_client == client_train1.2$default.payment.next.month)
#RDA
rda_client = rda(default.payment.next.month~., data = client_train1.1)
pred5_client = predict(rda_client, client_train1.2)
rda_acc = mean(pred5_client$class == client_train1.2$default.payment.next.month)

# Assumptions
mvn(client_train1.1[,-13]) #Not normal in any way
boxM(client_train1.1[,-13], client_train1.1$default.payment.next.month) #Non homogeneous

# Summary for accuracy
DA_accuracy = as.data.frame(cbind(lda_acc, qda_acc, mda_acc, fda_acc, rda_acc))
colnames(DA_accuracy) = c("LDA", "QDA", "MDA", "FDA", "RDA")
row.names(DA_accuracy) = c("Accuracy")

#From the accuracy and assumptions test, FDA and RDA seems to be the better models compared to the rest
fda_client$confusion
sensitivity(fda_client$confusion)
specificity(fda_client$confusion)
client_train1.1$fitted_fda = fda_client$fit$fitted.values
client_train1.1 %>% ggplot(aes(x = fitted_fda, y = 0), color = default.payment.next.month+1) + 
  geom_point(alpha = 1.8, size = 2.5, col = client_train1.1$default.payment.next.month+1) + 
  scale_color_manual(values = c('black', 'red', 'green')) + labs(x='Flex_Disc1', y=" ")
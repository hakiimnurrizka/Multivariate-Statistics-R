###Characterizing and displaying multivariate data###
library(car)
library(RColorBrewer)
library(stats)

##data iris from r datasets
data("iris")
iris = as.matrix(iris[, 1:4])
iris
##Covariance and correlation on multivariate variable
#for starter, it can be helpful to look on the scatterplot of the data
scatterplotMatrix(iris)
#from the scatterplot, subjectively, there are pairs of variables which shown a trend of covary
#covary on multivariate data can be described as : tendency to create pattern relative to each variables mean
#for instance : petal length and petal width which create a similar data point spread around each of their means
plot(iris[,3], iris[,4])

#to help on looking on this pattern, profile plot can be used
#profile plot
Profile_Plot = function(list,name){
  require(RColorBrewer)
  # finding how many variables to include
  numvar = length(list)
  # choose 'numvar' random colours
  colours = brewer.pal(numvar,"Set1")
  # finding the minimum and maximum values of the variables:
  mymin = 1e+20
  mymax = 1e-20
  for (i in 1:numvar)
  {
    vectori <- list[[i]]
    mini = min(vectori)
    maxi = max(vectori)
    if (mini < mymin) { mymin = mini }
    if (maxi > mymax) { mymax = maxi }
  }
  # plot the variables
  for (i in 1:numvar)
  {
    vectori <- list[[i]]
    namei <- name[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}

#the above function use list as the input
iris_list = list(iris[,1],iris[,2],iris[,3],iris[,4])
Profile_Plot(iris_list, names(iris[1,]))

##mean vector, covariance matrix, and correlation matrix
#to summarise the multivariate data, mean and standard deviation can be used
#mean for each column/variable 
n = dim(iris)[1]
j = as.matrix(rep(1, dim(iris)[1]))
mean_iris = 1/n*t(iris)%*%j #  calculate means of all columns
mean_iris
#using apply to use simplified function (in this case mean())
apply(iris, 2, mean)
#covariance matrix
mean_matrix = matrix(data = 1, nrow = n)%*%cbind(mean_iris[[1]], mean_iris[[2]], mean_iris[[3]], mean_iris[[4]])
head(mean_matrix)
cov = 1/(n-1)*t(iris-mean_matrix)%*%(iris-mean_matrix)
cov
cov(iris)#simplified using function from stats library
#correlation matrix
d = diag(diag(cov)^(-1/2))
cor = d%*%cov%*%d
cor
cor(iris)#simplified using function from stats library

##apply to food texture data
food_text = read.csv('food-texture.csv')
food_text = food_text[,-1]
#scatterplot food texture data
scatterplotMatrix(food_text)
plot(food_text[,1], food_text[,2])
food_text_list = list(food_text[,1], food_text[,2], food_text[,3], food_text[,4], food_text[,5])
Profile_Plot(food_text_list, names(food_text))#density has high value's gap than the rest
food_text_list.2 = list(food_text[,1], food_text[,3], food_text[,4], food_text[,5])
Profile_Plot(food_text_list.2, names(food_text[,-2]))#hardness has high value's gap than the rest
food_text_list.3 = list(food_text[,1], food_text[,3], food_text[,4])
Profile_Plot(food_text_list.3, names(food_text[,-c(2,5)]))
#Mean and sd
sapply(food_text, mean)
sapply(food_text, sd)
#covariance and correlation matrix
cov(food_text)
cor(food_text)

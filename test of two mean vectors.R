###Multivariate test on two mean vectors
##analogous on testing mean of two sample on univariate case
library(tibble)
library(readr)
library(plotly)
library(DT)
library(mvnormtest)
library(dplyr)
library(ICSNP)
library(Matrix)
library(Hotelling)
        
#Data
data("iris")
iris = iris[1:100,]
View(iris)

#Plot data each variable
box_sepal.l = plot_ly(iris,
              type = "box") %>% 
  add_boxplot(y = ~Sepal.Length,
              x = ~Species,
              name = "Sepal_l") %>% 
  layout(title = "Sepal Length",
         xaxis = list(title = "Species"),
         yaxis = list(title = "Sepal Length"))
box_sepal.l
box_sepal.w = plot_ly(iris,
                      type = "box") %>% 
  add_boxplot(y = ~Sepal.Width,
              x = ~Species,
              name = "Sepal_w") %>% 
  layout(title = "Sepal Width",
         xaxis = list(title = "Species"),
         yaxis = list(title = "Sepal Width"))
box_sepal.w
box_petal.l = plot_ly(iris,
                      type = "box") %>% 
  add_boxplot(y = ~Petal.Length,
              x = ~Species,
              name = "Petal_l") %>% 
  layout(title = "Petal Length",
         xaxis = list(title = "Species"),
         yaxis = list(title = "Petal Length"))
box_petal.l
box_petal.w = plot_ly(iris,
                      type = "box") %>% 
  add_boxplot(y = ~Petal.Width,
              x = ~Species,
              name = "Petal_w") %>% 
  layout(title = "Petal Width",
         xaxis = list(title = "Species"),
         yaxis = list(title = "Petal Width"))
box_petal.w

#Mean for each species
kekw = iris %>% group_by(Species)%>%
  summarise(mean_sepal_l = mean(Sepal.Length),
            mean_sepal_w = mean(Sepal.Width),
            mean_petal_l = mean(Petal.Length),
            mean_petal_w = mean(Petal.Width))
kekw


#Assumption
mshapiro.test(t(iris[,1:4]))
det(cov(iris[,1:4]))


#Statistic test
iris_set = filter(iris, Species == "setosa")[,1:4]
iris_vers = filter(iris, Species == "versicolor")[,1:4]
y1bar = apply(iris_set,2,mean)
y2bar = apply(iris_vers,2,mean)
n1 = dim(iris_set)[1]
n2 = dim(iris_vers)[1]
w1 = (n1-1)*cov(iris_set)
w2 = (n2-1)*cov(iris_vers)

spl = (1/(n1+n2-2))*(w1+w2)
t2 = (n1*n2/(n1+n2))*t(y1bar-y2bar)%*%solve(spl)%*%(y1bar-y2bar)
p = dim(iris_set)[2]
f = (n1+n2-p-1)/((n1+n2-2)*p)*t2
f

HotellingsT2(iris_set, iris_vers)
##hotellingsT2 statistics is already converting into F statistics



###Test for additional information
##say we want to test whether certain(s) variable(s) significantly contribute information
##on the separation of the other variable(s).
##in a sense, similar to significance test of full vs reduced model on regression.
##for the previous iris data, lets test whether petal width contribute significant information 
##on the separation of other variables
iris_p = iris[,-4]

iris_setp = filter(iris_p, Species == "setosa")[,1:3]
iris_versp = filter(iris_p, Species == "versicolor")[,1:3]
y1barp = apply(iris_setp,2,mean)
y2barp = apply(iris_versp,2,mean)
w1p = (n1-1)*cov(iris_setp)
w2p = (n2-1)*cov(iris_versp)

splp = (1/(n1+n2-2))*(w1p+w2p)
t2p = (n1*n2/(n1+n2))*t(y1barp-y2barp)%*%solve(splp)%*%(y1barp-y2barp)

#Statistic test
pp = dim(iris_setp)[2]
qq = 1
t_pq = (n1+n2-2-pp)*(t2-t2p)/(n1+n2-2+t2p)
t_pq

f_pq = (n1+n2-2-pp-qq+1)*(t2-t2p)/((n1+n2-2+t2p)*qq)
f_pq

##when q = 1, f and t2 will be the same for the additional information statistic test.
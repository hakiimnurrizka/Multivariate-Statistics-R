### Clustering ###
library(factoextra)
library(cluster)
library(ggplot2)
library(dplyr)

## Lets use dummy data to illustrate how it works first
set.seed(1010)
my_dummy = as.data.frame(cbind(c(rnorm(10, 5, 2)), c(rbeta(10, 2,4))))
names(my_dummy) = c("x", "y")
my_dummy %>% ggplot(aes(x,y)) + geom_point() + labs(x='X', y="Y") 

## Dissimilarity matrix
dist(my_dummy)

## Hierarchical clustering
hier = hclust(dist(my_dummy), method = "complete")
plot(hier)

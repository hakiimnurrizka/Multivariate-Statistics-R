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
# Decide by dendogram we have 3 clusters
clusterCut = cutree(hier, 3)
my_dummy$cluster = clusterCut
my_dummy %>% ggplot(aes(x,y), color = cluster) + geom_point(alpha = 1.8, size = 2.5 ,col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green')) + labs(x='X', y="Y")

## K Means clustering
#Lets cluster the dummy into 3 clusters, and choose the initiation for the centers
cent = as.data.frame(cbind(c(5,8.75, 1), c(0.2, 0.1, 0.4)))
names(cent) = c("x", "y")
cent$cluster = c(1, 2, 3)
cent %>% ggplot(aes(x,y)) + 
  geom_point(alpha = 1, size = 2, colour = c(2,3,4), shape = 2, stroke = 2) + 
  scale_color_manual(values = c('red', 'green', 'blue')) + labs(x='X', y="Y") 
#Plot the data (circles) and the initial centers (triangles)
my_dummy = my_dummy[,1:2]
my_dummy$cluster = c(1,1,1,1,1,1,1,1,1,1)
km_dummy = as.data.frame(rbind(cent, my_dummy))
km_dummy %>% ggplot(aes(x,y)) + 
  geom_point(alpha = 1, size = 2, shape = c(2,2,2,1,1,1,1,1,1,1,1,1,1), colour = c(2,3,4,1,1,1,1,1,1,1,1,1,1), stroke = c(2,2,2,1,1,1,1,1,1,1,1,1,1)) + 
  scale_color_manual(values = c('black', 'red', 'green', 'blue')) + labs(x='X', y="Y")
dist(km_dummy)
#Place the each observation to their respective nearest cluster
km_cl1 = c(1,2,3,1,1,2,1,1,2,2,3,1,1)
km_dummy$cluster1 = km_cl1
#Compute the new centers for each cluster
km_dummy[4:13,] %>% group_by(cluster1) %>%
  summarise(x = mean(as.numeric(x)),
            y = mean(y))
km_dummy[1:3,1] = c(4.41,8.1,0.278)
km_dummy[1:3,2] = c(0.267, 0.150, .381)
km_dummy %>% ggplot(aes(x,y)) + 
  geom_point(alpha = 1, size = 2, shape = c(2,2,2,1,1,1,1,1,1,1,1,1,1), colour = km_dummy$cluster1+1, stroke = c(1,1,1,1,1,1,1,1,1,1,1,1,1)) + 
  scale_color_manual(values = c('black', 'red', 'green', 'blue')) + labs(x='X', y="Y")
#Use the function kmeans
kmean_1st = kmeans(my_dummy, centers = cent, iter.max = 100)
kmean_1st$cluster #it is found that after 1st step the algorithm already reached convergence
#Optimal number of clusters based on several methods
my_dummy = my_dummy[,1:2]
fviz_nbclust(my_dummy, kmeans, method = "wss", k.max = 5)

## NOTE : The previous results may differs since we are doing illustration based on randomized dummy data


### Application to real data
#We use client data and cluster data based on the PCA result of 12 last predictors
library(readxl)
library(ggplot2)
library(caret)
library(ROCR)
library(dplyr)
client_train = read_excel("~/Github/Machine-Learning/client-data.xlsx", 
                          sheet = "client_train")
client_train1 = client_train[,-c(1,2:12)]

## PCA
pca_train1 = prcomp(client_train1[,-13], scale = TRUE) #use first 2 components
comp_client = as.data.frame(pca_train1$x[,1:2])
# Take 1200 from training for example and learn the cluster pattern
set.seed(12345)
examples = comp_client$PC1 %>% 
  createDataPartition(p = 0.05, list = FALSE)
exm_comp  = comp_client[examples,]

## Clustering
# Use Hierarchical to see how the good is 2 cluster in the data
hier_client = hclust(dist(exm_comp), method = "complete")
plot(hier_client)
# Result from the example doesnt show a good indication that the data can be clustered into 2 clusters
# Lets apply this k means and try to look for optimal k
# Using wss on k means to find optimal k number from example data
fviz_nbclust(exm_comp, kmeans, method = "wss", k.max = 6) #2 seems to be a reasonable number of clusters
# Lets try different quality comparison method
fviz_nbclust(exm_comp, kmeans, method = "silhouette", k.max = 6)
gap_stat = clusGap(exm_comp, FUN = kmeans, nstart = 25,
                    K.max = 6, B = 50)
fviz_gap_stat(gap_stat)
# We conclude that from the example 2 clusters is the optimal number of cluster
# Next we apply kmeans on the complete data. We choose to set 100 random starting points and 1000 max iterations.
km_client = kmeans(comp_client, iter.max = 1000, centers = 2, nstart = 100)
fviz_cluster(km_client, comp_client, stand = F, geom = "point")

## Comparison
# Extract the result and compare to actual label
# Since we dont know which cluster resembles which label, we'll do it interchangeably
result_kmcluster = as.data.frame(cbind(client_train$default.payment.next.month, km_client$cluster%%2))
result_kmcluster2 = as.data.frame(cbind(client_train$default.payment.next.month, km_client$cluster-1))
names(result_kmcluster) = c("actual label", "cluster")
names(result_kmcluster2) = c("actual label", "cluster")
# We'll use specificity and sensitivity to measure our accuracy of clustering prediction
specificity(table(result_kmcluster))
sensitivity(table(result_kmcluster))
specificity(table(result_kmcluster2))
sensitivity(table(result_kmcluster2))
table(result_kmcluster2)
# The 2nd scheme seems like a better approach. It means that cluster 1 is people with less tendency to skip payment
# As a side note, prediction using clustering based on PCA components doesnt seems to be better model than using logistics regression on the PCA components
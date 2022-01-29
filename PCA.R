library(factoextra)
library(readxl)
library(ggplot2)
library(performance)
library(caret)
library(ROCR)
library(dplyr)
client_train = read_excel("~/Github/Machine-Learning/client-data.xlsx", 
                           sheet = "client_train")

client_train1 = client_train[,-c(1:11)]

### PCA ###
pca_train1 = prcomp(client_train1[,-13], scale = TRUE)
View(head(pca_train1$x,7))
fviz_eig(pca_train1)
summary(pca_train1)

fviz_pca_ind(pca_train1, col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = T)
fviz_pca_var(pca_train1, col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

## Apply the previous transformation on test data
client_test = read_excel("~/Github/Machine-Learning/client-data.xlsx", 
                          sheet = "client_train")
client_test.PC = predict(pca_train1, newdata = client_test[,13:24])
client_test1 = as.data.frame(cbind(client_test[,-c(1,13:24)],client_test.PC[,1:2]))

### Logistic Regression for Predictive Model ###
## Building the logistic regression for predictive study
client_train_new = as.data.frame(cbind(client_train[,-c(6:23)],pca_train1$x[,1:2]))
client_train_new[,2] = as.factor(client_train_new[,2])
client_train_new[,3] = as.factor(client_train_new[,3])
client_train_new[,4] = as.factor(client_train_new[,4])
client_train_new[,6] = as.factor(client_train_new[,6])

client_log = glm(default.payment.next.month~0+LIMIT_BAL+EDUCATION+MARRIAGE+PC1+PC2, data = client_train_new, family = binomial)  
summary(client_log)

# Get the prediction for training data
predict_client_stat = predict(client_log, newdata = client_train_new[,c(1,3,4,7,8)], type = "response")
head(predict_client_stat, 100)

# Using specificity and sensitivity to decide the response threshold
predictions = prediction(predict_client_stat, client_train_new$default.payment.next.month) 
sens = data.frame(x=unlist(performance(predictions, "sens")@x.values), 
                   y=unlist(performance(predictions, "sens")@y.values))

spec = data.frame(x=unlist(performance(predictions, "spec")@x.values), 
                   y=unlist(performance(predictions, "spec")@y.values))

sens %>% ggplot(aes(x,y)) + 
  geom_line() + 
  geom_line(data=spec, aes(x,y,col="red")) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity")) +
  labs(x='Cutoff', y="Sensitivity") +
  theme(axis.title.y.right = element_text(colour = "red"), legend.position="none")

#get the optimum threshold
sens = cbind(unlist(performance(predictions, "sens")@x.values), unlist(performance(predictions, "sens")@y.values))
spec = cbind(unlist(performance(predictions, "spec")@x.values), unlist(performance(predictions, "spec")@y.values))
cutoff_opt_client = sens[which.min(apply(sens, 1, function(x) min(colSums(abs(t(spec) - x))))), 1]

#confusion matrix
predict_client_stat = ifelse(predict_client_stat>cutoff_opt_client,1,0)
conf_mat_client = table(predict_client_stat, client_train_new$default.payment.next.month)
conf_mat_client
specificity(conf_mat_client)
sensitivity(conf_mat_client)

#Apply model and cutoff on test data
predict_client_test = ifelse(predict(client_log, newdata = client_test1[,c(1,3,4,12,13)], type = "response")>cutoff_opt_client,1,0)
client_test1$predict.payment = predict_client_test
View(head(client_test1[,-c(2, 5:11)], 100))

library(RVAideMemoire)
install.packages("biotools")
library(biotools)
library(MVN)
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

###two way
#suppose we are to test whether the iris data is significantly affected by two independent
#variables that is the previous species and a new variable soil.
#first the model will include an interaction term to accommodate in case of both 
#independent variables affect the dependent variables simultanously 
iris$soil = rep(c("dry", "wet", "sand"), each = 5, times = 10)
attach(iris)
man.iris2 = manova(cbind(Sepal.Width, Sepal.Length, Petal.Length) ~ Species*soil)
summary(man.iris2)
#the conventional term-selection method dictate to eliminate the term that 
#isn't significant starting from the highest order. 
man.iris2 = manova(cbind(Sepal.Width, Sepal.Length, Petal.Length) ~ Species+soil)
summary(man.iris2)
#in the end it is found that soil isnt a significant separator.
#we proceed to univariate testing

lm1.iris2 = lm(Sepal.Width ~ Species*soil)
anova(lm1.iris2)
lm1.iris2 = lm(Sepal.Width ~ Species+soil)
anova(lm1.iris2)

lm2.iris2 = lm(Sepal.Length ~ Species*soil)
anova(lm2.iris2)
lm2.iris2 = lm(Sepal.Length ~ Species+soil)
anova(lm2.iris2)

lm3.iris2 = lm(Petal.Length ~ Species*soil)
anova(lm3.iris2)
lm3.iris2 = lm(Petal.Length ~ Species+soil)
anova(lm3.iris2)

#until the end of univariate test, it is found that soil isn't affecting the response variables
#next we'll apply the complete procedures when we are facing an already clean data
survtime = read.table("T6_22_SURVTIME.DAT")
colnames(survtime) = c("type", "gender", "age", "y1", "y2", "y3", "y4")
#from Rencher() the data is survival time from cancer patients (Cameron and Pauling, 1978)
#the data is taken from a repeated measure design with unbalanced groups
View(survtime)
#variables that will be used are y1-y4 and the factors are "type" of cancer and gender
survtime[,1] = as.factor(survtime[,1])
survtime[,2] = as.factor(survtime[,2])

library(dplyr)
boxplot(survtime[,4:7])
survtime %>% group_by(type)%>%
  summarize(mean_y1 = mean(y1),
            mean_y2 = mean(y2),
            mean_y3 = mean(y3),
            mean_y4 = mean(y4))
#from the table above, we can see some differences between each group on 4 variables
#the following lines will fit the data with factor "type" into 2 way MANOVA
man_surv = manova(cbind(y1,y2,y3,y4) ~ type*gender, data = survtime)
summary(man_surv)
#as it can be seen, the difference from at least 2 type of cancer is significant
#meanwhile for the other terms, it doesnt seems to be significant
man_surv2 = manova(cbind(y1,y2,y3,y4) ~ type+gender, data = survtime)
summary(man_surv2)
#as such, we proceed with the acceptance that only type of cancer as the sole significant
#factor
man_surv3 = manova(cbind(y1,y2,y3,y4) ~ type, data = survtime)
summary(man_surv3)
summary(man_surv3, test = 'Wilks')
summary(man_surv3, test = 'Hotelling-Lawley')
summary(man_surv3, test = 'Roy')
#then analyzing the assumptions
library(biotools)
library(MVN)
library(mvnormtest)
# H0: normality
mshapiro.test(man_surv3$residuals)
mvn(man.iris$residuals)
#normality is accepted
# H0 : homogeneity of variance
surv_la = data.frame(man_surv3$residuals)
boxM(surv_la, survtime$type) 
#homogeneity is not accepted and since we are dealing with unbalanced design
#we can either : 
#(1) only making inference for the data (generalization is not allowed) OR
#(2) delete some observation such that it would result in a balanced design OR
#(3) use a repeated measure MANOVA (also known as doubly MANOVA)

#for the (1) solution, we can say "based on the patient data there is significant difference
#between the type of cancer in observation" in some application it can be used to some extent
#but as the theoretical basis wasnt sufficient to generalize the inferences, thus
#it is safer to not use such result in making any decision/action solely based on that

#for the (2) we need to make each group has equal number of observation, thus following
#the least number of observation in total we need to delete 33 observations
#of course it can be done by doing the outlier detection method or some other methods
#but all in all this solution is NOT RECOMMENDED since not only we are losing
#a lot of information from the data we are not even guaranteed to get a valid result
#since setting the data such that it would accept the H0 is not an easy task
#not to mention the independence of data is not fulfilled by doing so

#the (3) is the safest option in this case since we can still keep all the data
#and approach in a semi parametric way which requires less strict assumptions

##MANOVA RM
library(MANOVA.RM)
man_surv4 = MANOVA.wide(cbind(y1,y2,y3,y4) ~ type, data = survtime)
summary(man_surv4)
#from the result above, it can be seen using modified MANOVA that the type of cancer
#has significant difference for at least 2 groups

#next, we proceed with ANOVA to check the difference for both type and gender groups
#in each variable
#since we were dealing it in a repeated measure-modified model, we'll also proceed in a
#similar manner for ANOVA
library(rstatix)
library(ggpubr)
#we'll start from analyzing data in univariate then computing the test statistics and 
#lastly checking for assumption violation

#y1
ggboxplot(survtime, x = "type", y = "y1", add = "point")
ggboxplot(survtime, x = "gender", y = "y1", add = "point")
survtime %>%
  group_by(type, gender) %>%
  identify_outliers(y1)
#delete the extreme outlier(s)
survtime2 = survtime[-c(32),]

lmsurv_y1 = anova_test(y1 ~ type*gender, data = survtime2)
get_anova_table(lmsurv_y1) #interaction is not significant, ignore the term
lmsurv_y1 = anova_test(y1 ~ type+gender, data = survtime2)
get_anova_table(lmsurv_y1) #gender is not significant
lmsurv_y1 = anova_test(y1 ~ type, data = survtime2)
get_anova_table(lmsurv_y1) #final result finds that only the type of cancer has different value of y1

shapiro_test(lm(y1 ~ type, data = survtime2)$residuals)
#normality is violated, but inference can still be made since the data contains rather
#sufficiently large number of observations. for sphericity, it is automatically checked during 
#the computation of the ANOVA test using the R function anova_test() and the result
#has already been modified such that it consider the sphericity correction.
#for other variables it can be done in a similar manner

#y2
ggboxplot(survtime, x = "type", y = "y2", add = "point")
ggboxplot(survtime, x = "gender", y = "y2", add = "point")
survtime %>%
  group_by(type, gender) %>%
  identify_outliers(y2)
#delete the extreme outlier(s)
survtime2 = survtime[-c(27),]

lmsurv_y2 = anova_test(y2 ~ type*gender, data = survtime2)
get_anova_table(lmsurv_y2) #interaction is not significant, ignore the term
lmsurv_y2 = anova_test(y2 ~ type+gender, data = survtime2)
get_anova_table(lmsurv_y2) #gender is not significant
lmsurv_y2 = anova_test(y2 ~ type, data = survtime2)
get_anova_table(lmsurv_y2) #final result finds that only the type of cancer has different value of y1

shapiro_test(lm(y2 ~ type, data = survtime2)$residuals)

#y3
ggboxplot(survtime, x = "type", y = "y3", add = "point")
ggboxplot(survtime, x = "gender", y = "y3", add = "point")
survtime %>%
  group_by(type, gender) %>%
  identify_outliers(y3)
#delete the extreme outlier(s)
survtime2 = survtime[-c(32),]

lmsurv_y3 = anova_test(y3 ~ type*gender, data = survtime2)
get_anova_table(lmsurv_y3) #interaction is not significant, ignore the term
lmsurv_y3 = anova_test(y3 ~ type+gender, data = survtime2)
get_anova_table(lmsurv_y3) #gender is not significant
lmsurv_y3 = anova_test(y3 ~ type, data = survtime2)
get_anova_table(lmsurv_y3) #final result finds that there is no difference between any type of group in regard of y3 value

#y4
ggboxplot(survtime, x = "type", y = "y4", add = "point")
ggboxplot(survtime, x = "gender", y = "y4", add = "point")
survtime %>%
  group_by(type, gender) %>%
  identify_outliers(y4)
#there is no extreme outlier

lmsurv_y4 = anova_test(y4 ~ type*gender, data = survtime2)
get_anova_table(lmsurv_y4) #interaction is not significant, ignore the term
lmsurv_y4 = anova_test(y4 ~ type+gender, data = survtime2)
get_anova_table(lmsurv_y4) #gender is not significant
lmsurv_y4 = anova_test(y4 ~ type, data = survtime2)
get_anova_table(lmsurv_y4) #final result finds that there is no difference between any type of group in regard of y4 value

#the univariate case finds that difference between type of cancer group happens
#in regard of variable y1 and y2
#a post-hoc analysis can be done for these 2 significant variables
#for simplicity we'll show the computation in post-hoc analysis for only y1
#and obviously post-hoc analysis for y2 can be done in a similar manner
survtime %>%
  pairwise_t_test(y1 ~ type, p.adjust.method = "bonferroni")
#from the result above the pairwises : {(1,5), (2,5), (3,5), (4,5), (5,6)} are
#significantly different.
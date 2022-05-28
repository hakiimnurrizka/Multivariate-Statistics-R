library(psych)
library(MVN)
library(CCA)
library(CCP)
library(ggplot2)

### Canonical correlation ###
#Extension of multiple correlation in which we have two sets of variable and each set has more than 1
#variable.
#We start by reviewing multiple correlation

##Data used is glucose.dat from Rencher, 2012
rm(T3_9_GLUCOSE)
glucose = read.table("~/Multivariate-Statistics-R/T3_9_GLUCOSE.DAT", quote="\"", comment.char="")
#we consider it has 2 sets of variables with each set has 3 variables each
gluc_1 = glucose[,1:3] #1st set
gluc_2 = glucose[,4:6] #2nd set

##Multiple correlation
#We'll visualize the correlation within each set between each pair then for both sets at once
pairs.panels(gluc_1)
pairs.panels(gluc_2)
pairs.panels(glucose) #complete correlation matrix
#Correlation above used pearson correlation coefficient, which means it is only appropriate IF 
#the data has normal distribution.
#This assumption is also needed in canonical correlation, the difference is we need a multivariate
#normal assumption instead of univariate one.
#Let's check normality for each variables AND multivariate normality for each set
mvn(gluc_1) #multivariate normality and 2 individuals normality is not accepted
mvn(gluc_2) #multivariate normality and all individuals normality is accepted
#In an ideal situation (no normality violation) we can proceed with using previous correlation matrix 
#then extend it to canonical correlation. 
#In this case, we need to adjust the correlation coefficient into nonparametric ones (spearman, kendall).
#We'll discuss canonical correlation in this ideal situation first then approach it with nonparametric
#correlation.

##Canonical correlation
#First we'll use the function from default and CCA then we proceed with breaking down the formula.
cancor(gluc_1, gluc_2) #default function from stats library
can.gluc = cc(gluc_1, gluc_2)
can.gluc$cor #Canonical correlation, sorted from highest value
can.coef = data.frame(rho = can.gluc$cor, dim = c("rho1", "rho2", "rho3"))
ggplot(can.coef, aes(x = dim, y = rho)) + geom_bar(stat = "identity", width = 0.5, fill = "steel blue") + 
  labs(title = "Canonical correlation")
#From the result above, it can be seen that the highest canonical correlation is close to .5
#The next section, the correlations are tested for their significances
p.asym(can.gluc$cor, dim(gluc_1)[1], length(gluc_1), length(gluc_2),
       tstat = "Hotelling")#Inputs are : correlation coef, num of observations+variables
#The result above shows that only when canonical correlation is built upon 3 dimensions, the correlations
#are significance. This is equivalent with test for overall significance using 3 dimensions
#Now we discuss about the coefficient for canonical correlation
can.gluc$xcoef
can.gluc$ycoef
#Interpretation for above is similar to that of regression.
#So for this case, we have 2 sets of regression equations (which is actually the canonical variate) represented
#by table. For each set, we have 3 equations represented by collumns while row is our predictor (in this case 
#is the variables themselves). Thus, we can interpret each value just like regression (i.e : each unit 
#change of V1 will decrease first variate of set 1 canonical variate by 0.065)
#We can also used the standardized version of canonical coefficient to see the relative effect of each variable
#and removing the difference in size/scale between variables.
diag(sqrt(diag(cov(gluc_1))))%*%can.gluc$xcoef
diag(sqrt(diag(cov(gluc_2))))%*%can.gluc$ycoef
#Interpretation for the coefficient above is similar to previous one but instead of a unit, it is a standard 
#deviation change (i.e : each std change of V1 will decrease first variate of set 1 canonical variate by 
#0.63 of its std)
plt.cc(can.gluc, var.label = TRUE)
#The plots above show the closeness for each variable and individual

##Using matrix operation to get canonical correlation
#Next, lets break down the formula.
#First, we get the correlation matrix of the data
rxx = cor(gluc_1)
ryy = cor(gluc_2)
rxy = cor(gluc_1, gluc_2)
ryx = cor(gluc_2, gluc_1)
#Following formula from Rencher (2012), we are going to compute multiplication of inverse and the partition 
#matrix above. That is : Rxx^-1 * Rxy * Ryy^-1 * Ryx
rho.mat = solve(rxx)%*%rxy%*%solve(ryy)%*%ryx
#Eigen values of the resulting matrix is the squared canonical correlations that we are interested in
sq.rho = eigen(rho.mat)$values #squared canonical correlations
sqrt(sq.rho) #canonical correlations
#Testing overall significance can also be made based only upon correlation matrix
wilk.gl = det(cor(glucose))/(det(cor(gluc_1))*det(cor(gluc_2)))
wilk.gl
#Approach with f distribution
wf = dim(glucose)[1]-(dim(gluc_1)[2]+dim(gluc_2)[2]+3)/2
tf = sqrt((dim(gluc_1)[2]^2*dim(gluc_2)[2]^2-4)/(dim(gluc_1)[2]^2+dim(gluc_2)[2]^2-5))
df1 = dim(gluc_1)[2]*dim(gluc_2)[2]
df2 = wf*tf-df1/2+1 
ap.f = (1-wilk.gl^(1/tf))*df2/(wilk.gl^(1/tf)*df1)
pf(ap.f, df1, df2, lower.tail = FALSE) #pvalue for f distribution approach
#Lets compare with our previous function using tstat wilks
p.asym(can.gluc$cor, dim(gluc_1)[1], length(gluc_1), length(gluc_2),
       tstat = "Wilks")
#We can also use the rho values instead of correlation matrix
wilk.gl = prod(1-sq.rho)
pillai.gl = sum(sq.rho)
hotel.gl = sum(sq.rho/(1-sq.rho))
roy.gl = max(sq.rho)
p.asym(can.gluc$cor, dim(gluc_1)[1], length(gluc_1), length(gluc_2),
       tstat = "Pillai")
p.asym(can.gluc$cor, dim(gluc_1)[1], length(gluc_1), length(gluc_2),
       tstat = "Roy")
#For roy test, it is basically only testing significance with 1st dimension.
#Extension for testing succeeding dimensions can be made by modifying the formula based on eigen values
wilk.gl2 = prod(1-sq.rho[-1]) #testing dimension 2 to 3
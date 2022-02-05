### Factor analysis ###
library(psych)
library(corrplot)
library(ggplot2)
library(car)
library(nFactors)

##Prepare the data
cricket = read.csv("https://raw.githubusercontent.com/housecricket/data/main/efa/sample1.csv")
describe(cricket)
dim(cricket)
head(cricket)
cricket1 = cricket[,-1]

##Pre-analysis
#After cleaning the data and making sure its the right type of data to be used (numeric type)
#identify whether the factor analysis is the valid and right method to analyze the data with.
#This can be done by judging off the correlation matrix and factorability measure.

##Correlation matrix
matcrick = cor(cricket1[,-13])
corrplot(matcrick, method = "number")
cricket2 = cricket1[,-13]
##Kaiser-Meyer-Olkin measure of sampling adequacy 
#(to determine whether factor analysis will be useful or not)
#The threshold for the measure value from Kaiser(1974) is
#0.00 to 0.49 unacceptable.
#0.50 to 0.59 miserable.
#0.60 to 0.69 mediocre.
#0.70 to 0.79 middling.
#0.80 to 0.89 meritorious.
#0.90 to 1.00 marvelous.
KMO(cricket2)
cortest.bartlett(cricket2) #bartlett test has similar function as kmo
#Check the determinant of the correlation matrix of the data, 
#positive value indicate that factor analysis will run just fine
det(cor(cricket2))

#After doing the previous pre-analysis, we found that factor analysis is a relevant method
#to analyze the cricket data. Next we'll apply the factor analysis.

##Apply the factor analysis
#First of all, after we know that there is almost a certainty that hidden variables present in the data
#based on previous pre-analysis, we can start by tranforming the data into factors.
#Then based on the factors that we'll find, decide the number of factor(s) based on the eigen value.
#Recall that eigen value of the covariance matrix from the data represent the informations of that is
#contained in the data.

##Determine number of factors
fa_crick = fa(cricket1, nfactors = ncol(cricket2), rotate = "none")
n_fa = length(fa_crick$e.values)
scree_fa = data.frame(
  factor_n = as.factor(1:n_fa),
  eigenvalues = fa_crick$e.values
)
ggplot(scree_fa, aes(x = factor_n, y = eigenvalues, group = 1))+
  geom_point()+geom_line()+xlab("number of factors")+ylab("initial eigenvalue")+
  labs(title = "scree plot", subtitle = "(based on unreduced correlation matrix)")
fa.parallel(cricket2)

#To apply factor analysis in R, there are 2 function which are widely used : fa and factanal

##Factor analysis using fa
fa_cricket =  fa(r = cricket2, nfactors = 4, covar = FALSE, SMC = TRUE,
   fm = "pa", max.iter = 100, rotate = "varimax")

##Using factanal
factanal(cricket2, factors = 4, rotation ="varimax", scores = c("regression"))

##Diagram of the factors
fa.diagram(fa_cricket)

##Regression analysis for the factors
head(fa_cricket$scores)
reg.data = as.data.frame(cbind(cricket$QD, fa_cricket$scores))
names(reg.data) = c("qd", "fac1", "fac2", "fac3", "fac4")
set.seed(123)
indice = sample(1:nrow(reg.data),0.7*nrow(reg.data))
reg.train = reg.data[indice,]
reg.test = reg.data[-indice,]

reg_fac = lm(qd~., reg.train)
summary(reg_fac)
vif(reg_fac)

pred_fac = predict(reg_fac, newdata = reg.test, type = "response")
reg.test$"qd predicted" = pred_fac
reg.test[c("qd","qd predicted")]


###Source : https://towardsdatascience.com/exploratory-factor-analysis-in-r-e31b0015f224###

#Let's try applying factor analysis to a different dataset.

##use a different dataset (BFI dataset from psych library)
bfi = remove_missing(bfi)
head(bfi,10)
bfi1 = bfi[,-c(26:28)]
matbfi = cor(bfi1)
corrplot(matbfi, method = "number")
KMO(bfi1)
cortest.bartlett(bfi1)
det(cor(bfi1))

fa_bfi = fa(bfi1, nfactors = ncol(bfi1), rotate = "none")
nfa_bfi = length(fa_bfi$e.values)
scree_bfi = data.frame(
  factor_nbfi = as.factor(1:nfa_bfi),
  eigenval_bfi = fa_bfi$e.values
)
ggplot(scree_bfi, aes(x = factor_nbfi, y = eigenval_bfi, group = 1))+geom_point()+geom_line()+
  xlab("number of factors")+ylab("initial eigenvalue")+labs(title = "scree plot", subtitle = "(based on unreduced correlation matrix")
fa.parallel(bfi1)

fa_bfi1 = fa(bfi, nfactors = 6, rotate = "varimax", fm = "pa")
fa.diagram(fa_bfi1)
fa_bfi1
head(fa_bfi1$scores,10)
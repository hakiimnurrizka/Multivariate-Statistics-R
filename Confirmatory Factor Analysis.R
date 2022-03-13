### Confirmatory factor analysis ###
#A quick introduction on analyzing data using CFA using library LAVAAN
#Focus on this material is to use the CFA function and interpreting the results
#For more detailed explanation (in mathematical views) of CFA, writer suggest to read : methods
#of multivariate analysis 3rd edition by Rencher and Christensen

#In EFA we use the method to extract information such that we assume there exist latent variable(s) as we called
#it as factor. In practice, more often than not, when the interpretation of the factor is not straightforward-ly
#known, we try to rotate the factor loadings such that we can find an easier definition for the factors.
#Similar to EFA, in CFA we want to find underlying latent variable(s) which is normally hard or impossible to measure
#(e.g : general intelligence, person skepticism, students sentiment toward certain subject(s)).
#In general practice, factor analysis has been used widely in these types of study which is referred as
#item response theory. The biggest difference between EFA and CFA is the constraint. That is CFA introduce 
#constraint and incorporate it into the model to ensure a uniquely identifiable set of model parameters.

### CFA Using lavaan
library(lavaan)
library(haven)
library(corrplot)
#Load the data : SPSS anxiety questionnaires by Andy Field (see : https://users.sussex.ac.uk/~andyf/factor.pdf)
SAQ = read_sav("~/Github/Multivariate-Statistics-R/SAQ.sav")
SAQ1 =na.omit(SAQ[,1:8]) #we'll only use first 8 questions
corrplot(cor(SAQ1))

## One factor - 3 items
#To make sure we have a just-identified model, we use marker method, that is we set the parameter for first loading
#as fixed 1. This is by default applied to the cfa function.
fac1_3 = 'f =~ q03+q04+q05'
mfac1_3 = cfa(fac1_3, data = SAQ1)
summary(mfac1_3)
#we can set marker method to fixed other parameter by defining NA*parameter for the first parameter and add 1*parameter
#for setting the fixed parameter.
fac1_3 = 'f =~ NA*q03+q04+1*q05' #setting the q5 as fixed
mfac1_3 = cfa(fac1_3, data = SAQ1)
summary(mfac1_3)
#For better understanding of the factor, we can add standardize in the summary function
summary(mfac1_3, standardized = TRUE)
#We can see the standardized loading that explain correlation for each question to the factor from the 
#summary output on the latent variables, under column std.all.
#We know that q3 have negative correlation to the factor while the other 2 questions are positively correlated
#Implication with the question, we can say a person that tends to be excited of standard equation, also tends to
#be less anxious of to learn SPSS. Also people that don't understand statistics tend to be anxious about
#learning SPSS.

## One factor - 2 items
#To fit such model, we need variance standardization that is to set the variance of factor to one and equate
#both factor loadings.
fac1_2 = 'f1 =~ a*q04 + a*q05'
mfac1_2 = cfa(fac1_2, data = SAQ1, std.lv = TRUE)
summary(mfac1_2, standardized = TRUE)

## One factor - >3 items
fac1 = 'f =~ q01 + q02 + q03 + q04 + q05 + q06 + q07 + q08' 
mfac1 = cfa(fac1, data =SAQ1, std.lv = TRUE)
summary(mfac1, fit.measures = TRUE, standardized = TRUE)
#Adding fit.measures will show the model fit. This is a method to see how well the model fits the data.
#To measure the fit, we use the difference between model covariance and the observed covariance. Small value 
#of the test statistics implies a good fit. Therefore a good model should have small statistics value and
#not rejecting the H0. You can either use the chi-square or RMSEA, rejecting any of these H0 statistics means
#the model is not close fitting. For CFI/TLI a good fit model should have value above 0.9

## Two or more factors
#For case where we assume there are more than 1 factor, there are 3 conditions to consider : (1)uncorrelated
#factors, (2)correlated factors, and (3)hierarchy/order of factors.
#For the (1) case we simply assume that each factor is uncorrelated with each other. Lets say for previous
#anxiety data we have 2 uncorrelated factors, thus the model :
fac2 = 'f1 =~ q01 +  q03 + q04 + q05 + q08
        f2 =~ a*q06 + a*q07
        f1 ~~ 0*f2' #item and factor are decided from EFA
mfac2 = cfa(fac2, data = SAQ1, std.lv = TRUE)
summary(mfac2, fit.measures = TRUE, standardized = TRUE)
#From the result above, model fit measurements show that 2 factors model with uncorrelated factors poorly fits
#the data. It is also has higher test statistics of chi-square and RMSEA than the previous one factor model.
#We can then try 2 factors model with correlated factors.
#Case (2) we assume there is strong correlation between 2 factors behind the anxiety data. The model for 
#2 correlated factors is :
fac2_c = 'f1 =~ q01 +  q03 + q04 + q05 + q08
          f2 =~ q06 + q07'
mfac2_c = cfa(fac2_c, data = SAQ1, std.lv = TRUE)
summary(mfac2_c, fit.measures = TRUE, standardized = TRUE)
#The result above shows some contradiction on the test statistics where RMSEA decide that the model is a close
#fitting for the data while chi square decide that the model is not a close fit. This is a normal occurrence
#since the chi square statistics tends to reject null hypothesis easily when the number observations is high.
#But all in all, when we also consider the fit measures from CFI and TLI, this 2 correlated factors is by far
#the best model for anxiety data compared to previous models.
#For the last case (3), we may consider that instead of 2 factors being correlated, there is another factor
#that affects both factors. In other words, we have a structure that is ordered from : exogenous factor which
#affects the endogenous factors which then these endogenous factors affects our item/data.
#The model is simply adding another layer of factor equation on the endogenous factors.
fac2_o = 'f1 =~ q01 +  q03 + q04 + q05 + q08
          f2 =~ q06 + q07
          f3 =~ a*f1 + a*f2'
mfac2_o = cfa(fac2_o, data = SAQ1, std.lv = TRUE)
summary(mfac2_o, fit.measures = TRUE, standardized = TRUE)
#The results above shows almost the same from our previous 2 correlated factors.
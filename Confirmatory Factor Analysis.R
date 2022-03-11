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

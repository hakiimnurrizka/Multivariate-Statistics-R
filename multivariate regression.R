### Multivariate Regression ###
#Extension from multiple linear regression where we have more than 1 target/dependent variables
#At this point, regression should have been very familiar for almost everyone so lets jump right into modelling
#We'll model glucose data with first 3 variables as x and the others are the target
mlm.gluc = lm(formula = cbind(V4, V5, V6) ~ V1 + V2 + V3, data = glucose)
summary(mlm.gluc)
mlm.gluc$coefficients #beta matrix
#We'll estimate the parameters using manual matrix operation
glucose1 = as.matrix(glucose)
xgl = cbind('1' = rep(1, dim(glucose1)[1]), glucose1[,1:3])
ygl = glucose1[,4:6]
solve(t(xgl)%*%xgl)%*%t(xgl)%*%ygl

#Overall test : alternative hypothesis that at least one element of beta matrix is not equal to 0 (of course
#except the beta zero)
beta1.mlgl =  as.matrix(mlm.gluc$coefficients)
ybar.gl = as.matrix(apply(ygl, 2, mean))
ngl = dim(glucose1)[1]
egl = t(ygl)%*%ygl-t(beta1.mlgl)%*%t(xgl)%*%ygl
hgl = t(beta1.mlgl)%*%t(xgl)%*%ygl-ngl*ybar.gl%*%t(ybar.gl)
eh = solve(egl)%*%hgl
eigen(eh)
prod(1/(1+eigen(eh)$values))
wilk.mlgl = det(t(ygl)%*%ygl-t(beta1.mlgl)%*%t(xgl)%*%ygl)/det(t(ygl)%*%ygl-ngl*ybar.gl%*%t(ybar.gl))
wilk.mlgl = det(cov(glucose1))/(det(cov(xgl[,2:4]))*det(cov(ygl)))

chem = read.table("D:/My Drive/Github/Multivariate-Statistics-R/T10_1_CHEM.DAT", quote="\"", comment.char="")
lm(formula = cbind(V2, V3, V4) ~ V5 + V6 + V7, data = chem)
xche = as.matrix(cbind(rep(1, dim(chem)[1]), chem[,5:7]))
yche = as.matrix(chem[,2:4])
solve(t(xche)%*%xche)%*%t(xche)%*%yche
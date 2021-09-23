###MN distribution exercises from Methods_of_Multivariate_Analysis-_3rd_Edition Rencher & Christensen
library(car)
library(rgl)
library(mgcv)
library(mvtnorm)
library(Hmisc)
library(matlib)
library(MVN)
set.seed(100)

##4.10
mu = c(3,1,4)
sigma = matrix(c(6,1,-2,1,13,4,-2,4,4), nrow = 3, ncol = 3, byrow = T)
##a
a_z = c(2,-1,3)
mu_z = t(a_z)%*%mu
sigma_z = t(a_z)%*%sigma%*%a_z
##b
a2_z = matrix(c(1,1,1,1,-1,2), ncol = 3, nrow = 2, byrow = T)
mu2_z = a2_z%*%mu
sigma2_z = a2_z%*%sigma%*%t(a2_z)
##e
a3_z = matrix(c(1,0,0,0,0,1,0.5,0.5,0), ncol = 3, nrow = 3, byrow = T)
mu3_z = a3_z%*%mu
sigma3_z = a3_z%*%sigma%*%t(a3_z)
sigma3_z
##4.11
#simulate vector y from the defined multivariate normal dist
y = rmvnorm(1,mu , sigma,method=c("eigen", "svd", "chol"), pre0.9_9994 = FALSE)
#compute cholesky decomposition
t = chol(sigma)
#compute the new vector
z1 = solve(t(t))%*%t(y-mu)
#compute symmetric decomposition
eig_sig = as.matrix(Eigen(sigma)$vectors)
eig_sig
sigma_dd = t(eig_sig)%*%sigma%*%eig_sig
sigma_dd = replace(sigma_dd, sigma_dd<0.0005, 0)
sqrt_sig = eig_sig%*%sqrt(t(sigma_dd))%*%t(eig_sig)
sqrt_sig
#compute the new vector
z2 = solve(sqrt_sig)%*%t(y-mu)
##4.16
mu_k = c(2,-1,3,1)
part_muk = partition.vector(mu_k, sep = c(2,2))
sigma_k = matrix(c(7,3,-3,2,3,6,0,4,-3,0,5,-2, 2,4,-2,4), nrow = 4, ncol = 4, byrow = T) 
part_sigmak = partition.matrix(sigma_k, rowsep = c(2,2), colsep = c(2,2))
#since we arent provided with the vector x, we assume x = (2.5,1)
xminmux = matrix(c(2.5,1)-part_muk$`2`, nrow = 2, ncol = 1)
expect_yx = (part_muk$`1`)+part_sigmak$`1`$`2`%*%solve(part_sigmak$`2`$`2`)%*%xminmux
expect_yx
cov_yx = part_sigmak$`1`$`1`-part_sigmak$`1`$`2`%*%solve(part_sigmak$`2`$`2`)%*%part_sigmak$`2`$`1`
cov_yx
##4.24
hematol = read.table("T4_3_HEMATOL.DAT")
View(hematol)
colnames(hematol) = NULL
hematol = as.matrix(hematol)
#mean vector n cov matrix
mean_hematol = apply(hematol,2,mean)
cov_hematol = cov(hematol)
#compute d[i]
d = matrix(data = 1, nrow = dim(hematol)[1])
for(i in 1:dim(hematol)[1]){
  d[i] = t(hematol[i,]-mean_hematol)%*%solve(cov_hematol)%*%(hematol[i,]-mean_hematol)
} 
d
#check the elements if needed
t(hematol[1,]-mean_hematol)%*%solve(cov_hematol)%*%(hematol[1,]-mean_hematol)
dn = max(d)
dn
#compute u[i]
u = (51/50^2)*d
u = sort(u)
#compute v[i]
alpha = (dim(hematol)[2]-2)/(2*dim(hematol)[2])
beta = (dim(hematol)[1]-dim(hematol)[2]-3)/(2*(dim(hematol)[1]-dim(hematol)[2]-1))
v = (c(1:51)-alpha)/(dim(hematol)[1]-alpha-beta+1)
v
#plot (u[i],v[i])
plot(u, v)
#compute g[i,j]
sigma_hem = (50/51)*cov_hematol
g = matrix(data = 1, nrow = dim(hematol)[1], ncol = dim(hematol)[1])
for(i in 1:dim(hematol)[1]){
  for(j in 1:i){
    g[i,j] = t(hematol[i,]-mean_hematol)%*%solve(sigma_hem)%*%(hematol[j,]-mean_hematol)
    g[j,i] = t(hematol[j,]-mean_hematol)%*%solve(sigma_hem)%*%(hematol[i,]-mean_hematol)
  }
}
#compute b1 and b2
b1 = (1/51^2)*sum(g^3)
b2 = (1/51)*sum(diag(g)^2)
#using z
z1 = b1*(dim(hematol)[2]+1)*(dim(hematol)[1]+1)*(dim(hematol)[1]+3)/(6*((dim(hematol)[1]+1)*(dim(hematol)[2]+1)-6))
#z1~chisq(p(p+1)(p+2)/6)
df1 = (dim(hematol)[2]+1)*(dim(hematol)[2]+2)
pvalue1 = dchisq(z1,df1)
#using MVN package
mvn(data= hematol,mvnTest="royston")
#testing outlier
scatter3d(hematol[,1], hematol[,3], hematol[,6], surface = FALSE)
#compute wilks statistics
w = 1 - ((51*dn)/50^2)
#using f (p,n-p-1)
f = (44/6)*((1/w)-1)
pvf = df(f,6,44)
#using MVN package
out1 = mvn(data= hematol, mvnTest="mardia", multivariateOutlierMethod="quan")
out2 = mvn(data= hematol, mvnTest="hz", multivariateOutlierMethod="adj")
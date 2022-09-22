###Matrix Algebra###
#Referring to "METHODS OF MULTIVARIATE ANALYSIS" by Alvin Rencher and co. (2012), below are answer to some of the
#2nd chapter's questions using R.
#The answer are available both by using built-in package from functions and also manual input of the corresponding
#operation.

library(Hmisc)
library(matlib)
library(polynom)
library(Matrix)

### 2.5
a = matrix(c(1,2,3,2,-1,1), nrow = 2, ncol = 3, byrow = T) #input matrix, byrow to clarify that we input the element of the matrix in a row-sequential manner
b = matrix(c(3,-2,2,0,-1,1), nrow = 3, ncol = 2, byrow = T)
a%*%b #multiplying matrix use %*%
b%*%a
tr(a%*%b) == tr(b%*%a) #tr() to compute trace, then checking whether both  trace have the same  value or not


### 2.13
mat_a = matrix(c(2,1,2,3,2,0,1,0,1), nrow = 3, ncol = 3, byrow = T)
mat_b = matrix(c(1,1,1,0,2,1,1,2,2,3,1,2), nrow = 3, ncol = 4, byrow = T)

##non partition
mat_ab = mat_a%*%mat_b
mat_ab
ab_i = cbind(mat_ab, diag(3)) #ab matrix and I in the right
eche_abi = echelon(ab_i,  verbose = T, fractions = T) #row echelon form+proccedures, the I matrix turned into the first 3  row of the general inverse
ginv_ab1 = rbind(eche_abi[1:3,5:7],c(0,0,0))
ginv_ab1

mat_ab%*%ginv_ab1%*%mat_ab #check if it fulfills  (AB)(AB)^(-)(AB)=AB

#moore-penrose
mp_inv = Ginv(mat_ab)
mp_inv
mat_ainva = mat_ab%*%mp_inv%*%mat_ab
mat_ainva

##partition
#manual partitioning
mat_a11 = mat_a[1:2,1:2]
mat_a12 = matrix(mat_a[1:2,3], nrow=2, ncol=1, byrow = T)
mat_a21 = matrix(mat_a[3,1:2], nrow=1, ncol=2, byrow = T)
mat_a22 = mat_a[3,3]

mat_b11 = mat_b[1:2,1:3]
mat_b12 = matrix(mat_b[1:2,4], nrow=2, ncol=1, byrow = T)
mat_b21 = matrix(mat_b[3,1:3], nrow=1, ncol=3, byrow = T)
mat_b22 = mat_b[3,4]

mat_ab11 = mat_a11%*%mat_b11+mat_a12%*%mat_b21
mat_ab21 = mat_a21%*%mat_b11+mat_a22%*%mat_b21
mat_ab12 = mat_a11%*%mat_b12+mat_a12%*%mat_b22
mat_ab22 = mat_a21%*%mat_b12+mat_a22%*%mat_b22

#faster partitioning
part_a = partition.matrix(mat_a, rowsep = c(2,1), colsep = c(2,1))
part_b = partition.matrix(mat_b, rowsep = c(2,1), colsep = c(3,1))

part_ab11 = part_a$`1`$`1`%*%part_b$`1`$`1`+part_a$`1`$`2`%*%part_b$`2`$`1` 
part_ab21 = part_a$`2`$`1`%*%part_b$`1`$`1`+part_a$`2`$`2`%*%part_b$`2`$`1` 
part_ab12 = part_a$`1`$`1`%*%part_b$`1`$`2`+part_a$`1`$`2`%*%part_b$`2`$`2` 
part_ab22 = part_a$`2`$`1`%*%part_b$`1`$`2`+part_a$`2`$`2`%*%part_b$`2`$`2` 
part_ab11

### 2.17
waw = as.matrix(data.frame(c(3,-5,-1),c(-5,13,0),c(-1,0,1)))
colnames(waw) = NULL #to disregard collumn naming from as.matrix()

##cholesky
#manual
chol_waw = as.matrix(data.frame(c(0,0,0),c(0,0,0),c(0,0,0)))
chol_waw[1,1] = sqrt(waw[1,1])
chol_waw[1,2] = -5/sqrt(waw[1,1])
chol_waw[1,3] = -1/sqrt(waw[1,1])
chol_waw[2,2] = sqrt(waw[2,2]-chol_waw[1,2]^2)
chol_waw[2,3] = (waw[2,3]-chol_waw[1,2]*chol_waw[1,3])/chol_waw[2,2]
chol_waw[3,3] = sqrt(waw[3,3]-chol_waw[1,3]^2-chol_waw[2,3]^2)
chol_waw

#using function
kekw = chol(waw)
kekw

### 2.18
waw2 = as.matrix(data.frame(c(1,2,1),c(-1,1,-1),c(1,0,-1)))
colnames(waw2) = NULL
#divide by each col length
waw2[,1] = waw2[,1]/sqrt(sum(waw2[,1]^2))
waw2[,2] = waw2[,2]/sqrt(sum(waw2[,2]^2))
waw2[,3] = waw2[,3]/sqrt(sum(waw2[,3]^2))
waw2%*%t(waw2) #we can regard the very small number as 0 since it's the numerical leftover

#other way to divide by each col length
omegalul = t(waw2)/sqrt(colSums(waw2^2))#transpose to divide each with collumn
t(omegalul)
omegalul%*%t(omegalul)


### 2.19
#look for eigen using the equation |A| = 0, the previouse equation yield a polynomial equation : -2+x+2(x^2)-(x^3)=0
p = polynomial(c(-2,1,2,-1)) #building the polynomial
p
solve(p) #solving for the previous polynomial

#eigen using function
h = matrix(c(1,1,-2,-1,2,1,0,1,-1), nrow = 3, ncol = 3, byrow = T)
eig_h = eigen(h)
eig_h$values
eig_h$vectors

tr(h) 
sum(eig_h$values)
det(h)
prod(eig_h$values)


### 2.20
j = matrix(c(3,1,1,1,0,2,1,2,0), nrow = 3, ncol = 3, byrow = T)

eig_vj = Eigen(j)$vectors
eig_vj
eig_vj%*%t(eig_vj)
gawr = t(eig_vj)%*%j%*%eig_vj
eig_vj%*%gawr%*%t(eig_vj)
j

### 2.21
k = matrix(c(2,-1,-1,2), nrow = 2, ncol = 2, byrow = T)
eig_k = Eigen(k)
eig_vk =eig_k$vectors

dd = t(eig_vk)%*%k%*%eig_vk
dd

dd[1,2] = 0
dd[2,1] = 0
sqrt_k = eig_vk%*%sqrt(t(dd))%*%t(eig_vk)
sqrt_k
sqrt_k%*%t(sqrt_k)

### 2.23
v = matrix(c(4,-5,-1, 7,-2,3, -1,4,-3, 8,2,6), nrow = 4, ncol = 3, byrow = T)
v
rankMatrix(v)
vvt = v%*%t(v) 
vtv = t(v)%*%v
d = diag(sqrt(Eigen(vvt)$value), 3, 3)
d

u = eigen(vvt)$vectors
u
u = u[,1:3] 
u

v_t = Eigen(vtv)$vectors
v_t
eigen(vtv)$vectors
v_t[,3] = eigen(vtv)$vectors[,3]
u%*%d%*%t(v_t)
v

svd(v)
svd(v)$u%*%diag(svd(v)$d, 3,3)%*%t(svd(v)$v)

###Exercise from Methods_of_Multivariate_Analysis-_3rd_Edition Rencher & Christensen###
library(fBasics)
###chapter 3
calcium = read.table("T3_4_CALCIUM.DAT", quote="\"", comment.char="")
calcium = calcium[,-1]
bone = read.table("T3_7_BONE.DAT", quote="\"", comment.char="")
bone = bone[,-1]
glucose = read.table("T3_9_GLUCOSE.DAT", quote="\"", comment.char="")

## 3.11
cov(calcium)#covariance matrix
det(cov(calcium))#generalized sample variance
tr(cov(calcium))#total sample variance

## 3.17
#by finding z first
z = matrix(c(apply(calcium,1, sum), 2*calcium[,1]-3*calcium[,2]+2*calcium[,3], -calcium[,1]-2*calcium[,2]-3*calcium[,3]),
           ncol = 3)
apply(z, 2,mean)
cov(z)
#using equation 3.62 and 3.64
mean.calc = matrix(apply(calcium, 2, mean), ncol = 1)
sd.calc = matrix(cov(calcium), ncol = 3)
mult311 = matrix(c(1,1,1,2,-3,2,-1,-2,-3), nrow = 3, byrow = T)
mult311%*%mean.calc #mean
mult311%*%sd.calc%*%t(mult311) #sd/covariance matrix
#correlation matrix
solve(matrix(sqrt(diag(diag(cov(z)))), ncol = 3))%*%cov(z)%*%solve(matrix(sqrt(diag(diag(cov(z)))), ncol = 3))
cor(z)

##3.20
bone = as.matrix(bone)
mean.bone = matrix(apply(bone, 2, mean), ncol = 1)
sd.bone = matrix(cov(bone), ncol = 4)
mult320 = matrix(c(2,3,-1,4,-2,-1,4,-2,3,-2,-1,3), ncol = 4, byrow = T)
mult320%*%mean.bone
covz.bone = mult320%*%sd.bone%*%t(mult320)
solve(matrix(sqrt(diag(diag(covz.bone))), ncol = 3))%*%covz.bone%*%solve(matrix(sqrt(diag(diag(covz.bone))), ncol = 3))
cor(bone%*%t(mult320))

##3.22
apply(glucose, 2, mean) #mean vectors
cov(glucose) #covariance matrix
#build the partition
partition = function(x, rowsep, colsep, ...){
  colmissing <- missing(colsep)
  rowmissing <- missing(rowsep)
  if (rowmissing && colmissing) {
    stop("Atleast one of rowsep or colsep args must be specified")
  }
  if (!rowmissing) {
    if (sum(rowsep) != NROW(x)) {
      stop("rowsep must sum to the number of columns in x")
    }
    if (!is.numeric(rowsep)) {
      stop("the rowsep vector must be numeric")
    }
  }
  if (!colmissing) {
    if (sum(colsep) != NCOL(x)) {
      stop("colsep must sum to the number of rows in x")
    }
    if (!is.numeric(colsep)) {
      stop("the colsep vector must be numeric")
    }
  }
  if (!rowmissing) {
    set <- lapply(split(seq(NROW(x)), rep(seq(along.with = rowsep), 
                                          times = rowsep)), function(index) x[index, , drop = FALSE])
  }
  else {
    set <- NULL
  }
  if (!colmissing) {
    FUN <- function(x) lapply(split(seq(NCOL(x)), rep(seq(along.with = colsep), 
                                                      times = colsep)), function(index) x[, index, drop = FALSE])
    if (is.null(set)) {
      FUN(x)
    }
    else {
      lapply(set, FUN)
    }
  }
  else {
    set
  }
}
part.gluc = partition(glucose, colsep = c(3,3))
apply(part.gluc$'1', 2, mean) #first partition mean vectors
apply(part.gluc$'2', 2, mean) #second partition mean vectors


part1.gluc = part.gluc$'1'
part2.gluc = part.gluc$'2'
cov(part1.gluc)
cov(part1.gluc, part2.gluc)
cov(part2.gluc, part1.gluc)
cov(part2.gluc)

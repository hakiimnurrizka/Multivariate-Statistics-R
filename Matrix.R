###Matrix
#Create
set.seed(123) #setting pseudo-random number initial point
a1 = matrix(c(rpois(10, 5)),nrow = 1) #generate random numbers from poison dist (5)
a12 = matrix(c(rpois(10, 6)), nrow = 1)
a13 = matrix(c(rpois(10, 4)), nrow = 1)
a2 = matrix(c(rbinom(10, 100,0.3)), nrow = 1) #generate random numbers from binomial dist (100,0.3) 
a = rbind(a1,a12,a13,a2) #merge 4 vectors above by row


###Partition of matrix
##Partition function
library(Hmisc)
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
partition(a1, colsep = c(2,3,2,3))
partition(a, colsep=c(5,2,3), rowsep = c(2,2))
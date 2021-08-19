function (x, rowsep, colsep, ...) 
{
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
#' Two-sample t-tests (equal variance) between two groups
#'
#' This function computes m sets of two sample t-tests between two groups.
#' For each row, it compares data in the first group and the second group.
#' Each group are of the same sample size (half of the number of columns).
#' It assumes equal variance between the two groups.
#'
#' @param x a m*n matrix. Data in each row are considered separately.
#' @param f a vector with length of n. The elements can take value 1 or 2. The total number of 1 equals the total number of 2. If the ith element of the vector equals 1, the ith column of x belongs to group 1; otherwise, the ith column of x belongs to group 2.
#'
#' @return a vector of t statistics with length of m
#'
#' @examples
#' set.seed(1)
#' x <- matrix(rnorm(50*4),nrow=50,ncol=4)
#' f <- c(1, 1, 2, 2)
#' getT(x, f)
#'
#' @export
getT <- function(x, f){
  n <- dim(x)[2]
  little.n <- n/2
  if (n != length(f)){
    stop("x and f should have consistent dimensions.")
  }
  if (sum(f == 1) != little.n){
    stop("The two groups don't have equal sample sizes.")
  }
  num <- (rowSums(x[,which(f == 1)]) - rowSums(x[, which(f == 2)]))/little.n
  se <- rowSums((x[,which(f == 1)] - rowMeans(x[, which(f == 1)])) ^2) +
    rowSums((x[,which(f == 2)] - rowMeans(x[, which(f == 2)])) ^2)
  denominator <- sqrt(se/(n - 2)) * sqrt(2/little.n)
  return(num/denominator)
}


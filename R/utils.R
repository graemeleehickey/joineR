#' Internal function to calculate the Z-design matrix
#'
#' @keywords internal
getD <- function(q, arg) {
  D <- matrix(0, q, length(arg))
  for (i in 1:q) {
    D[i, ] <- arg^(i - 1)
  }
  return(D)
}

#' Internal function to test for missing data
#'
#' @keywords internal
notNA <- function(x) {
  !(is.vector(x) & length(x) == 1 & any(is.na(x)))
}


#' Internal function to evaluate whether separate model fits should be reported
#'
#' @keywords internal
sep <- function(ests, logical) {
  if (logical == FALSE) {
    ests <- "No separate results requested"
  }
  ests
}
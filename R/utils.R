#' @keywords internal
getD <- function(q, arg) {
  D <- matrix(0, q, length(arg))
  for (i in 1:q) {
    D[i, ] <- arg^(i - 1)
  }
  return(D) # calculate the Z-design matrix
}


#' @keywords internal
notNA <- function(x) {
  !(is.vector(x) & length(x) == 1 & any(is.na(x)))
}


#' @keywords internal
sep <- function(ests, logical) {
  if (logical == FALSE) {
    ests <- "No separate results requested"
  }
  ests
}


#' @keywords internal
sortJointData <- function(longdat, survdat) {
  longid <- longdat[, 1]
  nn <- diff(match(unique(longid), longid))
  nn[length(nn) + 1] <- length(longid) - sum(nn)
  svec <- rep(survdat[, 2], nn)
  sort.long <- longdat[order(svec), ]
  os <- order(survdat[, 2])
  sort.surv <- survdat[os, ]
  list(long.s = data.frame(sort.long), surv.s = data.frame(sort.surv))
}

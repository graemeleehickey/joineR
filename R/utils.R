getD <- function(q, arg) {
  D <- matrix(0, q, length(arg))
  for (i in 1:q) {
    D[i, ] <- arg^(i - 1)
  }
  return(D)
}

notNA <- function(x) {
  !(is.vector(x) & length(x) == 1 & any(is.na(x)))
}

sep <- function(ests, logical) {
  if (logical == FALSE) {
    ests <- "No separate results requested"
  }
  ests
}
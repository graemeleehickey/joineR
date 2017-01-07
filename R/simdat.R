#' @keywords internal
simdat <- function(n, model, sepassoc, ntms, ran, b1, b2, gamma, sigu,
                   vare, theta0, theta1, censoring, censlam, truncation, 
                   trunctime, gridstep) {
  
  ctsx <- rnorm(n)
  binx <- runif(n, -sqrt(3), sqrt(3))
  X2 <- cbind(ctsx, binx)
  id <- 1:n
  idl <- rep(id, each = ntms)
  ctsxl <- rep(ctsx, each = ntms)
  binxl <- rep(binx, each = ntms)
  time <- rep(0:(ntms-1), length = n*ntms)
  X1 <- cbind(intercept = 1, ctsxl, binxl, ltime = time)
  U <- MASS::mvrnorm(n, mu = rep(0, ran), Sigma = sigu)
  Ul <- U[rep(1:n, each = ntms), ]
  D <- getD(ran, time)
  DU <- t(D) * Ul
  Y <- (X1 %*% b1) + rowSums(DU) + sqrt(vare) * rnorm(n*ntms)
  u0 <- U[, 1]
  if (model == "intslope") {
    u1 <- U[, 2]
  } else {
    u1 <- rep(0, n)
  }
  b2x <- X2 %*% b2
  cens <- rep(1, n)
  if (!sepassoc) {
    gamma <- rep(gamma[1], ran)
  }
  if (model != "quad") {
    if (model == "int") {
      gamma <- c(gamma[1], 0)
    }
    uu <- runif(n)
    if (model == "int") {
      survtime <- -log(uu) / exp(theta0 + b2x + gamma[1]*u0)
    } else {
      ii <- ((theta1 + gamma[2]*u1) < 0) & (uu < exp(exp(theta0 + b2x + gamma[1]*u0) / 
                                                       (theta1 + gamma[2]*u1)))
      survtime <- rep(0, n)
      survtime[ii] <- Inf
      survtime[!ii] <- log(1 - (theta1 + gamma[2]*u1[!ii]) * log(uu[!ii]) / 
                             exp(theta0 + b2x[!ii] + gamma[1]*u0[!ii])) / (theta1 + gamma[2]*u1[!ii])
    }
  } else {
    tau <- trunctime
    tgrid <- seq(runif(1, 0, gridstep), tau, gridstep)
    lam0 <- exp(theta0 + theta1 * tgrid)
    hazt <- gridstep * exp(b2x) %*% lam0
    gD2 <- gamma * getD(ran, tgrid)
    hmat <- exp(U %*% gD2) * hazt
    uu <- matrix(runif(length(hmat)), n, length(tgrid))
    tmat <- matrix(tgrid, n, length(tgrid), byrow = TRUE)
    tmat[hmat < uu] <- tau
    survtime <- apply(tmat, 1, min)
    cens[survtime == tau] <- 0
  }
  
  if (censoring) {
    censtime <- -log(runif(n)) / censlam
  } else {
    censtime <- rep(Inf, n)
  }
  if (model != "quad") {
    if (truncation) {
      censtime <- pmin(censtime, trunctime)
    }
  }
  ii <- (censtime < survtime)
  survtime[ii] <- censtime[ii]
  cens[ii] <- 0
  ls <- rep(survtime, each = ntms)
  Y <- Y[ls > time]
  X1 <- X1[ls > time, ]
  idl <- idl[ls > time]
  time <- time[ls > time]
  cat(paste0(round(100 * sum(cens) / n, 1), "% experienced event\n"))
  
  list(longdat = data.frame(id = idl, Y, time, X1),
       survdat = data.frame(id, survtime, cens, X2))
  
}

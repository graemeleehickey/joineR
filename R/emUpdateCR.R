#' @keywords internal
#' @importFrom statmod gauss.quad.prob
emUpdateCR <- function(longdat, survdat, paraests,
                   gpt, max.it, tol, loglik) {

  # longitudinal submodel data
  id <- longdat[, 1]
  Y <- longdat[, 2]
  lda.time <- longdat[, 3]
  X1 <- as.matrix(longdat[, 4:dim(longdat)[2]])
  p1 <- dim(X1)[2]
  b1 <- paraests$b1[, 1]
  sig <- paraests$sigma.u
  rho <- paraests$corr
  sigma.z <- paraests$sigma.z

  # failure time submodel data
  n <- length(survdat[, 2])
  surv.time <- survdat[, 2]
  cen.a <- survdat[, 3]
  cen.b <- survdat[, 4]
  nn <- diff(match(unique(id), id))
  nn <- c(nn, length(id) - sum(nn))
  p2 <- dim(survdat)[2] - 4
  X2 <- 0
  if (p2 > 0) {
    X2 <- as.matrix(survdat[, 5:dim(survdat)[2]])
  }
  haz.a <- paraests$haz.a[, 1]
  s.dista <- paraests$s.dist.a[, 1]
  id.a <- paraests$id.a[, 1]
  haz.b <- paraests$haz.b[, 1]
  s.distb <- paraests$s.dist.b[, 1]
  id.b <- paraests$id.b[, 1]
  b2.a <- c(paraests$b2.a, 0)
  b2.b <- c(paraests$b2.b, 0)

  # control params
  N <- sum(nn)
  maxn <- max(nn)
  ran <- 2
  ab <- vector("numeric", gpt)
  w <- vector("numeric", gpt)
  g <- statmod::gauss.quad.prob(gpt, "normal", sigma = sqrt(0.5))
  ab <- g$nodes
  w <- g$weights * sqrt(pi)
  conv <- FALSE
  

  # E-step setup
  gammat <- matrix(0, gpt^2, ran)
  gammat[, 1] <- rep(ab, each = gpt)
  gammat[, 2] <- rep(ab, gpt)
  wvec <- as.vector(w %*% t(w))
  EU <- matrix(0, n, 2)
  EUU <- matrix(0, n, 3)
  EexpU.a <- matrix(0, n, length(haz.a))
  EU0expU.a <- matrix(0, n, length(haz.a))
  EU1expU.a <- matrix(0, n, length(haz.a))
  EU0U0expU.a <- matrix(0, n, length(haz.a))
  EU0U1expU.a <- matrix(0, n, length(haz.a))
  EU1U1expU.a <- matrix(0, n, length(haz.a))
  EexpU.b <- matrix(0, n, length(haz.b))
  EU0expU.b <- matrix(0, n, length(haz.b))
  EU1expU.b <- matrix(0, n, length(haz.b))
  EU0U0expU.b <- matrix(0, n, length(haz.b))
  EU0U1expU.b <- matrix(0, n, length(haz.b))
  EU1U1expU.b <- matrix(0, n, length(haz.b))
  W11 <- diag(maxn)
  W3 <- matrix(0, maxn, 2)
  cvar <- matrix(0, ran, ran)
  cvarch <- matrix(0, ran, ran)

  if (loglik) {
    l1 <- 0
    l2 <- 0
  }
  
  # main loop over EM iterations begins here
  iter <- 0
  for (it in 1:max.it) {

    iter <- iter + 1
    W22 <- sig
    b2x <- matrix(0, n, 1)
    if (p2 > 0) {
      b2temp.a <- c(b2.a[1:p2])
      b2temp.b <- c(b2.b[1:p2])
      b2x.a <- X2 %*% b2temp.a
      b2x.b <- X2 %*% b2temp.b
      b2x <- b2x.a + b2x.b
    }
    rlong <- Y - (X1 %*% b1)
    count <- 1

    # main loop over subjects begins here (E-step)
    for (i in 1:n) {

      W21 <- matrix(0, 2, nn[i])
      rvec <- rlong[count:(count + nn[i] - 1)]
      W11 <- (sig[1, 2] + sig[2, 2] * lda.time[count:(count + nn[i] - 1)]) %*%
        t(lda.time[count:(count + nn[i]-1)]) +
        sig[1, 1] + sig[1, 2] * lda.time[count:(count + nn[i] - 1)]
      W21[1, 1:nn[i]] <- sig[1, 1] +
        (sig[1, 2] * lda.time[count:(count + nn[i] - 1)])
      W21[2, 1:nn[i]] <- sig[1, 2] +
        (sig[2, 2] * lda.time[count:(count + nn[i] - 1)])
      W11 <- W11 + (sigma.z * diag(nn[i]))
      count <- count + nn[i]
      W3 <- solve(W11, t(W21))
      cvar <- W22 - W21 %*% W3

      # Transform to independent variables (called gamma)
      cvar <- cvar * 2
      cvarch <- chol(cvar)
      cvarch <- t(cvarch)
      cm <- t(W3) %*% rvec
      cmmat <- matrix(0, gpt^2, ran)
      cmmat[, 1] <- rep(cm[1], gpt^2)
      cmmat[, 2] <- rep(cm[2], gpt^2)
      newumat <- cvarch %*% t(gammat) + t(cmmat)
      fvec <- exp(
        cen.a[i] * b2.a[p2 + 1] * (newumat[1, ] + newumat[2, ] * surv.time[i]) +
          (cen.b[i] * b2.b[p2 + 1] * (newumat[1, ] + newumat[2, ] * surv.time[i]))
      )
      ssvec.a <- 1

      if (id.a[i] > 0) {
        ssvec.a <- exp(
          b2.a[p2 + 1] * (newumat[1, ] + newumat[2, ] %*% t(s.dista[1:id.a[i]]))
        ) %*% haz.a[1:id.a[i]]
      }
      ssvec.b <- 1
      if (id.b[i] > 0) {
        ssvec.b <- exp(
          b2.b[p2 + 1] * (newumat[1, ] + newumat[2, ] %*% t(s.distb[1:id.b[i]]))
        ) %*% haz.b[1:id.b[i]]
      }
      ssvec <- ssvec.a * ssvec.b * exp(b2x[i, ])
      fvec <- fvec * wvec * exp(-ssvec)
      den <- sum(fvec)
      EU[i, 1:2] <- t(newumat %*% fvec / den)
      EUU[i, 1:2] <- t((newumat^2) %*% fvec / den)
      EUU[i, 3] <- (newumat[1, ] * newumat[2, ]) %*% fvec / den
      const.a <- exp(
        b2.a[p2 + 1] * (newumat[1, ] + newumat[2, ] %*% t(s.dista[1:id.a[i]])))
      if (id.a[i] == 0) {
        const.a <- const.a^0
      }
      EexpU.a[i, 1:id.a[i]] <- t(fvec) %*% const.a / den
      EU0expU.a[i, 1:id.a[i]] <- t(fvec) %*% (newumat[1, ] * const.a) / den
      EU1expU.a[i, 1:id.a[i]] <- t(fvec) %*% (newumat[2, ] * const.a) / den
      EU0U0expU.a[i, 1:id.a[i]] <- t(fvec) %*% (newumat[1, ]^2 * const.a) / den
      EU0U1expU.a[i, 1:id.a[i]] <- t(fvec) %*% (newumat[1, ] * newumat[2, ] * const.a) / den
      EU1U1expU.a[i, 1:id.a[i]] <- t(fvec) %*% (newumat[2, ]^2 * const.a) / den
      const.b <- exp(b2.b[p2 + 1] * (newumat[1, ] + newumat[2, ] %*% t(s.distb[1:id.b[i]])))

      if (id.b[i] == 0) {
        const.b <- const.b^0
      }
      EexpU.b[i, 1:id.b[i]] <- t(fvec) %*% const.b / den
      EU0expU.b[i, 1:id.b[i]] <- t(fvec) %*% (newumat[1, ] * const.b) / den
      EU1expU.b[i, 1:id.b[i]] <- t(fvec) %*% (newumat[2, ] * const.b) / den
      EU0U0expU.b[i, 1:id.b[i]] <- t(fvec) %*% (newumat[1, ]^2 * const.b) / den

      EU0U1expU.b[i, 1:id.b[i]] <- t(fvec) %*% (newumat[1, ] * newumat[2, ] * const.b) / den
      EU1U1expU.b[i, 1:id.b[i]] <- t(fvec) %*% (newumat[2, ]^2 * const.b) / den
      
      # calculate the log-likelihood
      if (loglik) {
        if (den > 0) {
          l2 <- l2 + log(den)
        }
        l1 <- l1 - nn[i] * 0.5 * log(2 * pi) - 0.5 * log(det(W11)) -
          0.5 * sum(rvec * solve(W11, rvec))
      }

    } # end of loop over subjects

    # M-step
    parac <- data.frame(c(b1, b2.a, b2.b, sigma.z, sig, rho))

    # update: baseline hazards
    ndist.a <- max(id.a)
    ndist.b <- max(id.b)
    for (i in 1:(ndist.a-1)) {
      sum3 <- sum(exp(b2x.a[match(i, id.a):n]) * (EexpU.a[match(i, id.a):n, i]))
      nfail <- sum(cen.a[match(i, id.a):(match(i + 1, id.a) - 1)])
      haz.a[i] <- nfail / sum3
    }
    sum3 <- sum(exp(b2x.a[match(ndist.a, id.a):n]) *
                  (EexpU.a[match(ndist.a, id.a):n, i]))
    nfail <- sum(cen.a[match(ndist.a, id.a):n])
    haz.a[ndist.a] <- nfail / sum3
    for (i in 1:(ndist.b-1)) {
      sum3 <- sum(exp(b2x.b[match(i, id.b):n]) *
                    (EexpU.b[match(i, id.b):n, i]))
      nfail <- sum(cen.b[match(i, id.b):(match(i + 1, id.b) - 1)])
      haz.b[i] <- nfail / sum3
    }
    sum3 <- sum(exp(b2x.b[match(ndist.b, id.b):n]) *
                  (EexpU.b[match(ndist.b, id.b):n, i]))
    nfail <- sum(cen.b[match(ndist.b, id.b):n])
    haz.b[ndist.b] <- nfail / sum3

    # update: initial setup for beta1 (longitudinal) and beta2 (failure time)
    EUmat <- matrix(0, N, 2)
    EUUmat <- matrix(0, N, 3)
    EUmat[, 1] <- rep(EU[, 1], nn)
    EUmat[, 2] <- rep(EU[, 2], nn)
    EUUmat[, 1] <- rep(EUU[, 1], nn)
    EUUmat[, 2] <- rep(EUU[, 2], nn)
    EUUmat[, 3] <- rep(EUU[, 3], nn)
    summat.a <- matrix(0, n, 1)
    summat2.a <- matrix(0, n, 2)
    summat3.a <- matrix(0, n, 3)
    summat.b <- matrix(0, n, 1)
    summat2.b <- matrix(0, n, 2)
    summat3.b <- matrix(0, n, 3)

    for (i in 1:n) {
      if (id.a[i] > 0) {
        summat.a[i, 1]  <- sum(EexpU.a[i, 1:id.a[i]] *
                                 haz.a[1:id.a[i]])
        summat2.a[i, 1] <- sum(EU0expU.a[i, 1:id.a[i]] *
                                 haz.a[1:id.a[i]])
        summat2.a[i, 2] <- sum(EU1expU.a[i, 1:id.a[i]] * s.dista[1:id.a[i]] *
                                 haz.a[1:id.a[i]])
        summat3.a[i, 1] <- sum(EU0U0expU.a[i, 1:id.a[i]] *
                                 haz.a[1:id.a[i]])
        summat3.a[i, 2] <- sum(EU1U1expU.a[i, 1:id.a[i]] * (s.dista[1:id.a[i]]^2) *
                                 haz.a[1:id.a[i]])
        summat3.a[i, 3] <- sum(EU0U1expU.a[i, 1:id.a[i]] * s.dista[1:id.a[i]] *
                                 haz.a[1:id.a[i]])
      }
      if (id.b[i] > 0) {
        summat.b[i, 1]  <- sum(EexpU.b[i, 1:id.b[i]] *
                                 haz.b[1:id.b[i]])
        summat2.b[i, 1] <- sum(EU0expU.b[i, 1:id.b[i]] *
                                 haz.b[1:id.b[i]])
        summat2.b[i, 2] <- sum(EU1expU.b[i, 1:id.b[i]] * s.distb[1:id.b[i]] *
                                 haz.b[1:id.b[i]])
        summat3.b[i, 1] <- sum(EU0U0expU.b[i, 1:id.b[i]] *
                                 haz.b[1:id.b[i]])
        summat3.b[i, 2] <- sum(EU1U1expU.b[i, 1:id.b[i]] * (s.distb[1:id.b[i]]^2) *
                                 haz.b[1:id.b[i]])
        summat3.b[i, 3] <- sum(EU0U1expU.b[i, 1:id.b[i]] * s.distb[1:id.b[i]] *
                                 haz.b[1:id.b[i]])
      }
    }

    # update: beta1
    tEUmat <- EUmat[, 2] * lda.time
    sum <- EUmat[, 1] + tEUmat
    Ystar <- Y-sum
    XTX <- t(X1) %*% X1
    XTY <- t(X1) %*% Ystar
    b1 <- solve(XTX, XTY)

    # update: sigmaz
    bx <- X1 %*% b1
    r <- Y-bx
    sum2 <- r^2-2 * r * (EUmat[, 1] + tEUmat) + EUUmat[, 1] +
      (EUUmat[, 2] * (lda.time^2))
    sum2 <- sum2 + 2 * EUUmat[, 3] * lda.time
    sigma.z <- sum(sum2) / N

    # update: U-matrix
    sig[1, 1] <- sum(EUU[, 1]) / n
    sig[2, 2] <- sum(EUU[, 2]) / n
    sig[1, 2] <- sum(EUU[, 3]) / n
    sig[2, 1] <- sig[1, 2]
    rho <- sig[1, 2] / sqrt(sig[1, 1] * sig[2, 2])

    # update: beta2 (N-R step)
    fd.a <- vector("numeric", p2 + 1)
    sd.a <- matrix(0, p2 + 1, p2 + 1)
    eb2x.a <- exp(b2x.a)
    fd.b <- vector("numeric", p2 + 1)
    sd.b <- matrix(0, p2 + 1, p2 + 1)
    eb2x.b <- exp(b2x.b)
    if (p2 > 0) {
      for (i in 1:p2) {
        fd.a[i] <- sum(cen.a * X2[, i])-sum(X2[, i] * eb2x.a * summat.a[, 1])
        sd.a[i, p2 + 1] <- (-sum(X2[, i] * eb2x.a * (summat2.a[, 1] +
                                                       summat2.a[, 2])))
        fd.b[i] <- sum(cen.b * X2[, i])-sum(X2[, i] * eb2x.b * summat.b[, 1])
        sd.b[i, p2 + 1] <- (-sum(X2[, i] * eb2x.b * (summat2.b[, 1] +
                                                       summat2.b[, 2])))}
    }
    fd.a[p2 + 1] <- sum(cen.a * (EU[, 1] + EU[, 2] * surv.time)) -
      sum(eb2x.a * (summat2.a[, 1] + summat2.a[, 2]))
    fd.b[p2 + 1] <- sum(cen.b * (EU[, 1] + EU[, 2] * surv.time)) -
      sum(eb2x.b * (summat2.b[, 1] + summat2.b[, 2]))
    sd.a <- sd.a + t(sd.a)
    sd.b <- sd.b + t(sd.b)
    if (p2 > 0) {
      for (i in 1:p2) {
        for (j in 1:p2) {
          sd.a[i, j] <- (-sum(X2[, i] * X2[, j] * eb2x.a * summat.a[, 1]))
          sd.b[i, j] <- (-sum(X2[, i] * X2[, j] * eb2x.b * summat.b[, 1]))
        }
      }
    }
    sd.a[p2 + 1, p2 + 1] <- (-sum(eb2x.a * (summat3.a[, 1] + 2 * summat3.a[, 3] +
                                              summat3.a[, 2])))
    sd.b[p2 + 1, p2 + 1] <- (-sum(eb2x.b * (summat3.b[, 1] + 2 * summat3.b[, 3] +
                                              summat3.b[, 2])))

    # N-R step
    b2.a <- b2.a - solve(sd.a, fd.a)
    b2.b <- b2.b - solve(sd.b, fd.b)

    # check convergence
    para <- data.frame(c(b1, b2.a, b2.b, sigma.z, sig, rho))
    dd <- abs(parac - para)
    if (max(dd) < tol) {
      conv <- TRUE
      break
    }

  }

  if ((conv != TRUE) & !loglik) {
    print("Not converged")
  }
  
  if (loglik) {
    ll <- l1 + l2 - 0.5 * ran * n * log(pi)
    list("log.like" = ll,
         "longlog.like" = l1,
         "survlog.like" = ll - l1)
  } else {
    list("b1" = data.frame(b1),
         "b2.a" = data.frame(b2.a),
         "b2.b" = data.frame(b2.b),
         "sigma.z" = sigma.z,
         "sigma.u" = sig,
         "cor" = rho,
         "haz.a" = haz.a,
         "haz.b" = haz.b,
         "random" = EU,
         "conv" = conv,
         "iters" = iter)
  }

}

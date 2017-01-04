#' Internal function for performing EM algorithm
#' 
#' @keywords internal
em.alg <- function(longdat, survdat, model, ran, lat, sepassoc, 
                   paraests, gpt, max.it, tol) {
  
  id <- longdat[, 1]
  Y <- longdat[, 2]
  tt <- longdat[, 3]
  X1 <- as.matrix(longdat[, 4:dim(longdat)[2]])
  n <- length(survdat[, 2])
  s <- survdat[, 2]
  cen <- survdat[, 3]
  p1 <- dim(longdat)[2] - 3
  p2 <- dim(survdat)[2] - 3
  X2 <- 0
  if (p2 > 0) {
    X2 <- as.matrix(survdat[, 4:dim(survdat)[2]])
  } else {
    b2x <- matrix(0, n, 1)
  }
  b1 <- paraests$b1[, 1]
  sigma.u <- paraests$sigma.u
  tsigu <- t(sigma.u)
  sigma.z <- paraests$sigma.z
  b2 <- c(paraests$b2, rep(0, lat))
  haz <- paraests$haz
  sf <- paraests$sf
  rs <- paraests$rs
  nev <- paraests$nev
  nn <- diff(match(unique(id), id))
  nn <- c(nn, length(id) - sum(nn))
  N <- sum(nn)
  g <- gauss.quad.prob(gpt, "normal", sigma = sqrt(0.5))
  ab <- g$nodes
  w <- g$weights * sqrt(pi)
  gmat <- matrix(0, gpt^ran, ran)
  gmat[, 1] <- rep(ab, each = gpt^(ran - 1))
  if (model != "int") {
    gmat[, 2] <- rep(ab, gpt)
    w <- as.vector(w %x% w)
  }
  if (model == "quad") {
    gmat[, 3] <- rep(ab, each = gpt)
    w <- as.vector(w %x% g$weights * sqrt(pi))
  }
  EU <- matrix(0, n, ran)
  EUU <- matrix(0, n, sum(1:ran))
  EexpU <- matrix(0, n, length(haz))
  EUexpU <- matrix(0, n, ran)
  EUUexpU <- matrix(0, n, sum(1:ran))
  r <- Y - X1 %*% b1
  Dtt <- getD(ran, tt)
  Dtt2 <- t(Dtt)
  if (model != "int") {
    Dttc <- t(getD(sum(1:ran) - ran, tt)) * tt
  }
  Ds <- getD(ran, s)
  Dst <- t(Ds)
  Dsf <- getD(ran, sf)
  Dsf2 <- Dsf^2
  Dsfc <- t(t(Dsf) * sf)
  Dnsf <- matrix(1, ran, length(sf))
  s1 <- rep(1:(ran - 1), (ran - 1):1)
  s2 <- sequence((ran - 1):1) + rep(1:(ran - 1), (ran - 1):1)
  cnn <- c(0, cumsum(nn))
  Inn <- diag(max(nn))
  conv <- FALSE
  
  for (it in 1 : max.it) {
    if (p2 > 0) {
      b2x <- X2 %*% b2[1:p2]
    }
    eb2x <- exp(b2x)
    sigma.zi <- sigma.z * Inn
    cov <- sigma.u %*% Dtt
    tcov <- Dtt2 %*% sigma.u
    DH <- Dnsf * rep(haz, each = ran)
    for (i in 1:n) {
      rv <- r[(cnn[i] + 1):cnn[i + 1]]
      ttv <- Dtt2[(cnn[i] + 1):cnn[i + 1], ]
      W21 <- cov[, (cnn[i] + 1):cnn[i + 1]]
      W12 <- tcov[(cnn[i] + 1):cnn[i + 1], ]
      if (model == "int") {
        W11 <- tcrossprod(ttv, W21) + sigma.zi[1:nn[i], 1:nn[i]]
      } else {
        W11 <- ttv %*% W21 + sigma.zi[1:nn[i], 1:nn[i]]
      }
      if (nn[i] == 1) {
        W3 <- W12/W11
        if (model == "int") {
          cvch <- sqrt((sigma.u - tcrossprod(W21, W3)) * 2)
        } else {
          cvch <- chol((sigma.u - tcrossprod(W21, W3)) * 2)
        }
        cm <- matrix(W3 * rv, gpt^ran, ran, byrow = TRUE)
      } else {
        W3 <- solve(W11, W12)
        if (model == "int") {
          cvch <- sqrt((sigma.u - W21 %*% W3) * 2)
        } else {
          cvch <- chol((sigma.u - W21 %*% W3) * 2)
        }
        cm <- matrix(rv %*% W3, gpt^ran, ran, byrow = TRUE)
      }
      newu <- gmat %*% cvch + cm
      newu2 <- newu^2
      if (model != "int") {
        newu2 <- cbind(newu2, newu[, s1] * newu[, s2])
      }
      egDUs <- 1
      if (cen[i] == 1) {
        egDUs <- exp(newu %*% (Dst[i, ] * b2[(p2 + 1):(p2 + lat)]))
      }
      egDUsf <- exp(newu %*% (Dsf[, 1:rs[i]] * b2[(p2 + 1):(p2 + lat)]))
      ess <- exp(-(eb2x[i, ] * egDUsf) %*% haz[1:rs[i]])
      f <- egDUs * ess * w
      den <- sum(f)
      EU[i, 1:ran] <- f[, 1] %*% newu/den
      EUU[i, 1:sum(1:ran)] <- f[, 1] %*% newu2/den
      C <- egDUsf[, 1:rs[i]]
      EexpU[i, 1:rs[i]] <- f[, 1] %*% C/den
      if (model == "int") {
        EUexpU[i, 1] <- sum(f[, 1] %*% (newu[, 1] * C) * haz[1:rs[i]])/den
        EUUexpU[i, 1] <- sum(f[, 1] %*% (newu[, 1]^2 * C) * haz[1:rs[i]])/den
      } else {
        EUexpU[i, 1:ran] <- rowSums(crossprod(newu * f[, 1], C) * 
                                      Dsf[, 1:rs[i]] * DH[, 1:rs[i]]) / den
        EUUexpU[i, 1:ran] <- rowSums(crossprod(newu2[, 1:ran] * f[, 1], C) *
                                       Dsf2[, 1:rs[i]] * DH[, 1:rs[i]]) / den
        if (model == "intslope") {
          EUUexpU[i, ran + 1] <- 2 * sum(f[, 1] %*% (newu2[, ran + 1] * C) * 
                                           haz[1:rs[i]] * sf[1:rs[i]]) / den
        } else {
          EUUexpU[i, (ran + 1):sum(1:ran)] <- 2 * 
            rowSums(crossprod(newu2[, (ran + 1):sum(1:ran)] * f[, 1], C) * 
                      Dsfc[, 1:rs[i]] * DH[, 1:rs[i]]) / den
        }
      }
    }
    parac <- data.frame(c(b1, b2, sigma.z, sigma.u))
    EexpUi <- colSums(t(EexpU) * haz)
    haz <- nev / colSums(EexpU * eb2x[, 1])
    EUmat <- apply(EU, 2, rep, nn)
    EUUmat <- apply(EUU, 2, rep, nn)
    Ut <- rowSums(EUmat * Dtt2)
    UUt <- rowSums(EUUmat[, 1:ran] * Dtt2^2)
    UUt2 <- 0
    if (model != "int") {
      UUt2 <- rowSums(EUUmat[, (ran + 1):sum(1:ran)] * Dttc)
    }
    b1 <- solve(crossprod(X1), crossprod(X1, Y - Ut))
    r <- Y - X1 %*% b1
    sigma.z <- sum(r^2 - 2 * r * Ut + UUt + 2 * UUt2) / N
    diag(sigma.u) <- colMeans(EUU)[1:ran]
    if (model != "int") {
      sigma.u[lower.tri(sigma.u)] <- colMeans(EUU)[-(1:ran)]
      sigma.u[upper.tri(sigma.u)] <- t(sigma.u)[upper.tri(sigma.u)]
    }
    fd <- vector("numeric", p2 + ran)
    sd <- matrix(0, p2 + ran, p2 + ran)
    fd[(p2 + 1):(p2 + ran)] <- colSums(cen * (EU * t(Ds))) - 
      colSums(eb2x[, 1] * EUexpU)
    if (model != "int") {
      inds1 <- c((p2 + 1):(p2 + ran), (p2 + 1):(p2 + ran))
      inds2 <- upper.tri(sd[(p2 + 1):(p2 + ran), (p2 + 1):(p2 + ran)])
      sd[inds1][inds2] <- -colSums(eb2x[, 1] * 0.5 * EUUexpU)[(ran + 1):sum(1:ran)]
    }
    if (p2 > 0) {
      fd[1:p2] <- c(colSums((cen * X2) - (X2 * eb2x[, 1] * EexpUi)))
      sd[(1:p2), (p2 + 1):(p2 + ran)] <- -t(X2) %*% (eb2x[, 1] * EUexpU)
      sd <- sd + t(sd)
      for (i in 1:p2) {
        for (j in 1:p2) {
          sd[i, j] <- -(sum(X2[, i] * X2[, j] * eb2x[, 1] * EexpUi))
        }
      }
    }
    if (model == "int") {
      sd[(p2 + 1), (p2 + 1)] <- -colSums(eb2x[, 1] * EUUexpU)[1:ran]
    } else {
      diag(sd[(p2 + 1):(p2 + ran), (p2 + 1):(p2 + ran)]) <- 
        -colSums(eb2x[, 1] * EUUexpU)[1:ran]
    }
    if (!sepassoc) {
      if (model == "int") {
        fd <- fd
        sd <- sd
      } else {
        fd[p2 + 1] <- sum(fd[(p2 + 1):(p2 + ran)])
        fd <- fd[1:(p2 + 1)]
        if (p2 > 1) {
          sd[1:p2, p2 + 1] <- rowSums(sd[(1:p2), (p2 + 1):(p2 + ran)])
        } else {
          sd[1:p2, p2 + 1] <- sum(sd[(1:p2), (p2 + 1):(p2 + ran)])
        }
        sd[p2 + 1, 1:p2] <- sd[1:p2, p2 + 1]
        sd[p2 + 1, p2 + 1] <- sum(sd[(p2 + 1):(p2 + ran), (p2 + 1):(p2 + ran)])
        sd <- sd[1:(p2 + 1), 1:(p2 + 1)]
      }
    }
    b2 <- b2 - solve(sd, fd)
    para <- data.frame(c(b1, b2, sigma.z, sigma.u))
    dd <- abs(parac - para)
    if (max(dd) < tol) {
      conv <- TRUE
      break
    }
  }
  if (conv != TRUE) {
    print("Not converged")
  }
  
  list(b1 = data.frame(b1), b2 = data.frame(b2), sigma.z = sigma.z, 
       sigma.u = sigma.u, haz = haz, random = EU, conv = conv, 
       iters = it)
  
}

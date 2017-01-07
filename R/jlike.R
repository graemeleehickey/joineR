#' @keywords internal
#' @importFrom statmod gauss.quad.prob
jlike <- function(longdat, survdat, model, ran, lat, sepassoc,
                  likeests, lgpt) {
  
  id <- longdat[, 1]
  Y <- longdat[, 2]
  tt <- longdat[, 3]
  X1 <- as.matrix(longdat[, 4:dim(longdat)[2]])
  n <- length(survdat[, 2])
  s <- survdat[, 2]
  cen <- survdat[, 3]
  nn <- diff(match(unique(id), id))
  nn[length(nn) + 1] <- length(id) - sum(nn)
  p1 <- dim(longdat)[2] - 3
  p2 <- dim(survdat)[2] - 3
  X2 <- 0
  if (p2 > 0) {
    X2 <- as.matrix(survdat[, 4:dim(survdat)[2]])
  }
  b1 <- likeests$b1[, 1]
  sigma.u <- likeests$sigma.u
  sigma.z <- likeests$sigma.z
  b2 <- likeests$b2[, 1]
  haz <- likeests$haz
  sf <- likeests$sf
  rs <- likeests$rs
  N <- sum(nn)
  g <- statmod::gauss.quad.prob(lgpt, "normal", sigma = sqrt(0.5))
  ab <- g$nodes
  w <- g$weights * sqrt(pi)
  gmat <- matrix(0, lgpt^ran, ran)
  gmat[, 1] <- rep(ab, each = lgpt^(ran - 1))
  if (model != "int") {
    gmat[, 2] <- rep(ab, lgpt)
    w <- as.vector(w %x% w)
  }
  if (model == "quad") {
    gmat[, 3] <- rep(ab, each = lgpt)
    w <- as.vector(w %x% g$weights * sqrt(pi))
  }
  l1 <- 0
  l2 <- 0
  r <- Y - X1 %*% b1
  Dtt <- getD(ran, tt)
  Dtt2 <- t(Dtt)
  Ds <- getD(ran, s)
  Dst <- t(Ds)
  Dsf <- getD(ran, sf)
  cnn <- c(0, cumsum(nn))
  b2x <- X2 %*% b2[1:p2]
  if (p2 == 0) {
    b2x <- matrix(0, n, 1)
  }
  sigma.zi <- sigma.z * diag(max(nn))
  cov <- sigma.u %*% Dtt
  tcov <- Dtt2 %*% sigma.u
  
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
      W3 <- W12 / W11
      if (model == "int") {
        cvch <- sqrt((sigma.u - tcrossprod(W21, W3)) * 2)
      } else {
        cvch <- chol((sigma.u - tcrossprod(W21, W3)) * 2)
      }
      cm <- matrix(W3 * rv, lgpt^ran, ran, byrow = TRUE)
    } else {
      W3 <- solve(W11, W12)
      if (model == "int") {
        cvch <- sqrt((sigma.u - W21 %*% W3) * 2)
      } else {
        cvch <- chol((sigma.u - W21 %*% W3) * 2)
      }
      cm <- matrix(rv %*% W3, lgpt^ran, ran, byrow = TRUE)
    }
    newu <- gmat %*% cvch + cm
    DUs <- newu %*% Ds[, i]
    DUsf <- newu %*% Dsf[, 1:rs[i]]
    ss <- exp(b2x[i, ]) * 
      exp(newu %*% (Dsf[, 1:rs[i]] * b2[(p2 + 1):(p2 + lat)])) %*% haz[1:rs[i]]
    den <- sum(
      exp(cen[i] * (newu %*% (Dst[i, ] * b2[(p2 + 1):(p2 + lat)]) + b2x[i, ])) * 
        (haz[rs[i]]^cen[i]) * w * exp(-ss))
    l2 <- l2 + 0
    if (den > 0) {
      l2 <- l2 + log(den)
    }
    l1 <- l1 - nn[i] * 0.5 * log(2 * pi) - 0.5 * log(det(W11)) - 
      0.5 * sum(rv * solve(W11, rv))
  }
  
  ll <- l1 + l2 - 0.5 * ran * n * log(pi)
  
  list(log.like = ll, longlog.like = l1, survlog.like = ll - l1)
  
}
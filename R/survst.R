#' @keywords internal
survst <- function(survdat, surv.formula, survdat2) {
  
  survdat2 <- survdat2[order(survdat2[, 2]), ]
  n <- length(survdat[, 2])
  s <- survdat[, 2]
  cen <- survdat[, 3]
  cen[1] <- 1
  survdat[1, 3] <- 1
  survdat2[1, 3] <- 1
  p2 <- dim(survdat)[2] - 3
  surv.start <- survival::coxph(surv.formula, data = survdat2, x = TRUE)
  surv.start.f <- survival::survfit(surv.start)
  sf <- surv.start.f$time[surv.start.f$n.event != 0]
  nf <- length(sf)
  nev <- surv.start.f$n.event[surv.start.f$n.event != 0]
  if (p2 > 0) {
    haz <- survival::coxph.detail(surv.start)$hazard
  } else {
    haz <- (surv.start.f$n.event) / (surv.start.f$n.risk)
    haz <- haz[surv.start.f$n.event > 0]
  }
  rs <- rep(1:nf, c(diff(match(sf, s)), n + 1 - match(sf, s)[nf]))
  b2 <- coef(surv.start)
  ll <- surv.start$loglik - sum(cen)
  
  out <- list("b2" = b2,
       "haz" = haz,
       "rs" = rs,
       "sf" = sf,
       "nev" = nev,
       "log.like" = ll)
  
  return(out)
  
}


#' @keywords internal
survstCR <- function(survdat, event) {
  
  n <- length(survdat[, 2])
  surv.time <- survdat[, 2]
  cen <- survdat[ , ifelse(event == 1, 3, 4)]
  #if(cen[1] == 0) {
  #  cen[1] <- 1
  #}
  p2 <- dim(survdat)[2] - 5
  if (p2 == 0) {
    surv.start <- survival::coxph(Surv(surv.time, cen) ~ 0)
  } else {
    X2 <- as.matrix(survdat[, 6:dim(survdat)[2]])
    surv.start <- survival::coxph(Surv(surv.time, cen) ~ X2)
  }
  alpha.0 <- survival::basehaz(surv.start, FALSE)
  l <- length(alpha.0[, 2])
  haz <- vector("numeric", l)
  s.dist <- vector("numeric", l)
  haz[1] <- alpha.0[1, 1]
  haz[2:l] <- diff(alpha.0[, 1])
  s.dist <- alpha.0[, 2]
  id.1 <- match(surv.time[1:n], s.dist)
  dummy <- match(s.dist[1:l], surv.time)
  d.dummy <- diff(dummy)
  id.2 <- rep(id.1[dummy[1:(l-1)]], d.dummy[1:(l-1)])
  id.3 <- rep(id.1[dummy[l]], (n - dummy[l] + 1))
  id.4 <- vector("numeric", n)
  id.4 <- c(id.2, id.3)
  id.5 <- c(rep(0, match(1, id.1) - 1), id.4)
  ll <- surv.start$loglik
  
  out <- list(
    "b2" = data.frame(c(coef(surv.start), 0)),
    "haz" = data.frame(haz),
    "id" = data.frame(id.5),
    "s.dist" = data.frame(s.dist),
    "ll" = data.frame(ll)
  )
  
  names(out) <- paste0(c("b2", "haz", "id", "s.dist", "ll"),
                       ifelse(event == 1, ".a", ".b"))
  return(out)
  
}

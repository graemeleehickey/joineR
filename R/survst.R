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
  
  list("b2" = b2,
       "haz" = haz,
       "rs" = rs,
       "sf" = sf,
       "nev" = nev,
       "log.like" = ll)
  
}
#' Internal function for calculating the initial parameters of the longitudinal
#' data sub-model
#' 
#' @keywords internal
longst <- function(longdat, long.formula, model, longdat2) {
  
  if (model == "int") {
    rf <- as.formula(paste("~1", colnames(longdat)[1], sep = "|"))
    long.start <- lme(long.formula, random = rf, method = "ML", 
                      data = data.frame(longdat2), na.action = na.omit)
  } else if (model == "intslope") {
    rf <- as.formula(paste(paste0("~", colnames(longdat)[3]),
                           colnames(longdat)[1], sep = "|"))
    long.start <- lme(long.formula, random = rf, method = "ML", 
                      data = data.frame(longdat2), na.action = na.omit)
  } else {
    tsq <- paste0(paste0("I(", paste(colnames(longdat)[3], "^2", sep = "")), ")")
    rf <- as.formula(paste(paste0("~", 
                                  paste(colnames(longdat)[3], tsq, sep = "+")), 
                           colnames(longdat)[1], sep = "|"))
    long.start <- lme(long.formula, random = rf, method = "ML", 
                      data = data.frame(longdat2), na.action = na.omit)
  }
  
  q <- dim(VarCorr(long.start))[1] - 1
  sigma.u <- diag(as.double(VarCorr(long.start)[1:q, 1]), q, q)
  
  if (q > 1) {
    vv <- tcrossprod(diag(sqrt(sigma.u))) * lower.tri(sigma.u)
    rho <- as.double(VarCorr(long.start, rdig = 8)[-c(1, q + 1), -(1:2)])
    rho <- rho[!is.na(rho)]
    vars <- diag(sigma.u)
    sigma.u[lower.tri(sigma.u)] <- vv[lower.tri(vv)] * rho
    sigma.u <- sigma.u + t(sigma.u) - diag(vars)
  }
  
  rownames(sigma.u) <- paste("U_", 0:(q - 1), sep = "")
  colnames(sigma.u) <- paste("U_", 0:(q - 1), sep = "")
  sigma.z <- long.start$sigma^2
  ll <- long.start$logLik
  b1 <- fixef(long.start)
  
  list(b1 = data.frame(b1), sigma.z = sigma.z, sigma.u = sigma.u, log.like = ll)
  
}

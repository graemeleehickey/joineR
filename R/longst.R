#' @keywords internal
#' @import nlme
longst <- function(longdat, long.formula, model, longdat2) {
  
  if (model == "int") {
    rf <- as.formula(
      paste("~1", colnames(longdat)[1], sep = "|"))
  } else if (model == "intslope") {
    rf <- as.formula(
      paste(paste0("~", colnames(longdat)[3]), colnames(longdat)[1], sep = "|"))
  } else {
    tsq <- paste0(paste0("I(", paste(colnames(longdat)[3], "^2", sep = "")), ")")
    rf <- as.formula(
      paste(paste0("~", paste(colnames(longdat)[3], tsq, sep = "+")),
            colnames(longdat)[1], sep = "|"))
  }
  
  long.start <- nlme::lme(long.formula,
                          random = rf,
                          method = "ML", 
                          data = data.frame(longdat2),
                          na.action = na.omit,
                          control = lmeControl(maxIter = 100, 
                                               msMaxIter = 100,
                                               opt = "optim"))
  
  q <- dim(nlme::VarCorr(long.start))[1] - 1
  sigma.u <- as.matrix(nlme::getVarCov(long.start))
  rownames(sigma.u) <- paste("U_", 0:(q - 1), sep = "")
  colnames(sigma.u) <- paste("U_", 0:(q - 1), sep = "")
  sigma.z <- long.start$sigma^2
  ll <- long.start$logLik
  b1 <- nlme::fixef(long.start)
  
  list("b1" = data.frame(b1),
       "sigma.z" = sigma.z,
       "sigma.u" = sigma.u,
       "log.like" = ll)
  
}

#' @keywords internal
#' @import nlme
longstCR <- function(longdat, long.formula, longdat2) {
  
  rf <- as.formula(paste(paste0("~", colnames(longdat)[3]),
                         colnames(longdat)[1], sep = "|"))
  
  long.start <- nlme::lme(long.formula,
                          random = rf,
                          method = "ML",
                          data = data.frame(longdat2),
                          na.action = na.omit,
                          control = nlme::lmeControl(maxIter = 100,
                                                     msMaxIter = 100,
                                                     opt = "optim"))
  
  q <- dim(nlme::VarCorr(long.start))[1] - 1
  sigma.u <- as.matrix(nlme::getVarCov(long.start))
  rownames(sigma.u) <- paste("U_", 0:(q - 1), sep = "")
  colnames(sigma.u) <- paste("U_", 0:(q - 1), sep = "")
  corr <- as.numeric(nlme::VarCorr(long.start)[2, 3])
  varz <- long.start$sigma^2
  ll <- long.start$logLik
  b1 <- nlme::fixef(long.start)
  
  list("b1" = data.frame(b1),
       "varz" = varz,
       "sigma.u" = sigma.u,
       "corr" = corr,
       "log.like" = ll)
  
}

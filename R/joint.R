joint <- function(data, long.formula, surv.formula, 
                  model = c("intslope", "int", "quad"), 
                  sepassoc = FALSE, longsep = FALSE, survsep = FALSE, 
                  gpt, lgpt, max.it, tol) {
  
  id <- data$subj.col
  time.long <- data$time.col
  
  # Control parameters
  if (missing(gpt)) {
    gpt <- 3
  }
  if (missing(lgpt)) {
    lgpt <- 10
  }
  if (missing(max.it)) {
    max.it <- 200
  }
  if (missing(tol)) {
    tol <- 0.001
  }
  
  Call <- match.call()
  
  if (any(sapply(data$baseline, "class") == "factor")) {
    data$baseline <- drop.levels(data$baseline)
  }
  
  # Longitudinal data
  long.data <- merge(data$longitudinal, data$baseline, by = id, sort = FALSE)
  long.frame <- model.frame(long.formula, data = long.data)
  long.cov <- model.matrix(long.formula, long.frame)
  long.terms <- terms(long.formula, data = long.data)
  long.names <- colnames(long.cov)
  rll <- !is.na(data$longitudinal[[names(long.frame[1])]])
  longdat <- cbind(data$longitudinal[[id]][rll], 
                   long.frame[, 1], 
                   data$longitudinal[[time.long]][rll], 
                   long.cov)
  longdat <- as.data.frame(longdat)
  names(longdat) <- c(id, names(long.frame)[1], time.long, long.names)
  
  # Survival data
  surv.frame <- model.frame(surv.formula, 
                            data = cbind(data$survival, data$baseline))
  srv <- model.extract(surv.frame, "response")
  surv.terms <- terms(surv.formula, 
                      data = cbind(data$survival, data$baseline))
  attr(surv.terms, "intercept") <- 1
  surv.cov <- model.matrix(surv.terms, 
                           data = cbind(data$survival, data$baseline))
  surv.cov <- as.matrix(surv.cov[, -1])
  rss <- as.integer(row.names(surv.cov))
  survdat <- cbind(data$survival[[id]][rss], srv[rss, 1], 
                   srv[rss, 2], surv.cov[rss, ])
  survdat <- as.data.frame(survdat)
  names(survdat) <- c(id, surv.formula[2][[1]][[2]], surv.formula[2][[1]][[3]], 
                      colnames(surv.cov))    
  if (dim(survdat)[2] > 3) {
    survdat[, 4:dim(survdat)[2]] <- scale(survdat[, 4:dim(survdat)[2]], scale = FALSE)
  }
  survdat2 <- data.frame(data$survival[[id]][rss], srv[rss, 1], 
                         srv[rss, 2], surv.frame[, -1])
  names(survdat2) <- c(id, surv.formula[2][[1]][[2]], surv.formula[2][[1]][[3]], 
                       attr(surv.terms, "term.labels"))
  
  # Latent association structure
  model <- match.arg(model)
  if (model != "intslope" && model != "int" && model != "quad") {
    stop(paste("Unknown model:", model))
  }
  ran <- 2
  if (model == "int") {
    ran <- 1
  }
  if (model == "quad") {
    ran <- 3
  }
  lat <- ran
  if (!sepassoc) {
    lat <- 1
  }
  
  # Sort the data
  sort.dat <- function(longdat, survdat) {
    longid <- longdat[, 1]
    nn <- diff(match(unique(longid), longid))
    nn[length(nn) + 1] <- length(longid) - sum(nn)
    svec <- rep(survdat[, 2], nn)
    sort.long <- longdat[order(svec), ]
    os <- order(survdat[, 2])
    sort.surv <- survdat[os, ]
    list(long.s = data.frame(sort.long), surv.s = data.frame(sort.surv))
  }
  sort <- sort.dat(longdat, survdat)
  longdat <- as.matrix(sort$long.s)
  survdat <- as.matrix(sort$surv.s)
  p2 <- dim(survdat)[2] - 3
  ldaests <- longst(longdat, long.formula, model = model, long.data)
  survests <- survst(survdat, surv.formula, survdat2)
  sep.ll <- ldaests$log.like + survests$log.like[2]
  sep.loglik <- list(seplhood = sep.ll, sepy = ldaests$log.like, 
                     sepn = survests$log.like[2])
  paraests <- c(ldaests, survests)
  
  # Run the EM algorithm
  jointfit <- em.alg(longdat, survdat, ran, paraests, gpt, max.it, tol)
  
  # Extract MLEs
  likeests <- c(jointfit, list(rs = survests$rs, sf = survests$sf))
  b1 <- jointfit$b1
  sigma.u <- jointfit$sigma.u
  rownames(b1) <- rownames(paraests$b1)
  if (p2 > 0) {
    b2 <- jointfit$b2[1:p2, ]
    names(b2) <- names(paraests$b2)
  } else {
    b2 <- NULL
  }
  fixed <- list(longitudinal = b1, survival = b2)
  latent <- jointfit$b2[(p2 + 1):(p2 + lat), ]
  names(latent) <- paste0("gamma_", 0:(lat - 1))
  random <- jointfit$random
  colnames(random) <- paste0("U_", 0:(ran - 1))
  rownames(random) <- survdat[, 1]
  coefficients <- list(fixed = fixed, random = random, latent = latent)
  
  # Log-likelihood at MLE
  jointll <- jlike(longdat, survdat, ran, likeests, lgpt)
  loglik <- list(jointlhood = jointll$log.like, jointy = jointll$longlog.like, 
                 jointn = jointll$survlog.like)
  
  # Separate model estimates
  sepests <- list(longests = sep(ldaests, longsep), 
                  survests = sep(survests, survsep))
  
  # Return
  results <- list(coefficients = coefficients, sigma.z = jointfit$sigma.z, 
                  sigma.u = jointfit$sigma.u, hazard = jointfit$haz, loglik = loglik, 
                  numIter = jointfit$iters, convergence = jointfit$conv, 
                  model = model, sepassoc = sepassoc, sepests = sepests, 
                  sep.loglik = sep.loglik, 
                  formulae = list(lformula = long.formula, sformula = surv.formula),
                  data = data, call = Call)
  class(results) <- "joint"
  return(results)
  
}

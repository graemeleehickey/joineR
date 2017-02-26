#' @import stats
#' @export
cjoint <- function(data, long.formula, surv.formula,
                    longsep = FALSE, survsep = FALSE,
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
    data$baseline <- droplevels(data$baseline)
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
  names(longdat) <- c(id,
                      names(long.frame)[1],
                      time.long,
                      long.names)

  # Sample sizes
  longid <- longdat[, 1]
  nn <- diff(match(unique(longid), longid))
  nn[length(nn) + 1] <- length(longid) - sum(nn)
  nn <- data.frame(unique(longid), nn)
  colnames(nn) <- c(id, "nobs")

  # Survival data
  event <- data$survival[, as.character(surv.formula[[2]][[3]])]
  if (!all(unique(event) == 0:2)) {
    stop("Competing risks data must be coded as 0 (censored), 1 and 2 (causes)")
  }
  event1 <- as.numeric(event == 1)
  event2 <- as.numeric(event == 2)
  survdat <- cbind(
    data$survival[, c(id, as.character(surv.formula[[2]][[2]])), ],
    event1, event2)
  survdat <- merge(survdat, nn)

  surv.formula1 <- surv.formula
  surv.formula1[[2]][[3]] <- as.symbol("event1")
  surv.terms <- terms(surv.formula1,
                      data = cbind(survdat, data$baseline))
  surv.cov <- model.matrix(surv.terms,
                           data = cbind(survdat, data$baseline))
  surv.cov <- as.data.frame(surv.cov[, -1, drop = FALSE])
  survdat <- cbind(survdat, surv.cov)
  p2 <- dim(survdat)[2] - 5
  if (p2 > 0) {
    survdat[, 6:dim(survdat)[2]] <- scale(survdat[, 6:dim(survdat)[2]],
                                          scale = FALSE)
  }

  # Sorting survival & longitudinal data by survival time
  sortcr.dat <- function(longdat, survdat) {
    sort.long <- matrix(0, dim(longdat)[1], dim(longdat)[2])
    index <- rep(survdat[, 2], survdat[, 5])
    sort.long <- longdat[order(index), ]
    sort.surv <- survdat[order(survdat[, 2]), ]
    list("long.s" = sort.long,
         "surv.s" = sort.surv)
  }

  # Run the EM algorithm
  sort <- sortcr.dat(longdat, survdat)
  ldaests <- longstCR(longdat, long.formula, long.data)
  ran <- dim(ldaests$sigma.u)[1]
  longdat <- as.matrix(sort$long.s)
  survdat <- as.matrix(sort$surv.s)
  survests.a <- survstCR(survdat, event = 1)
  survests.b <- survstCR(survdat, event = 2)
  paraests <- c(ldaests, survests.a, survests.b)
  jointfit <- emUpdateCR(longdat, survdat, paraests, gpt, lgpt, max.it, tol)

  # Process output
  b1 <- jointfit$b1
  rownames(b1) <- rownames(paraests$b1)
  sigma.u <- jointfit$sigma.u
  rownames(sigma.u) <- colnames(sigma.u) <- rownames(ldaests$sigma.u)
  if (p2 > 0) {
    b2a <- jointfit$b2a[1:p2, ]
    b2b <- jointfit$b2b[1:p2, ]
    rownames(b2a) <- rownames(b2b) <- rownames(paraests$b2a[1:p2, ])
  } else {
    b2a <- b2b <- NULL
  }
  fixed <- list(longitudinal = b1,
                survival1 = b2a,
                survival2 = b2b)
  latent <- with(jointfit, c(b2a[(p2 + 1), ],  b2b[(p2 + 1), ]))
  names(latent) <- paste0("gamma_", 1:2)
  random <- jointfit$random
  colnames(random) <- paste0("U_", 0:(ran - 1))
  rownames(random) <- survdat[, 1]
  coefficients <- list(fixed = fixed,
                       random = random,
                       latent = latent)

  loglik <- NULL
  sepests <- NULL
  sep.loglik <- NULL

  # Return
  results <- list(coefficients = coefficients,
                  sigma.z = jointfit$sigma.z,
                  sigma.u = sigma.u,
                  haz.a = jointfit$haz.a,
                  haz.b = jointfit$haz.b,
                  loglik = loglik,
                  numIter = jointfit$iters,
                  conv = jointfit$conv,
                  sepests = sepests,
                  sep.loglik = sep.loglik,
                  formulae = list(lformula = long.formula,
                                  sformula = surv.formula),
                  data = data,
                  call = Call)

  class(results) <- c("list", "cjoint")
  return(results)

}





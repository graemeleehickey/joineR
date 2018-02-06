#' @keywords internal
prepSurvData <- function(surv.formula, data, id, time.long) {
  
  # Test for compRisk
  event <- data$survival[, as.character(surv.formula[[2]][[3]])]
  ctest <- length(unique(event))
  if (ctest > 3) {
    stop("More than 3 event types detected; joineR can only model 2 competing risks")
  } else if (ctest == 3) {
    compRisk <- TRUE
    message("Competing risks detected")
  } else if (ctest == 2) {
    compRisk <- FALSE
  }
  
  if (!compRisk) {
    # single event time
    surv.frame <- model.frame(surv.formula,
                              data = cbind(data$survival, data$baseline))
    srv <- model.extract(surv.frame, "response")
    surv.terms <- terms(surv.formula,
                        data = cbind(data$survival, data$baseline))
    attr(surv.terms, "intercept") <- 1
    surv.cov <- model.matrix(surv.terms,
                             data = cbind(data$survival, data$baseline))
    surv.cov <- surv.cov[, -1, drop = FALSE]
    rss <- as.integer(row.names(surv.cov))
    survdat <- data.frame(data$survival[[id]][rss],
                          srv[rss, 1],
                          srv[rss, 2],
                          surv.cov[rss, ],
                          stringsAsFactors = FALSE)
    survdat <- as.data.frame(survdat)
    names(survdat) <- c(id,
                        surv.formula[2][[1]][[2]],
                        surv.formula[2][[1]][[3]],
                        colnames(surv.cov))
    p2 <- dim(survdat)[2] - 3
    if (p2 > 0) {
      survdat[, 4:dim(survdat)[2]] <- scale(survdat[, 4:dim(survdat)[2]],
                                            scale = FALSE)
    }
    survdat2 <- data.frame(data$survival[[id]][rss],
                           srv[rss, 1],
                           srv[rss, 2],
                           surv.frame[, -1],
                           stringsAsFactors = FALSE)
    names(survdat2) <- c(id,
                         surv.formula[2][[1]][[2]],
                         surv.formula[2][[1]][[3]],
                         attr(surv.terms, "term.labels"))
  } else {
    # competing risks
    event <- data$survival[, as.character(surv.formula[[2]][[3]])]
    if (!all(unique(event) %in% 0:2)) {
      stop("Competing risks data must be coded as 0 (censored), 1 and 2 (causes)")
    }
    surv.formula1 <- surv.formula
    surv.formula1[[2]][[3]] <- as.symbol("event1")
    event1 <- as.numeric(event == 1)
    event2 <- as.numeric(event == 2)
    survdat <- cbind(
      data$survival[, c(id, as.character(surv.formula[[2]][[2]])), ],
      event1,
      event2)
    surv.terms <- terms(surv.formula1,
                        data = cbind(survdat, data$baseline))
    attr(surv.terms, "intercept") <- 1
    surv.frame <- model.frame(surv.formula1,
                              data = cbind(survdat, data$baseline))
    surv.cov <- model.matrix(surv.terms,
                             data = cbind(survdat, data$baseline))
    surv.cov <- as.data.frame(surv.cov[, -1, drop = FALSE])
    rss <- as.integer(row.names(surv.cov))
    survdat <- survdat[rss, ]
    survdat2 <- survdat
    survdat <- cbind(survdat, surv.cov[rss, ])
    p2 <- dim(survdat)[2] - 4
    if (p2 > 0) {
      survdat[, 5:dim(survdat)[2]] <- scale(survdat[, 5:dim(survdat)[2]],
                                            scale = FALSE)
    }
    survdat2 <- cbind(survdat2, surv.frame[rss, ])
    if (p2 > 0) {
      names(survdat2)[(ncol(survdat) + 1):ncol(survdat2)] <- attr(surv.terms, "term.labels")
    }
  }  
  
  out <- list(p2 = p2,
              survdat = survdat,
              survdat2 = survdat2,
              compRisk = compRisk)
  
  return(out)
  
}
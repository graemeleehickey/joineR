#' Summarise a random effects joint model fit
#'
#' Generic function used to produce summary information from a fitted random
#' effects joint model as represented by \code{object} of class \code{joint}.
#'
#' @param object an object of class \code{joint}.
#' @param variance should the variance components be output as variances or
#'   standard deviations? Defaults to \code{variance = TRUE}.
#' @param ... further arguments for the summary.
#'
#' @author Pete Philipson (\email{pete.philipson@@northmbria.ac.uk})
#' @keywords methods
#'
#' @return An object inheriting from class \code{summary.joint} with all
#'   components included in \code{object} (see \code{\link{joint}} for a full
#'   description of the components) plus the following components:
#'
#'   \item{\code{nobs}}{the total number of (typically longitudinal)
#'   observations (i.e. rows in an unbalanced data set).}
#'
#'   \item{\code{ngrps}}{the number of groups in the analyzed dataset, often
#'   individual subjects.}
#' @export
#'
#' @examples
#'
#' data(heart.valve)
#' heart.surv <- UniqueVariables(heart.valve,
#'                               var.col = c("fuyrs","status"),
#'                               id.col = "num")
#' heart.long <- heart.valve[, c("num", "time", "log.lvmi")]
#' heart.cov <- UniqueVariables(heart.valve,
#'                              c("age", "hs", "sex"),
#'                              id.col = "num")
#' heart.valve.jd <- jointdata(longitudinal = heart.long,
#'                             baseline = heart.cov,
#'                             survival = heart.surv,
#'                             id.col = "num",
#'                             time.col = "time")
#' fit <- joint(data = heart.valve.jd,
#'              long.formula = log.lvmi ~ 1 + time + hs,
#'              surv.formula = Surv(fuyrs,status) ~ hs,
#'              model = "intslope")
#' summary(fit)
summary.joint <- function(object, variance = TRUE, ...) {
  
  cat("\nCall:\n", paste(deparse(object$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  
  # Random effects
  cat("Random effects joint model\n")
  model <- object$model
  cat(" Data:", deparse(object$call$data), "\n")
  cat(" Log-likelihood:", object$loglik$jointlhood, "\n")
  cat("\n")
  
  # Longitudinal sub-model
  cat("Longitudinal sub-model fixed effects:", deparse(object$formulae$lformula))
  lfixed <- object$coefficients$fixed$longitudinal
  names(lfixed) <- ""
  print(lfixed)
  cat("\n")
  
  # Survival sub-model
  if (class(object)[2] == "joint") {
    # single event time
    cat("Survival sub-model fixed effects:", deparse(object$formulae$sformula))
    sfixed <- data.frame(object$coefficients$fixed$survival)
    if (sum(dim(sfixed)) == 0) {
      cat("\n", "No survival baseline covariates specified", "\n")
    } else {
      names(sfixed) <- ""
      print(sfixed)
    }
    cat("\n")
  } else {
    # competing risks
    cat("Competing risks sub-model effects:", deparse(object$formulae$sformula), "\n")
    sfixed1 <- data.frame(object$coefficients$fixed$survival1)
    sfixed2 <- data.frame(object$coefficients$fixed$survival2)
    if (sum(dim(sfixed1)) == 0) {
      cat("No survival baseline covariates specified", "\n")
    } else {
      names(sfixed1) <- names(sfixed2) <- ""
      cat("Failure cause 1:")
      print(sfixed1)
      cat("Failure cause 2:")
      print(sfixed2)
    }
    cat("\n")
  }
  
  # Latent associations
  cat("Latent association:")
  lat <- data.frame(object$coefficients$latent)
  names(lat) <- ""
  print(lat)
  cat("\n")
  
  # Variances
  cat("Variance components:\n")
  sigu <- diag(object$sigma.u)
  names(sigu) <- rownames(object$sigma.u)
  sigz <- object$sigma.z
  names(sigz) <- "Residual"
  vars <- c(sigu, sigz)
  names(vars) <- c(names(sigu), names(sigz))
  if (!variance) {
    vars <- sqrt(vars)
  }
  print(vars)
  if (!variance) {
    cat(" Note: the above are standard deviations\n")
  }
  cat("\n")
  
  # Convergence
  if (object$convergence) {
    cat("Convergence at iteration:", object$numIter,"\n")
  } else {
    cat("Convergence not achieved\n")
  }
  cat("\n")
  
  # Data summaries
  cat("Number of observations:", dim(object$data$longitudinal)[1],"\n")
  cat("Number of groups:", dim(object$data$survival)[1],"\n")
  object$nobs <- dim(object$data$longitudinal)[1]
  object$ngrps <- dim(object$data$survival)[1]
  class(object) <- c("summary.joint")
  
  invisible(object)
  
}
